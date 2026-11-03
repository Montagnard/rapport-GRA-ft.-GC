import math

import pandas as pd

SECTORS = [
    "Luxury cars",
    "Personal luxury goods",
    "Hospitality (hotels, cruises)",
    "Yachts",
    "Fine wines and spirits",
    "Gourmet food and fine dining",
    "Design and furniture",
]

SPENDING_COLUMN = "EU-27 spending (EUR bn/yr)"

# EU-27 spending per sector in EUR bn/yr; values are filled in by later steps
eu27_spending = {sector: None for sector in SECTORS}

# Share of European values attributed to the EU-27
EUROPE_TO_EU27 = 0.74

# ECCIA/Bain (2025): global market size in EUR bn and share bought in Europe
ECCIA_GLOBAL = {
    "Personal luxury goods": 363,
    "Fine wines and spirits": 99,
    "Gourmet food and fine dining": 72,
    "Design and furniture": 51,
    "Hospitality (hotels, cruises)": 242,
}
EUROPE_SHARE = 0.30
for sector, global_value in ECCIA_GLOBAL.items():
    eu27_spending[sector] = global_value * EUROPE_SHARE * EUROPE_TO_EU27

# Luxury cars: only the part of each car's net price above the threshold is taxed
CAR_REGISTRATIONS = 10_822_831  # new passenger car registrations in the EU, 2025 (ACEA 2025)
CAR_MEAN_NET_PRICE = 32_000     # assumed average net price in EUR
CAR_PRICE_SIGMA = 0.65          # sigma of the assumed log-normal price distribution
CAR_THRESHOLD = CAR_MEAN_NET_PRICE


def normal_cdf(x):
    return 0.5 * (1 + math.erf(x / math.sqrt(2)))


def lognormal_excess_above(mean, sigma, threshold):
    """Expected value of max(price - threshold, 0) for a log-normal price with the given mean."""
    mu = math.log(mean) - sigma**2 / 2
    d1 = (mu + sigma**2 - math.log(threshold)) / sigma
    d2 = d1 - sigma
    return mean * normal_cdf(d1) - threshold * normal_cdf(d2)


car_market_volume = CAR_REGISTRATIONS * CAR_MEAN_NET_PRICE / 1e9  # EUR bn
car_excess_share = lognormal_excess_above(CAR_MEAN_NET_PRICE, CAR_PRICE_SIGMA, CAR_THRESHOLD) / CAR_MEAN_NET_PRICE
eu27_spending["Luxury cars"] = car_market_volume * car_excess_share

# Yachts: annual consumption flow (r + delta) * K + OPEX, with OPEX as a share of K
YACHT_R = 0.04
SUPERYACHTS_GLOBAL = 6_174  # yachts over 30 m in operation globally (SuperYacht Times 2025)
SUPERYACHT_EU_TIME = 0.32   # assumed effective share of time spent in EU waters

# size class: share of superyacht fleet, value per yacht in EUR m (loosely based on yacht
# broker listings across ages), delta, OPEX share of K (Fraser Yachts: 10-15% of purchase value)
SUPERYACHT_CLASSES = {
    "30-40 m": {"fleet_share": 0.63, "value": 15, "delta": 0.09, "opex": 0.15},
    "40-50 m": {"fleet_share": 0.21, "value": 27, "delta": 0.08, "opex": 0.15},
    "50-80 m": {"fleet_share": 0.13, "value": 50, "delta": 0.07, "opex": 0.15},
    "over 80 m": {"fleet_share": 0.03, "value": 150, "delta": 0.06, "opex": 0.15},
}

# yachts per size class counted as full-year-equivalent presence in EU waters
yacht_classes = {
    size: {
        "count": SUPERYACHTS_GLOBAL * params["fleet_share"] * SUPERYACHT_EU_TIME,
        "value": params["value"],
        "delta": params["delta"],
        "opex": params["opex"],
    }
    for size, params in SUPERYACHT_CLASSES.items()
}

# Yachts of 10-30 m: marina berths per country (ICOMIA) times the share of berths used by 10-30 m yachts
MARINA_BERTHS = {
    "Sweden": 500_000,
    "France": 253_000,
    "Italy": 181_000,
    "Netherlands": 165_000,
    "Spain": 130_900,
    "Poland": 48_900,
    "Croatia": 18_200,
    "Greece": 15_000,
    "Portugal": 13_300,
    "Ireland": 10_000,
}
SMALL_YACHT_BERTH_SHARE = {
    "Italy": 0.28,        # Il Diporto Nautico
    "Sweden": 0.03,       # Transportstyrelsen survey: boats over 10 m
    "France": 0.28,       # assumption
    "Netherlands": 0.07,  # assumption
    "Spain": 0.15,        # assumption
    "Poland": 0.07,       # assumption
    "Croatia": 0.15,      # assumption
    "Greece": 0.15,       # assumption
    "Portugal": 0.15,     # assumption
    "Ireland": 0.07,      # assumption
}
small_yachts_per_country = {
    country: berths * SMALL_YACHT_BERTH_SHARE[country] for country, berths in MARINA_BERTHS.items()
}
yacht_classes["10-30 m"] = {
    "count": sum(small_yachts_per_country.values()),
    "value": 0.25,  # assumed average value per yacht in EUR m
    "delta": 0.10,
    "opex": 0.10,
}


def yacht_consumption_flow(classes):
    """Annual consumption flow in EUR bn summed over all size classes."""
    return sum(
        c["count"] * c["value"] * (YACHT_R + c["delta"] + c["opex"]) for c in classes.values()
    ) / 1_000


eu27_spending["Yachts"] = yacht_consumption_flow(yacht_classes)


def build_spending_table(spending):
    """Table 1: EU-27 spending per sector plus a total row, without decimals."""
    table = pd.DataFrame({
        "Sector": list(spending.keys()),
        SPENDING_COLUMN: pd.Series(list(spending.values()), dtype="float"),
    })
    total = table[SPENDING_COLUMN].sum(min_count=1)
    table.loc[len(table)] = ["Total", total]
    table[SPENDING_COLUMN] = table[SPENDING_COLUMN].round(0).astype("Int64")
    return table


MEMBER_STATES = [
    "Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", "Czechia", "Denmark",
    "Estonia", "Finland", "France", "Germany", "Greece", "Hungary", "Ireland",
    "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", "Netherlands",
    "Poland", "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden",
]

# Country allocation keys per sector: 
# each sector's key gives the member state's share of that sector's EU-27 spending and sums to 1.
# The keys are built on many assumptions (car registrations and premium-brand shares, tourist
# VAT refunds, luxury hotels and starred restaurants, wine consumption, yacht presence and
# charters, GDP, GNI and population) and anchor the allocation.
# Columns follow SECTORS: cars, personal goods, hospitality, yachts, wine, food, design
COUNTRY_SECTOR_KEYS = {
    "Austria":     (0.028760, 0.032473, 0.060837, 0.000000, 0.026496, 0.040204, 0.030858),
    "Belgium":     (0.071125, 0.033170, 0.023063, 0.003052, 0.027679, 0.050592, 0.038666),
    "Bulgaria":    (0.001509, 0.002709, 0.003312, 0.002035, 0.004638, 0.001574, 0.003185),
    "Croatia":     (0.002321, 0.003973, 0.008992, 0.061038, 0.005337, 0.005322, 0.003299),
    "Cyprus":      (0.000850, 0.002250, 0.002427, 0.010173, 0.001507, 0.001146, 0.001755),
    "Czechia":     (0.012223, 0.016579, 0.010079, 0.000000, 0.015654, 0.007087, 0.014338),
    "Denmark":     (0.023338, 0.023324, 0.014792, 0.005086, 0.021309, 0.021668, 0.028904),
    "Estonia":     (0.001596, 0.001837, 0.001766, 0.001017, 0.001657, 0.001139, 0.001733),
    "Finland":     (0.007625, 0.013019, 0.006598, 0.002035, 0.010113, 0.007589, 0.017046),
    "France":      (0.115224, 0.196974, 0.202249, 0.244151, 0.205861, 0.251402, 0.165445),
    "Germany":     (0.341054, 0.193868, 0.161239, 0.015259, 0.221492, 0.186577, 0.273263),
    "Greece":      (0.005374, 0.015499, 0.050815, 0.132248, 0.012155, 0.006478, 0.009214),
    "Hungary":     (0.005503, 0.008007, 0.009043, 0.000000, 0.011580, 0.005193, 0.008027),
    "Ireland":     (0.012498, 0.017042, 0.020019, 0.003052, 0.013085, 0.013246, 0.020428),
    "Italy":       (0.117998, 0.166286, 0.185668, 0.274669, 0.173864, 0.152504, 0.114299),
    "Latvia":      (0.000833, 0.001696, 0.002274, 0.000509, 0.001435, 0.001068, 0.001497),
    "Lithuania":   (0.001864, 0.002812, 0.002044, 0.000509, 0.002709, 0.002245, 0.003357),
    "Luxembourg":  (0.009663, 0.004395, 0.002878, 0.000000, 0.004666, 0.005313, 0.005331),
    "Malta":       (0.000295, 0.001908, 0.004273, 0.025432, 0.000847, 0.002522, 0.001186),
    "Netherlands": (0.050592, 0.065838, 0.033409, 0.020346, 0.052858, 0.058915, 0.079820),
    "Poland":      (0.036222, 0.023967, 0.016361, 0.002035, 0.017815, 0.013393, 0.033298),
    "Portugal":    (0.015841, 0.017312, 0.046759, 0.015259, 0.032872, 0.020072, 0.012236),
    "Romania":     (0.005572, 0.008500, 0.006487, 0.001017, 0.016990, 0.005199, 0.012173),
    "Slovakia":    (0.004359, 0.004002, 0.002508, 0.000000, 0.004784, 0.002221, 0.005341),
    "Slovenia":    (0.002879, 0.002745, 0.003997, 0.003052, 0.003366, 0.004068, 0.003246),
    "Spain":       (0.074105, 0.114012, 0.100993, 0.172940, 0.082763, 0.115637, 0.077026),
    "Sweden":      (0.050777, 0.025804, 0.017119, 0.005086, 0.026465, 0.017627, 0.035030),
}

# Luxury good base per country and sector in EUR bn
country_sector_base = {
    country: {sector: key * eu27_spending[sector] for sector, key in zip(SECTORS, keys)}
    for country, keys in COUNTRY_SECTOR_KEYS.items()
}

# GDP per country in EUR bn, 2024, current prices (Eurostat nama_10_gdp)
gdp = {
    "Austria": 481, "Belgium": 609, "Bulgaria": 102, "Croatia": 86, "Cyprus": 34,
    "Czechia": 318, "Denmark": 393, "Estonia": 39, "Finland": 276, "France": 2920,
    "Germany": 4306, "Greece": 237, "Hungary": 211, "Ireland": 533, "Italy": 2192,
    "Latvia": 40, "Lithuania": 78, "Luxembourg": 86, "Malta": 22, "Netherlands": 1120,
    "Poland": 848, "Portugal": 285, "Romania": 354, "Slovakia": 132, "Slovenia": 68,
    "Spain": 1593, "Sweden": 549,
}


def build_country_table(sector_base, gdp):
    """Table 2: luxury good base per member state, its revenue share, GDP share and their ratio."""
    base = {
        country: pd.Series(sectors, dtype="float").sum(min_count=1)
        for country, sectors in sector_base.items()
    }
    table = pd.DataFrame({
        "Country": list(base.keys()),
        "Base € bn": list(base.values()),
        "GDP € bn": pd.Series([gdp[country] for country in base], dtype="float"),
    })
    table = table.sort_values("Base € bn", ascending=False, ignore_index=True)
    table.loc[len(table)] = [
        "EU-27",
        table["Base € bn"].sum(min_count=1),
        table["GDP € bn"].sum(min_count=1),
    ]

    total_base = table["Base € bn"].iloc[-1]
    total_gdp = table["GDP € bn"].iloc[-1]
    table["Rev. %"] = table["Base € bn"] / total_base * 100
    table["GDP %"] = table["GDP € bn"] / total_gdp * 100
    table["Ratio"] = table["Rev. %"] / table["GDP %"]

    table = table.drop(columns="GDP € bn")
    return table.round({"Base € bn": 1, "Rev. %": 1, "GDP %": 1, "Ratio": 2})


if __name__ == "__main__":
    table_1 = build_spending_table(eu27_spending)
    print(table_1.to_string(index=False))
    print()
    table_2 = build_country_table(country_sector_base, gdp)
    print(table_2.to_string(index=False))
