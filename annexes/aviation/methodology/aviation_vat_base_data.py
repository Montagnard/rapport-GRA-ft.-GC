import pandas as pd

# Air passengers carried, in thousands (Eurostat, Air passenger transport statistics)
# National: domestic legs within the member state
NATIONAL_PASSENGERS = {
    "Belgium": 7, "Bulgaria": 241, "Czechia": 17, "Denmark": 1_490, "Germany": 11_951,
    "Estonia": 57, "Ireland": 160, "Greece": 10_742, "Spain": 48_829, "France": 25_189,
    "Croatia": 456, "Italy": 36_207, "Cyprus": 1, "Latvia": 1, "Lithuania": 0,
    "Luxembourg": 0, "Hungary": 0, "Malta": 0, "Netherlands": 2, "Austria": 267,
    "Poland": 1_800, "Portugal": 6_446, "Romania": 851, "Slovenia": 0, "Slovakia": 1,
    "Finland": 1_877, "Sweden": 4_214,
}
# Intra-EU: departures and arrivals between the member state and other EU-27 countries
INTRA_EU_PASSENGERS = {
    "Belgium": 23_214, "Bulgaria": 7_586, "Czechia": 10_390, "Denmark": 21_017,
    "Germany": 100_457, "Estonia": 2_627, "Ireland": 20_572, "Greece": 38_837,
    "Spain": 123_829, "France": 67_007, "Croatia": 8_491, "Italy": 92_703, "Cyprus": 6_426,
    "Latvia": 5_161, "Lithuania": 4_382, "Luxembourg": 4_077, "Hungary": 11_255,
    "Malta": 6_450, "Netherlands": 40_074, "Austria": 22_544, "Poland": 32_565,
    "Portugal": 34_398, "Romania": 16_425, "Slovenia": 652, "Slovakia": 1_321,
    "Finland": 10_978, "Sweden": 18_097,
}
# Each intra-EU leg is counted at both its departure and its arrival country, so it is halved
INTRA_EU_WEIGHT = 0.5

# Assumed average revenue per leg in EUR, anchored on Ryanair (EUR 70) and Wizz Air (EUR 83)
# with higher fares for network-carrier point-to-point and feeder legs
FARE_PER_LEG = 98

# Business travel: share of passengers (mean of 20-25%, Transport & Environment) and assumed
# fare relative to other travel
BUSINESS_PASSENGER_SHARE = 0.225
BUSINESS_FARE_PREMIUM = 1.33
# Assumed share of VAT on business travel that is effectively deducted
BUSINESS_VAT_DEDUCTED = 0.80

business_revenue_share = (BUSINESS_PASSENGER_SHARE * BUSINESS_FARE_PREMIUM) / (
    BUSINESS_PASSENGER_SHARE * BUSINESS_FARE_PREMIUM + (1 - BUSINESS_PASSENGER_SHARE)
)
# share of ticket revenue on which VAT is not deducted
taxable_revenue_share = 1 - business_revenue_share * BUSINESS_VAT_DEDUCTED

# Gross national income 2024 in EUR bn (Eurostat teina080). Bulgaria only reports Q1 2024, so its value is Q1 x 4.
GNI = {
    "Belgium": 630.1, "Bulgaria": 99.8, "Czechia": 311.4, "Denmark": 411.5,
    "Germany": 4_545.3, "Estonia": 39.1, "Ireland": 429.0, "Greece": 229.8,
    "Spain": 1_594.6, "France": 2_983.6, "Croatia": 85.0, "Italy": 2_204.7, "Cyprus": 31.4,
    "Latvia": 40.3, "Lithuania": 77.8, "Luxembourg": 58.0, "Hungary": 202.3, "Malta": 18.6,
    "Netherlands": 1_102.1, "Austria": 504.2, "Poland": 822.0, "Portugal": 286.2,
    "Romania": 347.0, "Slovenia": 67.1, "Slovakia": 127.5, "Finland": 276.3, "Sweden": 590.2,
}

# EU-27 revenue in EUR bn: passenger-flow-preserving reform, and the additional revenue from a
# further additive VAT increase of 20 percentage points
REFORM_REVENUE = {
    "Reform bp": 2.5,
    "+20pp bp": 8.0,
}


def aviation_vat_base(country):
    """VAT base from intra-EU and national flights for one member state, in EUR bn."""
    legs = NATIONAL_PASSENGERS[country] + INTRA_EU_WEIGHT * INTRA_EU_PASSENGERS[country]
    return legs * 1_000 * FARE_PER_LEG * taxable_revenue_share / 1e9


def build_aviation_table():
    """VAT base per member state, its share of the EU-27 base and of EU-27 GNI, and their ratio."""
    table = pd.DataFrame({
        "Country": list(NATIONAL_PASSENGERS),
        "Base € bn": [aviation_vat_base(country) for country in NATIONAL_PASSENGERS],
        "GNI € bn": [GNI[country] for country in NATIONAL_PASSENGERS],
    })
    table.loc[len(table)] = ["EU-27", table["Base € bn"].sum(), table["GNI € bn"].sum()]
    table["Share %"] = table["Base € bn"] / table["Base € bn"].iloc[-1] * 100
    table["GNI %"] = table["GNI € bn"] / table["GNI € bn"].iloc[-1] * 100
    table["Ratio"] = table["Share %"] / table["GNI %"]
    # revenue distributed in proportion to the tax base, in basis points of the country's GNI
    for column, revenue in REFORM_REVENUE.items():
        table[column] = revenue * table["Share %"] / 100 / table["GNI € bn"] * 10_000
    table = table.drop(columns="GNI € bn")
    # sort member states by the last column, keeping the EU-27 row at the bottom
    countries = table.iloc[:-1].sort_values(table.columns[-1], ascending=False)
    table = pd.concat([countries, table.iloc[[-1]]], ignore_index=True)
    return table.round({
        "Base € bn": 2, "Share %": 2, "GNI %": 2, "Ratio": 2,
        **{column: 2 for column in REFORM_REVENUE},
    })


if __name__ == "__main__":
    print(build_aviation_table().to_string(index=False))
