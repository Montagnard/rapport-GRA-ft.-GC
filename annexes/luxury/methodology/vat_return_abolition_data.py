import pandas as pd

# Static VAT revenue from abolishing tax-free shopping refunds in the EU-27:
#   R_static = S x tau / (1 + tau) x rho
# S: tax-free sales in the EU-27 with an export form issued, incl. VAT (EUR bn)
# tau: weighted average standard VAT rate on those sales
# rho: share of issued forms that are refunded
# Period: Global Blue FY2024/25 (April 2024 - March 2025); no behavioural response
SCENARIOS = ["Low", "Central", "High"]


def vat_content(weights_and_rates):
    """tau / (1 + tau) for a destination-weighted average VAT rate."""
    total_weight = sum(weight for weight, _ in weights_and_rates.values())
    tau = sum(weight * rate for weight, rate in weights_and_rates.values()) / total_weight
    return tau / (1 + tau)


# Step 1: Global Blue tax-free sales in store, FY2024/25 (EUR bn)
# Nine months to December 2024 (Global Blue 6-K, Q3 FY2024/25)
TAX_FREE_SIS_9M = 18.3
GROUP_SIS_9M = 25.0
# Full year FY2024/25 (Global Blue FY24/25 results)
GROUP_SIS_FY = 32.9

# Q4 is extrapolated with the nine-month tax-free share of group sales in store
tax_free_share_9m = TAX_FREE_SIS_9M / GROUP_SIS_9M
tax_free_sis = TAX_FREE_SIS_9M + (GROUP_SIS_FY - GROUP_SIS_9M) * tax_free_share_9m

# Step 5 (needed in step 2): VAT content of European tax-free sales
# destination: (share of 2023 tax-free spend in %, standard VAT rate), Global Blue Jan 2024 update
EUROPE_DESTINATIONS = {
    "France": (26, 0.20),
    "Italy": (25, 0.22),
    "Spain": (15, 0.21),
    "Germany": (8, 0.19),
}
europe_vat_content = vat_content(EUROPE_DESTINATIONS)

# Step 2: Continental Europe share of Global Blue tax-free sales in store
# Tax-free revenue by region in EUR m (Global Blue FY24/25 results)
REVENUE_CONTINENTAL_EUROPE = 321.3
REVENUE_ASIA_PACIFIC = 63.2
# destination: (share of 2023 tax-free spend in %, statutory VAT/GST rate), Global Blue Jan 2024 update
ASIA_PACIFIC_DESTINATIONS = {
    "Japan": (65, 0.10),
    "Singapore": (27, 0.09),
    "South Korea": (8, 0.10),
}
asia_pacific_vat_content = vat_content(ASIA_PACIFIC_DESTINATIONS)

# Revenue is assumed proportional to the VAT content of sales, so sales = revenue / VAT content
europe_sales_proxy = REVENUE_CONTINENTAL_EUROPE / europe_vat_content
asia_pacific_sales_proxy = REVENUE_ASIA_PACIFIC / asia_pacific_vat_content
europe_share = {
    "Low": 0.70,
    "Central": europe_sales_proxy / (europe_sales_proxy + asia_pacific_sales_proxy),
    "High": 0.78,
}

# Step 3: EU-27 share of Continental Europe (low = floor from France, Italy, Spain and
# Germany making up 74% of Continental European spend; central and high are assumptions)
EU27_SHARE = {"Low": 0.74, "Central": 0.85, "High": 0.90}

# Step 4: Global Blue market share (~70%, Global Blue investor presentation April 2024);
# a lower market share means a larger total market
MARKET_SHARE = {"Low": 0.75, "Central": 0.70, "High": 0.60}

# Step 6: refund ratio by value (Global Blue 20-F FY2024, as of 31 March 2024)
REFUND_RATIO = 0.83


def static_vat_revenue(scenario):
    """All steps of the estimate for one scenario, in EUR bn where applicable."""
    s = (
        tax_free_sis
        * europe_share[scenario]
        * EU27_SHARE[scenario]
        / MARKET_SHARE[scenario]
    )
    vat_on_issued_forms = s * europe_vat_content
    return {
        "1 Tax-free sales in store (EUR bn)": tax_free_sis,
        "2 x Europe share": europe_share[scenario],
        "3 x EU-27 share": EU27_SHARE[scenario],
        "4 / Market share": MARKET_SHARE[scenario],
        "= S (EUR bn)": s,
        "5 x VAT content": europe_vat_content,
        "= VAT on issued forms (EUR bn)": vat_on_issued_forms,
        "6 x Refund ratio": REFUND_RATIO,
        "= R_static (EUR bn)": vat_on_issued_forms * REFUND_RATIO,
    }


def build_range_table():
    """Low, central and high case of every step."""
    table = pd.DataFrame({scenario: static_vat_revenue(scenario) for scenario in SCENARIOS})
    table.index.name = "Step"
    return table


if __name__ == "__main__":
    table = build_range_table()
    print(table.round(3).to_string())
    print()
    revenue = table.loc["= R_static (EUR bn)"]
    print(
        f"Static VAT revenue from abolishing tax-free refunds: EUR {revenue['Low']:.1f}-"
        f"{revenue['High']:.1f} bn (central EUR {revenue['Central']:.1f} bn)"
    )
