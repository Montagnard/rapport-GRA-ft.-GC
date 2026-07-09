#' Reproduce the revenue table of the one-off EU wealth tax (see wealth.tex, Table 1).
#'
#' Run from this directory with:  Rscript wealth_revenue.R
#'
#' Inputs (committed next to this script):
#'  - raw_data_global_tax_simulator_WID.csv: the tabulation underlying the WID
#'    Global Wealth Tax Simulator (https://wid.world/world-wealth-tax-simulator/).
#'    Columns used: iso, year, threshold, w (total net wealth held above
#'    `threshold`, in EUR), n (number of adults above `threshold`).
#'  - wid_bulk_extract.csv: a compact extract of the WID bulk database
#'    (ahwealj992/npopuli992/xlceuxi999, 27 EU states, 2023). Rebuilt from the
#'    full ~880 MB download by section 3 below if the file is deleted.
#'
#' Method (see Section "Revenue estimates" of wealth.tex):
#'  - The wealth taxable at a single marginal rate in a band [A, B) is
#'    base(A, B) = W(A) - W(B) - A*N(A) + B*N(B), where W(t) and N(t) are wealth
#'    and headcount above threshold t. Open-ended top brackets use W(A) - A*N(A).
#'    This is exact on the simulator grid, whose nodes include 1e6, 1e7, 1e8, 1e9.
#'  - Annual rates: a = 0.1% above 1M, b = 0.3% above 10M, c = 0.5% above 100M,
#'    d = 2% above 1G. Each is one thirtieth of the one-off marginal liability
#'    (3%, 9%, 15%, 60%). Central scenario = a + b + c; top-heavy variant = c + d.
#'  - Haircut: two successive 15% reductions of the base (one for asset-price
#'    depreciation, one for evasion incl. valuation discounts), i.e. a factor
#'    0.85 * 0.85 = 0.7225. Figures are annual, EUR million.
#'
#' Data sources for the top brackets (c, d):
#'  - The WID simulator reports distributional-national-accounts (DINA) wealth.
#'    DINA systematically UNDERSTATES the very top: it does NOT apply any Forbes
#'    (rich-list) correction. The simulator's own numbers therefore give a
#'    conservative floor for brackets c and d.
#'  - A "billionaire-gap" correction reprices c and d on Forbes / EU Tax
#'    Observatory (2025) billionaire wealth, scaled by Zucman's (2024) 1.55
#'    billionaire-to-centi-millionaire multiplier. The published Table 1 reports
#'    the conservative DINA figures; both variants are exported to Excel (sec. 5).
#'
#' Outputs:
#'  - Console: the published table (simulator for the 16 covered states + WID bulk
#'    for the 11 small ones), its LaTeX rows, and a simulator-vs-bulk discrepancy
#'    report.
#'  - wealth_revenue_tables.xlsx: six variants (WID bulk / simulator / published,
#'    each with and without the Forbes correction of the top brackets).

year_ref     <- 2023
haircut_depreciation <- 0.15         # asset-price depreciation
haircut_evasion      <- 0.15         # tax evasion, incl. valuation discounts
haircut      <- (1 - haircut_depreciation) * (1 - haircut_evasion)  # 0.7225 (two successive 15% haircuts)

rate_a <- 0.001    # >  1M   (one-off  3% over 30 years)
rate_b <- 0.003    # > 10M   (one-off  9% over 30 years)
rate_c <- 0.005    # >100M   (one-off 15% over 30 years)
rate_d <- 0.02     # >  1G   (one-off 60% over 30 years)
forbes_mult <- 1.55  # Zucman (2024) billionaire-to-centi-millionaire multiplier

## ---- 1. Simulator tabulation ----------------------------------------------
sim <- read.csv("raw_data_global_tax_simulator_WID.csv")
sim <- sim[sim$year == year_ref, c("iso", "threshold", "w", "n")]

w_above <- function(iso, t) sim$w[sim$iso == iso & sim$threshold == t]
n_above <- function(iso, t) sim$n[sim$iso == iso & sim$threshold == t]

#' Wealth taxable at one marginal rate in the band [A, B).
band <- function(iso, A, B) w_above(iso, A) - w_above(iso, B) - A * n_above(iso, A) + B * n_above(iso, B)
#' Wealth above A (open-ended top bracket).
top  <- function(iso, A) w_above(iso, A) - A * n_above(iso, A)

sim_iso <- unique(sim$iso)   # the simulator dataset is global (many non-EU states)

## ---- 2. EU Tax Observatory (2025) Forbes-based billionaire wealth, EUR bn ---
bill_bn <- c(FR = 695.2, DE = 606.7, IT = 299.0, ES = 185.7, SE = 165.7, AT = 68.6,
             CZ = 61.0, IE = 51.4, DK = 45.7, CY = 42.9, GR = 37.1, BE = 36.2,
             NL = 34.3, PL = 26.7, FI = 14.3, RO = 11.4, HU = 8.6, PT = 5.7,
             BG = 4.8, SK = 2.9, EE = 2.9, HR = 1.9, LU = 1.0,
             LV = 0.0, LT = 0.0, MT = 0.0, SI = 0.0)

## ---- 3. WID bulk database (all-DINA figures for every member state) ---------
## The simulator dataset omits 11 small states; the bulk DINA g-percentiles cover
## all 27 and let us (i) fill the lower brackets of those 11 and (ii) build an
## all-DINA comparison table. Both products use 2023 current euros (no inflation
## adjustment needed).

## 3a. Build a compact extract from the full WID bulk database, once. Keeps only
## the variables/countries/year needed, then deletes the ~880 MB download. The
## extract (wid_bulk_extract.csv) is committed, so this block is normally skipped.
bulk_extract_path <- "wid_bulk_extract.csv"
if (!file.exists(bulk_extract_path)) {
  message("Building WID bulk extract (one-off ~880 MB download)...")
  library(data.table)
  eu27 <- c("AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE","GR","HU",
            "IE","IT","LV","LT","LU","MT","NL","PL","PT","RO","SK","SI","ES","SE")
  keep_vars <- c("ahwealj992", "npopuli992", "xlceuxi999")
  zip_path <- tempfile(fileext = ".zip")
  download.file("https://wid.world/bulk_download/wid_all_data.zip", zip_path, mode = "wb")
  ex_dir <- tempfile(); dir.create(ex_dir)
  files <- paste0("WID_data_", eu27, ".csv")
  unzip(zip_path, files = files, exdir = ex_dir)   # one unzip call (Zip64 archive)
  first <- TRUE
  for (f in files) {                                # stream file-by-file to limit memory
    d <- fread(file.path(ex_dir, f), select = c("country", "variable", "percentile", "year", "value"))
    fwrite(d[variable %in% keep_vars & year == year_ref], bulk_extract_path, append = !first)
    first <- FALSE; rm(d); gc()
  }
  unlink(zip_path); unlink(ex_dir, recursive = TRUE)
}

bulk <- read.csv(bulk_extract_path)
euro_area <- c("AT","BE","HR","CY","EE","FI","FR","DE","GR","IE","IT","LV","LT",
               "LU","MT","NL","PT","SK","SI","ES")   # HR joined the euro in 2023

## WID generalised-percentile code, e.g. pcode(99.99, 99.991) -> "p99.99p99.991"
pcode <- function(lo, hi) {
  f <- function(x) sub("[.]$", "", sub("0+$", "", formatC(x, format = "f", digits = 3)))
  paste0("p", f(lo), "p", f(hi))
}
## 127 non-overlapping brackets, bottom up to the top 0.001%
cuts_top <- c(99, 99.1, 99.2, 99.3, 99.4, 99.5, 99.6, 99.7, 99.8, 99.9,
              99.91, 99.92, 99.93, 99.94, 99.95, 99.96, 99.97, 99.98, 99.99,
              99.991, 99.992, 99.993, 99.994, 99.995, 99.996, 99.997, 99.998, 99.999, 100)
brk <- rbind(data.frame(lo = 0:98, hi = 1:99),
             data.frame(lo = head(cuts_top, -1), hi = tail(cuts_top, -1)))
clip <- function(x, lo, hi) pmax(0, pmin(x, hi) - lo)

#' All four brackets for `iso` from the bulk DINA g-percentiles, EUR million.
#'
#' The WID bulk data give, for each of the 127 generalised-percentile bins, only
#' the *average* net wealth of the adults in that bin (ahwealj992), not a wealth
#' threshold. The tax cut-offs (1e6, 1e7, 1e8, 1e9) therefore do NOT coincide
#' with bin edges. We handle this with a bin-average (mean-value) approximation:
#' every adult in bin k is assigned that bin's mean wealth w_k, and the marginal
#' schedule is applied to w_k. The revenue from bin k (with n_k adults) is
#'   n_k * sum_bands rate_band * (portion of w_k falling in that band),
#' e.g. for the >100M bracket, n_k * rate_c * max(0, w_k - 1e8). Summing over the
#' bins approximates the integral of the marginal schedule against the wealth
#' distribution. Because the bins are very fine at the top (down to the top
#' 0.001%), the cut-off/bin mismatch matters little for the lower brackets; its
#' one real limitation is at the extreme top, where a single bin pools very
#' dispersed fortunes, so the bin mean understates wealth above 1e9 (column d is
#' small or zero for most states -- the smoothed simulator tabulation, which
#' models the Pareto tail, does better and is used for the 16 covered states).
bulk_row <- function(iso) {
  bi   <- bulk[bulk$country == iso, ]
  pop  <- bi$value[bi$variable == "npopuli992" & bi$percentile == "p0p100"]
  fx   <- if (iso %in% euro_area) 1 else bi$value[bi$variable == "xlceuxi999"]
  av   <- setNames(bi$value[bi$variable == "ahwealj992"], bi$percentile[bi$variable == "ahwealj992"])
  w_bin <- as.numeric(av[pcode(brk$lo, brk$hi)]) / fx      # bin mean wealth, EUR
  popb  <- pop * (brk$hi - brk$lo) / 100                   # adults per bin
  ok <- !is.na(w_bin)
  c(a = sum(popb[ok] * rate_a * clip(w_bin[ok], 1e6, 1e7)),
    b = sum(popb[ok] * rate_b * clip(w_bin[ok], 1e7, 1e8)),
    c = sum(popb[ok] * rate_c * pmax(0, w_bin[ok] - 1e8)),
    d = sum(popb[ok] * rate_d * pmax(0, w_bin[ok] - 1e9))) * haircut / 1e6   # EUR million
}

## ---- 4. Constants: 2023 nominal GDP (EUR bn, Eurostat) and country names -----
gdp_bn <- c(DE = 4185, FR = 2803, IT = 2085, ES = 1498, NL = 1034, PL = 710,
            SE = 509, BE = 584, IE = 511, AT = 478, DK = 376, RO = 350, CZ = 311,
            FI = 277, PT = 265, GR = 220, HU = 203, SK = 122, BG = 102, LU = 85,
            HR = 77, LT = 74, SI = 68, LV = 40, EE = 38, CY = 32, MT = 21)

country_name <- c(FR = "France", DE = "Germany", IT = "Italy", ES = "Spain",
  SE = "Sweden", AT = "Austria", CZ = "Czechia", IE = "Ireland", DK = "Denmark",
  CY = "Cyprus", GR = "Greece", BE = "Belgium", NL = "Netherlands", PL = "Poland",
  FI = "Finland", RO = "Romania", HU = "Hungary", PT = "Portugal", BG = "Bulgaria",
  SK = "Slovakia", EE = "Estonia", HR = "Croatia", LU = "Luxembourg",
  LV = "Latvia", LT = "Lithuania", MT = "Malta", SI = "Slovenia")

all_iso <- names(bill_bn)              # the 27 EU member states
sim_eu  <- intersect(all_iso, sim_iso) # the 16 EU states covered by the simulator

## ---- 5. Per-source (a, b, c, d) providers, EUR million ----------------------
## Simulator DINA (only defined for the 16 covered states).
sim_abcd <- function(iso) c(
  a = rate_a * band(iso, 1e6, 1e7) * haircut / 1e6,
  b = rate_b * band(iso, 1e7, 1e8) * haircut / 1e6,
  c = rate_c * top(iso, 1e8) * haircut / 1e6,
  d = rate_d * top(iso, 1e9) * haircut / 1e6)

## Bulk DINA (defined for all 27 states).
bulk_abcd <- function(iso) bulk_row(iso)

## Forbes-based top brackets (c, d) for one state, EUR million.
forbes_cd <- function(iso) c(
  c = bill_bn[[iso]] * forbes_mult * rate_c * haircut * 1e3,
  d = bill_bn[[iso]] * rate_d * haircut * 1e3)

## ---- 6. Table assembler ----------------------------------------------------
#' Build a sorted revenue table from a function get(iso) -> c(a, b, c, d).
assemble <- function(isos, get) {
  rows <- lapply(isos, function(iso) {
    v <- get(iso)
    central <- v[["a"]] + v[["b"]] + v[["c"]]
    data.frame(country = country_name[iso], a = v[["a"]], b = v[["b"]],
               c = v[["c"]], d = v[["d"]], central = central,
               gdp_pct = 100 * central / (gdp_bn[iso] * 1e3),
               top = v[["c"]] + v[["d"]], row.names = NULL)
  }) |> (\(x) do.call(rbind, x))()
  rows[order(-rows$central), ]
}

## Published table: simulator (DINA tabulation) for the 16 covered states; all
## four brackets from the WID bulk DINA g-percentiles for the 11 small ones that
## the simulator omits.
get_main <- function(iso) {
  if (iso %in% sim_iso) sim_abcd(iso) else bulk_row(iso)
}
## Same, with the Forbes correction applied to the top brackets of every state.
get_main_forbes <- function(iso) {
  ab <- if (iso %in% sim_iso) sim_abcd(iso) else bulk_row(iso)
  c(a = ab[["a"]], b = ab[["b"]], forbes_cd(iso))
}

rows <- assemble(all_iso, get_main)

totals <- colSums(rows[, c("a", "b", "c", "d", "central", "top")])
eu_gdp_pct <- 100 * totals[["central"]] / (sum(gdp_bn) * 1e3)

## ---- 7. Console output of the published table ------------------------------
disp <- rows
disp[, c("a", "b", "c", "d", "central", "top")] <- round(disp[, c("a", "b", "c", "d", "central", "top")])
disp$gdp_pct <- sprintf("%.2f%%", disp$gdp_pct)
cat("Annual revenue by EU member state, EUR million (two 15% haircuts, factor 0.7225):\n\n")
print(disp, row.names = FALSE)
cat(sprintf("\nEU-27 total: a=%.0f  b=%.0f  c=%.0f  d=%.0f  central=%.0f (%.2f%% of GDP)  top=%.0f\n",
            totals[["a"]], totals[["b"]], totals[["c"]], totals[["d"]],
            totals[["central"]], eu_gdp_pct, totals[["top"]]))

## LaTeX body rows (paste into the longtable in wealth.tex)
tex_num <- function(x) formatC(round(x), format = "d", big.mark = "{,}")
cat("\n% --- LaTeX rows ---\n")
for (i in seq_len(nrow(rows))) {
  r <- rows[i, ]
  cat(sprintf("%-13s & %s & %s & %s & %s & %s & %.2f\\%% & %s \\\\\n",
      r$country, tex_num(r$a), tex_num(r$b), tex_num(r$c), tex_num(r$d),
      tex_num(r$central), r$gdp_pct, tex_num(r$top)))
}
cat(sprintf("\\textbf{EU-27 total} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} & \\textbf{%s} & \\textbf{%.2f\\%%} & \\textbf{%s} \\\\\n",
    tex_num(totals[["a"]]), tex_num(totals[["b"]]), tex_num(totals[["c"]]),
    tex_num(totals[["d"]]), tex_num(totals[["central"]]), eu_gdp_pct, tex_num(totals[["top"]])))

## ---- 8. Excel export: three sources x (with / without Forbes correction) ----
## "Without Forbes" keeps the DINA top brackets; "with Forbes" reprices c and d
## on rich-list wealth (relevant only where DINA understates the top, i.e. c, d).
add_total <- function(df) {
  tot <- colSums(df[, c("a", "b", "c", "d", "central", "top")])
  gp  <- 100 * tot[["central"]] / (sum(gdp_bn) * 1e3)
  rbind(df, data.frame(country = "EU-27 total", a = tot[["a"]], b = tot[["b"]],
        c = tot[["c"]], d = tot[["d"]], central = tot[["central"]],
        gdp_pct = gp, top = tot[["top"]], row.names = NULL))
}
round_tbl <- function(df) {
  val <- c("a", "b", "c", "d", "central", "top")
  df[, val] <- round(df[, val])          # value columns: round to the unit
  tot <- df$country == "EU-27 total"
  df$gdp_pct[!tot] <- signif(df$gdp_pct[!tot], 2)  # countries: two significant digits
  df$gdp_pct[tot]  <- signif(df$gdp_pct[tot], 4)   # EU-27 total %GDP: four significant digits
  df$central[tot]  <- signif(df$central[tot], 3)   # EU-27 total central: three sig. digits
  df
}

tables <- list(
  `WID bulk (DINA)`        = assemble(all_iso, bulk_abcd),
  `WID bulk + Forbes`      = assemble(all_iso, function(iso) { v <- bulk_row(iso); c(a = v[["a"]], b = v[["b"]], forbes_cd(iso)) }),
  `Simulator (DINA)`       = assemble(sim_eu, sim_abcd),
  `Simulator + Forbes`     = assemble(sim_eu, function(iso) { v <- sim_abcd(iso); c(a = v[["a"]], b = v[["b"]], forbes_cd(iso)) }),
  `Sim+WID published`      = rows,
  `Sim+WID + Forbes`       = assemble(all_iso, get_main_forbes))

if (requireNamespace("openxlsx", quietly = TRUE)) {
  wb <- openxlsx::createWorkbook()
  hdr <- c("Country", "a: 0.1%>1M", "b: 0.3%>10M", "c: 0.5%>100M", "d: 2%>1G",
           "Central (a+b+c)", "Central %GDP", "Top (c+d)")
  for (nm in names(tables)) {
    openxlsx::addWorksheet(wb, nm)
    out <- round_tbl(add_total(tables[[nm]])); names(out) <- hdr
    openxlsx::writeData(wb, nm, out)
  }
  openxlsx::saveWorkbook(wb, "wealth_revenue_tables.xlsx", overwrite = TRUE)
  cat("\nWrote wealth_revenue_tables.xlsx (6 sheets).\n")
} else {
  cat("\nopenxlsx not available; skipping Excel export.\n")
}

## EU totals of each variant, for the text of wealth.tex.
cat("\nEU-27 totals by variant (EUR million):\n")
for (nm in names(tables)) {
  t <- colSums(tables[[nm]][, c("a", "b", "c", "d", "central", "top")])
  cat(sprintf("  %-20s a=%.0f b=%.0f c=%.0f d=%.0f central=%.0f top=%.0f\n",
              nm, t[["a"]], t[["b"]], t[["c"]], t[["d"]], t[["central"]], t[["top"]]))
}

## ---- 9. Simulator vs bulk discrepancy (published table sanity check) --------
brows <- assemble(all_iso, bulk_abcd)
cmp <- merge(rows[, c("country","a","b","c","d")],
             brows[, c("country","a","b","c","d")],
             by = "country", suffixes = c("_sim", "_bulk"))
disc <- data.frame()
for (col in c("a","b","c","d")) {
  s <- cmp[[paste0(col,"_sim")]]; u <- cmp[[paste0(col,"_bulk")]]
  ok <- s > 0                                   # % undefined where simulator cell is 0
  disc <- rbind(disc, data.frame(country = cmp$country[ok], column = col,
                                 sim = s[ok], bulk = u[ok],
                                 pct = 100 * abs(u[ok] - s[ok]) / s[ok]))
}
disc <- disc[order(-disc$pct), ]
cat("\nPer-column maximum discrepancy (simulator vs bulk, cells where simulator > 0):\n")
for (col in c("a", "b", "c", "d")) {
  dd <- disc[disc$column == col, ]
  cat(sprintf("  column %s: max %.1f%% (%s); median %.1f%%\n", col, dd$pct[1], dd$country[1], median(dd$pct)))
}
cat("\n(Column d is 0 in the bulk table for most countries: the raw g-percentile bin\n averages do not resolve wealth above EUR 1bn, which the simulator tabulation does.)\n")
