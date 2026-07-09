#' Reproduce the revenue table of the one-off EU wealth tax (see wealth.tex, Table 1).
#'
#' Run from this directory with:  Rscript wealth_revenue.R
#'
#' Input (committed next to this script):
#'  - raw_data_global_tax_simulator_WID.csv: the tabulation underlying the WID
#'    Global Wealth Tax Simulator (https://wid.world/world-wealth-tax-simulator/).
#'    Columns used: iso, year, threshold, w (total net wealth held above
#'    `threshold`, in EUR), n (number of adults above `threshold`).
#'
#' Method (see Section "Revenue estimates" of wealth.tex):
#'  - Lower brackets a (>1M) and b (>10M) are computed directly from the
#'    simulator tabulation. The wealth taxable at a single marginal rate in a
#'    band [A, B) is  base(A, B) = W(A) - W(B) - A*N(A) + B*N(B), where W(t) and
#'    N(t) are wealth and headcount above threshold t. This is exact: the grid
#'    contains the statutory thresholds 1e6, 1e7, 1e8, 1e9 as nodes.
#'  - Upper brackets c (>100M) and d (>1G) are, by default, also computed from
#'    the simulator tabulation (DINA), i.e. 0.5%*top(1e8) and 2%*top(1e9). An
#'    optional Forbes / EU Tax Observatory (2025) "billionaire-gap" correction
#'    (disabled below, see section 5) can replace them with Forbes-based figures,
#'    which are ~2x higher because DINA understates the very top; that correction
#'    uses Zucman's (2024) 1.55 billionaire-to-centi-millionaire multiplier.
#'  - The 11 small member states absent from the simulator file (see `miss_a`,
#'    `miss_b`) have their a, b computed from the WID bulk database
#'    (variable ahwealj992, equal-split adults, 2023, market-rate converted).
#'  - Haircuts: 15% depreciation and 20% evasion. Figures are annual (one
#'    thirtieth of the one-off liability), reported in EUR million.
#'  - Section 7 rebuilds the whole table from the raw WID bulk data (all brackets
#'    DINA, all 27 states), extracts a compact committed copy of the bulk data,
#'    and reports the maximum per-cell discrepancy between the two tables.

year_ref     <- 2023
depreciation <- 0.15
evasion      <- 0.20
haircut      <- (1 - depreciation) * (1 - evasion)   # 0.68

## ---- 1. Simulator tabulation: lower brackets a and b -----------------------
sim <- read.csv("raw_data_global_tax_simulator_WID.csv")
sim <- sim[sim$year == year_ref, c("iso", "threshold", "w", "n")]

w_above <- function(iso, t) sim$w[sim$iso == iso & sim$threshold == t]
n_above <- function(iso, t) sim$n[sim$iso == iso & sim$threshold == t]

#' Wealth taxable at one marginal rate in the band [A, B).
band <- function(iso, A, B) w_above(iso, A) - w_above(iso, B) - A * n_above(iso, A) + B * n_above(iso, B)
#' Wealth above A (open-ended top bracket).
top  <- function(iso, A) w_above(iso, A) - A * n_above(iso, A)

sim_iso <- unique(sim$iso)

## ---- 2. EU Tax Observatory (2025) Forbes-based billionaire wealth, EUR bn ---
## Used only by the optional billionaire-gap correction in section 5 (disabled).
bill_bn <- c(FR = 695.2, DE = 606.7, IT = 299.0, ES = 185.7, SE = 165.7, AT = 68.6,
             CZ = 61.0, IE = 51.4, DK = 45.7, CY = 42.9, GR = 37.1, BE = 36.2,
             NL = 34.3, PL = 26.7, FI = 14.3, RO = 11.4, HU = 8.6, PT = 5.7,
             BG = 4.8, SK = 2.9, EE = 2.9, HR = 1.9, LU = 1.0,
             LV = 0.0, LT = 0.0, MT = 0.0, SI = 0.0)

## ---- 3. Lower brackets for the 11 states absent from the simulator file -----
## EUR million; computed from the WID bulk database (same method as section 7).
miss_a <- c(BG = 10.66, HR = 14.51, CY = 9.11, EE = 12.41, GR = 51.18, LV = 8.16,
            LT = 11.56, LU = 47.62, MT = 7.03, SK = 9.13, SI = 14.46)
miss_b <- c(BG = 12.00, HR = 9.71, CY = 1.91, EE = 6.12, GR = 14.99, LV = 5.69,
            LT = 7.89, LU = 27.76, MT = 0.11, SK = 5.60, SI = 13.69)

## ---- 4. 2023 nominal GDP, EUR bn (Eurostat) --------------------------------
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

## ---- 5. Assemble the table -------------------------------------------------
rows <- lapply(names(bill_bn), function(iso) {
  if (iso %in% sim_iso) {
    a  <- 0.001  * band(iso, 1e6, 1e7) * haircut / 1e6   # EUR million
    b  <- 0.0025 * band(iso, 1e7, 1e8) * haircut / 1e6
    cc <- 0.005  * top(iso, 1e8) * haircut / 1e6         # DINA upper brackets
    dd <- 0.02   * top(iso, 1e9) * haircut / 1e6
  } else {
    a <- unname(miss_a[iso]); b <- unname(miss_b[iso])
    cc <- 0; dd <- 0                                      # not in simulator dataset
  }
  ## ---- Forbes / EU Tax Observatory billionaire-gap correction (DISABLED) ----
  ## Uncomment the two lines below to replace the DINA-based upper brackets with
  ## Forbes-based figures. These are roughly twice as high, because tax-and-survey
  ## data (DINA) systematically understate the very top of the distribution.
  # cc <- unname(bill_bn[iso]) * 1.55 * 0.005 * haircut * 1e3
  # dd <- unname(bill_bn[iso]) * 0.02 * haircut * 1e3
  central <- a + b + cc
  data.frame(country = country_name[iso], a = a, b = b, c = cc, d = dd,
             central = central, gdp_pct = 100 * central / (gdp_bn[iso] * 1e3),
             top = cc + dd, row.names = NULL)
}) |> (\(x) do.call(rbind, x))()

rows <- rows[order(-rows$central), ]

totals <- colSums(rows[, c("a", "b", "c", "d", "central", "top")])
eu_gdp_pct <- 100 * totals[["central"]] / (sum(gdp_bn) * 1e3)

## ---- 6. Output -------------------------------------------------------------
disp <- rows
disp[, c("a", "b", "c", "d", "central", "top")] <- round(disp[, c("a", "b", "c", "d", "central", "top")])
disp$gdp_pct <- sprintf("%.2f%%", disp$gdp_pct)
cat("Annual revenue by EU member state, EUR million (15% depreciation, 20% evasion):\n\n")
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


## ============================================================================
## 7. Alternative table: all four brackets from the WID bulk database (DINA)
## ============================================================================
## This recomputes the whole table from the raw WID distributional-national-
## accounts g-percentiles, so that the upper brackets are DINA-based for every
## member state (the simulator dataset omits 11 small ones). It lets us gauge how
## much the choice of WID product (packaged simulator tabulation vs. raw bulk)
## moves each cell. Both tables use the same 2023 reference year and the same
## 15%/20% haircuts; no inflation adjustment is applied because both are in
## current 2023 euros (a units check against the simulator table confirms this).

## ---- 7a. Build a compact extract from the full WID bulk database, once -------
## Keeps only the variables/countries/year needed, then deletes the ~880 MB
## download. The compact extract (wid_bulk_extract.csv, ~0.4 MB) is committed, so
## this block is normally skipped; delete that file to force a rebuild. Reading
## the 27 full country files needs a few GB of RAM (a memory-starved machine may
## need to process them in smaller batches or fresh sessions).
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

## ---- 7b. Compute the all-DINA table from the extract -------------------------
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

bulk_row <- function(iso) {
  bi   <- bulk[bulk$country == iso, ]
  pop  <- bi$value[bi$variable == "npopuli992" & bi$percentile == "p0p100"]
  rate <- if (iso %in% euro_area) 1 else bi$value[bi$variable == "xlceuxi999"]
  av   <- setNames(bi$value[bi$variable == "ahwealj992"], bi$percentile[bi$variable == "ahwealj992"])
  a_bin <- as.numeric(av[pcode(brk$lo, brk$hi)]) / rate      # bracket average wealth, EUR
  popb  <- pop * (brk$hi - brk$lo) / 100                     # adults per bracket
  ok <- !is.na(a_bin)
  ## same haircut-on-revenue convention as the main table (haircut = 0.68)
  c(a = sum(popb[ok] * 0.001  * clip(a_bin[ok], 1e6, 1e7)),
    b = sum(popb[ok] * 0.0025 * clip(a_bin[ok], 1e7, 1e8)),
    c = sum(popb[ok] * 0.005  * pmax(0, a_bin[ok] - 1e8)),
    d = sum(popb[ok] * 0.02   * pmax(0, a_bin[ok] - 1e9))) * haircut / 1e6   # EUR million
}

brows <- lapply(names(bill_bn), function(iso) {
  v <- bulk_row(iso)
  data.frame(country = country_name[iso], a = v[["a"]], b = v[["b"]],
             c = v[["c"]], d = v[["d"]], central = v[["a"]] + v[["b"]] + v[["c"]],
             top = v[["c"]] + v[["d"]], row.names = NULL)
}) |> (\(x) do.call(rbind, x))()
brows <- brows[order(-brows$central), ]
btot  <- colSums(brows[, c("a", "b", "c", "d", "central", "top")])

bdisp <- brows
bdisp[, c("a","b","c","d","central","top")] <- round(bdisp[, c("a","b","c","d","central","top")])
cat("\n\nAlternative table from WID bulk data (all brackets DINA), EUR million:\n\n")
print(bdisp, row.names = FALSE)
cat(sprintf("\nEU-27 total (bulk): a=%.0f  b=%.0f  c=%.0f  d=%.0f  central=%.0f  top=%.0f\n",
            btot[["a"]], btot[["b"]], btot[["c"]], btot[["d"]], btot[["central"]], btot[["top"]]))

## ---- 7c. Maximum per-cell discrepancy between the two tables -----------------
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
cat("\nLargest single-cell discrepancies overall:\n")
print(head(disc, 8), row.names = FALSE)
cat(sprintf("\nMaximum discrepancy in any comparable cell: %.1f%% (%s, column %s)\n",
            disc$pct[1], disc$country[1], disc$column[1]))
