# Minimise the heterogeneity of new own-resource costs (% of GNI) across Member States
#
# Data: combined.xlsx (source of Table 2 = tables/table_gni.tex and Table 4 = tables/table_scenario_gni.tex)
# and disaggregated_luxury_base.csv (luxury tax base by country and product category).
# Each tax j gets a coefficient 0 <= c_j <= rate_cap applied to the report's rate; the cost of country i is
# cost_i = sum_j c_j * t_ij, where t_ij is the Table 2 cost (% GNI) at the report's rates.
# Constraint: total new revenues = target (EUR bn), read from Parameters!B2 of the output workbook
# (default: the report's total at the report's rates, EUR 102.6 bn = .705% of EU GNI).
# Two models: the report's 6 taxes (tab Summary) and the same with the luxury tax split into
# category-specific taxes (tab Summary_lux).
#
# Run from the project root: Rscript min_heterogeneity.R

library(openxlsx)
library(lpSolveAPI)

# ── Parameters ────────────────────────────────────────────────────────────────
report_rates <- c(dst = 3, ad = 15, ftt = 0.5, wealth = 0.5, aviation = 20, luxury = 20)  # report's rates (%)
tax_labels <- c(dst = "DST", ad = "Ad tax", ftt = "FTT", wealth = "Wealth tax", aviation = "Aviation tax", luxury = "Luxury VAT")
new_funds_share <- c(dst = 0, ad = 0, ftt = 0, wealth = 3/5, aviation = 0, luxury = 1/3)  # share of revenues financing new funds (Table 4, col. 3)
lux_categories <- c(lux_automotive = "Automotive", lux_personal = "Personal luxury goods", lux_hospitality = "Hospitality", lux_wines = "Wines & spirits", lux_gourmet = "Gourmet food & dining", lux_design = "Design & furniture", lux_yachts = "Yachts")
rate_cap <- 4  # maximum coefficient: each rate <= rate_cap x the report's rate
budget_expansion <- 14  # EUR bn, expansion of the EU budget (combined.xlsx, Q46)
frugal <- c("Austria", "Germany", "Ireland", "Netherlands", "Sweden")
bold_ids <- c("A1", "B2", "C2")  # scenarios highlighted in the summary tabs
custom_row <- 9  # row of the summary tabs with user-defined rates
tol <- 1e-9
out_file <- "min_heterogeneity.xlsx"
lux_file <- "disaggregated_luxury_base.csv"
country_codes <- c(AT = "Austria", BE = "Belgium", BG = "Bulgaria", HR = "Croatia", CY = "Cyprus", CZ = "Czechia", DK = "Denmark", EE = "Estonia", FI = "Finland", FR = "France", DE = "Germany", GR = "Greece", HU = "Hungary", IE = "Ireland", IT = "Italy", LV = "Latvia", LT = "Lithuania", LU = "Luxembourg", MT = "Malta", NL = "Netherlands", PL = "Poland", PT = "Portugal", RO = "Romania", SK = "Slovakia", SI = "Slovenia", ES = "Spain", SE = "Sweden")

# ── Data ──────────────────────────────────────────────────────────────────────
raw <- read.xlsx("combined.xlsx", rows = 8:34, cols = c(1, 2, 4, 6, 8, 10, 13, 16, 25, 26), colNames = FALSE)
names(raw) <- c("country", names(report_rates), "gni", "foregone_dst", "foregone_ftt")
stopifnot(nrow(raw) == 27, raw$country[1] == "Austria", raw$country[27] == "Sweden", setequal(raw$country, country_codes), all(frugal %in% raw$country))
if (anyNA(raw[, c(names(report_rates), "gni")])) stop("Missing revenue or GNI values in combined.xlsx")
# Blank foregone cells mean no existing domestic DST/FTT: set to 0 explicitly
raw$foregone_dst <- ifelse(is.na(raw$foregone_dst), 0, raw$foregone_dst)
raw$foregone_ftt <- ifelse(is.na(raw$foregone_ftt), 0, raw$foregone_ftt)

countries <- raw$country
gni <- raw$gni  # EUR bn
gni_eu <- sum(gni)
is_frugal <- countries %in% frugal
foregone_pct <- (raw$foregone_dst + raw$foregone_ftt) / gni / 10  # % of national GNI
base_revenue <- as.matrix(raw[, names(report_rates)])  # EUR millions at the report's rates
report_total_bn <- sum(base_revenue) / 1000

lux_raw <- read.csv(lux_file, check.names = FALSE)
names(lux_raw)[1] <- "code"
stopifnot(all(lux_categories %in% names(lux_raw)))
lux_total <- lux_raw[lux_raw$code == "Total", unname(lux_categories), drop = FALSE]  # check row, excluded from the country data
lux_raw <- lux_raw[lux_raw$code %in% names(country_codes), ]
stopifnot(setequal(lux_raw$code, names(country_codes)), nrow(lux_total) == 1,
          isTRUE(all.equal(unlist(lux_total), colSums(lux_raw[unname(lux_categories)]), check.attributes = FALSE)))
lux_base <- as.matrix(lux_raw[match(names(country_codes)[match(countries, country_codes)], lux_raw$code), unname(lux_categories)])
if (anyNA(lux_base) || any(lux_base < 0)) stop("Missing or negative luxury base values in ", lux_file)
# Luxury revenue of each country split across categories in proportion to its category bases
lux_revenue <- base_revenue[, "luxury"] * (lux_base / rowSums(lux_base))
colnames(lux_revenue) <- names(lux_categories)

#' Read the revenue target (EUR bn) from the Parameters sheet of a previous output workbook
#'
#' @param path Path to the output workbook.
#' @return Target in EUR bn, or NA if the workbook, sheet or value is missing or not a positive number.
read_target_bn <- function(path) {
  if (!file.exists(path) || !"Parameters" %in% getSheetNames(path)) return(NA_real_)
  cell <- tryCatch(read.xlsx(path, sheet = "Parameters", rows = 2, cols = 2, colNames = FALSE), error = \(e) NULL)
  value <- if (is.null(cell) || nrow(cell) == 0) NA_real_ else suppressWarnings(as.numeric(cell[1, 1]))
  if (is.na(value) || value <= 0) NA_real_ else value
}

target_bn <- read_target_bn(out_file)
if (is.na(target_bn)) {
  target_bn <- report_total_bn
  message(sprintf("No valid target in %s (Parameters!B2): using the report's total, EUR %.1f bn", out_file, target_bn))
}
target_total <- 100 * target_bn / gni_eu  # % of EU GNI

#' Build a tax model
#'
#' @param revenue Matrix (countries x taxes) of revenues in EUR millions at the report's rates.
#' @param rates,labels,funds_share Named vectors: report's rates (%), labels, share of revenues financing new funds.
#' @param suffix Suffix of the model's tabs ("" or "_lux").
#' @return List describing the model.
make_model <- function(revenue, rates, labels, funds_share, suffix) {
  list(cost_mat = revenue / gni / 10, eu_cost = colSums(revenue) / gni_eu / 10, rates = rates, labels = labels,
       funds_share = funds_share, n_tax = length(rates), suffix = suffix)
}

lux_keep <- setdiff(names(report_rates), "luxury")
models <- list(
  base = make_model(base_revenue, report_rates, tax_labels, new_funds_share, ""),
  lux = make_model(cbind(base_revenue[, lux_keep], lux_revenue), c(report_rates[lux_keep], setNames(rep(report_rates[["luxury"]], length(lux_categories)), names(lux_categories))),
                   c(tax_labels[lux_keep], setNames(paste("Luxury:", lux_categories), names(lux_categories))),
                   c(new_funds_share[lux_keep], setNames(rep(new_funds_share[["luxury"]], length(lux_categories)), names(lux_categories))), "_lux")
)

# ── Solvers ───────────────────────────────────────────────────────────────────
#' State budget gain as a linear function of the coefficients (Table 4, col. 5)
#'
#' gain_i = (EU-budget revenues - budget expansion) / EU GNI - foregone_i / GNI_i, in % of GNI, where EU-budget
#' revenues are new revenues net of those financing new funds; foregone domestic revenues are fixed.
#' @param m Model.
#' @return List with `slope` (27 x n_tax matrix) and `intercept` (length 27) such that gain = slope %*% c + intercept.
gain_linear <- function(m) {
  slope <- matrix(m$eu_cost * (1 - m$funds_share), nrow = length(countries), ncol = m$n_tax, byrow = TRUE)
  list(slope = slope, intercept = -100 * budget_expansion / gni_eu - foregone_pct)
}

#' Solve a linear programme min obj'x s.t. a_ineq x <= b_ineq, a_eq x = b_eq
#'
#' @param obj Objective vector.
#' @param a_ineq,b_ineq Inequality constraints (may be NULL).
#' @param a_eq,b_eq Equality constraints.
#' @param lower,upper Bounds of the variables.
#' @return Optimal x.
solve_lp <- function(obj, a_ineq, b_ineq, a_eq, b_eq, lower, upper) {
  lp <- make.lp(0, length(obj))
  lp.control(lp, sense = "min")
  set.objfn(lp, obj)
  if (!is.null(a_ineq)) for (k in seq_len(nrow(a_ineq))) add.constraint(lp, a_ineq[k, ], "<=", b_ineq[k])
  for (k in seq_len(nrow(a_eq))) add.constraint(lp, a_eq[k, ], "=", b_eq[k])
  set.bounds(lp, lower = lower, upper = upper)
  status <- solve(lp)
  if (status != 0) stop("LP solver failed with status ", status)
  get.variables(lp)
}

#' Minimise the maximum country cost (% GNI) under linear constraints on the coefficients
#'
#' @param m Model.
#' @param a_ineq,b_ineq Extra constraints a_ineq c <= b_ineq (may be NULL).
#' @return Coefficient vector.
solve_minimax_cost <- function(m, a_ineq = NULL, b_ineq = NULL) {
  a <- rbind(cbind(m$cost_mat, -1), if (!is.null(a_ineq)) cbind(a_ineq, 0))
  b <- c(rep(0, length(countries)), b_ineq)
  x <- solve_lp(c(rep(0, m$n_tax), 1), a, b, matrix(c(m$eu_cost, 0), 1), target_total, c(rep(0, m$n_tax), -Inf), c(rep(rate_cap, m$n_tax), Inf))
  setNames(x[1:m$n_tax], names(m$rates))
}

#' Maximise the minimum State budget gain (% GNI)
#'
#' @param m Model.
#' @param gain Output of gain_linear().
#' @return Optimal minimum gain (% GNI).
solve_maxmin_gain <- function(m, gain) {
  x <- solve_lp(c(rep(0, m$n_tax), -1), cbind(-gain$slope, 1), gain$intercept, matrix(c(m$eu_cost, 0), 1), target_total, c(rep(0, m$n_tax), -Inf), c(rep(rate_cap, m$n_tax), Inf))
  x[m$n_tax + 1]
}

#' Minimise the sum over countries of squared deviations of cost from the EU mean (target_total)
#'
#' Exact solution of the strictly convex QP by the primal active-set method (Nocedal & Wright, Algorithm 16.3),
#' started from a feasible point given by the LP solver. Bounds 0 <= c <= rate_cap are added to the constraints.
#' @param m Model.
#' @param a_ineq,b_ineq Extra constraints a_ineq c <= b_ineq (may be NULL).
#' @return Coefficient vector.
solve_least_squares <- function(m, a_ineq = NULL, b_ineq = NULL) {
  n <- m$n_tax
  a_all <- rbind(-diag(n), diag(n), a_ineq)
  b_all <- c(rep(0, n), rep(rate_cap, n), b_ineq)
  # Drop duplicated constraint rows, keeping the tightest bound
  key <- apply(round(a_all, 12), 1, paste, collapse = "|")
  keep <- !duplicated(key)
  b_all <- sapply(key[keep], \(k) min(b_all[key == k])) |> unname()
  a_all <- a_all[keep, , drop = FALSE]
  a_eq <- matrix(m$eu_cost, 1)
  hess <- 2 * crossprod(m$cost_mat)
  lin <- 2 * drop(crossprod(m$cost_mat, rep(target_total, length(countries))))  # objective: x'Hx/2 - lin'x + const
  x <- solve_lp(rep(0, n), a_ineq, b_ineq, a_eq, target_total, rep(0, n), rep(rate_cap, n))
  working <- integer(0)
  for (iter in 1:1000) {
    a_w <- rbind(a_eq, a_all[working, , drop = FALSE])
    kkt <- rbind(cbind(hess, t(a_w)), cbind(a_w, matrix(0, nrow(a_w), nrow(a_w))))
    sol <- solve(kkt, c(lin - drop(hess %*% x), rep(0, nrow(a_w))))
    step <- sol[1:n]
    if (max(abs(step)) < 1e-10) {
      mult <- sol[-(1:(n + 1))]  # multipliers of working inequality constraints (>= 0 at the optimum)
      if (length(mult) == 0 || min(mult) >= -1e-10) return(setNames(pmin(pmax(x, 0), rate_cap), names(m$rates)))
      working <- working[-which.min(mult)]
    } else {
      slope <- drop(a_all %*% step)
      candidates <- setdiff(which(slope > 1e-14), working)
      ratios <- pmax(b_all[candidates] - drop(a_all[candidates, , drop = FALSE] %*% x), 0) / slope[candidates]
      alpha <- min(1, ratios)
      x <- x + alpha * step
      if (alpha < 1) working <- c(working, candidates[which.min(ratios)])
    }
  }
  stop("Active-set QP did not converge")
}

# ── Scenarios and indicators ──────────────────────────────────────────────────
#' Homogeneity scores of a cost distribution (1 = same cost in % GNI everywhere)
#'
#' @param cost Country costs (% GNI).
#' @param mean_cost EU (GNI-weighted) mean cost.
#' @return Named vector: minimax = mean / max; sq_dev = mean / (mean + root mean squared deviation from the mean).
homogeneity <- function(cost, mean_cost) c(minimax = mean_cost / max(cost), sq_dev = mean_cost / (mean_cost + sqrt(mean((cost - mean_cost)^2))))

#' Solve all scenarios of a model
#'
#' @param m Model.
#' @return List of scenarios (id, name, coef).
run_scenarios <- function(m) {
  frugal_a <- m$cost_mat[is_frugal, , drop = FALSE]
  frugal_b <- rep(target_total, sum(is_frugal))
  gain <- gain_linear(m)
  floor_a <- -gain$slope
  floor_b <- gain$intercept - solve_maxmin_gain(m, gain) + 1e-7
  list(
    list(id = "R0", name = sprintf("Report rates (EUR %.1f bn)", report_total_bn), coef = setNames(rep(1, m$n_tax), names(m$rates))),
    list(id = "A1", name = "Min max cost", coef = solve_minimax_cost(m)),
    list(id = "A2", name = "Min squared deviation", coef = solve_least_squares(m)),
    list(id = "B1", name = "Min max cost, frugal <= EU mean", coef = solve_minimax_cost(m, frugal_a, frugal_b)),
    list(id = "B2", name = "Min squared deviation, frugal <= EU mean", coef = solve_least_squares(m, frugal_a, frugal_b)),
    list(id = "C1", name = "Max min State budget gain, then min max cost", coef = solve_minimax_cost(m, floor_a, floor_b)),
    list(id = "C2", name = "Max min State budget gain, then min squared deviation", coef = solve_least_squares(m, floor_a, floor_b))
  )
}

#' Compute indicators for one scenario
#'
#' @param m Model.
#' @param sc Scenario list with `id`, `name` and `coef`.
#' @return List with summary row, country costs, country gains and cost by tax.
evaluate <- function(m, sc) {
  coef <- replace(sc$coef, sc$coef < 1e-4, 0)  # remove solver tolerance residuals
  gain_lin <- gain_linear(m)
  cost <- drop(m$cost_mat %*% coef)
  gain <- drop(gain_lin$slope %*% coef + gain_lin$intercept)
  total <- sum(m$eu_cost * coef)
  scores <- homogeneity(cost, total)
  suffix_names <- \(prefix, x) setNames(x, paste0(prefix, names(m$rates)))
  summary_row <- data.frame(
    id = sc$id, scenario = sc$name, t(suffix_names("rate_", coef * m$rates)),
    rmsd_from_eu_mean = sqrt(mean((cost - total)^2)), max_cost = max(cost), max_cost_country = countries[which.max(cost)],
    range_cost = max(cost) - min(cost), homogeneity_minimax = scores[["minimax"]], homogeneity_sq_dev = scores[["sq_dev"]],
    t(suffix_names("coef_", coef)), t(suffix_names("share_", 100 * m$eu_cost * coef / total)),
    total_pct_gni = total, total_eur_bn = total * gni_eu / 100,
    new_funds_eur_bn = sum(m$eu_cost * coef * m$funds_share) * gni_eu / 100,
    min_cost = min(cost), min_cost_country = countries[which.min(cost)], max_frugal_cost = max(cost[is_frugal]),
    max_frugal_cost_country = countries[is_frugal][which.max(cost[is_frugal])],
    min_state_gain = min(gain), min_state_gain_country = countries[which.min(gain)],
    check.names = FALSE
  )
  by_tax <- data.frame(id = sc$id, country = rep(countries, m$n_tax), tax = rep(m$labels, each = length(countries)),
                       rate = rep(coef * m$rates, each = length(countries)), cost_pct_gni = as.vector(sweep(m$cost_mat, 2, coef, `*`)))
  list(summary = summary_row, cost = cost, gain = gain, by_tax = by_tax)
}

#' Solve and evaluate all scenarios of a model
#'
#' @param m Model.
#' @return List with scenario ids and names, summary, cost, gain and by-tax data frames, and homogeneity by tax.
run_model <- function(m) {
  scenarios <- run_scenarios(m)
  results <- lapply(scenarios, \(sc) evaluate(m, sc))
  ids <- vapply(scenarios, \(sc) sc$id, character(1))
  eu_row <- "European Union (GNI-weighted)"
  list(
    ids = ids, names = vapply(scenarios, \(sc) sc$name, character(1)),
    summary = do.call(rbind, lapply(results, \(r) r$summary)),
    cost = data.frame(country = c(countries, eu_row), sapply(setNames(results, ids), \(r) c(r$cost, sum(r$cost * gni) / gni_eu)), is_frugal = c(is_frugal, NA), check.names = FALSE),
    gain = data.frame(country = c(countries, eu_row), sapply(setNames(results, ids), \(r) c(r$gain, sum(r$gain * gni) / gni_eu)), check.names = FALSE),
    by_tax = do.call(rbind, lapply(results, \(r) r$by_tax)),
    tax_scores = sapply(names(m$rates), \(j) homogeneity(m$cost_mat[, j], m$eu_cost[[j]]))  # 2 x n_tax, independent of the rate
  )
}

res <- lapply(models, run_model)

# Sanity checks: report scenario reproduces Table 4 State budget gains; splitting the luxury tax leaves it unchanged
gain_r0 <- setNames(res$base$gain$R0, res$base$gain$country)
stopifnot(abs(gain_r0[["Austria"]] - 0.379) < 5e-4, abs(gain_r0[["France"]] - 0.242) < 5e-4, isTRUE(all.equal(res$base$cost$R0, res$lux$cost$R0)))

for (r in res) print(r$summary[, c("id", grep("^rate_", names(r$summary), value = TRUE), "rmsd_from_eu_mean", "max_cost", "homogeneity_minimax", "homogeneity_sq_dev", "max_frugal_cost", "min_state_gain")], digits = 3)

# ── Export ────────────────────────────────────────────────────────────────────
notes <- data.frame(note = c(
  "Source: combined.xlsx (Table 2 = tables/table_gni.tex, Table 4 = tables/table_scenario_gni.tex) and disaggregated_luxury_base.csv. Script: min_heterogeneity.R.",
  "To change the revenue target, edit Parameters!B2 (EUR bn) and rerun Rscript min_heterogeneity.R: Excel does not recompute the optimisations. An empty cell resets it to the report's total.",
  "Summary: the report's 6 taxes. Summary_lux: same, with the luxury tax split into 7 category-specific taxes (report's rate 20% each). Each country's luxury revenue (Table 2) is split across categories in proportion to its category bases in disaggregated_luxury_base.csv, so country totals at 20% are unchanged; 1/3 of each category's revenue finances new funds.",
  "Coefficients c_j >= 0 multiply the report's rates. Revenues are assumed proportional to rates (no additional behavioural response).",
  sprintf("Constraint: total new revenues = EUR %.1f bn = %.3f%% of EU GNI (EUR %.0f bn). At the report's rates the total is EUR %.1f bn (%.3f%%).", target_bn, target_total, gni_eu, report_total_bn, 100 * report_total_bn / gni_eu),
  "Cost of country i = sum over taxes of c_j x (Table 2 cost at the report's rate), in % of national GNI. The EU mean is GNI-weighted and equals the target by construction.",
  "A1/B1/C1: minimise the maximum country cost (linear programme). A2/B2/C2: minimise the sum over the 27 Member States (unweighted) of squared deviations of cost from the EU mean (exact QP, primal active-set method).",
  sprintf("B1-B2: additional constraint cost <= EU mean for %s.", paste(frugal, collapse = ", ")),
  sprintf("C1-C2: maximise the minimum State budget gain (Table 4, col. 5) = (new revenues financing the EU budget - EUR %s bn budget expansion) / EU GNI - foregone domestic revenues (fixed) / national GNI; ties broken by min max cost (C1) or min squared deviation (C2). The gain differs across countries only through foregone revenues, so this amounts to maximising the EU-budget share: no wealth or luxury tax.", budget_expansion),
  "Row 9 of the summary tabs: custom rates (edit the rate cells); coefficients, total, RMSD, max/min cost, range and homogeneity scores are computed by Excel from the Data tabs (Table 2 costs at the report's rates).",
  "Rows 10-11 of the summary tabs: homogeneity scores of each tax alone (independent of its rate).",
  "Homogeneity scores (1 = identical cost in % GNI in every Member State, lower = more heterogeneous): minimax = EU mean / maximum country cost; squared deviations = EU mean / (EU mean + root mean squared deviation from the EU mean).",
  "Colours of tax rates: white = report's rate, green = above (full green at twice the report's rate or more), red = below (full red at 0).",
  sprintf("Rates are capped at %s times the report's rate (0 <= c_j <= %s).", rate_cap, rate_cap)
))

wb <- createWorkbook()
num3 <- createStyle(numFmt = "0.000")
num2 <- createStyle(numFmt = "0.00")
head_style <- createStyle(textDecoration = "bold", wrapText = TRUE, border = "bottom")
bold_style <- createStyle(textDecoration = "bold")
italic_style <- createStyle(textDecoration = "italic")
input_style <- createStyle(numFmt = "0.00", textDecoration = "bold", border = "TopBottomLeftRight", borderColour = "#BF9000", borderStyle = "medium")

#' Add a sheet with a data frame, number format, frozen header and auto widths
add_sheet <- function(sheet, df, fmt_cols, fmt) {
  addWorksheet(wb, sheet)
  writeData(wb, sheet, df, headerStyle = head_style)
  if (length(fmt_cols) > 0) addStyle(wb, sheet, fmt, rows = 2:(nrow(df) + 1), cols = fmt_cols, gridExpand = TRUE, stack = TRUE)
  freezePane(wb, sheet, firstRow = TRUE, firstCol = TRUE)
  setColWidths(wb, sheet, cols = seq_along(df), widths = "auto")
}

#' Write the summary tab of a model: scenarios (rows 2-8), custom rates computed by Excel (row 9), tax scores (rows 10-11)
#'
#' @param m Model.
#' @param r Output of run_model().
write_summary <- function(m, r) {
  sheet <- paste0("Summary", m$suffix)
  data_sheet <- paste0("Data", m$suffix)
  df <- r$summary
  n_scen <- nrow(df)
  add_sheet(sheet, df, which(vapply(df, is.numeric, logical(1))), num3)
  col_of <- \(name) match(name, names(df))
  rate_cols <- 2 + seq_len(m$n_tax)
  letters_rate <- int2col(rate_cols)
  addStyle(wb, sheet, num2, rows = 2:(n_scen + 1), cols = rate_cols, gridExpand = TRUE)
  addStyle(wb, sheet, bold_style, rows = match(bold_ids, df$id) + 1, cols = 2, gridExpand = TRUE, stack = TRUE)
  setColWidths(wb, sheet, cols = 2, widths = 55)

  # Row 9: custom rates, indicators computed by Excel
  row_custom <- n_scen + 2
  stopifnot(row_custom == custom_row)
  writeData(wb, sheet, data.frame("Custom", "Custom rates: edit the rate cells, indicators are computed by Excel"), startRow = row_custom, colNames = FALSE)
  writeData(wb, sheet, t(m$rates), startCol = rate_cols[1], startRow = row_custom, colNames = FALSE)
  addStyle(wb, sheet, input_style, rows = row_custom, cols = rate_cols)
  cost_col <- int2col(m$n_tax + 3)  # custom cost column in the data tab
  rng <- sprintf("%s!$%s$3:$%s$29", data_sheet, cost_col, cost_col)
  gni_rng <- sprintf("%s!$B$3:$B$29", data_sheet)
  cty_rng <- sprintf("%s!$A$3:$A$29", data_sheet)
  cell <- \(name) paste0(int2col(col_of(name)), row_custom)
  formulas <- c(
    total_pct_gni = sprintf("SUMPRODUCT(%s,%s)/SUM(%s)", rng, gni_rng, gni_rng),
    total_eur_bn = sprintf("%s*SUM(%s)/100", cell("total_pct_gni"), gni_rng),
    rmsd_from_eu_mean = sprintf("SQRT(SUMPRODUCT((%s-%s)^2)/COUNT(%s))", rng, cell("total_pct_gni"), rng),
    max_cost = sprintf("MAX(%s)", rng),
    max_cost_country = sprintf("INDEX(%s,MATCH(MAX(%s),%s,0))", cty_rng, rng, rng),
    range_cost = sprintf("MAX(%s)-MIN(%s)", rng, rng),
    homogeneity_minimax = sprintf("%s/%s", cell("total_pct_gni"), cell("max_cost")),
    homogeneity_sq_dev = sprintf("%s/(%s+%s)", cell("total_pct_gni"), cell("total_pct_gni"), cell("rmsd_from_eu_mean")),
    min_cost = sprintf("MIN(%s)", rng),
    min_cost_country = sprintf("INDEX(%s,MATCH(MIN(%s),%s,0))", cty_rng, rng, rng),
    setNames(sprintf("%s%d/%s!%s$2", letters_rate, row_custom, data_sheet, letters_rate), paste0("coef_", names(m$rates)))
  )
  for (name in names(formulas)) writeFormula(wb, sheet, formulas[[name]], startCol = col_of(name), startRow = row_custom)
  addStyle(wb, sheet, num3, rows = row_custom, cols = col_of(setdiff(names(formulas), c("max_cost_country", "min_cost_country"))), gridExpand = TRUE)

  # Rows 10-11: homogeneity score of each tax alone
  score_rows <- row_custom + 1:2
  writeData(wb, sheet, data.frame(c("H_minimax", "H_sq_dev"), c("Homogeneity score of each tax alone: minimax", "Homogeneity score of each tax alone: squared deviations")), startRow = score_rows[1], colNames = FALSE)
  writeData(wb, sheet, r$tax_scores, startCol = rate_cols[1], startRow = score_rows[1], colNames = FALSE)
  addStyle(wb, sheet, num3, rows = score_rows, cols = rate_cols, gridExpand = TRUE)
  addStyle(wb, sheet, italic_style, rows = row_custom:score_rows[2], cols = 1:2, gridExpand = TRUE, stack = TRUE)

  for (j in seq_len(m$n_tax)) conditionalFormatting(wb, sheet, cols = rate_cols[j], rows = 2:row_custom, type = "colourScale", style = c("#F8696B", "#FFFFFF", "#63BE7B"), rule = c(0, m$rates[[j]], 2 * m$rates[[j]]))
}

#' Write the detail tabs of a model: country costs, State budget gains, costs by tax, and Table 2 data used by row 9 formulas
#'
#' @param m Model.
#' @param r Output of run_model().
write_details <- function(m, r) {
  n_scen <- length(r$ids)
  add_sheet(paste0("Cost_pct_GNI", m$suffix), r$cost, 2:(n_scen + 1), num3)
  add_sheet(paste0("State_budget_gain_pct_GNI", m$suffix), r$gain, 2:(n_scen + 1), num3)
  add_sheet(paste0("Cost_by_tax", m$suffix), r$by_tax, 4:5, num3)
  data_sheet <- paste0("Data", m$suffix)
  data_df <- data.frame(country = c("Report rate (%)", countries), gni_eur_bn = c(NA, gni), rbind(m$rates, m$cost_mat), custom_cost_pct_gni = NA, check.names = FALSE)
  add_sheet(data_sheet, data_df, 2:(m$n_tax + 3), num3)
  rate_letters <- int2col(2 + seq_len(m$n_tax))
  first <- rate_letters[1]
  last <- rate_letters[m$n_tax]
  custom <- sprintf("SUMPRODUCT(%s%d:%s%d,Summary%s!$%s$%d:$%s$%d/$%s$2:$%s$2)", first, 3:29, last, 3:29, m$suffix, first, custom_row, last, custom_row, first, last)
  writeFormula(wb, data_sheet, custom, startCol = m$n_tax + 3, startRow = 3)
}

for (k in names(models)) write_summary(models[[k]], res[[k]])
addWorksheet(wb, "Parameters")
params <- data.frame(
  parameter = c("Total new revenues (target)", "Total new revenues (target)", "Report's total at the report's rates", "EU27 GNI"),
  value = c(target_bn, NA, report_total_bn, gni_eu),
  unit = c("EUR bn", "% of EU GNI", "EUR bn", "EUR bn"),
  description = c("Input: edit, then rerun Rscript min_heterogeneity.R (empty = report's total)", "= B2 / B5 x 100", sprintf("%.3f%% of EU GNI", 100 * report_total_bn / gni_eu), "Sum of Member States' GNI (combined.xlsx)")
)
writeData(wb, "Parameters", params, headerStyle = head_style)
writeFormula(wb, "Parameters", "B2/B5*100", startCol = 2, startRow = 3)
addStyle(wb, "Parameters", createStyle(fgFill = "#FFF2CC", textDecoration = "bold", numFmt = "0.0", border = "TopBottomLeftRight"), rows = 2, cols = 2)
addStyle(wb, "Parameters", num3, rows = 3, cols = 2)
addStyle(wb, "Parameters", createStyle(numFmt = "#,##0.0"), rows = 4:5, cols = 2, gridExpand = TRUE)
setColWidths(wb, "Parameters", cols = 1:4, widths = c(36, 12, 14, 80))

addWorksheet(wb, "Notes")
writeData(wb, "Notes", data.frame(id = res$base$ids, scenario = res$base$names), headerStyle = head_style)
writeData(wb, "Notes", notes, startCol = 2, startRow = length(res$base$ids) + 3, headerStyle = head_style)
setColWidths(wb, "Notes", cols = 1:2, widths = c(12, 90))
for (k in names(models)) write_details(models[[k]], res[[k]])
saveWorkbook(wb, out_file, overwrite = TRUE)

#' Remove worksheet relationships pointing to parts missing from the archive
#'
#' Works around openxlsx 4.2.8.1, which references drawing files it does not write
#' (the file then fails to open in openpyxl and triggers a repair prompt in Excel).
#' @param path Path to the .xlsx file, modified in place.
fix_dangling_rels <- function(path) {
  tmp_dir <- tempfile("xlsx_")
  unzip(path, exdir = tmp_dir)
  for (f in list.files(file.path(tmp_dir, "xl", "worksheets", "_rels"), full.names = TRUE)) {
    xml <- readLines(f, warn = FALSE, encoding = "UTF-8") |> paste(collapse = "")
    nodes <- regmatches(xml, gregexpr("<Relationship [^>]*/>", xml))[[1]]
    targets <- sub('.*Target="([^"]*)".*', "\\1", nodes)
    for (node in nodes[!file.exists(file.path(tmp_dir, "xl", "worksheets", targets))]) xml <- sub(node, "", xml, fixed = TRUE)
    writeLines(xml, f, useBytes = TRUE)
  }
  file.remove(path)
  zip::zipr(normalizePath(path, mustWork = FALSE), list.files(tmp_dir, full.names = TRUE), include_directories = FALSE)
  unlink(tmp_dir, recursive = TRUE)
}
fix_dangling_rels(out_file)
cat("Written", out_file, "\n")

# ── LaTeX tables (scenarios in columns) ───────────────────────────────────────
#' Format a number as in the report's tables: no leading zero, trailing zeros dropped
#'
#' @param x Numeric vector.
#' @param digits Number of decimals.
fmt_tex <- function(x, digits) {
  s <- formatC(round(x, digits), format = "f", digits = digits)
  s <- ifelse(grepl("\\.", s), sub("\\.?0+$", "", s), s)
  s <- sub("^(-?)0\\.", "\\1.", s)
  ifelse(s %in% c("", "-"), "0", s)
}

#' Format tax rates (%): 2 decimals below 1, 1 decimal below 10, none above
fmt_rate <- function(x) ifelse(abs(x) < 1, fmt_tex(x, 2), ifelse(abs(x) < 10, fmt_tex(x, 1), fmt_tex(x, 0)))

#' Escape LaTeX special characters
escape_tex <- function(x) gsub("([&%_#$])", "\\\\\\1", x)

#' Write a model's summary as a LaTeX table with scenarios in columns
#'
#' @param m Model.
#' @param r Output of run_model().
#' @param path Output .tex file.
#' @param caption,label Caption and label of the table.
write_tex_table <- function(m, r, path, caption, label) {
  df <- r$summary
  code_of <- setNames(names(country_codes), country_codes)
  row_tex <- \(name, values) paste0(name, " & ", paste(values, collapse = " & "), " \\\\")
  section <- \(title) sprintf("\\multicolumn{%d}{@{}l}{\\textit{%s}} \\\\", nrow(df) + 1, title)
  short <- c(lux_automotive = "Lux.: automotive", lux_personal = "Lux.: personal goods", lux_hospitality = "Lux.: hospitality", lux_wines = "Lux.: wines \\& spirits",
             lux_gourmet = "Lux.: gourmet \\& dining", lux_design = "Lux.: design \\& furniture", lux_yachts = "Lux.: yachts")
  labels <- ifelse(names(m$rates) %in% names(short), short[names(m$rates)], escape_tex(m$labels))
  rates <- vapply(seq_len(m$n_tax), \(j) row_tex(labels[[j]], fmt_rate(df[[paste0("rate_", names(m$rates)[j])]])), character(1))
  lines <- c(
    "% Generated by min_heterogeneity.R",
    "\\begin{table}[htbp]", "\\centering", "\\footnotesize", "\\setlength{\\tabcolsep}{4pt}",
    sprintf("\\caption{%s}", caption), sprintf("\\label{%s}", label),
    sprintf("\\begin{tabular}{@{}l*{%d}{r}@{}}", nrow(df)),
    "\\toprule",
    " & Report & \\multicolumn{2}{c}{Unconstrained} & \\multicolumn{2}{c}{Frugal $\\leq$ EU mean} & \\multicolumn{2}{c}{Max.\\ min.\\ gain} \\\\",
    "\\cmidrule(lr){3-4}\\cmidrule(lr){5-6}\\cmidrule(lr){7-8}",
    " & rates & Minimax & Sq.\\ dev. & Minimax & Sq.\\ dev. & Minimax & Sq.\\ dev. \\\\",
    row_tex("", df$id),
    "\\midrule",
    section("Tax rates (\\%)"), rates,
    "\\midrule",
    section("Minimax criterion"),
    row_tex("Max.\\ cost (\\% GNI)", fmt_tex(df$max_cost, 3)),
    row_tex("Country with max.\\ cost", code_of[df$max_cost_country]),
    row_tex("Homogeneity score", fmt_tex(df$homogeneity_minimax, 2)),
    section("Squared-deviation criterion"),
    row_tex("RMSD from mean (\\% GNI)", fmt_tex(df$rmsd_from_eu_mean, 3)),
    row_tex("Range of costs (\\% GNI)", fmt_tex(df$range_cost, 3)),
    row_tex("Homogeneity score", fmt_tex(df$homogeneity_sq_dev, 2)),
    section("State budget gain criterion"),
    row_tex("Min.\\ State gain (\\% GNI)", fmt_tex(df$min_state_gain, 3)),
    row_tex("Country with min.\\ gain", code_of[df$min_state_gain_country]),
    row_tex("New funds (EUR bn)", fmt_tex(df$new_funds_eur_bn, 1)),
    "\\bottomrule", "\\end{tabular}", "\\end{table}"
  )
  writeLines(lines, path, useBytes = TRUE)
  cat("Written", path, "\n")
}

write_tex_table(models$base, res$base, "tables/table_min_heterogeneity.tex",
                sprintf("Tax rates minimising the heterogeneity of costs across Member States (total new revenues: EUR %.1f bn; rates capped at %s times the report's rates).", target_bn, rate_cap), "tab:min_heterogeneity")
write_tex_table(models$lux, res$lux, "tables/table_min_heterogeneity_lux.tex",
                sprintf("Tax rates minimising the heterogeneity of costs across Member States, with category-specific luxury taxes (total new revenues: EUR %.1f bn; rates capped at %s times the report's rates).", target_bn, rate_cap), "tab:min_heterogeneity_lux")

#' Write the merged LaTeX table: the report's rates and selected scenarios of both models
#'
#' Columns: report's rates, then `ids` for the 6-tax model and for the model with category-specific
#' luxury taxes. Rows: tax rates, then the RMSD, the maximum cost, the countries bearing the highest
#' cost overall and among the frugal countries, and the new funds.
#' @param path Output .tex file.
#' @param caption,note,label Caption (above), note (below the table) and label.
#' @param ids Scenario ids shown for each model, in the order of the columns.
#' @param highlight Scenario id of the category-specific luxury model printed in bold (NULL for none).
write_tex_merged <- function(path, caption, note, label, ids = c("A2", "A1", "B2", "C2"), highlight = "A1") {
  code_of <- setNames(names(country_codes), country_codes)
  short <- c(lux_automotive = "\\quad automotive", lux_personal = "\\quad personal goods", lux_hospitality = "\\quad hospitality", lux_wines = "\\quad wines \\& spirits",
             lux_gourmet = "\\quad gourmet \\& dining", lux_design = "\\quad design \\& furniture", lux_yachts = "\\quad yachts")
  taxes <- c(names(report_rates), names(lux_categories))
  labels <- ifelse(taxes %in% names(short), short[taxes], escape_tex(tax_labels[taxes]))
  labels[taxes == "luxury"] <- "Luxury VAT"
  cols <- c(list(list(m = models$base, r = res$base, id = "R0")),
            lapply(ids, \(id) list(m = models$base, r = res$base, id = id)),
            lapply(ids, \(id) list(m = models$lux, r = res$lux, id = id)))
  #' Value of one indicator (or one tax rate) in one column, empty if the tax is absent from the model
  value <- function(col, name, tax = FALSE) {
    row <- col$r$summary[col$r$summary$id == col$id, ]
    if (tax) {
      if (col$id == "R0") return(if (name %in% names(report_rates) || name %in% names(lux_categories)) fmt_rate(report_rates[[if (name %in% names(report_rates)) name else "luxury"]]) else "")
      if (!name %in% names(col$m$rates)) return("")
      return(fmt_rate(row[[paste0("rate_", name)]]))
    }
    switch(name,
      max_cost = fmt_tex(row$max_cost, 3), max_cost_country = code_of[[row$max_cost_country]],
      max_frugal = sprintf("%s (%s)", code_of[[row$max_frugal_cost_country]], fmt_tex(row$max_frugal_cost, 2)),
      rmsd = fmt_tex(row$rmsd_from_eu_mean, 3), new_funds = fmt_tex(row$new_funds_eur_bn, 1))
  }
  # Column of the category-specific luxury model printed in bold (the compromise discussed in the text)
  bold_col <- if (is.null(highlight)) integer(0) else which(vapply(cols, \(col) identical(col$m$suffix, "_lux") && col$id == highlight, logical(1)))
  emph <- function(values) { in_bold <- intersect(bold_col, which(nzchar(values))); values[in_bold] <- paste0("\\textbf{", values[in_bold], "}"); values }
  row_tex <- \(name, values) paste0(name, " & ", paste(emph(values), collapse = " & "), " \\\\")
  crit <- c(A1 = "Minimax", A2 = "\\makecell{Sq.\\\\dev.}", B1 = "Frugal", B2 = "Frugal", C1 = "\\makecell{Max.\\\\gain}", C2 = "\\makecell{Max.\\\\gain}")
  lines <- c(
    "% Generated by min_heterogeneity.R", "\\begin{table}[htbp]", "\\centering", "\\footnotesize", "\\setlength{\\tabcolsep}{3pt}",
    sprintf("\\caption{%s}", caption), sprintf("\\label{%s}", label),
    sprintf("\\begin{tabular}{@{}l*{%d}{r}@{}}", length(cols)),
    "\\toprule",
    sprintf(" & Report & \\multicolumn{%d}{c}{Uniform luxury VAT} & \\multicolumn{%d}{c}{Luxury taxed by category} \\\\", length(ids), length(ids)),
    sprintf("\\cmidrule(lr){3-%d}\\cmidrule(lr){%d-%d}", 2 + length(ids), 3 + length(ids), 2 + 2 * length(ids)),
    row_tex("", c("rates", crit[ids], crit[ids])),
    "\\midrule",
    sprintf("\\multicolumn{%d}{@{}l}{\\textit{Tax rates (\\%%)}} \\\\", length(cols) + 1),
    vapply(seq_along(taxes), \(k) row_tex(labels[[k]], vapply(cols, value, character(1), name = taxes[k], tax = TRUE)), character(1)),
    "\\midrule",
    row_tex("RMSD from mean (\\% GNI)", vapply(cols, value, character(1), name = "rmsd")),
    row_tex("Max.\\ cost (\\% GNI)", vapply(cols, value, character(1), name = "max_cost")),
    row_tex("Highest-cost country", vapply(cols, value, character(1), name = "max_cost_country")),
    row_tex("Highest-cost frugal country", vapply(cols, value, character(1), name = "max_frugal")),
    row_tex("New funds (EUR bn)", vapply(cols, value, character(1), name = "new_funds")),
    "\\bottomrule", "\\end{tabular}",
    sprintf("\\begin{minipage}{\\textwidth}\\vspace{4pt}\\footnotesize %s\\end{minipage}", note),
    "\\end{table}"
  )
  writeLines(lines, path, useBytes = TRUE)
  cat("Written", path, "\n")
}

write_tex_merged("tables/table_min_heterogeneity_merged.tex",
                 "Tax rates minimising the heterogeneity of costs across Member States.",
                 sprintf("\\textit{Note:} Total new revenues are held at EUR %.1f bn, i.e.\\ %s\\%% of EU GNI, and each rate is capped at %s times the report's rate. The criteria are: minimising the sum over the 27 Member States of squared deviations of costs from the EU average (Sq.\\ dev.), minimising the maximum cost (Minimax), minimising squared deviations under the constraint that the frugal countries (%s) pay less than the EU average (Frugal), and maximising the minimum State budget gain (Max.\\ gain).",
                         target_bn, fmt_tex(target_total, 3), rate_cap, paste(names(country_codes)[match(frugal, country_codes)], collapse = ", ")),
                 "tab:min_heterogeneity")
