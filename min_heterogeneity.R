# Optimal combination of tax rates minimising the heterogeneity of costs (% of GNI) across Member States
#
# Data: combined.xlsx (source of Table 2 = tables/table_gni.tex and Table 4 = tables/table_scenario_gni.tex).
# Each tax j gets a coefficient c_j >= 0 applied to the report's rate; the cost of country i is
# cost_i = sum_j c_j * t_ij, where t_ij is the Table 2 cost (% GNI) at the report's rates.
# Constraint: total new revenues = target (EUR bn), read from the Parameters sheet of the output workbook
# (default: the report's total at the report's rates, EUR 102.6 bn = .705% of EU GNI).
#
# Run from the project root: Rscript optimal_rates/optimal_rates.R

library(openxlsx)
library(lpSolveAPI)

# ── Parameters ────────────────────────────────────────────────────────────────
report_rates <- c(dst = 3, ad = 15, ftt = 0.5, wealth = 0.5, aviation = 20, luxury = 20)  # report's rates (%)
tax_labels <- c(dst = "DST", ad = "Ad tax", ftt = "FTT", wealth = "Wealth tax", aviation = "Aviation tax", luxury = "Luxury VAT")
new_funds_share <- c(dst = 0, ad = 0, ftt = 0, wealth = 3/5, aviation = 0, luxury = 1/3)  # share of revenues financing new funds (Table 4, col. 3)
budget_expansion <- 14  # EUR bn, expansion of the EU budget (combined.xlsx, Q46)
frugal <- c("Germany", "Ireland", "Netherlands", "Sweden")
tol <- 1e-9
out_file <- "optimal_rates/optimal_rates.xlsx"

# ── Data ──────────────────────────────────────────────────────────────────────
raw <- read.xlsx("combined.xlsx", rows = 8:34, cols = c(1, 2, 4, 6, 8, 10, 13, 16, 25, 26), colNames = FALSE)
names(raw) <- c("country", names(report_rates), "gni", "foregone_dst", "foregone_ftt")
stopifnot(nrow(raw) == 27, raw$country[1] == "Austria", raw$country[27] == "Sweden", all(frugal %in% raw$country))
if (anyNA(raw[, c(names(report_rates), "gni")])) stop("Missing revenue or GNI values in combined.xlsx")
# Blank foregone cells mean no existing domestic DST/FTT: set to 0 explicitly
raw$foregone_dst <- ifelse(is.na(raw$foregone_dst), 0, raw$foregone_dst)
raw$foregone_ftt <- ifelse(is.na(raw$foregone_ftt), 0, raw$foregone_ftt)

countries <- raw$country
n_tax <- length(report_rates)
revenue <- as.matrix(raw[, names(report_rates)])  # EUR millions at the report's rates
gni <- raw$gni  # EUR bn
gni_eu <- sum(gni)
cost_mat <- revenue / gni / 10  # Table 2: % of national GNI
eu_cost <- colSums(revenue) / gni_eu / 10  # % of EU GNI, per tax
is_frugal <- countries %in% frugal
report_total_bn <- sum(revenue) / 1000

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

#' State budget gain as a linear function of the coefficients (Table 4, col. 5)
#'
#' gain_i = (EU-budget revenues - budget expansion) / EU GNI - foregone_i / GNI_i, in % of GNI,
#' where EU-budget revenues are new revenues net of those financing new funds.
#' @param foregone_scales If TRUE, foregone domestic revenues scale with the DST and FTT coefficients
#'   (they are proxied by the EU tax's revenues in combined.xlsx); if FALSE they are fixed.
#' @return List with `slope` (27 x 6 matrix) and `intercept` (length 27) such that gain = slope %*% c + intercept.
gain_linear <- function(foregone_scales = FALSE) {
  slope <- matrix(eu_cost * (1 - new_funds_share), nrow = length(countries), ncol = n_tax, byrow = TRUE, dimnames = list(countries, names(report_rates)))
  intercept <- rep(-100 * budget_expansion / gni_eu, length(countries))
  foregone_pct <- cbind(dst = raw$foregone_dst, ftt = raw$foregone_ftt) / gni / 10
  # Austria's foregone revenue is its national ad tax, not a proxy from the EU DST: always fixed
  scalable <- countries != "Austria"
  if (foregone_scales) {
    slope[scalable, "dst"] <- slope[scalable, "dst"] - foregone_pct[scalable, "dst"]
    slope[, "ftt"] <- slope[, "ftt"] - foregone_pct[, "ftt"]
    intercept <- intercept - ifelse(scalable, 0, foregone_pct[, "dst"])
  } else intercept <- intercept - rowSums(foregone_pct)
  list(slope = slope, intercept = intercept)
}

#' Solve a linear programme min obj'x s.t. a_ineq x <= b_ineq, a_eq x = b_eq
#'
#' @param obj Objective vector.
#' @param a_ineq,b_ineq Inequality constraints (may be NULL).
#' @param a_eq,b_eq Equality constraints.
#' @param lower Lower bounds of the variables.
#' @return Optimal x.
solve_lp <- function(obj, a_ineq, b_ineq, a_eq, b_eq, lower) {
  lp <- make.lp(0, length(obj))
  lp.control(lp, sense = "min")
  set.objfn(lp, obj)
  if (!is.null(a_ineq)) for (k in seq_len(nrow(a_ineq))) add.constraint(lp, a_ineq[k, ], "<=", b_ineq[k])
  for (k in seq_len(nrow(a_eq))) add.constraint(lp, a_eq[k, ], "=", b_eq[k])
  set.bounds(lp, lower = lower, upper = rep(Inf, length(obj)))
  status <- solve(lp)
  if (status != 0) stop("LP solver failed with status ", status)
  get.variables(lp)
}

#' Minimise the maximum country cost (% GNI) under linear constraints on the coefficients
#'
#' @param a_ineq,b_ineq Extra constraints a_ineq c <= b_ineq (may be NULL).
#' @return Coefficient vector.
solve_minimax_cost <- function(a_ineq = NULL, b_ineq = NULL) {
  a <- rbind(cbind(cost_mat, -1), if (!is.null(a_ineq)) cbind(a_ineq, 0))
  b <- c(rep(0, nrow(cost_mat)), b_ineq)
  x <- solve_lp(c(rep(0, n_tax), 1), a, b, matrix(c(eu_cost, 0), 1), target_total, c(rep(0, n_tax), -Inf))
  setNames(x[1:n_tax], names(report_rates))
}

#' Maximise the minimum State budget gain (% GNI)
#'
#' @param gain Output of gain_linear().
#' @return Optimal minimum gain (% GNI).
solve_maxmin_gain <- function(gain) {
  a <- cbind(-gain$slope, 1)
  x <- solve_lp(c(rep(0, n_tax), -1), a, gain$intercept, matrix(c(eu_cost, 0), 1), target_total, c(rep(0, n_tax), -Inf))
  x[n_tax + 1]
}

#' Minimise the sum over countries of squared deviations of cost from the EU mean (target_total)
#'
#' Exact solution of the convex QP by enumeration of active sets: for each subset of inequality
#' constraints treated as binding, the equality-constrained problem is solved through its KKT system;
#' the best feasible candidate is the global optimum. Tractable because there are only 6 coefficients.
#' @param a_ineq,b_ineq Extra constraints a_ineq c <= b_ineq (may be NULL); non-negativity is added.
#' @return Coefficient vector.
solve_least_squares <- function(a_ineq = NULL, b_ineq = NULL) {
  a_all <- rbind(-diag(n_tax), a_ineq)
  b_all <- c(rep(0, n_tax), b_ineq)
  # Drop duplicated constraint rows, keeping the tightest bound
  key <- apply(round(a_all, 12), 1, paste, collapse = "|")
  keep <- !duplicated(key)
  b_all <- sapply(key[keep], \(k) min(b_all[key == k])) |> unname()
  a_all <- a_all[keep, , drop = FALSE]
  hess <- 2 * crossprod(cost_mat)
  grad <- 2 * drop(crossprod(cost_mat, rep(target_total, nrow(cost_mat))))
  best <- NULL
  best_obj <- Inf
  for (k in 0:(n_tax - 1)) {
    subsets <- if (k == 0) list(integer(0)) else combn(nrow(a_all), k, simplify = FALSE)
    for (s in subsets) {
      a_act <- rbind(eu_cost, a_all[s, , drop = FALSE])
      b_act <- c(target_total, b_all[s])
      kkt <- rbind(cbind(hess, t(a_act)), cbind(a_act, matrix(0, nrow(a_act), nrow(a_act))))
      sol <- tryCatch(solve(kkt, c(grad, b_act)), error = \(e) NULL)
      if (is.null(sol)) next
      x <- sol[1:n_tax]
      if (any(a_all %*% x > b_all + 1e-8)) next
      obj <- sum((cost_mat %*% x - target_total)^2)
      if (obj < best_obj - tol) { best_obj <- obj; best <- x }
    }
  }
  if (is.null(best)) stop("No feasible least-squares solution")
  setNames(pmax(best, 0), names(report_rates))
}

# ── Scenarios ─────────────────────────────────────────────────────────────────
frugal_a <- cost_mat[is_frugal, , drop = FALSE]
frugal_b <- rep(target_total, sum(is_frugal))

#' Constraint rows imposing gain_i >= g_star for all countries
#' @param gain Output of gain_linear().
#' @param g_star Minimum gain (% GNI).
gain_floor <- function(gain, g_star) list(a = -gain$slope, b = gain$intercept - g_star + 1e-7)

gain_fixed <- gain_linear(foregone_scales = FALSE)
gain_scaled <- gain_linear(foregone_scales = TRUE)
g_star_fixed <- solve_maxmin_gain(gain_fixed)
g_star_scaled <- solve_maxmin_gain(gain_scaled)
floor_fixed <- gain_floor(gain_fixed, g_star_fixed)
floor_scaled <- gain_floor(gain_scaled, g_star_scaled)

#' Coefficients raising the whole target from a single tax
#' @param j Tax name.
single_tax_coef <- function(j) setNames(ifelse(names(report_rates) == j, target_total / eu_cost[j], 0), names(report_rates))

scenarios <- list(
  list(id = "R0", name = sprintf("Report rates (EUR %.1f bn)", report_total_bn), coef = setNames(rep(1, n_tax), names(report_rates)), gain = gain_fixed),
  if (abs(target_bn - report_total_bn) > 1e-6) list(id = "R1", name = "Report rates scaled to target", coef = setNames(rep(target_total / sum(eu_cost), n_tax), names(report_rates)), gain = gain_fixed),
  list(id = "A1", name = "Min max cost", coef = solve_minimax_cost(), gain = gain_fixed),
  list(id = "B1", name = "Min squared deviation", coef = solve_least_squares(), gain = gain_fixed),
  list(id = "A2", name = "Min max cost, frugal <= EU mean", coef = solve_minimax_cost(frugal_a, frugal_b), gain = gain_fixed),
  list(id = "B2", name = "Min squared deviation, frugal <= EU mean", coef = solve_least_squares(frugal_a, frugal_b), gain = gain_fixed),
  list(id = "C1", name = "Max min State budget gain, then min max cost", coef = solve_minimax_cost(floor_fixed$a, floor_fixed$b), gain = gain_fixed),
  list(id = "C2", name = "Max min State budget gain, then min squared deviation", coef = solve_least_squares(floor_fixed$a, floor_fixed$b), gain = gain_fixed),
  list(id = "C3", name = "Max min State budget gain (foregone rev. scale with rates), then min max cost", coef = solve_minimax_cost(floor_scaled$a, floor_scaled$b), gain = gain_scaled),
  list(id = "C4", name = "Max min State budget gain (foregone rev. scale with rates), then min squared deviation", coef = solve_least_squares(floor_scaled$a, floor_scaled$b), gain = gain_scaled)
)
# Homogeneity of each tax: the whole target raised from that tax alone
single_tax <- lapply(names(report_rates), \(j) list(id = paste0("H_", j), name = paste0("Homogeneity: ", tax_labels[[j]], " only"), coef = single_tax_coef(j), gain = gain_fixed))
scenarios <- c(Filter(Negate(is.null), scenarios), single_tax)

# ── Results ───────────────────────────────────────────────────────────────────
#' Compute indicators for one scenario
#' @param sc Scenario list with `coef` and `gain`.
#' @return List with summary row, country costs, country gains and cost by tax.
evaluate <- function(sc) {
  sc$coef[sc$coef < 1e-6] <- 0  # remove solver tolerance residuals
  cost <- drop(cost_mat %*% sc$coef)
  gain <- drop(sc$gain$slope %*% sc$coef + sc$gain$intercept)
  total <- sum(eu_cost * sc$coef)
  summary_row <- data.frame(
    id = sc$id, scenario = sc$name,
    t(setNames(sc$coef * report_rates, paste0("rate_", names(report_rates)))),
    t(setNames(sc$coef, paste0("coef_", names(report_rates)))),
    t(setNames(100 * eu_cost * sc$coef / total, paste0("share_", names(report_rates)))),
    total_pct_gni = total, total_eur_bn = total * gni_eu / 100,
    new_funds_eur_bn = sum(eu_cost * sc$coef * new_funds_share) * gni_eu / 100,
    max_cost = max(cost), max_cost_country = countries[which.max(cost)],
    min_cost = min(cost), min_cost_country = countries[which.min(cost)],
    range_cost = max(cost) - min(cost), rmsd_from_eu_mean = sqrt(mean((cost - total)^2)),
    homogeneity_minimax = total / max(cost), homogeneity_sq_dev = total / (total + sqrt(mean((cost - total)^2))),
    max_frugal_cost = max(cost[is_frugal]),
    min_state_gain = min(gain), min_state_gain_country = countries[which.min(gain)],
    check.names = FALSE
  )
  by_tax <- data.frame(id = sc$id, country = rep(countries, n_tax), tax = rep(tax_labels, each = length(countries)), rate = rep(sc$coef * report_rates, each = length(countries)), cost_pct_gni = as.vector(sweep(cost_mat, 2, sc$coef, `*`)))
  list(summary = summary_row, cost = cost, gain = gain, by_tax = by_tax)
}

results <- lapply(scenarios, evaluate)
ids <- vapply(scenarios, \(sc) sc$id, character(1))
summary_df <- do.call(rbind, lapply(results, \(r) r$summary))
cost_df <- data.frame(country = c(countries, "European Union (GNI-weighted)"), sapply(setNames(results, ids), \(r) c(r$cost, sum(r$cost * gni) / gni_eu)), is_frugal = c(is_frugal, NA), check.names = FALSE)
gain_df <- data.frame(country = c(countries, "European Union (GNI-weighted)"), sapply(setNames(results, ids), \(r) c(r$gain, sum(r$gain * gni) / gni_eu)), check.names = FALSE)
by_tax_df <- do.call(rbind, lapply(results, \(r) r$by_tax))

# Sanity check: report scenario reproduces Table 2 totals and Table 4 State budget gains
stopifnot(abs(results[[1]]$gain[countries == "Austria"] - 0.379) < 5e-4, abs(results[[1]]$gain[countries == "France"] - 0.242) < 5e-4)

print(summary_df[, c("id", paste0("rate_", names(report_rates)), "total_eur_bn", "max_cost", "rmsd_from_eu_mean", "homogeneity_minimax", "homogeneity_sq_dev", "max_frugal_cost", "min_state_gain")], digits = 3)

# ── Export ────────────────────────────────────────────────────────────────────
notes <- data.frame(note = c(
  "Source: combined.xlsx (Table 2 = tables/table_gni.tex, Table 4 = tables/table_scenario_gni.tex). Script: optimal_rates/optimal_rates.R.",
  "To change the revenue target, edit Parameters!B2 (EUR bn) and rerun Rscript optimal_rates/optimal_rates.R: Excel does not recompute the optimisations. An empty cell resets it to the report's total.",
  sprintf("Coefficients c_j >= 0 multiply the report's rates: DST %s%%, Ad tax %s%%, FTT %s%%, Wealth tax %s%%, Aviation tax %s%%, Luxury VAT %s%%. Revenues are assumed proportional to rates (no additional behavioural response).", report_rates[1], report_rates[2], report_rates[3], report_rates[4], report_rates[5], report_rates[6]),
  sprintf("Constraint: total new revenues = EUR %.1f bn = %.3f%% of EU GNI (EUR %.0f bn). At the report's rates the total is EUR %.1f bn (%.3f%%).", target_bn, target_total, gni_eu, report_total_bn, sum(eu_cost)),
  "Cost of country i = sum over taxes of c_j x (Table 2 cost at the report's rate), in % of national GNI. The EU mean is GNI-weighted and equals the target by construction.",
  "A: minimise the maximum country cost (linear programme). B: minimise the sum over the 27 Member States (unweighted) of squared deviations of cost from the EU mean (exact QP by active-set enumeration).",
  sprintf("Frugal variants (A2, B2): cost <= EU mean for %s.", paste(frugal, collapse = ", ")),
  sprintf("State budget gain (Table 4, col. 5) = (new revenues financing the EU budget - EUR %s bn budget expansion) / EU GNI - foregone domestic revenues / national GNI. New funds are financed by 60%% of the wealth tax and a third of the luxury tax.", budget_expansion),
  "C1-C2: foregone domestic revenues fixed (existing national DSTs/FTTs do not depend on EU rates). The gain is then uniform across countries except for foregone revenues, so maximising the minimum gain amounts to maximising the EU-budget share, i.e. no wealth or luxury tax; ties are broken by min max cost (C1) or min squared deviation (C2).",
  "C3-C4: foregone domestic revenues scale with the DST and FTT coefficients, as in combined.xlsx where they are proxied by the EU tax's revenues (except Austria's national ad tax, kept fixed); ties broken as in C1-C2.",
  "H rows: homogeneity of each tax, i.e. the whole target raised from that tax alone (rate shown is the rate this would require). Scores do not depend on the target.",
  "Homogeneity scores (1 = identical cost in % GNI in every Member State, lower = more heterogeneous): minimax = EU mean / maximum country cost; squared deviations = EU mean / (EU mean + root mean squared deviation from the EU mean).",
  "Colours of tax rates (Summary): white = report's rate, green = above (full green at twice the report's rate or more), red = below (full red at 0).",
  "Least-squares solutions are unique (strictly convex objective); minimax solutions were checked to be unique (range of each rate over the optimal set is degenerate).",
  "Rates are not bounded above; very high coefficients should be read as a direction rather than as a feasible rate (revenue proportionality would break down)."
))

wb <- createWorkbook()
pct3 <- createStyle(numFmt = "0.000")
pct_rate <- createStyle(numFmt = "0.00")
head_style <- createStyle(textDecoration = "bold", wrapText = TRUE, border = "bottom")
input_style <- createStyle(fgFill = "#FFF2CC", textDecoration = "bold", numFmt = "0.0", border = "TopBottomLeftRight")
add_sheet <- function(sheet, df, fmt_cols, fmt) {
  addWorksheet(wb, sheet)
  writeData(wb, sheet, df, headerStyle = head_style)
  if (length(fmt_cols) > 0) addStyle(wb, sheet, fmt, rows = 2:(nrow(df) + 1), cols = fmt_cols, gridExpand = TRUE)
  freezePane(wb, sheet, firstRow = TRUE, firstCol = TRUE)
  setColWidths(wb, sheet, cols = seq_along(df), widths = "auto")
}

addWorksheet(wb, "Parameters")
params <- data.frame(
  parameter = c("Total new revenues (target)", "Total new revenues (target)", "Report's total at the report's rates", "EU27 GNI"),
  value = c(target_bn, NA, report_total_bn, gni_eu),
  unit = c("EUR bn", "% of EU GNI", "EUR bn", "EUR bn"),
  description = c("Input: edit, then rerun Rscript optimal_rates/optimal_rates.R (empty = report's total)", "= B2 / B5 x 100", sprintf("%.3f%% of EU GNI", sum(eu_cost)), "Sum of Member States' GNI (combined.xlsx)")
)
writeData(wb, "Parameters", params, headerStyle = head_style)
writeFormula(wb, "Parameters", "B2/B5*100", startCol = 2, startRow = 3)
addStyle(wb, "Parameters", input_style, rows = 2, cols = 2)
addStyle(wb, "Parameters", pct3, rows = 3, cols = 2)
addStyle(wb, "Parameters", createStyle(numFmt = "#,##0.0"), rows = 4:5, cols = 2, gridExpand = TRUE)
setColWidths(wb, "Parameters", cols = 1:4, widths = c(36, 12, 14, 80))

addWorksheet(wb, "Notes")
writeData(wb, "Notes", data.frame(id = ids, scenario = vapply(scenarios, \(sc) sc$name, character(1))), headerStyle = head_style)
writeData(wb, "Notes", notes, startCol = 2, startRow = length(ids) + 3, headerStyle = head_style)
setColWidths(wb, "Notes", cols = 1:2, widths = c(12, 90))

add_sheet("Summary", summary_df, which(vapply(summary_df, is.numeric, logical(1))), pct3)
rate_rows <- 2:(nrow(summary_df) + 1)
addStyle(wb, "Summary", pct_rate, rows = rate_rows, cols = 3:(2 + n_tax), gridExpand = TRUE)
for (j in seq_len(n_tax)) conditionalFormatting(wb, "Summary", cols = 2 + j, rows = rate_rows, type = "colourScale", style = c("#F8696B", "#FFFFFF", "#63BE7B"), rule = c(0, report_rates[[j]], 2 * report_rates[[j]]))
add_sheet("Cost_pct_GNI", cost_df, 2:(length(ids) + 1), pct3)
add_sheet("State_budget_gain_pct_GNI", gain_df, 2:(length(ids) + 1), pct3)
add_sheet("Cost_by_tax", by_tax_df, 4:5, pct3)
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
