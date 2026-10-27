# Validate actual cached study records without generating substitute company data.
source("R/00_setup.R")
panel <- read_csv("data/processed/panel.csv", col_types = cols(cik = "c"))
companies <- read_csv("data/processed/companies.csv", col_types = cols(cik = "c"))
returns <- read_csv("data/processed/filing_returns.csv", col_types = cols(cik = "c"))
terms <- read_csv("data/processed/term_counts.csv", col_types = cols(cik = "c"))
coefficients <- read_csv("output/tables/regression_coefficients.csv")
models <- readRDS("output/tables/models.rds")
checks <- character()
check <- function(condition, label) {
  if (!isTRUE(condition)) stop("Validation failed: ", label, call. = FALSE)
  checks <<- c(checks, label)
}
near_all <- function(x, y, tolerance = 1e-9) {
  all(abs(x - y) <= tolerance * pmax(1, abs(x), abs(y)), na.rm = FALSE)
}

# Check the intended issuer-year grain, eligibility dates, and exposure units.
check(nrow(panel) > 0, "Panel is nonempty")
check(!anyDuplicated(companies$cik), "Company sample has one share class per issuer")
check(!anyDuplicated(panel[c("cik", "period_end")]), "Panel has one row per issuer and fiscal period end")
check(all(panel$cik %in% companies$cik), "Every panel issuer belongs to the selected sample")
check(all(panel$fiscal_year >= config$start_fy), "Fiscal-year floor is applied after fiscal label extraction")
check(all(abs(panel$fiscal_year - lubridate::year(panel$period_end)) <= 1), "Fiscal labels stay close to their actual fiscal period ends")
december_rows <- lubridate::month(panel$period_end) == 12
check(all(panel$fiscal_year[december_rows] == lubridate::year(panel$period_end[december_rows])),
      "December fiscal labels agree with the verified calendar-year period ends")
cover_checks <- read_csv("config/fiscal_year_overrides.csv", col_types = cols(cik = "c")) %>%
  inner_join(panel %>% select(cik, accession, fiscal_year), by = c("cik", "accession"))
check(all(cover_checks$fiscal_year == cover_checks$fiscal_year_cover), "Verified cover corrections override missing or erroneous source tags")
check(all(panel$filing_date <= config$as_of), "No filings occur after the study cutoff")
check(all(panel$period_end <= panel$filing_date), "Fiscal periods end before their annual filings")
check(all(panel$word_count >= 1000), "Every parsed report contains at least 1,000 words")
check(near_all(panel$ai_intensity, panel$ai_count * 10000 / panel$word_count), "AI intensity has the requested per-10,000-word units")
check(all(panel$post2022 == as.integer(panel$filing_date >= as.Date("2022-11-30"))), "Post indicator uses actual ChatGPT release date")
term_totals <- aggregate(count ~ cik + accession, data = terms, FUN = sum)
matched_terms <- match(paste(panel$cik, panel$accession), paste(term_totals$cik, term_totals$accession))
check(!anyNA(matched_terms), "All panel reports have dictionary-level count records")
check(all(panel$ai_count == term_totals$count[matched_terms]), "Report AI totals reconcile to dictionary counts")

# Test boundaries and overlapping phrases with a short text fixture, not company data.
dictionary <- read_csv("config/ai_dictionary.csv")
count_expression <- Filter(function(x) is.call(x) && identical(x[[1]], as.name("<-")) &&
  identical(x[[2]], as.name("count_ai_terms")), as.list(parse("R/03_count_ai_terms.R")))
check(length(count_expression) == 1, "The dictionary counting function can be inspected independently")
eval(count_expression[[1]])
fixture <- "AI ai RAIN paid AIG AIs generative AI artificial intelligence machine learning large language models LLM LLMs"
fixture_counts <- count_ai_terms(fixture, dictionary)
expected_counts <- c("generative AI" = 1L, "artificial intelligence" = 1L,
                     "machine learning" = 1L, "large language model" = 1L,
                     "LLM" = 2L, "AI" = 1L)
check(all(fixture_counts == expected_counts[dictionary$term]), "AI case sensitivity, whole-word boundaries, plurals, and phrase overlaps behave as documented")

# Independently reconcile observed revenue growth to retained source amounts.
current_rows <- is.finite(panel$current_growth)
check(all(panel$prior_revenue[current_rows] > 0), "Current growth has positive prior-revenue denominators")
check(near_all(panel$current_growth[current_rows], panel$revenue[current_rows] / panel$prior_revenue[current_rows] - 1), "Current revenue growth reconciles to source amounts")
next_rows <- is.finite(panel$next_revenue_growth)
check(all(as.integer(panel$next_period_end[next_rows] - panel$period_end[next_rows]) >= 330 &
          as.integer(panel$next_period_end[next_rows] - panel$period_end[next_rows]) <= 400), "Next revenue outcomes use an adjacent fiscal year")
check(all(panel$next_prior_end[next_rows] == panel$period_end[next_rows]), "Next growth's prior period matches the report's fiscal period")
check(near_all(panel$next_revenue_growth[next_rows], panel$next_revenue[next_rows] / panel$next_prior_revenue[next_rows] - 1),
      "Next-year growth reconciles to the following original filing's annual comparison")

# Check complete windows, identical stock/SPY endpoints, and return arithmetic.
valid_returns <- returns[returns$return_status == "ok", ]
invalid_returns <- returns[returns$return_status != "ok", ]
check(nrow(valid_returns) > 0, "At least one complete 12-month return exists")
check(all(valid_returns$return_start > valid_returns$filing_date), "Return entry occurs strictly after filing")
check(all(valid_returns$return_start <= valid_returns$filing_date + 7), "Return entry has a nearby common trading day")
check(all(valid_returns$return_end >= valid_returns$anniversary), "Return exit reaches the calendar anniversary")
check(all(valid_returns$return_end <= valid_returns$anniversary + 7 & valid_returns$return_end <= config$as_of), "Return exits are observed and near the anniversary")
check(near_all(valid_returns$stock_return_12m, valid_returns$stock_end / valid_returns$stock_start - 1), "Stock returns reconcile to adjusted endpoints")
check(near_all(valid_returns$spy_return_12m, valid_returns$spy_end / valid_returns$spy_start - 1), "SPY returns reconcile to adjusted endpoints")
check(near_all(valid_returns$excess_return_12m, valid_returns$stock_return_12m - valid_returns$spy_return_12m), "Excess return is the arithmetic percentage-point difference")
check(all(is.na(invalid_returns$excess_return_12m)), "Incomplete or unavailable windows do not acquire return values")
check(all(panel$return_status[is.finite(panel$excess_return_12m)] == "ok"), "Every panel return has a valid complete-window status")

# Reconcile retained endpoints to the daily stock and benchmark input records.
source_prices <- read_csv("data/processed/prices.csv")
check(!anyDuplicated(source_prices[c("ticker", "date")]), "Daily prices have unique ticker-date keys")
for (endpoint in c("start", "end")) {
  check_prices <- valid_returns %>%
    mutate(endpoint_date = .data[[paste0("return_", endpoint)]]) %>%
    left_join(source_prices %>% select(ticker, endpoint_date = date, source_stock = adjusted),
              by = c("ticker", "endpoint_date")) %>%
    left_join(source_prices %>% filter(ticker == "SPY") %>%
                select(endpoint_date = date, source_spy = adjusted), by = "endpoint_date")
  check(near_all(check_prices[[paste0("stock_", endpoint)]], check_prices$source_stock),
        paste("Stock", endpoint, "endpoints match actual daily price inputs"))
  check(near_all(check_prices[[paste0("spy_", endpoint)]], check_prices$source_spy),
        paste("SPY", endpoint, "endpoints match actual daily price inputs"))
}

# Verify that each outcome's main regressions retain its own eligible sample.
model_status <- read_csv("output/tables/model_status.csv")
main_status <- filter(model_status, model %in% c("baseline", "interaction"))
check(nrow(main_status) == 4 && all(main_status$status == "estimated"), "All four requested main regressions estimated successfully")
for (outcome in c("next_revenue_growth", "excess_return_12m")) {
  eligible <- Reduce(`&`, lapply(panel[c(outcome, "ai_intensity", "log_revenue", "current_growth", "post2022")], is.finite))
  outcome_status <- filter(main_status, .data$outcome == !!outcome)
  check(all(outcome_status$n_available == sum(eligible)), paste(outcome, "uses its separate complete-case sample"))
  check(all(outcome_status$n_obs <= sum(eligible)), paste(outcome, "does not add observations during estimation"))
}
check(all(coefficients$p_value >= 0 & coefficients$p_value <= 1), "Regression p-values lie between zero and one")
check(all(coefficients$conf_low <= coefficients$estimate & coefficients$estimate <= coefficients$conf_high), "Regression confidence intervals contain their estimates")

# Recompute the interaction model's post-period slope directly from the saved covariance matrix.
slopes <- read_csv("output/tables/effect_slopes.csv")
for (outcome in c("next_revenue_growth", "excess_return_12m")) {
  fit <- models[[paste(outcome, "interaction", sep = "__")]]
  beta <- coef(fit)
  interaction <- intersect(c("ai_intensity:post2022", "post2022:ai_intensity"), names(beta))
  check(length(interaction) == 1, paste(outcome, "has an identifiable AI interaction"))
  expected_slope <- beta[["ai_intensity"]] + beta[[interaction]]
  variance <- vcov(fit)
  expected_se <- sqrt(variance["ai_intensity", "ai_intensity"] + variance[interaction, interaction] +
                        2 * variance["ai_intensity", interaction])
  actual <- filter(slopes, .data$outcome == !!outcome, model == "interaction", period == "post")
  check(nrow(actual) == 1 && near_all(actual$estimate, expected_slope) && near_all(actual$std_error, expected_se),
        paste(outcome, "post slope includes coefficient covariance"))
}
write_csv(tibble(check = checks, status = "passed"), "output/tables/validation_checks.csv")
message(length(checks), " actual-data and dictionary checks passed.")
