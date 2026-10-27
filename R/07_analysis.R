# Compare AI disclosure with later outcomes using the observed company-year panel.
source("R/00_setup.R")
suppressPackageStartupMessages(library(fixest))

panel <- readr::read_csv("data/processed/panel.csv", show_col_types = FALSE,
                         col_types = cols(cik = col_character())) %>%
  mutate(filing_date = as.Date(filing_date), period_end = as.Date(period_end),
         cik = as.character(cik), industry = replace_na(industry, "Unclassified"))
stopifnot(nrow(panel) > 0, !anyDuplicated(panel[c("cik", "period_end")]))
stopifnot(all(panel$ai_intensity >= 0, na.rm = TRUE))
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)

# Keep missing outcomes distinct from a measured zero.
observed_mean <- function(x) if (any(is.finite(x))) mean(x[is.finite(x)]) else NA_real_
observed_median <- function(x) if (any(is.finite(x))) median(x[is.finite(x)]) else NA_real_
write_table <- function(x, name) readr::write_csv(x, file.path("output/tables", name), na = "")

# Use filing year for timing and retain fiscal-year trends as a separate table.
trend_year <- panel %>%
  group_by(filing_year) %>%
  summarise(n_filings = n(), n_firms = n_distinct(cik),
            mean_ai_intensity = observed_mean(ai_intensity),
            median_ai_intensity = observed_median(ai_intensity),
            pooled_ai_intensity = 10000 * sum(ai_count) / sum(word_count),
            share_any_ai = mean(ai_count > 0),
            n_revenue_outcomes = sum(is.finite(next_revenue_growth)),
            n_return_outcomes = sum(is.finite(excess_return_12m)),
            mean_next_revenue_growth = observed_mean(next_revenue_growth),
            mean_excess_return_12m = observed_mean(excess_return_12m), .groups = "drop")
trend_fiscal_year <- panel %>%
  group_by(fiscal_year) %>%
  summarise(n_filings = n(), n_firms = n_distinct(cik),
            mean_ai_intensity = observed_mean(ai_intensity),
            median_ai_intensity = observed_median(ai_intensity), .groups = "drop")
trend_industry <- panel %>%
  group_by(industry, filing_year) %>%
  summarise(n_filings = n(), n_firms = n_distinct(cik),
            mean_ai_intensity = observed_mean(ai_intensity),
            median_ai_intensity = observed_median(ai_intensity),
            share_any_ai = mean(ai_count > 0), .groups = "drop")
period_comparison <- panel %>%
  mutate(period = if_else(post2022 == 1L, "On/after 2022-11-30", "Before 2022-11-30")) %>%
  group_by(period) %>%
  summarise(n_filings = n(), n_firms = n_distinct(cik),
            first_filing = min(filing_date), last_filing = max(filing_date),
            mean_ai_intensity = observed_mean(ai_intensity),
            median_ai_intensity = observed_median(ai_intensity), share_any_ai = mean(ai_count > 0),
            mean_next_revenue_growth = observed_mean(next_revenue_growth),
            n_revenue_outcomes = sum(is.finite(next_revenue_growth)),
            mean_excess_return_12m = observed_mean(excess_return_12m),
            n_return_outcomes = sum(is.finite(excess_return_12m)), .groups = "drop")
write_table(trend_year, "trend_year.csv")
write_table(trend_fiscal_year, "trend_fiscal_year.csv")
write_table(trend_industry, "trend_industry.csv")
write_table(period_comparison, "period_comparison.csv")

# Show a constant-company trend over complete filing years to flag composition changes.
last_observed_date <- max(panel$filing_date, na.rm = TRUE)
complete_years <- sort(unique(panel$filing_year[panel$filing_year > config$start_fy &
                                               panel$filing_year < lubridate::year(config$as_of)]))
balanced_ciks <- panel %>%
  filter(filing_year %in% complete_years) %>%
  distinct(cik, filing_year) %>% count(cik, name = "n_years") %>%
  filter(n_years == length(complete_years)) %>% pull(cik)
trend_balanced <- panel %>%
  filter(cik %in% balanced_ciks, filing_year %in% complete_years) %>%
  group_by(filing_year) %>%
  summarise(n_filings = n(), n_firms = n_distinct(cik),
            mean_ai_intensity = observed_mean(ai_intensity), .groups = "drop")
write_table(trend_balanced, "trend_balanced.csv")

# Rank the latest complete filing year rather than an unfinished current year.
ranking_year <- if (length(complete_years) > 0) max(complete_years) else max(panel$filing_year)
company_rankings <- panel %>%
  filter(filing_year == ranking_year) %>%
  arrange(cik, desc(filing_date)) %>% distinct(cik, .keep_all = TRUE) %>%
  arrange(desc(ai_intensity), cik) %>% mutate(rank = row_number()) %>%
  select(rank, ticker, company, industry, filing_year, fiscal_year, filing_date,
         ai_count, word_count, ai_intensity, next_revenue_growth, excess_return_12m)
write_table(company_rankings, "company_rankings.csv")

# Rank disclosures within each filing year; tied values are ordered by company CIK.
panel <- panel %>%
  arrange(filing_year, ai_intensity, cik, filing_date) %>%
  group_by(filing_year) %>% mutate(ai_quintile = ntile(ai_intensity, 5L)) %>% ungroup()
positive_groups <- panel %>% filter(ai_intensity > 0) %>%
  group_by(filing_year) %>% mutate(positive_quartile = ntile(ai_intensity, 4L)) %>% ungroup() %>%
  select(cik, period_end, positive_quartile)
panel <- panel %>% left_join(positive_groups, by = c("cik", "period_end")) %>%
  mutate(exposure_group = if_else(ai_intensity == 0, "0: No AI mentions",
                                  paste0(positive_quartile, ": Positive quartile ", positive_quartile)))
quintile_membership <- panel %>%
  group_by(filing_year, ai_quintile) %>%
  summarise(n_filings = n(), ai_min = min(ai_intensity), ai_max = max(ai_intensity),
            ai_mean = mean(ai_intensity), n_zero_ai = sum(ai_intensity == 0), .groups = "drop")
quintile_summary <- panel %>%
  group_by(ai_quintile) %>%
  summarise(n_filings = n(), n_firms = n_distinct(cik),
            ai_min = min(ai_intensity), ai_max = max(ai_intensity),
            ai_mean = mean(ai_intensity), n_zero_ai = sum(ai_intensity == 0),
            n_revenue_outcomes = sum(is.finite(next_revenue_growth)),
            mean_next_revenue_growth = observed_mean(next_revenue_growth),
            median_next_revenue_growth = observed_median(next_revenue_growth),
            n_return_outcomes = sum(is.finite(excess_return_12m)),
            mean_excess_return_12m = observed_mean(excess_return_12m),
            median_excess_return_12m = observed_median(excess_return_12m), .groups = "drop")
exposure_groups_summary <- panel %>%
  group_by(exposure_group) %>%
  summarise(n_filings = n(), n_firms = n_distinct(cik),
            ai_min = min(ai_intensity), ai_max = max(ai_intensity),
            n_revenue_outcomes = sum(is.finite(next_revenue_growth)),
            mean_next_revenue_growth = observed_mean(next_revenue_growth),
            n_return_outcomes = sum(is.finite(excess_return_12m)),
            mean_excess_return_12m = observed_mean(excess_return_12m), .groups = "drop")
write_table(quintile_membership, "quintile_membership.csv")
write_table(quintile_summary, "quintile_summary.csv")
write_table(exposure_groups_summary, "exposure_groups_summary.csv")
write_table(panel, "panel_with_groups.csv")

# Estimate each outcome on its own complete sample, without trimming the main models.
models <- list()
coefficient_rows <- list()
status_rows <- list()
slope_rows <- list()
outcomes <- c("next_revenue_growth", "excess_return_12m")
model_labels <- c("baseline", "interaction", "log_ai", "firm_fe", "firm_fe_interaction", "winsorized", "winsorized_interaction")
for (outcome in outcomes) {
  model_data <- panel %>%
    filter(if_all(all_of(c(outcome, "ai_intensity", "log_revenue", "current_growth", "post2022")), is.finite)) %>%
    mutate(outcome_value = .data[[outcome]], log_ai = log1p(ai_intensity))
  for (model_name in model_labels) {
    current_data <- model_data
    if (grepl("winsorized", model_name) && nrow(current_data) > 0) {
      limits <- quantile(current_data$outcome_value, c(0.01, 0.99), names = FALSE)
      current_data$outcome_value <- pmin(pmax(current_data$outcome_value, limits[1]), limits[2])
    }
    right_hand_side <- switch(model_name,
      baseline = "ai_intensity + log_revenue + current_growth | filing_year + industry",
      interaction = "ai_intensity * post2022 + log_revenue + current_growth | filing_year + industry",
      log_ai = "log_ai + log_revenue + current_growth | filing_year + industry",
      firm_fe = "ai_intensity + log_revenue + current_growth | filing_year + cik",
      firm_fe_interaction = "ai_intensity * post2022 + log_revenue + current_growth | filing_year + cik",
      winsorized = "ai_intensity + log_revenue + current_growth | filing_year + industry",
      winsorized_interaction = "ai_intensity * post2022 + log_revenue + current_growth | filing_year + industry")
    model_key <- paste(outcome, model_name, sep = "__")
    fit <- tryCatch(
      feols(as.formula(paste("outcome_value ~", right_hand_side)), data = current_data,
            vcov = ~cik, notes = TRUE, data.save = TRUE),
      error = function(e) e)
    if (inherits(fit, "error")) {
      status_rows[[model_key]] <- tibble(outcome = outcome, model = model_name,
        status = "failed", n_available = nrow(current_data), n_obs = NA_integer_,
        n_firms = n_distinct(current_data$cik), message = conditionMessage(fit))
      warning(paste(model_key, conditionMessage(fit)), call. = FALSE)
      next
    }
    models[[model_key]] <- fit
    used_rows <- fixest::obs(fit)
    used_firms <- n_distinct(current_data$cik[used_rows])
    coefficient_table <- as.data.frame(coeftable(fit))
    intervals <- as.matrix(confint(fit, level = 0.95))
    coefficient_rows[[model_key]] <- tibble(outcome = outcome, model = model_name,
      term = rownames(coefficient_table), estimate = coefficient_table[[1]],
      std_error = coefficient_table[[2]], p_value = coefficient_table[[4]],
      conf_low = intervals[, 1], conf_high = intervals[, 2],
      n_obs = nobs(fit), n_firms = used_firms)
    status_rows[[model_key]] <- tibble(outcome = outcome, model = model_name,
      status = "estimated", n_available = nrow(current_data), n_obs = nobs(fit),
      n_firms = used_firms, message = paste(fit$collin.var, collapse = "; "))

    # The post-period slope is the sum of the main AI and interaction coefficients.
    if (grepl("interaction", model_name)) {
      interaction_term <- intersect(c("ai_intensity:post2022", "post2022:ai_intensity"), names(coef(fit)))
      if ("ai_intensity" %in% names(coef(fit)) && length(interaction_term) == 1) {
        coefficient_vector <- coef(fit)
        covariance_matrix <- vcov(fit)
        for (period in c("pre", "post", "change")) {
          weights <- setNames(rep(0, length(coefficient_vector)), names(coefficient_vector))
          if (period != "change") weights["ai_intensity"] <- 1
          if (period != "pre") weights[interaction_term] <- 1
          estimate <- sum(weights * coefficient_vector)
          standard_error <- sqrt(as.numeric(t(weights) %*% covariance_matrix %*% weights))
          degrees_freedom <- fixest::degrees_freedom(fit, type = "t")
          critical_value <- qt(0.975, df = degrees_freedom)
          slope_rows[[paste(model_key, period, sep = "__")]] <- tibble(
            outcome = outcome, model = model_name, period = period, estimate = estimate,
            std_error = standard_error, p_value = 2 * pt(-abs(estimate / standard_error), df = degrees_freedom),
            conf_low = estimate - critical_value * standard_error,
            conf_high = estimate + critical_value * standard_error,
            n_obs = nobs(fit), n_firms = used_firms)
        }
      }
    }
  }
}
regression_coefficients <- bind_rows(coefficient_rows)
model_status <- bind_rows(status_rows)
effect_slopes <- bind_rows(slope_rows)
write_table(regression_coefficients, "regression_coefficients.csv")
write_table(regression_coefficients %>% filter(!model %in% c("baseline", "interaction")), "sensitivity_coefficients.csv")
write_table(model_status, "model_status.csv")
write_table(effect_slopes, "effect_slopes.csv")
saveRDS(models, "output/tables/models.rds")
if (nrow(model_status %>% filter(model %in% c("baseline", "interaction"), status == "failed")) > 0) {
  warning("One or more requested main regressions failed. See output/tables/model_status.csv.", call. = FALSE)
}

# Use the same readable visual style across all exported figures.
navy <- "#15344A"
orange <- "#D87032"
plot_theme <- theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold", colour = navy, size = 16),
        plot.subtitle = element_text(colour = "#4C5C68", size = 11),
        panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
        plot.caption = element_text(colour = "#5F6B73", hjust = 0, size = 9),
        plot.background = element_rect(fill = "white", colour = NA),
        legend.position = "bottom")
save_figure <- function(plot, name, width = 9, height = 5.5) {
  ggsave(file.path("output/figures", name), plot, width = width, height = height, dpi = 180, bg = "white")
}
source_caption <- "Source: SEC EDGAR 10-K main documents. Current S&P 500 issuers; observational sample."
trend_long <- trend_year %>%
  select(filing_year, mean_ai_intensity, median_ai_intensity) %>%
  pivot_longer(-filing_year, names_to = "measure", values_to = "intensity") %>%
  mutate(measure = recode(measure, mean_ai_intensity = "Mean", median_ai_intensity = "Median"))
ai_trend <- ggplot(trend_long, aes(filing_year, intensity, colour = measure, linetype = measure)) +
  geom_line(linewidth = 0.9) + geom_point(size = 2.3) +
  geom_vline(xintercept = 2022.91, linetype = "dotted", colour = "#667782") +
  scale_colour_manual(values = c(Mean = navy, Median = orange)) +
  scale_x_continuous(breaks = sort(unique(panel$filing_year))) +
  labs(title = "How much do annual reports talk about AI?",
       subtitle = "Company-level AI mentions per 10,000 words; dotted line marks ChatGPT's November 2022 release",
       x = "10-K filing year", y = "AI mentions per 10,000 words", colour = NULL, linetype = NULL,
       caption = paste(source_caption, "Boundary filing years are partial because of the fiscal-year floor and evidence cutoff.", sep = "\n")) + plot_theme
save_figure(ai_trend, "ai_trend.png")
industry_trends <- ggplot(trend_industry, aes(filing_year, mean_ai_intensity)) +
  geom_line(linewidth = 0.75, colour = navy) + geom_point(size = 1.4, colour = orange) +
  facet_wrap(~industry, scales = "fixed", ncol = 3) +
  scale_x_continuous(breaks = seq(min(panel$filing_year), max(panel$filing_year), by = 2)) +
  labs(title = "AI disclosure differs across SIC industry groups", subtitle = "Unweighted company-year mean; common vertical scale",
       x = "10-K filing year", y = "AI mentions per 10,000 words", caption = source_caption) + plot_theme
save_figure(industry_trends, "industry_trends.png", width = 11, height = 8)
rank_plot <- company_rankings %>% slice_head(n = 15) %>%
  mutate(label = paste0(ticker, "  ", str_trunc(company, 32)), label = forcats::fct_reorder(label, ai_intensity))
company_ranking_plot <- ggplot(rank_plot, aes(ai_intensity, label)) +
  geom_col(fill = navy, width = 0.7) +
  labs(title = paste("Companies mentioning AI most in", ranking_year, "filings"),
       subtitle = "Top 15 by AI mentions per 10,000 words; latest complete filing year",
       x = "AI mentions per 10,000 words", y = NULL, caption = source_caption) + plot_theme
save_figure(company_ranking_plot, "company_rankings.png", width = 10, height = 7)
quintile_plot_data <- quintile_summary %>%
  select(ai_quintile, mean_next_revenue_growth, mean_excess_return_12m) %>%
  pivot_longer(-ai_quintile, names_to = "outcome", values_to = "value") %>%
  mutate(outcome = recode(outcome, mean_next_revenue_growth = "Next fiscal-year revenue growth (%)",
                         mean_excess_return_12m = "12-month return above SPY (pp)"))
quintile_plot <- ggplot(quintile_plot_data, aes(factor(ai_quintile), 100 * value)) +
  geom_hline(yintercept = 0, colour = "#8A969D", linewidth = 0.4) +
  geom_col(fill = navy, width = 0.65, na.rm = TRUE) + facet_wrap(~outcome, scales = "free_y") +
  labs(title = "Do companies talking more about AI perform better?",
       subtitle = "Unadjusted outcome means; quintile ranks are formed within each filing year",
       x = "AI intensity quintile (1 = lowest, 5 = highest)", y = "Mean outcome",
       caption = paste("Different outcome samples; complete 12-month return windows only. Tied AI values can span quintiles.",
                       "These descriptive averages do not control for industry, size, or year.", sep = "\n")) + plot_theme
save_figure(quintile_plot, "quintile_outcomes.png", width = 10, height = 5.5)
main_effects <- regression_coefficients %>% filter(model == "baseline", term == "ai_intensity") %>%
  transmute(outcome, effect = "Overall association", estimate, conf_low, conf_high)
if (nrow(effect_slopes) > 0) {
  main_effects <- bind_rows(main_effects, effect_slopes %>% filter(model == "interaction", period %in% c("pre", "post")) %>%
    transmute(outcome, effect = recode(period, pre = "Before 2022-11-30", post = "On/after 2022-11-30"), estimate, conf_low, conf_high))
}
main_effects <- main_effects %>%
  mutate(outcome = recode(outcome, next_revenue_growth = "Next fiscal-year revenue growth", excess_return_12m = "12-month return above SPY"),
         effect = factor(effect, levels = c("On/after 2022-11-30", "Before 2022-11-30", "Overall association")))
coefficient_plot <- ggplot(main_effects, aes(100 * estimate, effect)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "#8A969D") +
  geom_errorbar(aes(xmin = 100 * conf_low, xmax = 100 * conf_high), orientation = "y", width = 0.12, colour = navy) +
  geom_point(size = 2.8, colour = orange) + facet_wrap(~outcome, scales = "free_x") +
  labs(title = "AI disclosure and later outcomes after controls",
       subtitle = "Estimated association per extra AI mention per 10,000 words; 95% confidence intervals",
       x = "Change in outcome (percentage points)", y = NULL,
       caption = "Year and SIC industry fixed effects; log revenue and current revenue growth controls. Standard errors clustered by company.\nPre/post slopes come from the interaction model. Correlation does not establish causation.") + plot_theme
save_figure(coefficient_plot, "regression_coefficients.png", width = 11, height = 5.5)

# Save machine-readable facts so the README, report, and app use the same run.
summary_list <- list(
  generated_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
  as_of = as.character(config$as_of),
  sample_mode = if (is.finite(config$sample_size)) "pilot_20" else "full",
  n_filings = nrow(panel), n_firms = n_distinct(panel$cik),
  first_filing = as.character(min(panel$filing_date)), last_filing = as.character(max(panel$filing_date)),
  fiscal_years = sort(unique(panel$fiscal_year)), filing_years = sort(unique(panel$filing_year)),
  n_zero_ai = sum(panel$ai_intensity == 0), share_zero_ai = mean(panel$ai_intensity == 0),
  mean_ai_intensity = mean(panel$ai_intensity), median_ai_intensity = median(panel$ai_intensity),
  n_revenue_outcomes = sum(is.finite(panel$next_revenue_growth)),
  n_return_outcomes = sum(is.finite(panel$excess_return_12m)),
  ranking_filing_year = ranking_year, n_balanced_trend_firms = length(balanced_ciks),
  post_definition = "10-K filing date on or after 2022-11-30",
  quintile_definition = "Within filing year, ntile(ai_intensity, 5); ties ordered by CIK. Identical values may span groups.",
  model_units = "Outcomes are decimal rates; multiply coefficients by 100 for percentage-point changes.",
  main_sample_rule = "Separate complete-case samples by outcome; no outcome trimming in main models.",
  period_comparison = period_comparison, trend_year = trend_year,
  top_companies = company_rankings %>% slice_head(n = 10),
  main_coefficients = regression_coefficients %>% filter(model %in% c("baseline", "interaction")),
  interaction_slopes = effect_slopes %>% filter(model == "interaction"),
  model_status = model_status)
jsonlite::write_json(summary_list, "output/tables/run_summary.json", pretty = TRUE, auto_unbox = TRUE, na = "null")
message("Analysis complete: ", nrow(panel), " filings from ", n_distinct(panel$cik), " issuers. See output/tables/model_status.csv for model coverage.")
