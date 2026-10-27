# Align the next fiscal period without jumping across missing company-years.
mentions <- read_csv("data/processed/ai_mentions.csv", col_types = cols(cik = "c"))
revenue <- read_csv("data/processed/annual_revenues.csv", col_types = cols(cik = "c"))
returns <- read_csv("data/processed/filing_returns.csv", col_types = cols(cik = "c"))
fiscal_labels <- read_csv("data/processed/fiscal_labels.csv", col_types = cols(cik = "c"))
# Explicit cover checks fill missing labels and correct a documented erroneous inline tag.
cover_labels <- read_csv("config/fiscal_year_overrides.csv", col_types = cols(cik = "c")) |>
  select(cik, accession, fiscal_year_cover)
label_corrections <- mentions |> select(cik, ticker, accession, fiscal_year) |>
  inner_join(cover_labels, by = c("cik", "accession")) |>
  filter(!is.na(fiscal_year), fiscal_year != fiscal_year_cover)
for (i in seq_len(nrow(label_corrections))) {
  row <- label_corrections[i, ]
  record_issue("panel", row$ticker, row$accession, "fiscal_year_label_corrected",
    paste("Inline DEI says", row$fiscal_year, "; verified report cover says", row$fiscal_year_cover))
}
revenue <- revenue |> group_by(cik) |> arrange(period_end, .by_group = TRUE) |>
  mutate(next_period_end = lead(period_end), next_prior_end = lead(prior_period_end),
         next_revenue_tag = lead(revenue_tag),
         next_revenue = lead(revenue), next_prior_revenue = lead(prior_revenue),
         next_revenue_growth = if_else(
           between(as.integer(next_period_end - period_end), 330L, 400L) & next_prior_end == period_end,
           lead(current_growth), NA_real_)) |> ungroup()
panel <- mentions |> left_join(revenue |> select(-ticker, -period_end, -fiscal_year_xbrl), by = c("cik", "accession")) |>
  left_join(fiscal_labels, by = c("cik", "accession")) |>
  left_join(cover_labels, by = c("cik", "accession")) |>
  left_join(returns |> select(-ticker, -filing_date), by = c("cik", "accession")) |>
  mutate(fiscal_year_dei = fiscal_year,
         fiscal_year_source = case_when(!is.na(fiscal_year_cover) ~ "verified_10k_cover",
           !is.na(fiscal_year) ~ "10k_DEI",
           !is.na(fiscal_year_xbrl) ~ "original_companyfacts_fy", TRUE ~ "verified_10k_cover"),
         fiscal_year = coalesce(fiscal_year_cover, fiscal_year, fiscal_year_xbrl),
         filing_year = year(filing_date), post2022 = as.integer(filing_date >= as.Date("2022-11-30")),
         log_revenue = log(revenue),
         has_controls = is.finite(log_revenue) & is.finite(current_growth) & !is.na(industry),
         revenue_eligible = has_controls & is.finite(next_revenue_growth),
         return_eligible = has_controls & is.finite(excess_return_12m) & return_status == "ok") |>
  filter(!is.na(fiscal_year), fiscal_year >= config$start_fy) |> arrange(cik, period_end)
stopifnot(!anyDuplicated(panel[c("cik", "accession")]),
          !anyDuplicated(panel[c("cik", "period_end")]),
          all(panel$ai_intensity >= 0), all(panel$word_count > 1000))
write_csv(panel, "data/processed/panel.csv")
write_csv(panel |> select(ticker, accession, period_end, fiscal_year, fiscal_year_dei,
                          fiscal_year_xbrl, fiscal_year_cover, fiscal_year_source) |>
  filter(!is.na(fiscal_year_cover) | (!is.na(fiscal_year_dei) & !is.na(fiscal_year_xbrl) &
                                      fiscal_year_dei != fiscal_year_xbrl)),
  "output/tables/fiscal_year_crosscheck.csv")

# Reconcile each outcome separately and preserve row-level exclusion reasons.
exclusions <- panel |> mutate(
  revenue_status = case_when(!is.finite(revenue) ~ "missing_revenue",
    !is.finite(current_growth) ~ "missing_prior_growth", !is.finite(next_revenue_growth) ~ "missing_next_fy_growth",
    !has_controls ~ "missing_other_control", TRUE ~ "included"),
  return_analysis_status = case_when(return_status != "ok" ~ return_status,
    !has_controls ~ "missing_controls", TRUE ~ "included")) |>
  select(cik, ticker, accession, fiscal_year, filing_date, revenue_status, return_analysis_status)
write_csv(exclusions, "output/tables/panel_exclusions.csv")
# Reconcile every discovered filing, including text and fiscal-label exclusions.
filing_coverage <- read_csv("data/processed/filings.csv", col_types = cols(cik = "c", sec_cik = "c")) |>
  select(cik, ticker, accession, filing_date, period_end, download_status) |>
  left_join(mentions |> select(cik, accession, fiscal_year, word_count), by = c("cik", "accession")) |>
  left_join(fiscal_labels |> select(cik, accession, fiscal_year_xbrl), by = c("cik", "accession")) |>
  left_join(cover_labels, by = c("cik", "accession")) |>
  mutate(fiscal_year = coalesce(fiscal_year_cover, fiscal_year, fiscal_year_xbrl),
         text_panel_status = case_when(download_status != "ok" ~ download_status,
           is.na(word_count) ~ "text_parse_failed", is.na(fiscal_year) ~ "missing_fiscal_label",
           fiscal_year < config$start_fy ~ "before_fy_floor", TRUE ~ "included"))
write_csv(filing_coverage, "output/tables/filing_coverage.csv")
company_coverage <- panel |> group_by(cik) |>
  summarise(n_panel_filings = n(), first_period = min(period_end), last_period = max(period_end),
            observed_fiscal_years = paste(sort(unique(fiscal_year)), collapse = ", "),
            n_revenue_analysis = sum(revenue_eligible), n_return_analysis = sum(return_eligible), .groups = "drop")
company_coverage <- read_csv("data/processed/companies.csv", col_types = cols(cik = "c", sec_cik = "c")) |>
  left_join(count(filing_coverage, cik, name = "n_discovered_filings"), by = "cik") |>
  left_join(company_coverage, by = "cik") |>
  mutate(across(starts_with("n_"), ~replace_na(.x, 0L)))
write_csv(company_coverage, "output/tables/company_coverage.csv")
coverage <- tibble(metric = c("constituent_listings", "unique_issuers_selected", "issuers_with_text",
                              "filings_discovered", "panel_filings", "revenue_analysis_rows", "return_analysis_rows",
                              "incomplete_return_windows"),
  count = c(nrow(read_csv("data/raw/sp500_membership.csv")),
            nrow(read_csv("data/processed/companies.csv")), n_distinct(panel$cik),
            nrow(read_csv("data/processed/filings.csv")), nrow(panel),
            sum(panel$revenue_eligible), sum(panel$return_eligible),
            sum(panel$return_status == "incomplete_12m_window", na.rm = TRUE)))
write_csv(coverage, "output/tables/coverage.csv")
# Mark earlier source warnings that now have a documented successful resolution.
issues <- read_csv("output/tables/data_issues.csv", col_types = cols(.default = "c"))
issues <- issues |> mutate(status = case_when(
  reason == "fiscal_year_label_corrected" ~ "resolved_with_verified_10k_cover",
  reason == "missing_fiscal_year_dei" & item %in% cover_labels$accession ~ "resolved_with_verified_10k_cover",
  reason == "missing_fiscal_year_dei" & item %in% panel$accession ~ "resolved_with_original_companyfacts_fy",
  reason %in% c("no_eligible_10k", "submissions_failed") & ticker %in% panel$ticker ~ "resolved_in_final_snapshot",
  reason == "membership_disagreement" ~ "resolved_with_declared_membership_snapshot",
  reason == "revenue_concept_disagreement" ~ "selected_by_documented_total_revenue_priority",
  TRUE ~ "see_coverage_and_exclusion_ledgers"))
write_csv(issues, "output/tables/data_issues.csv")
jsonlite::write_json(list(as_of = as.character(config$as_of), start_fy = config$start_fy,
  scope = if (is.finite(config$sample_size)) "pilot20" else "full",
  selected_issuers = nrow(read_csv("data/processed/companies.csv"))),
  "output/tables/run_config.json", auto_unbox = TRUE, pretty = TRUE)
print(coverage)
