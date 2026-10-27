# Merge standard total-revenue concepts by priority; never add overlapping tags.
revenue_tags <- c("Revenues", "RevenueFromContractWithCustomerExcludingAssessedTax",
                  "RevenueFromContractWithCustomerIncludingAssessedTax", "SalesRevenueNet",
                  "RevenuesNetOfInterestExpense", "RevenueFromContractWithCustomer",
                  "SalesRevenueGoodsNet", "SalesRevenueServicesNet")
companies <- read_csv("data/processed/companies_with_sic.csv", col_types = cols(cik = "c"))
filings <- read_csv("data/processed/filings.csv", col_types = cols(cik = "c"))
annual_rows <- list()
candidate_rows <- list()
fiscal_label_rows <- list()
for (i in seq_len(nrow(companies))) {
  company_row <- companies[i, ]
  ticker <- company_row$ticker
  cik <- company_row$cik
  sec_cik <- company_row$sec_cik
  message("Financials ", i, "/", nrow(companies), ": ", ticker)
  tryCatch({
    file <- fetch_cache(paste0("https://data.sec.gov/api/xbrl/companyfacts/CIK", sec_cik, ".json"),
                        paste0("data/raw/companyfacts/CIK", sec_cik, ".json"))
    facts <- read_json_cache(file)$facts[["us-gaap"]]
    # Older non-inline reports can obtain their fiscal label from any original fact.
    label_tags <- intersect(c("Assets", "NetIncomeLoss", "StockholdersEquity", "LiabilitiesAndStockholdersEquity"), names(facts))
    label_values <- map_dfr(label_tags, function(tag) {
      map_dfr(facts[[tag]]$units, as_tibble) |>
        select(any_of(c("accn", "fy", "form"))) |> mutate(label_tag = tag)
    })
    if (nrow(label_values)) {
      labels <- label_values |> filter(form == "10-K", !is.na(fy)) |>
        distinct(accn, fy, .keep_all = TRUE) |> group_by(accn) |>
        filter(n_distinct(fy) == 1L) |> slice(1) |> ungroup() |>
        transmute(cik, accession = accn, fiscal_year_xbrl = as.integer(fy), fiscal_label_tag = label_tag)
      fiscal_label_rows[[i]] <- labels
    }
    # Goods-only and services-only concepts are not substitutes for a mixed total.
    tags <- intersect(revenue_tags[!revenue_tags %in% c("SalesRevenueGoodsNet", "SalesRevenueServicesNet")], names(facts))
    if (between(company_row$sic, 6000, 6199)) {
      tags <- intersect(c("RevenuesNetOfInterestExpense", "Revenues"), tags)
    }
    values <- map_dfr(tags, function(tag) {
      usd <- facts[[tag]]$units$USD
      if (is.null(usd) || !is.data.frame(usd) || !"start" %in% names(usd)) return(tibble())
      as_tibble(usd) |> mutate(revenue_tag = tag, tag_priority = match(tag, tags))
    })
    if (!nrow(values)) stop("No supported standard total-revenue USD facts")
    values <- values |>
      mutate(start = as.Date(start), end = as.Date(end), filed = as.Date(filed),
             duration = as.integer(end - start) + 1L) |>
      filter(form == "10-K", between(duration, 330, 400), filed <= config$as_of, val > 0)
    company_filings <- filings |> filter(.data$cik == !!cik) |> arrange(period_end)
    for (j in seq_len(nrow(company_filings))) {
      row <- company_filings[j, ]
      # Predictors use facts available in the initial annual filing itself.
      current <- values |> filter(accn == row$accession, end == row$period_end) |>
        arrange(tag_priority, desc(start)) |> distinct(revenue_tag, .keep_all = TRUE)
      if (!nrow(current)) {
        record_issue("financials", ticker, row$accession, "missing_original_annual_revenue",
                     "No supported standard annual USD total for exact original accession and period end.")
        next
      }
      candidate_rows[[length(candidate_rows) + 1L]] <- current |>
        mutate(cik, ticker, accession = row$accession)
      selected <- current |> slice(1)
      prior <- values |> filter(accn == row$accession, revenue_tag == selected$revenue_tag,
                                between(as.integer(row$period_end - end), 330, 400)) |>
        arrange(desc(end), desc(start)) |> slice(1)
      fiscal_year <- as.integer(selected$fy)
      prior_value <- if (nrow(prior)) prior$val else NA_real_
      prior_end <- if (nrow(prior)) prior$end else as.Date(NA)
      annual_rows[[length(annual_rows) + 1L]] <- tibble(
        cik, ticker, accession = row$accession, period_end = row$period_end,
        fiscal_year_xbrl = fiscal_year, revenue = selected$val, revenue_tag = selected$revenue_tag,
        revenue_start = selected$start, revenue_filed = selected$filed,
        prior_revenue = prior_value, prior_period_end = prior_end,
        current_growth = selected$val / prior_value - 1,
        revenue_concept_count = nrow(current),
        revenue_concepts_disagree = (max(current$val) / min(current$val) - 1) > 0.01
      )
      if (nrow(current) > 1 && max(current$val) / min(current$val) > 1.01) {
        record_issue("financials", ticker, row$accession, "revenue_concept_disagreement",
                     paste("Selected", selected$revenue_tag, "by documented priority; inspect revenue_candidates.csv."))
      }
    }
  }, error = function(e) record_issue("financials", ticker, cik, "companyfacts_failed", conditionMessage(e)))
  write_csv(bind_rows(annual_rows), "data/processed/annual_revenues.csv")
  write_csv(bind_rows(fiscal_label_rows), "data/processed/fiscal_labels.csv")
}
write_csv(bind_rows(candidate_rows), "output/tables/revenue_candidates.csv")
