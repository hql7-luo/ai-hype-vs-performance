# Cache adjusted Yahoo prices through tidyquant, including the SPY benchmark.
companies <- read_csv("data/processed/companies.csv", col_types = cols(cik = "c"))
tickers <- unique(c(companies$ticker, "SPY"))
price_rows <- list()
for (i in seq_along(tickers)) {
  ticker <- tickers[i]
  file <- paste0("data/raw/prices/", ticker, "_", config$as_of, ".csv")
  message("Prices ", i, "/", length(tickers), ": ", ticker)
  tryCatch({
    if (!file.exists(file)) {
      prices <- tidyquant::tq_get(str_replace_all(ticker, fixed("."), "-"),
                                  get = "stock.prices", from = "2018-01-01",
                                  to = as.character(config$as_of + 1), complete_cases = FALSE)
      if (!is.data.frame(prices) || !nrow(prices) || !"adjusted" %in% names(prices)) {
        stop("tq_get returned no adjusted price observations")
      }
      prices <- prices |> mutate(symbol = ticker) |> filter(date <= config$as_of)
      write_csv(prices, file)
    }
    price_rows[[ticker]] <- read_csv(file) |> transmute(ticker, date, adjusted) |>
      filter(!is.na(adjusted), is.finite(adjusted), adjusted > 0) |> distinct(date, .keep_all = TRUE)
  }, error = function(e) record_issue("prices", ticker, config$as_of, "price_download_failed", conditionMessage(e)))
}
prices <- bind_rows(price_rows)
write_csv(prices, "data/processed/prices.csv")
spy <- prices |> filter(ticker == "SPY") |> select(date, spy_adjusted = adjusted)
if (!nrow(spy)) stop("SPY unavailable: no return comparison can be estimated.")
common_prices <- split(prices, prices$ticker) |>
  map(~inner_join(.x, spy, by = "date") |> arrange(date))

# Use identical common trading dates strictly after filing and at the calendar anniversary.
filings <- read_csv("data/processed/filings.csv", col_types = cols(cik = "c"))
return_rows <- list()
for (i in seq_len(nrow(filings))) {
  row <- filings[i, ]
  anniversary <- row$filing_date %m+% years(1)
  common <- common_prices[[row$ticker]]
  if (is.null(common)) common <- tibble(date = as.Date(character()), adjusted = double(), spy_adjusted = double())
  status <- "ok"
  start_row <- common |> filter(date > row$filing_date, date <= row$filing_date + 7) |> slice(1)
  end_row <- common |> filter(date >= anniversary, date <= anniversary + 7) |> slice(1)
  if (anniversary > config$as_of) status <- "incomplete_12m_window"
  else if (!nrow(common)) status <- "missing_stock_prices"
  else if (!nrow(start_row)) status <- "missing_start_endpoint"
  else if (!nrow(end_row)) status <- "missing_end_endpoint"
  stock_return <- spy_return <- excess_return <- NA_real_
  if (status == "ok") {
    stock_return <- end_row$adjusted / start_row$adjusted - 1
    spy_return <- end_row$spy_adjusted / start_row$spy_adjusted - 1
    excess_return <- stock_return - spy_return
  }
  return_rows[[i]] <- tibble(cik = row$cik, ticker = row$ticker, accession = row$accession,
    filing_date = row$filing_date, anniversary, return_status = status,
    return_start = if (nrow(start_row)) start_row$date else as.Date(NA),
    return_end = if (nrow(end_row)) end_row$date else as.Date(NA),
    stock_start = if (nrow(start_row)) start_row$adjusted else NA_real_,
    stock_end = if (nrow(end_row)) end_row$adjusted else NA_real_,
    spy_start = if (nrow(start_row)) start_row$spy_adjusted else NA_real_,
    spy_end = if (nrow(end_row)) end_row$spy_adjusted else NA_real_,
    stock_return_12m = stock_return, spy_return_12m = spy_return, excess_return_12m = excess_return)
}
returns <- bind_rows(return_rows)
write_csv(returns, "data/processed/filing_returns.csv")
write_csv(count(returns, return_status, name = "n_filings"), "output/tables/return_exclusions.csv")
