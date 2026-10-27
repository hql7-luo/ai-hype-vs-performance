# Reconcile a small, documented sample directly to cached SEC source records.
source("R/00_setup.R")
panel <- read_csv("data/processed/panel.csv", col_types = cols(cik = "c"))
dictionary <- read_csv("config/ai_dictionary.csv")
samples <- panel %>% filter(ticker %in% c("AAPL", "MSFT", "NVDA", "JPM", "BRK.B"),
                            is.finite(revenue), !is.na(revenue_tag)) %>%
  group_by(ticker) %>% arrange(period_end, .by_group = TRUE) %>%
  filter(row_number() %in% c(1L, n())) %>% ungroup()

# A single combined regular expression independently checks nonoverlapping phrase totals.
combined_pattern <- paste0(if_else(dictionary$case_sensitive, "(?-i:", "(?i:"),
                           dictionary$pattern, ")", collapse = "|")
audits <- list()
for (i in seq_len(nrow(samples))) {
  row <- samples[i, ]
  started <- Sys.time()
  message("Source audit ", i, "/", nrow(samples), ": ", row$ticker, " FY", row$fiscal_year)
  compressed <- readBin(row$local_file, "raw", n = file.info(row$local_file)$size)
  document <- xml2::read_html(memDecompress(compressed, "gzip"))

  # Match inline amounts only to annual, entity-wide contexts in the actual primary document.
  contexts <- xml2::xml_find_all(document, "//*[local-name()='xbrli:context' or local-name()='context']")
  context_table <- tibble(
    context_id = xml2::xml_attr(contexts, "id"),
    start = as.Date(xml2::xml_text(xml2::xml_find_first(contexts, ".//*[local-name()='xbrli:startdate' or local-name()='startdate']"))),
    end = as.Date(xml2::xml_text(xml2::xml_find_first(contexts, ".//*[local-name()='xbrli:enddate' or local-name()='enddate']"))),
    has_dimensions = !is.na(xml2::xml_name(xml2::xml_find_first(contexts, ".//*[local-name()='xbrli:segment' or local-name()='segment' or local-name()='xbrli:scenario' or local-name()='scenario']"))))
  inline_values <- numeric()
  if (nrow(context_table) > 0 && !is.na(row$revenue_tag)) {
    eligible_contexts <- context_table %>% filter(end == row$period_end, !has_dimensions,
      as.integer(end - start) + 1 >= 330, as.integer(end - start) + 1 <= 400)
    nodes <- xml2::xml_find_all(document, "//*[@name and @contextref]")
    nodes <- nodes[xml2::xml_attr(nodes, "name") == paste0("us-gaap:", row$revenue_tag) &
                   xml2::xml_attr(nodes, "contextref") %in% eligible_contexts$context_id]
    if (length(nodes) > 0) {
      raw_values <- suppressWarnings(as.numeric(gsub("[, $]", "", xml2::xml_text(nodes))))
      scale <- suppressWarnings(as.numeric(xml2::xml_attr(nodes, "scale")))
      scale[is.na(scale)] <- 0
      sign <- ifelse(xml2::xml_attr(nodes, "sign") %in% "-", -1, 1)
      inline_values <- unique(raw_values * 10^scale * sign)
      inline_values <- inline_values[is.finite(inline_values)]
    }
  }
  inline_match <- if (length(inline_values) > 0) any(abs(inline_values - row$revenue) <= 1) else NA

  # Verify the retained revenue is an original-accession USD fact, not a later restatement.
  source_cik <- if ("sec_cik" %in% names(row)) row$sec_cik else row$cik
  source_file <- paste0("data/raw/companyfacts/CIK", source_cik, ".json")
  if (!file.exists(source_file)) source_file <- paste0("data/raw/companyfacts/CIK", row$cik, ".json")
  facts <- jsonlite::fromJSON(source_file)$facts[["us-gaap"]][[row$revenue_tag]]$units$USD
  original <- facts[facts$accn == row$accession & facts$end == as.character(row$period_end), ]
  original_fact_match <- nrow(original) > 0 && any(abs(original$val - row$revenue) <= 1)

  # Extract report prose without recursive browser-style HTML traversal.
  hidden <- xml2::xml_find_all(document,
    "//script|//style|//noscript|//*[local-name()='hidden' or local-name()='ix:hidden']|//*[@hidden]|//*[@aria-hidden='true']|//*[contains(translate(@style,'ABCDEFGHIJKLMNOPQRSTUVWXYZ ', 'abcdefghijklmnopqrstuvwxyz'),'display:none')]")
  xml2::xml_remove(hidden)
  visible_html <- gsub("(?i)</?(?:p|div|br|hr|li|tr|td|th|table|h[1-6]|section|article|header|footer|ul|ol|pre|blockquote|address|dl|dt|dd)\\b[^>]*>",
                       " ", as.character(document), perl = TRUE)
  text <- rvest::html_text(xml2::read_html(visible_html)) %>% str_squish()
  independent_ai_count <- str_count(text, regex(combined_pattern))
  match_locations <- str_locate_all(text, regex(combined_pattern))[[1]]
  first_context <- if (nrow(match_locations) > 0) {
    substr(text, max(1, match_locations[1, 1] - 80), min(nchar(text), match_locations[1, 2] + 120))
  } else "No dictionary phrase found"
  audits[[i]] <- tibble(ticker = row$ticker, cik = row$cik, fiscal_year = row$fiscal_year,
    accession = row$accession, url = row$url, revenue_tag = row$revenue_tag,
    revenue_in_panel = row$revenue, original_companyfacts_match = original_fact_match,
    inline_annual_values = paste(format(inline_values, scientific = FALSE, trim = TRUE), collapse = "; "),
    inline_document_match = inline_match,
    inline_document_status = ifelse(length(inline_values) == 0, "No matching inline amount; companyfacts-only verification", "Inline annual amount inspected"),
    ai_count_in_panel = row$ai_count, independent_ai_count,
    ai_count_match = independent_ai_count == row$ai_count, first_ai_context = first_context)
  message("  Completed in ", round(as.numeric(difftime(Sys.time(), started, units = "secs")), 2), " seconds")
}
source_audit <- bind_rows(audits)
write_csv(source_audit, "output/tables/source_spot_checks.csv")
stopifnot(nrow(source_audit) > 0,
          all(source_audit$original_companyfacts_match),
          all(source_audit$inline_document_match, na.rm = TRUE),
          all(source_audit$ai_count_match))
message(nrow(source_audit), " sampled original-accession and AI-count source checks passed; see source_spot_checks.csv for inline HTML coverage.")
