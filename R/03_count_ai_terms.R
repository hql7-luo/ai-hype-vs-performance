# Count longer phrases first so generative AI is not counted again as standalone AI.
dictionary <- read_csv("config/ai_dictionary.csv")
dictionary_hash <- unname(tools::md5sum("config/ai_dictionary.csv"))
dir.create("data/raw/text_metrics", recursive = TRUE, showWarnings = FALSE)
count_ai_terms <- function(text, dictionary) {
  remaining <- text
  counts <- integer(nrow(dictionary))
  for (k in seq_len(nrow(dictionary))) {
    pattern <- regex(dictionary$pattern[k], ignore_case = !dictionary$case_sensitive[k])
    counts[k] <- str_count(remaining, pattern)
    remaining <- str_replace_all(remaining, pattern, " ")
  }
  counts
}

# Extract only visible report text; keep fiscal labels separate from calendar years.
filings <- read_csv("data/processed/filings.csv", col_types = cols(cik = "c"))
text_rows <- list()
term_rows <- list()
row_indices <- seq_len(nrow(filings))
if (Sys.getenv("AI_CACHE_ONLY", "0") == "1") {
  first_row <- as.integer(Sys.getenv("AI_CACHE_START", "1"))
  last_row <- as.integer(Sys.getenv("AI_CACHE_END", as.character(nrow(filings))))
  row_indices <- row_indices[row_indices >= first_row & row_indices <= last_row]
}
for (i in row_indices) {
  if (i %% 20L == 1L) message("Text ", i, "/", nrow(filings))
  row <- filings[i, ]
  if (row$download_status != "ok") next
  metric_cache <- paste0("data/raw/text_metrics/", row$cik, "_", row$accession, "_", dictionary_hash, "_v2.rds")
  if (file.exists(metric_cache)) {
    cached <- readRDS(metric_cache)
    if (is.na(cached$metrics$fiscal_year)) {
      record_issue("text", row$ticker, row$accession, "missing_fiscal_year_dei",
                   "Fiscal year needs accession-specific companyfacts fallback.")
    }
    text_rows[[i]] <- bind_cols(row, cached$metrics)
    term_rows[[i]] <- tibble(cik = row$cik, accession = row$accession, term = dictionary$term, count = cached$counts)
    next
  }
  tryCatch({
    compressed <- readBin(row$local_file, "raw", n = file.info(row$local_file)$size)
    html <- memDecompress(compressed, "gzip")
    # Normalize early cache files to a standard gzip container without new requests.
    if (!identical(compressed[1:2], as.raw(c(31, 139)))) {
      connection <- gzfile(row$local_file, "wb")
      writeBin(html, connection)
      close(connection)
    }
    document <- xml2::read_html(html)
    fiscal_node <- xml2::xml_find_first(document,
      "//*[translate(@name,'ABCDEFGHIJKLMNOPQRSTUVWXYZ','abcdefghijklmnopqrstuvwxyz')='dei:documentfiscalyearfocus']")
    fiscal_year <- suppressWarnings(as.integer(xml2::xml_text(fiscal_node)))
    if (is.na(fiscal_year)) {
      record_issue("text", row$ticker, row$accession, "missing_fiscal_year_dei",
                   "Fiscal year needs accession-specific companyfacts fallback.")
    }
    hidden <- xml2::xml_find_all(document,
      "//script|//style|//noscript|//*[local-name()='hidden' or local-name()='ix:hidden']|//*[@hidden]|//*[@aria-hidden='true']|//*[contains(translate(@style,'ABCDEFGHIJKLMNOPQRSTUVWXYZ ', 'abcdefghijklmnopqrstuvwxyz'),'display:none')]")
    xml2::xml_remove(hidden)
    # Native XML text extraction is fast; spaces at block boundaries preserve words.
    visible_html <- as.character(document)
    visible_html <- gsub("(?i)</?(?:p|div|br|hr|li|tr|td|th|table|h[1-6]|section|article|header|footer|ul|ol|pre|blockquote|address|dl|dt|dd)\\b[^>]*>",
                         " ", visible_html, perl = TRUE)
    text <- xml2::read_html(visible_html) |> rvest::html_text() |> str_squish()
    word_count <- str_count(text, "\\b[[:alpha:]][[:alnum:]'’-]*\\b")
    if (word_count < 1000) stop("Fewer than 1,000 words in primary document")
    counts <- count_ai_terms(text, dictionary)
    text_rows[[i]] <- row |> mutate(fiscal_year, word_count, ai_count = sum(counts),
                                    ai_intensity = ai_count / word_count * 10000)
    term_rows[[i]] <- tibble(cik = row$cik, accession = row$accession, term = dictionary$term, count = counts)
    temporary_cache <- paste0(metric_cache, ".", Sys.getpid(), ".tmp")
    saveRDS(list(metrics = text_rows[[i]] |> select(fiscal_year, word_count, ai_count, ai_intensity),
                 counts = counts), temporary_cache)
    file.rename(temporary_cache, metric_cache)
  }, error = function(e) record_issue("text", row$ticker, row$accession, "text_parse_failed", conditionMessage(e)))
}
if (Sys.getenv("AI_CACHE_ONLY", "0") != "1") {
  write_csv(bind_rows(text_rows), "data/processed/ai_mentions.csv")
  write_csv(bind_rows(term_rows), "data/processed/term_counts.csv")
  manifest <- read_csv("data/raw/download_manifest.csv")
  manifest$bytes <- file.info(manifest$file)$size
  manifest$md5 <- unname(tools::md5sum(manifest$file))
  write_csv(manifest, "data/raw/download_manifest.csv")
}
