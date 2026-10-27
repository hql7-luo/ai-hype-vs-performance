# Write project summaries directly from the saved analysis, without entering findings by hand.
suppressPackageStartupMessages(library(tidyverse))

# Read the current run and fail clearly when the analysis has not been produced.
read_study_outputs <- function() {
  summary_path <- "output/tables/run_summary.json"
  if (!file.exists(summary_path)) {
    stop("No analysis results found. Run Rscript run.R --sample20 first.")
  }
  table_names <- c("trend_year", "trend_balanced", "period_comparison", "company_rankings",
                   "quintile_summary", "quintile_membership", "exposure_groups_summary",
                   "regression_coefficients", "effect_slopes", "model_status")
  result <- list(summary = jsonlite::fromJSON(summary_path, simplifyVector = TRUE))
  for (name in table_names) {
    file <- file.path("output/tables", paste0(name, ".csv"))
    if (!file.exists(file)) stop("Required analysis table is missing: ", file)
    result[[name]] <- read_csv(file, show_col_types = FALSE)
  }
  result$panel <- read_csv("data/processed/panel.csv", show_col_types = FALSE,
                           col_types = cols(cik = col_character()))
  company_file <- "data/processed/companies.csv"
  result$companies <- if (file.exists(company_file)) read_csv(company_file, show_col_types = FALSE) else tibble()
  issue_file <- "output/tables/data_issues.csv"
  result$issues <- if (file.exists(issue_file)) read_csv(issue_file, show_col_types = FALSE,
                          col_types = cols(.default = col_character())) else tibble()
  coverage_paths <- list.files("output/tables", pattern = "coverage|exclusion", full.names = TRUE)
  result$coverage <- setNames(lapply(coverage_paths, read_csv, show_col_types = FALSE), basename(coverage_paths))
  membership_file <- "output/tables/membership_comparison_summary.json"
  result$membership_comparison <- if (file.exists(membership_file)) jsonlite::fromJSON(membership_file) else NULL
  membership <- result[["membership_comparison"]]
  if (!is.null(membership) && isTRUE(membership$remaining_unresolved_differences == 0) &&
      isTRUE(membership$wikipedia_only == 0) && isTRUE(membership$both == membership$wikipedia_n)) {
    result$membership_note <- paste0("A saved fresh SSGA/tidyquant comparison matches all ",
      format_count(membership$both), " archived Wikipedia constituent tickers after share-class punctuation normalization. ",
      "The ", format_count(membership$tidyquant_n), " ETF holdings rows also include cash and a Hologic contra position, ",
      "which are not company constituents. This resolves the original raw-list warning for the compared current snapshots; no membership change was needed.")
  } else if (nrow(result$issues) && any(result$issues$reason == "membership_disagreement")) {
    result$membership_note <- "The live tidyquant index list and Wikipedia constituent table disagreed; the explicitly archived Wikipedia table with CIKs supplies the declared membership snapshot."
  } else if (nrow(result$issues) && any(result$issues$reason %in% c("tq_index_failed", "wikipedia_membership_used"))) {
    result$membership_note <- "The live tidyquant index request was unavailable or incomplete; the archived Wikipedia constituent table with CIKs supplies the membership snapshot."
  } else {
    result$membership_note <- "The archived Wikipedia constituent table with CIKs supplies the declared membership snapshot; inspect the source ledger for comparisons with the live index list."
  }
  publication_file <- "output/tables/publication.json"
  publication <- if (file.exists(publication_file)) jsonlite::fromJSON(publication_file) else NULL
  result$publication <- publication
  result$publication_note <- "**Status:** local project; not published or deployed."
  if (is.list(publication)) {
    publication_url <- publication$url
    if (is.null(publication_url)) publication_url <- publication$repository_url
    if (isTRUE(publication$verified) && length(publication_url) == 1 &&
        grepl("^https://github[.]com/hql7-luo/ai-hype-vs-performance/?$", publication_url)) {
      result$publication_note <- paste0("**Status:** published repository at [hql7-luo/ai-hype-vs-performance](", publication_url,
        "). The Shiny application runs locally; a hosted Shiny deployment is not claimed.")
    }
  }
  validation_file <- "output/tables/validation_checks.csv"
  result$validation <- if (file.exists(validation_file)) read_csv(validation_file, show_col_types = FALSE) else tibble()
  source_file <- "output/tables/source_spot_checks.csv"
  result$source_checks <- if (file.exists(source_file)) read_csv(source_file, show_col_types = FALSE,
                                   col_types = cols(cik = col_character())) else tibble()
  concept_file <- "output/tables/revenue_concept_review.csv"
  result$revenue_review <- if (file.exists(concept_file)) read_csv(concept_file, show_col_types = FALSE,
                                     col_types = cols(cik = col_character(), sec_cik = col_character())) else tibble()
  cover_file <- "config/fiscal_year_overrides.csv"
  result$fiscal_overrides <- if (file.exists(cover_file)) read_csv(cover_file, show_col_types = FALSE,
                                      col_types = cols(cik = col_character())) else tibble()
  crosscheck_file <- "output/tables/fiscal_year_crosscheck.csv"
  result$fiscal_crosscheck <- if (file.exists(crosscheck_file)) read_csv(crosscheck_file, show_col_types = FALSE) else tibble()
  result$as_of <- if (!is.null(result$summary$as_of)) result$summary$as_of else Sys.getenv("ANALYSIS_AS_OF", "2026-10-26")
  result
}

# Keep percentages, percentage points, and missing results distinct.
format_number <- function(x, digits = 2) {
  ifelse(is.finite(x), formatC(x, format = "f", digits = digits, big.mark = ","), "unavailable")
}
format_count <- function(x) format_number(x, 0)
format_p <- function(x) ifelse(!is.finite(x), "unavailable", ifelse(x < 0.001, "<0.001", paste0("=", format_number(x, 3))))
outcome_label <- function(x) recode(x, next_revenue_growth = "next fiscal-year revenue growth", excess_return_12m = "12-month return above SPY")

# Describe estimates with their actual uncertainty, including failed or unidentified models.
coefficient_sentence <- function(study, outcome) {
  row <- study$regression_coefficients %>% filter(.data$outcome == .env$outcome, model == "baseline", term == "ai_intensity")
  if (nrow(row) != 1) {
    status <- study$model_status %>% filter(.data$outcome == .env$outcome, model == "baseline")
    detail <- if (nrow(status)) paste(status$status, status$message, collapse = "; ") else "model output is missing"
    return(paste0("The baseline model for ", outcome_label(outcome), " is unavailable: ", detail, "."))
  }
  crosses_zero <- row$conf_low <= 0 && row$conf_high >= 0
  paste0("For ", outcome_label(outcome), ", one extra AI mention per 10,000 words is associated with ",
         format_number(100 * row$estimate, 3), " percentage points (95% CI ",
         format_number(100 * row$conf_low, 3), " to ", format_number(100 * row$conf_high, 3),
         "; p", format_p(row$p_value), "; ", format_count(row$n_obs), " observations, ",
         format_count(row$n_firms), " firms). The interval ",
         ifelse(crosses_zero, "includes", "does not include"), " zero.")
}
interaction_sentence <- function(study, outcome) {
  rows <- study$effect_slopes %>% filter(.data$outcome == .env$outcome, model == "interaction")
  if (!all(c("pre", "post", "change") %in% rows$period)) {
    return(paste0("The before/after slope comparison for ", outcome_label(outcome),
                  " is unavailable; inspect model_status.csv and the regression table for identification or coverage problems."))
  }
  pre <- rows %>% filter(period == "pre")
  post <- rows %>% filter(period == "post")
  change <- rows %>% filter(period == "change")
  paste0("For ", outcome_label(outcome), ", the estimated slope is ",
         format_number(100 * pre$estimate, 3), " percentage points before 2022-11-30 and ",
         format_number(100 * post$estimate, 3), " on/after that date. The slope change is ",
         format_number(100 * change$estimate, 3), " percentage points (95% CI ",
         format_number(100 * change$conf_low, 3), " to ", format_number(100 * change$conf_high, 3),
         "; p", format_p(change$p_value), "). ",
         ifelse(change$conf_low <= 0 && change$conf_high >= 0,
                "This interval includes zero, so the model does not establish a changed relationship at the 5% level.",
                "This interval excludes zero, indicating a difference in association at the 5% level; it is not a causal effect of ChatGPT."))
}

# Put sensitivity results beside the main finding rather than hiding them in an appendix.
sensitivity_sentence <- function(study, outcome = "excess_return_12m") {
  rows <- study$regression_coefficients %>%
    filter(.data$outcome == .env$outcome, model %in% c("firm_fe", "winsorized"), term == "ai_intensity")
  baseline <- study$regression_coefficients %>%
    filter(.data$outcome == .env$outcome, model == "baseline", term == "ai_intensity")
  if (nrow(rows) != 2 || nrow(baseline) != 1) {
    return(paste0("Sensitivity results for ", outcome_label(outcome),
      " are incomplete; model availability is recorded in model_status.csv."))
  }
  firm <- rows %>% filter(model == "firm_fe")
  winsor <- rows %>% filter(model == "winsorized")
  includes_zero <- rows$conf_low <= 0 & rows$conf_high >= 0
  baseline_includes_zero <- baseline$conf_low <= 0 && baseline$conf_high >= 0
  changed_pattern <- any(sign(rows$estimate) != sign(baseline$estimate)) ||
    any(includes_zero != baseline_includes_zero)
  interpretation <- if (changed_pattern) {
    "The association is not stable across these specifications."
  } else {
    "These two checks retain the baseline's direction and confidence-interval pattern, while remaining observational."
  }
  paste0("For ", outcome_label(outcome), ", the AI slope is ",
    format_number(100 * firm$estimate, 3), " percentage points with firm and year fixed effects (p", format_p(firm$p_value),
    ") and ", format_number(100 * winsor$estimate, 3),
    " with outcomes winsorized at the 1st/99th percentiles (p", format_p(winsor$p_value), "). ",
    if (all(includes_zero)) "Both 95% confidence intervals include zero. " else "", interpretation)
}

# Compare observed filing periods without treating composition changes as a causal result.
trend_sentence <- function(study) {
  before <- study$period_comparison %>% filter(period == "Before 2022-11-30")
  after <- study$period_comparison %>% filter(period == "On/after 2022-11-30")
  if (nrow(before) != 1 || nrow(after) != 1) return("Both before and after periods are not available in this run.")
  paste0("Mean AI intensity is ", format_number(before$mean_ai_intensity),
         " before 2022-11-30 and ", format_number(after$mean_ai_intensity),
         " on/after that date, measured as mentions per 10,000 words. The share of filings with any dictionary mention is ",
         format_number(100 * before$share_any_ai, 1), "% and ", format_number(100 * after$share_any_ai, 1),
         "%, respectively. These are unweighted observations with changing company and year composition.")
}

# Preserve the saved exclusion details in the report and summarize them for the README.
coverage_markdown <- function(study) {
  if (!length(study$coverage)) return("Detailed coverage tables are not available in this run; consult `output/tables/data_issues.csv` and the panel's missing outcomes.")
  pieces <- character()
  for (name in intersect(c("coverage.csv", "return_exclusions.csv"), names(study$coverage))) {
    table <- study$coverage[[name]]
    pieces <- c(pieces, paste0("**", name, "**"), "", knitr::kable(table, format = "pipe"), "")
  }
  pieces
}

# State exactly what the saved validation and selected source sample checked.
verification_sentence <- function(study) {
  pieces <- character()
  if (nrow(study$validation)) {
    pieces <- c(pieces, paste0(format_count(sum(study$validation$status == "passed", na.rm = TRUE)),
      " of ", format_count(nrow(study$validation)), " saved validation checks passed."))
  }
  if (nrow(study$source_checks)) {
    checks <- study$source_checks
    pieces <- c(pieces, paste0("A selected source spot check covers ", format_count(nrow(checks)),
      " original filings: original-accession companyfacts values matched in ",
      format_count(sum(checks$original_companyfacts_match, na.rm = TRUE)),
      " and independent AI counts matched in ", format_count(sum(checks$ai_count_match, na.rm = TRUE)),
      ". Inline annual revenue matched in ", format_count(sum(checks$inline_document_match, na.rm = TRUE)),
      "; ", format_count(sum(is.na(checks$inline_document_match))),
      " had no matching inline amount and used companyfacts-only revenue verification. This selected sample is not a source audit of every filing."))
  }
  if (nrow(study$revenue_review)) {
    review <- study$revenue_review
    pieces <- c(pieces, paste0("A separate targeted revenue-concept review covers ",
      format_count(n_distinct(review$accession)), " primary filings and ", format_count(nrow(review)),
      " concept rows: ", format_count(sum(review$inline_source_match, na.rm = TRUE)),
      " inline amount matches and ", format_count(sum(!review$inline_source_match, na.rm = TRUE)),
      " API-only checks. This is a targeted review, not full-universe source assurance."))
  }
  if (nrow(study$fiscal_overrides)) {
    corrected <- if (nrow(study$fiscal_crosscheck)) study$fiscal_crosscheck %>%
      filter(!is.na(fiscal_year_cover), !is.na(fiscal_year_dei), fiscal_year_cover != fiscal_year_dei) else tibble()
    pieces <- c(pieces, paste0("The verified cover-label ledger contains ", format_count(nrow(study$fiscal_overrides)),
      " filings, including ", format_count(nrow(corrected)),
      " corrections to nonmissing inline DEI labels; the panel retains the original DEI value."))
  }
  if (!length(pieces)) return("No saved validation or source spot-check ledger is available for this run.")
  paste(pieces, collapse = " ")
}

# Generate the English README and reusable bilingual portfolio copy from one source of results.
write_project_summaries <- function(study = read_study_outputs()) {
  summary <- study$summary
  baseline <- study$regression_coefficients %>% filter(model == "baseline", term == "ai_intensity")
  readme <- c(
    "# AI Talk vs. AI Results", "", "**Independent project · Haoqi Luo · R / SEC EDGAR / panel regression**", "", study$publication_note, "",
    "Do companies that mention AI more in their annual reports later grow faster or outperform the market? Did that association change after ChatGPT's November 2022 release?", "",
    "## Current results", "",
    paste0("As-of cutoff: **", study$as_of, "**; saved run mode: **", summary$sample_mode, "**. The analysis contains **", format_count(summary$n_filings),
           " 10-K filings from ", format_count(summary$n_firms), " issuers**, with fiscal years ",
           min(summary$fiscal_years), "–", max(summary$fiscal_years), ". ",
           "The selected run and its coverage, rather than the target universe, determine these findings."), "",
    paste0("- ", trend_sentence(study)),
    paste0("- ", coefficient_sentence(study, "next_revenue_growth")),
    paste0("- ", coefficient_sentence(study, "excess_return_12m")),
    paste0("- ", sensitivity_sentence(study, "excess_return_12m")), "",
    interaction_sentence(study, "next_revenue_growth"), "",
    interaction_sentence(study, "excess_return_12m"), "",
    "![AI mentions per 10,000 words by filing year](output/figures/ai_trend.png)", "",
    "![Observed revenue growth and excess return by within-year AI quintile](output/figures/quintile_outcomes.png)", "",
    "![Regression estimates and 95% confidence intervals](output/figures/regression_coefficients.png)", "",
    "## Data and methods", "",
    paste0("- **Universe:** current S&P 500 constituents. ", study$membership_note, " Multiple share classes are collapsed to issuer CIK, choosing the alphabetically first ticker and logging the others. This is not historical index membership."),
    "- **Filings:** SEC submissions JSON, its historical pages, and original 10-K main documents, FY2018 onward. Amendments are not extra observations. Fiscal-year labels, period ends, and filing years are separate; verified cover-page labels take priority to fill gaps or correct a documented erroneous DEI tag, while preserving the original DEI value, documented in `config/fiscal_year_overrides.csv`. XOM historical filings use the SEC-documented predecessor CIK after its July 2026 one-for-one successor reorganization; `config/cik_history_overrides.csv` preserves the source mapping. HONA and VYLR are not automatically joined to consolidated parent histories; `config/issuer_history_notes.csv` records those decisions.",
    "- **AI measure:** non-overlapping dictionary matches per 10,000 visible-text words. Longer phrases take priority; standalone uppercase `AI` is case-sensitive. Word tokens start with a letter and can include letters, digits, apostrophes, and hyphens; standalone numbers are excluded. Hidden XBRL, scripts, and styles are removed. Edit `config/ai_dictionary.csv` to change the dictionary.",
    "- **Revenue:** annual USD companyfacts values are combined across alternative revenue tags by precedence, not addition. Predictors use the original current filing. Next-FY growth uses the next original 10-K's same-tag current/prior comparison, so its prior-year denominator can reflect a restatement; fiscal endpoints must be adjacent and match exactly. The selected issuer-specific revenue proxy can include other or investment income; bank totals can be net of interest expense. It is not standardized operating sales. Selected tags and gaps remain inspectable.",
    "- **Returns:** tidyquant / Yahoo adjusted prices for the chosen share class and SPY. Excess return is the stock's 12-month adjusted-price return minus SPY's return over identical trading dates. Both endpoints use the first common trading close after the filing date / on or after its 12-month calendar anniversary, within seven days. Only mature, complete windows enter the return analysis.",
    "- **Models:** `fixest` OLS with calendar filing-year and broad SIC-industry fixed effects, log revenue and current observed fiscal-year revenue growth controls, and standard errors clustered by issuer. The interaction uses `filing_date >= 2022-11-30`. The post-period slope is the AI coefficient plus its interaction, using their covariance for uncertainty.",
    "- **Comparisons:** quintiles are ranked within filing year. Ties are ordered by CIK, so identical zero intensities can span groups. The report also shows zero mentions versus positive-intensity quartiles and sensitivities. The first and last filing years are partial because of the FY floor and cutoff; the constant-company trend excludes both boundary years.", "",
    "## Coverage and exclusions", "",
    paste0("There are ", format_count(summary$n_revenue_outcomes), " observed next-FY revenue outcomes and ",
           format_count(summary$n_return_outcomes), " complete 12-month return outcomes before regression control exclusions. ",
           "The two regressions use separate complete-case samples."), "",
    coverage_markdown(study), "", "Return exclusion counts cover all discovered filings before the final fiscal-year/text filter; panel and model counts apply after that filter. Each panel exclusion keeps its own outcome and control status.", "",
    paste0("The source-issue ledger contains ", format_count(nrow(study$issues)),
           " distinct recorded issues, including successful fallbacks. See [`data_issues.csv`](output/tables/data_issues.csv), ",
           "[`model_status.csv`](output/tables/model_status.csv), and the report for exact reasons. Source failures are not measured zeros."), "",
    verification_sentence(study), " See [validation checks](output/tables/validation_checks.csv), [selected source checks](output/tables/source_spot_checks.csv), and [revenue-concept review](output/tables/revenue_concept_review.csv).", "",
    "## Limitations", "",
    "This is an observational association, not a causal test or a trading strategy. Current constituents create survivorship and selection bias. Revenue growth, firm size, business model, prior AI investment, and investor expectations may drive both disclosure and outcomes; industry/year controls do not remove all these differences. Reverse causality remains possible.", "",
    "The text scope is the requested main 10-K document; incorporated annual reports and exhibits are excluded, so comparable narrative sections can be omitted for some issuers. A dictionary measures language, including risk disclosures, rather than real AI investment or realized value. SEC standard-taxonomy coverage, restatements, reorganizations, spin-offs, and revenue-tag changes can affect comparability. Current SIC classifications may not represent earlier businesses. The next fiscal year has normally already begun when a 10-K is filed, and recent filings have fewer available outcomes. Returns are benchmark-adjusted, not factor-adjusted abnormal returns. Repeated tests and skewed outcomes make p-values exploratory; a confidence interval containing zero does not prove no relationship.", "",
    "## Run the project", "",
    "Install R and Quarto, then run the project from its root:", "",
    "```r", "source(\"install.R\")", "```", "",
    "```sh", "Rscript run.R --sample20", "Rscript run.R --full", "quarto render report.qmd", "Rscript -e 'shiny::runApp(\"app\")'", "```", "",
    "To rebuild the included processed snapshot without SEC or Yahoo downloads:", "",
    "```sh", "Rscript scripts/replay_analysis.R --full", "quarto render report.qmd", "```", "",
    "Replay runs the analysis, panel checks, and summary generator. The included CI workflow is configured for this offline-data replay; it does not recollect sources.", "",
    "Optional pinned package versions are recorded in `renv.lock`. In R, install `renv` if needed, run `renv::activate()` (restart if prompted), then `renv::restore()`. Activation is opt-in; the repository does not automatically activate renv.", "",
    "Run and inspect the 20-company validation before expanding. Every successful source response is cached under `data/raw`; repeated runs reuse those files. SEC requests identify `Haoqi Luo haoqi.luo23@hoosac.org` and share a four-requests-per-second throttle, with up to five concurrent primary-document transfers; retries use the same throttle. The default evidence cutoff is 2026-10-26; set `ANALYSIS_AS_OF` deliberately for a new dated snapshot. Membership remains frozen until its cache is explicitly archived/refreshed; changing the cutoff alone does not refresh it. Keep the original raw cache when reproducing this snapshot.", "",
    "`Rscript scripts/write_summary.R` regenerates this README and `output/portfolio_blurbs.md` from saved results. The HTML report and Shiny application read the same processed panel and analysis tables. The raw cache is excluded from Git; preserve it locally to replay source extraction; analysis-only replay uses the included processed snapshot.", "",
    "## Project files", "",
    "- `R/01_get_companies.R` through `R/07_analysis.R`: data collection, text measurement, panel construction, and analysis.",
    "- `data/raw/`: cached source responses and download manifest; `data/processed/`: selected records and panel.",
    "- `output/figures/` and `output/tables/`: figures, coverage, model estimates, and source-issue ledger.",
    "- `report.qmd`: reproducible Quarto report; `app/`: local Shiny explorer.", "",
    "## Sources", "",
    "- [SEC EDGAR API documentation](https://www.sec.gov/search-filings/edgar-application-programming-interfaces)",
    "- [SEC access and fair-use policy](https://www.sec.gov/search-filings/edgar-search-assistance/accessing-edgar-data)",
    "- [ExxonMobil successor filing, July 1, 2026](https://www.sec.gov/Archives/edgar/data/2115436/000119312526291990/d71068d8k12b.htm)",
    "- [SEC SIC codes](https://www.sec.gov/search-filings/standard-industrial-classification-sic-code-list)",
    "- [Saved membership reconciliation](output/tables/membership_reconciliation.md)",
    "- [Archived current S&P 500 constituent source](https://en.wikipedia.org/wiki/List_of_S%26P_500_companies)",
    "- [tidyquant index universe](https://business-science.github.io/tidyquant/reference/tq_index.html) and [price data](https://business-science.github.io/tidyquant/reference/tq_get.html)",
    "- [fixest estimation](https://lrberge.github.io/fixest/reference/feols.html)",
    "- [ChatGPT release announcement, November 30, 2022](https://openai.com/index/chatgpt/)", ""
  )
  writeLines(readme, "README.md", useBytes = TRUE)
  revenue <- baseline %>% filter(outcome == "next_revenue_growth")
  returns <- baseline %>% filter(outcome == "excess_return_12m")
  return_firm <- study$regression_coefficients %>% filter(outcome == "excess_return_12m", model == "firm_fe", term == "ai_intensity")
  return_winsor <- study$regression_coefficients %>% filter(outcome == "excess_return_12m", model == "winsorized", term == "ai_intensity")
  model_en <- if (nrow(revenue) == 1 && nrow(returns) == 1) {
    paste0("In the main industry/year model with size and prior-growth controls, one additional mention per 10,000 words is associated with ",
      format_number(100 * revenue$estimate, 3), " percentage points of next-FY revenue growth and ",
      format_number(100 * returns$estimate, 3), " percentage points of 12-month return above SPY.")
  } else "The report records available regression estimates and any model failures without replacing missing results."
  model_zh <- if (nrow(revenue) == 1 && nrow(returns) == 1) {
    paste0("在控制行业、年份、营收规模和当期增速的主模型中，每万词增加一次 AI 提及，与下一财年营收增速变化 ",
      format_number(100 * revenue$estimate, 3), " 个百分点、12 个月相对 SPY 收益变化 ",
      format_number(100 * returns$estimate, 3), " 个百分点相关。")
  } else "报告如实保留可估计的回归结果及无法估计的原因，不用虚构数据补齐缺失结果。"
  sensitivity_en <- if (nrow(return_firm) == 1 && nrow(return_winsor) == 1) {
    paste0("The return slope changes to ", format_number(100 * return_firm$estimate, 3),
      " percentage points with firm effects (p", format_p(return_firm$p_value), ") and ",
      format_number(100 * return_winsor$estimate, 3), " with winsorization (p", format_p(return_winsor$p_value),
      "), so the report presents specification sensitivity alongside survivorship and measurement limits and treats all estimates as correlations.")
  } else "The study treats its estimates as correlations and documents sensitivity, survivorship bias, and measurement limits."
  sensitivity_zh <- if (nrow(return_firm) == 1 && nrow(return_winsor) == 1) {
    paste0("收益斜率在加入公司固定效应后为 ", format_number(100 * return_firm$estimate, 3),
      " 个百分点（p", format_p(return_firm$p_value), "），在缩尾处理后为 ",
      format_number(100 * return_winsor$estimate, 3), " 个百分点（p", format_p(return_winsor$p_value),
      "），因此报告明确呈现模型敏感性、幸存者偏差和测量局限，并将全部结果限定为相关性。")
  } else "研究将结果限定为相关性，并明确记录模型敏感性、幸存者偏差和测量局限。"
  blurbs <- c("# Portfolio project copy", "", "## English", "",
    paste0("AI Talk vs. AI Results is an independent R data-science project linking ", format_count(summary$n_filings),
           " real SEC 10-K filings from ", format_count(summary$n_firms), " current S&P 500 issuers to later revenue growth and stock returns. ",
           "I built a cached EDGAR pipeline, a transparent AI dictionary, a Quarto report, and a Shiny explorer. ",
           model_en, " ", sensitivity_en), "",
    "## 中文", "",
    paste0("AI Talk vs. AI Results 是一个独立完成的 R 语言数据科学项目，将 ", format_count(summary$n_firms),
           " 家当前标普 500 成分公司的 ", format_count(summary$n_filings), " 份真实 SEC 10-K 年报，与后续营收增长和股价表现连接起来。",
           "我搭建了可缓存的 EDGAR 数据流程、透明可修改的 AI 词典、Quarto 报告和 Shiny 交互应用。",
           model_zh, sensitivity_zh), "")
  dir.create("output", showWarnings = FALSE)
  writeLines(blurbs, "output/portfolio_blurbs.md", useBytes = TRUE)
  message("Updated README.md and output/portfolio_blurbs.md from saved results.")
  invisible(study)
}

if (sys.nframe() == 0L) write_project_summaries()
