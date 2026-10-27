# AI Talk vs. AI Results

**Independent project · Haoqi Luo · R / SEC EDGAR / panel regression**

**Status:** local project; not published or deployed.

Do companies that mention AI more in their annual reports later grow faster or outperform the market? Did that association change after ChatGPT's November 2022 release?

## Current results

As-of cutoff: **2026-10-26**; saved run mode: **full**. The analysis contains **3,910 10-K filings from 498 issuers**, with fiscal years 2018–2026. The selected run and its coverage, rather than the target universe, determine these findings.

- Mean AI intensity is 0.36 before 2022-11-30 and 2.69 on/after that date, measured as mentions per 10,000 words. The share of filings with any dictionary mention is 28.2% and 76.0%, respectively. These are unweighted observations with changing company and year composition.
- For next fiscal-year revenue growth, one extra AI mention per 10,000 words is associated with 0.981 percentage points (95% CI 0.519 to 1.443; p<0.001; 3,289 observations, 487 firms). The interval does not include zero.
- For 12-month return above SPY, one extra AI mention per 10,000 words is associated with 1.273 percentage points (95% CI 0.254 to 2.292; p=0.014; 3,304 observations, 488 firms). The interval does not include zero.
- For 12-month return above SPY, the AI slope is -0.242 percentage points with firm and year fixed effects (p=0.708) and 0.729 with outcomes winsorized at the 1st/99th percentiles (p=0.074). Both 95% confidence intervals include zero. The association is not stable across these specifications.

For next fiscal-year revenue growth, the estimated slope is 0.584 percentage points before 2022-11-30 and 1.065 on/after that date. The slope change is 0.482 percentage points (95% CI -0.257 to 1.220; p=0.201). This interval includes zero, so the model does not establish a changed relationship at the 5% level.

For 12-month return above SPY, the estimated slope is 1.211 percentage points before 2022-11-30 and 1.298 on/after that date. The slope change is 0.087 percentage points (95% CI -1.439 to 1.613; p=0.911). This interval includes zero, so the model does not establish a changed relationship at the 5% level.

![AI mentions per 10,000 words by filing year](output/figures/ai_trend.png)

![Observed revenue growth and excess return by within-year AI quintile](output/figures/quintile_outcomes.png)

![Regression estimates and 95% confidence intervals](output/figures/regression_coefficients.png)

## Data and methods

- **Universe:** current S&P 500 constituents. A saved fresh SSGA/tidyquant comparison matches all 503 archived Wikipedia constituent tickers after share-class punctuation normalization. The 505 ETF holdings rows also include cash and a Hologic contra position, which are not company constituents. This resolves the original raw-list warning for the compared current snapshots; no membership change was needed. Multiple share classes are collapsed to issuer CIK, choosing the alphabetically first ticker and logging the others. This is not historical index membership.
- **Filings:** SEC submissions JSON, its historical pages, and original 10-K main documents, FY2018 onward. Amendments are not extra observations. Fiscal-year labels, period ends, and filing years are separate; verified cover-page labels take priority to fill gaps or correct a documented erroneous DEI tag, while preserving the original DEI value, documented in `config/fiscal_year_overrides.csv`. XOM historical filings use the SEC-documented predecessor CIK after its July 2026 one-for-one successor reorganization; `config/cik_history_overrides.csv` preserves the source mapping. HONA and VYLR are not automatically joined to consolidated parent histories; `config/issuer_history_notes.csv` records those decisions.
- **AI measure:** non-overlapping dictionary matches per 10,000 visible-text words. Longer phrases take priority; standalone uppercase `AI` is case-sensitive. Word tokens start with a letter and can include letters, digits, apostrophes, and hyphens; standalone numbers are excluded. Hidden XBRL, scripts, and styles are removed. Edit `config/ai_dictionary.csv` to change the dictionary.
- **Revenue:** annual USD companyfacts values are combined across alternative revenue tags by precedence, not addition. Predictors use the original current filing. Next-FY growth uses the next original 10-K's same-tag current/prior comparison, so its prior-year denominator can reflect a restatement; fiscal endpoints must be adjacent and match exactly. The selected issuer-specific revenue proxy can include other or investment income; bank totals can be net of interest expense. It is not standardized operating sales. Selected tags and gaps remain inspectable.
- **Returns:** tidyquant / Yahoo adjusted prices for the chosen share class and SPY. Excess return is the stock's 12-month adjusted-price return minus SPY's return over identical trading dates. Both endpoints use the first common trading close after the filing date / on or after its 12-month calendar anniversary, within seven days. Only mature, complete windows enter the return analysis.
- **Models:** `fixest` OLS with calendar filing-year and broad SIC-industry fixed effects, log revenue and current observed fiscal-year revenue growth controls, and standard errors clustered by issuer. The interaction uses `filing_date >= 2022-11-30`. The post-period slope is the AI coefficient plus its interaction, using their covariance for uncertainty.
- **Comparisons:** quintiles are ranked within filing year. Ties are ordered by CIK, so identical zero intensities can span groups. The report also shows zero mentions versus positive-intensity quartiles and sensitivities. The first and last filing years are partial because of the FY floor and cutoff; the constant-company trend excludes both boundary years.

## Coverage and exclusions

There are 3,304 observed next-FY revenue outcomes and 3,410 complete 12-month return outcomes before regression control exclusions. The two regressions use separate complete-case samples.

**coverage.csv**

|metric                    | count|
|:-------------------------|-----:|
|constituent_listings      |   503|
|unique_issuers_selected   |   500|
|issuers_with_text         |   498|
|filings_discovered        |  3924|
|panel_filings             |  3910|
|revenue_analysis_rows     |  3289|
|return_analysis_rows      |  3304|
|incomplete_return_windows |   498|

**return_exclusions.csv**

|return_status          | n_filings|
|:----------------------|---------:|
|incomplete_12m_window  |       498|
|missing_start_endpoint |         2|
|ok                     |      3424|


Return exclusion counts cover all discovered filings before the final fiscal-year/text filter; panel and model counts apply after that filter. Each panel exclusion keeps its own outcome and control status.

The source-issue ledger contains 986 distinct recorded issues, including successful fallbacks. See [`data_issues.csv`](output/tables/data_issues.csv), [`model_status.csv`](output/tables/model_status.csv), and the report for exact reasons. Source failures are not measured zeros.

48 of 48 saved validation checks passed. A selected source spot check covers 10 original filings: original-accession companyfacts values matched in 10 and independent AI counts matched in 10. Inline annual revenue matched in 6; 4 had no matching inline amount and used companyfacts-only revenue verification. This selected sample is not a source audit of every filing. A separate targeted revenue-concept review covers 12 primary filings and 14 concept rows: 11 inline amount matches and 3 API-only checks. This is a targeted review, not full-universe source assurance. The verified cover-label ledger contains 20 filings, including 16 corrections to nonmissing inline DEI labels; the panel retains the original DEI value.
 See [validation checks](output/tables/validation_checks.csv), [selected source checks](output/tables/source_spot_checks.csv), and [revenue-concept review](output/tables/revenue_concept_review.csv).

## Limitations

This is an observational association, not a causal test or a trading strategy. Current constituents create survivorship and selection bias. Revenue growth, firm size, business model, prior AI investment, and investor expectations may drive both disclosure and outcomes; industry/year controls do not remove all these differences. Reverse causality remains possible.

The text scope is the requested main 10-K document; incorporated annual reports and exhibits are excluded, so comparable narrative sections can be omitted for some issuers. A dictionary measures language, including risk disclosures, rather than real AI investment or realized value. SEC standard-taxonomy coverage, restatements, reorganizations, spin-offs, and revenue-tag changes can affect comparability. Current SIC classifications may not represent earlier businesses. The next fiscal year has normally already begun when a 10-K is filed, and recent filings have fewer available outcomes. Returns are benchmark-adjusted, not factor-adjusted abnormal returns. Repeated tests and skewed outcomes make p-values exploratory; a confidence interval containing zero does not prove no relationship.

## Run the project

Install R and Quarto, then run the project from its root:

```r
source("install.R")
```

```sh
Rscript run.R --sample20
Rscript run.R --full
quarto render report.qmd
Rscript -e 'shiny::runApp("app")'
```

To rebuild the included processed snapshot without SEC or Yahoo downloads:

```sh
Rscript scripts/replay_analysis.R --full
quarto render report.qmd
```

Replay runs the analysis, panel checks, and summary generator. The included CI workflow is configured for this offline-data replay; it does not recollect sources.

Optional pinned package versions are recorded in `renv.lock`. In R, install `renv` if needed, run `renv::activate()` (restart if prompted), then `renv::restore()`. Activation is opt-in; the repository does not automatically activate renv.

Run and inspect the 20-company validation before expanding. Every successful source response is cached under `data/raw`; repeated runs reuse those files. SEC requests identify `Haoqi Luo haoqi.luo23@hoosac.org` and share a four-requests-per-second throttle, with up to five concurrent primary-document transfers; retries use the same throttle. The default evidence cutoff is 2026-10-26; set `ANALYSIS_AS_OF` deliberately for a new dated snapshot. Membership remains frozen until its cache is explicitly archived/refreshed; changing the cutoff alone does not refresh it. Keep the original raw cache when reproducing this snapshot.

`Rscript scripts/write_summary.R` regenerates this README and `output/portfolio_blurbs.md` from saved results. The HTML report and Shiny application read the same processed panel and analysis tables. The raw cache is excluded from Git; preserve it locally to replay source extraction; analysis-only replay uses the included processed snapshot.

## Project files

- `R/01_get_companies.R` through `R/07_analysis.R`: data collection, text measurement, panel construction, and analysis.
- `data/raw/`: cached source responses and download manifest; `data/processed/`: selected records and panel.
- `output/figures/` and `output/tables/`: figures, coverage, model estimates, and source-issue ledger.
- `report.qmd`: reproducible Quarto report; `app/`: local Shiny explorer.

## Sources

- [SEC EDGAR API documentation](https://www.sec.gov/search-filings/edgar-application-programming-interfaces)
- [SEC access and fair-use policy](https://www.sec.gov/search-filings/edgar-search-assistance/accessing-edgar-data)
- [ExxonMobil successor filing, July 1, 2026](https://www.sec.gov/Archives/edgar/data/2115436/000119312526291990/d71068d8k12b.htm)
- [SEC SIC codes](https://www.sec.gov/search-filings/standard-industrial-classification-sic-code-list)
- [Saved membership reconciliation](output/tables/membership_reconciliation.md)
- [Archived current S&P 500 constituent source](https://en.wikipedia.org/wiki/List_of_S%26P_500_companies)
- [tidyquant index universe](https://business-science.github.io/tidyquant/reference/tq_index.html) and [price data](https://business-science.github.io/tidyquant/reference/tq_get.html)
- [fixest estimation](https://lrberge.github.io/fixest/reference/feols.html)
- [ChatGPT release announcement, November 30, 2022](https://openai.com/index/chatgpt/)

