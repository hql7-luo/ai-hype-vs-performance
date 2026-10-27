The published snapshot contains real public-source records, with one panel row per issuer and fiscal period end. Empty/NA values are unavailable observations, not zeroes. The analysis cutoff and selected sample are saved in `output/tables/run_config.json`.

| Variable | Meaning / units |
|---|---|
| `cik`, `sec_cik` | Current issuer identifier and SEC identifier used for the source history; retain leading zeroes. |
| `ticker`, `company` | Selected traded share class and current company name. |
| `accession`, `url`, `local_file` | Original annual filing, direct SEC source URL, and local raw cache path. |
| `fiscal_year`, `period_end` | Issuer's fiscal-year label and exact fiscal end date; these can differ from the calendar filing year. |
| `fiscal_year_source`, `fiscal_label_tag`, `fiscal_year_dei` | Selected fiscal-label provenance (verified cover, inline DEI, or original-accession companyfacts), supporting tag, and preserved original DEI value. |
| `filing_date`, `filing_year` | Public filing date and calendar year used for year fixed effects. |
| `sic`, `industry` | Current SIC code and broad SIC division. |
| `ai_count`, `word_count` | Non-overlapping dictionary matches and alphabetic-start visible-text word tokens. |
| `ai_intensity` | `ai_count / word_count * 10000`. |
| `post2022` | One when filing date is on/after 2022-11-30. |
| `revenue`, `revenue_tag` | Original filing's selected standard annual USD total and its XBRL concept; concepts are not added together. |
| `prior_revenue`, `prior_period_end` | Same-tag prior-year comparative in the original current filing. |
| `current_growth` | `revenue / prior_revenue - 1`; decimal rate, used as the lag relative to next-year growth. |
| `log_revenue` | Natural log of annual revenue in USD. |
| `next_revenue`, `next_prior_revenue` | Annual amounts from the next original 10-K's same-tag comparison, possibly including a revised prior-year amount. |
| `next_revenue_growth` | Next filing's annual growth, only when its prior endpoint exactly matches this period and the next fiscal year is adjacent; decimal rate. |
| `return_start`, `return_end` | Identical stock/SPY trading endpoints strictly after filing and on/after the calendar anniversary, within seven days. |
| `stock_start`, `stock_end`, `spy_start`, `spy_end` | Actual adjusted daily endpoint prices. |
| `excess_return_12m` | Stock adjusted-price return minus SPY return; decimal difference. Multiply by 100 for percentage points. |
| `return_status` | Complete-window status or the exact reason the return is missing. |
| `revenue_eligible`, `return_eligible` | Outcome-specific complete-control sample indicators. |

`annual_revenues.csv`, `fiscal_labels.csv`, `term_counts.csv`, `filing_returns.csv`, and `prices.csv` retain the main selected inputs. `membership_snapshot.csv` includes all current listings before issuer deduplication. `companies.csv` is the selected issuer universe; its absence from `panel.csv` does not imply an omitted source request. Use the coverage and exclusion ledgers to inspect missing histories and outcomes.

Raw companyfacts and primary HTML remain in the local `data/raw` cache and are excluded from Git. The included processed snapshot supports offline analysis replay; exact source-level spot checks also need that raw cache.
