Raw public-source downloads are cached here and excluded from Git because the annual-report HTML is large.

- `submissions/`: SEC submissions and historical filing lists.
- `filings/`: original primary 10-K HTML, compressed with gzip.
- `companyfacts/`: SEC XBRL companyfacts JSON.
- `prices/`: daily adjusted Yahoo stock and SPY prices obtained with `tidyquant::tq_get()`.
- `sp500_wikipedia.html` and `sp500_membership.csv`: dated current-member snapshot with CIKs.
- `download_manifest.csv`: source URLs, retrieval timestamps, bytes, and MD5 hashes.

Successful cached SEC downloads are reused. To deliberately refresh a source, move its cache out of this folder first. Price cache names include the analysis cutoff. Changing `ANALYSIS_AS_OF` does not refresh the current-constituent snapshot; archive/remove the membership cache separately when starting a new snapshot. The data are subject to their original providers' terms; the project license covers the project code only.
