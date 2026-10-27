# Rebuild published findings from the included processed public-data snapshot.
source("R/07_analysis.R")
source("tests/validate_panel.R")
source("scripts/write_summary.R")
write_project_summaries()
writeLines(capture.output(sessionInfo()), "output/sessionInfo.txt")
