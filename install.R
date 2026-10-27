# Install the packages used by the scripts, report, and app.
packages <- c("tidyverse", "tidyquant", "fixest", "httr2", "jsonlite", "rvest", "xml2",
              "shiny", "knitr", "rmarkdown", "testthat", "renv")
missing <- setdiff(packages, rownames(installed.packages()))
if (length(missing)) install.packages(missing, repos = "https://cloud.r-project.org")
