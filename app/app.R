library(shiny)
library(dplyr)
library(readr)
library(ggplot2)

# Find the project data whether the app starts from its folder or the project root.
data_paths <- c("data/processed/panel.csv", "../data/processed/panel.csv")
data_path <- data_paths[file.exists(data_paths)][1]
panel <- if (!is.na(data_path)) {
  read_csv(data_path, col_types = cols(
    cik = col_character(), sic = col_character(),
    filing_date = col_date(), period_end = col_date(),
    .default = col_guess()
  ), show_col_types = FALSE)
} else {
  NULL
}

navy <- "#16324F"
orange <- "#CF6736"

# Use the same clear visual style for every chart.
app_theme <- theme_minimal(base_size = 13) +
  theme(
    text = element_text(colour = navy),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, colour = "#526477"),
    legend.position = "bottom", legend.title = element_blank(),
    axis.title = element_text(size = 11),
    plot.margin = margin(12, 16, 12, 12)
  )

app_css <- "
  body { background: #f5f6f7; color: #16324f; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif; }
  .container-fluid { max-width: 1320px; padding: 24px; }
  h1 { font-size: 34px; font-weight: 750; letter-spacing: -.8px; margin-bottom: 8px; }
  h2 { font-size: 20px; font-weight: 650; }
  h3 { font-size: 17px; font-weight: 650; }
  .eyebrow { text-transform: uppercase; font-size: 11px; font-weight: 700; letter-spacing: 1.7px; color: #93623f; }
  .intro { color: #526477; font-size: 15px; max-width: 950px; line-height: 1.6; }
  .filters { margin: 24px 0 18px; padding: 16px 20px 4px; background: white; border: 1px solid #e2e6eb; border-radius: 10px; }
  .card { background: white; border: 1px solid #e2e6eb; border-radius: 10px; padding: 14px 16px; margin-bottom: 20px; }
  .stat { min-height: 104px; }
  .stat-label { color: #526477; font-size: 12px; }
  .stat-value { font-size: 27px; font-weight: 700; margin-top: 6px; }
  .scope { margin-bottom: 16px; color: #526477; }
  .note { font-size: 12px; color: #526477; line-height: 1.6; }
  .callout { background: #fff9f3; border-left: 3px solid #cf6736; padding: 12px 16px; margin: 16px 0 24px; line-height: 1.6; }
  .table-wrap { overflow-x: auto; max-height: 460px; overflow-y: auto; }
  .table { font-size: 12px; }
  .btn { border-radius: 6px; }
  .shiny-output-error-validation { color: #526477; font-size: 14px; padding-top: 30px; }
  .nav-tabs { margin-top: 8px; margin-bottom: 16px; }
  @media (max-width: 767px) { .container-fluid { padding: 16px; } h1 { font-size: 28px; } .stat { min-height: 88px; } }
"

# Explain missing data instead of displaying a synthetic dashboard.
if (is.null(panel) || nrow(panel) == 0) {
  ui <- fluidPage(
    tags$head(tags$style(HTML(app_css))),
    div(class = "eyebrow", "Independent project · R / SEC EDGAR"),
    h1("AI Talk vs. AI Results"),
    div(class = "callout",
        h2("The project data has not been generated yet."),
        p("Run the numbered data scripts from the project root, then restart this app."),
        p("Expected input: data/processed/panel.csv. No example values are substituted."))
  )
  server <- function(input, output, session) {}
} else {
  panel <- panel %>%
    mutate(
      filing_year = as.integer(filing_year),
      industry = coalesce(industry, "Unclassified")
    ) %>%
    arrange(ticker, filing_date)

  # Provide sector filters and a company comparison without hiding the baseline.
  ui <- fluidPage(
    tags$head(tags$style(HTML(app_css))),
    div(class = "eyebrow", "Independent project · R / SEC EDGAR"),
    h1("AI Talk vs. AI Results"),
    p(class = "intro",
      "Do companies that discuss AI more often in their annual reports grow faster or outperform SPY? ",
      "Explore the observed relationships in current S&P 500 companies, using their 10-K filings from FY2018 onward."),
    div(class = "filters", fluidRow(
      column(4, selectInput("industry", "SIC industry group",
                            choices = c("All industries", sort(unique(panel$industry))))),
      column(4, selectInput("company", "Company comparison",
                            choices = c("All companies", sort(unique(panel$ticker))))),
      column(4, selectInput("outcome", "Performance measure", choices = c(
        "Next fiscal-year revenue growth" = "next_revenue_growth",
        "12-month return above SPY" = "excess_return_12m"
      )))
    )),
    uiOutput("scope_label"),
    fluidRow(
      column(3, div(class = "card stat", div(class = "stat-label", "Companies in selection"),
                    div(class = "stat-value", textOutput("n_companies")))),
      column(3, div(class = "card stat", div(class = "stat-label", "10-K filings in selection"),
                    div(class = "stat-value", textOutput("n_filings")))),
      column(3, div(class = "card stat", div(class = "stat-label", "Observed next-year revenue growth"),
                    div(class = "stat-value", textOutput("n_revenue")))),
      column(3, div(class = "card stat", div(class = "stat-label", "Complete 12-month stock windows"),
                    div(class = "stat-value", textOutput("n_returns"))))
    ),
    tabsetPanel(
      tabPanel("Explore",
        fluidRow(
          column(6, div(class = "card", plotOutput("ai_trend", height = "340px"))),
          column(6, div(class = "card", plotOutput("performance_trend", height = "340px")))
        ),
        div(class = "card", plotOutput("relationship", height = "360px")),
        div(class = "note", textOutput("coverage_note"))
      ),
      tabPanel("Company-year data",
        div(class = "card",
          p(class = "note", "Rows are ordered by ticker and filing date. Growth and return columns are percentages. Missing values are shown as NA."),
          downloadButton("download", "Download selected rows"),
          br(), br(),
          div(class = "table-wrap", tableOutput("data_table"))
        )
      ),
      tabPanel("Read the charts",
        div(class = "card",
          h2("Definitions"),
          p("AI intensity counts dictionary matches per 10,000 visible words in the 10-K primary document. The dictionary includes artificial intelligence, machine learning, generative AI, large language model, LLM and the case-sensitive whole word AI."),
          p("Next fiscal-year revenue growth compares the next annual revenue period with the year described by the filing. It is an accounting outcome, not a return or a causal effect."),
          p("12-month return above SPY is the stock's adjusted-price holding-period return minus SPY's return over the same trading dates. Incomplete 12-month windows have no return value."),
          p("The horizontal axis uses the year the 10-K was filed. It is different from the fiscal year the filing describes. The ChatGPT period starts on November 30, 2022; filing-year 2022 therefore contains both periods."),
          h2("How the comparison works"),
          p("The navy line is the average of available filings in the selected industry group, with each filing weighted equally. Selecting a company adds its observations in orange. A gap means no eligible observations were available; it is not a zero."),
          p("The scatterplot shows the selected observations without statistical controls. Refer to the Quarto report for regressions with year and industry fixed effects and standard errors clustered by company."),
          h2("Data sources"),
          p(tags$a(href = "https://www.sec.gov/search-filings/edgar-application-programming-interfaces",
                   target = "_blank", rel = "noopener", "SEC EDGAR submissions and companyfacts"),
            " supply the filings, revenue facts and SIC codes. Prices are retrieved with tidyquant from Yahoo Finance. The README documents the constituent snapshot, data cutoff and exclusions.")
        )
      )
    ),
    div(class = "callout",
      strong("Correlation, not causation. "),
      "Current S&P 500 membership creates survivorship bias. Company size, business mix, investment, acquisitions and reverse causality may explain observed differences. ",
      "The charts use the available observations, so the company mix can change over time."
    )
  )

  server <- function(input, output, session) {
    # Keep company choices consistent with the industry selection.
    industry_data <- reactive({
      if (input$industry == "All industries") panel else
        filter(panel, industry == input$industry)
    })

    observeEvent(input$industry, {
      choices <- c("All companies", sort(unique(industry_data()$ticker)))
      selected <- if (isolate(input$company) %in% choices) isolate(input$company) else "All companies"
      updateSelectInput(session, "company", choices = choices, selected = selected)
    })

    selected_data <- reactive({
      data <- industry_data()
      if (input$company == "All companies") data else filter(data, ticker == input$company)
    })

    outcome_label <- reactive({
      if (input$outcome == "next_revenue_growth") "Next fiscal-year revenue growth" else
        "12-month return above SPY"
    })

    output$scope_label <- renderUI({
      data <- selected_data()
      company_label <- if (input$company == "All companies") "All companies" else
        paste0(input$company, " · ", first(data$company))
      div(class = "scope", strong(company_label), " | ", input$industry,
          " | Grouping by 10-K filing year")
    })

    output$n_companies <- renderText(format(n_distinct(selected_data()$cik), big.mark = ","))
    output$n_filings <- renderText(format(nrow(selected_data()), big.mark = ","))
    output$n_revenue <- renderText(format(sum(is.finite(selected_data()$next_revenue_growth)), big.mark = ","))
    output$n_returns <- renderText(format(sum(is.finite(selected_data()$excess_return_12m)), big.mark = ","))

    # Average only observed values, retaining absent years as gaps.
    output$ai_trend <- renderPlot({
      data <- industry_data()
      validate(need(nrow(data) > 0, "No observations for this selection."))
      trend <- data %>%
        group_by(filing_year) %>%
        summarise(ai_intensity = mean(ai_intensity, na.rm = TRUE), .groups = "drop")
      chart <- ggplot(trend, aes(filing_year, ai_intensity)) +
        geom_line(aes(colour = "Industry average"), linewidth = 0.9) +
        geom_point(aes(colour = "Industry average"), size = 2.5)
      if (input$company != "All companies") {
        chart <- chart +
          geom_line(data = selected_data(), aes(colour = "Selected company"), linewidth = 0.9) +
          geom_point(data = selected_data(), aes(colour = "Selected company"), size = 2.5)
      }
      chart +
        scale_colour_manual(values = c("Industry average" = navy, "Selected company" = orange)) +
        scale_x_continuous(breaks = sort(unique(data$filing_year))) +
        labs(title = "How much do the filings discuss AI?",
             subtitle = "Each filing has equal weight in the industry average",
             x = "10-K filing year", y = "AI mentions per 10,000 words") + app_theme
    })

    # Keep the outcome in its own units instead of using a misleading second axis.
    output$performance_trend <- renderPlot({
      data <- industry_data() %>% mutate(outcome = .data[[input$outcome]])
      validate(need(any(is.finite(data$outcome)), "No complete outcome observations for this selection."))
      trend <- data %>%
        group_by(filing_year) %>%
        summarise(outcome = if (any(is.finite(outcome))) mean(outcome, na.rm = TRUE) else NA_real_,
                  .groups = "drop")
      chart <- ggplot(trend, aes(filing_year, outcome)) +
        geom_hline(yintercept = 0, colour = "#b9c3ce", linewidth = 0.4) +
        geom_line(aes(colour = "Industry average"), linewidth = 0.9, na.rm = TRUE) +
        geom_point(aes(colour = "Industry average"), size = 2.5, na.rm = TRUE)
      if (input$company != "All companies") {
        company <- selected_data() %>% mutate(outcome = .data[[input$outcome]])
        chart <- chart +
          geom_line(data = company, aes(colour = "Selected company"), linewidth = 0.9, na.rm = TRUE) +
          geom_point(data = company, aes(colour = "Selected company"), size = 2.5, na.rm = TRUE)
      }
      chart +
        scale_colour_manual(values = c("Industry average" = navy, "Selected company" = orange)) +
        scale_x_continuous(breaks = sort(unique(data$filing_year))) +
        scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
        labs(title = outcome_label(),
             subtitle = if (input$outcome == "excess_return_12m") "Mean of complete 12-month windows" else "Mean of observed next-year growth; missing values excluded",
             x = "10-K filing year", y = outcome_label()) + app_theme
    })

    output$relationship <- renderPlot({
      data <- selected_data() %>%
        mutate(outcome = .data[[input$outcome]],
               period = if_else(post2022 == 1, "Filed on/after Nov 30, 2022", "Filed before Nov 30, 2022")) %>%
        filter(is.finite(ai_intensity), is.finite(outcome))
      validate(need(nrow(data) > 0, "No complete AI and outcome pairs for this selection."))
      ggplot(data, aes(ai_intensity, outcome, colour = period)) +
        geom_hline(yintercept = 0, colour = "#b9c3ce", linewidth = 0.4) +
        geom_point(alpha = 0.55, size = 2.3) +
        scale_colour_manual(values = c("Filed before Nov 30, 2022" = navy,
                                       "Filed on/after Nov 30, 2022" = orange)) +
        scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
        labs(title = "AI discussion and subsequent performance",
             subtitle = paste(format(nrow(data), big.mark = ","),
                              "observed filing-outcome pairs · unadjusted comparison"),
             x = "AI mentions per 10,000 words", y = outcome_label()) + app_theme
    })

    output$coverage_note <- renderText({
      data <- selected_data()
      paste0("Selection coverage: ", nrow(data), " filings; ",
             sum(!is.finite(data$next_revenue_growth)), " lack an eligible next-year revenue value; ",
             sum(!is.finite(data$excess_return_12m)),
             " lack an eligible 12-month excess return. The two outcomes use separate available samples. ",
             "Source rows are cached when the app starts; restart the app after rebuilding the data.")
    })

    output$data_table <- renderTable({
      selected_data() %>%
        transmute(
          Ticker = ticker, Company = company, Industry = industry,
          `Fiscal year` = fiscal_year, `Period end` = as.character(period_end),
          `Filing date` = as.character(filing_date),
          `AI mentions / 10k words` = round(ai_intensity, 2),
          `Revenue (USD bn)` = round(revenue / 1e9, 2),
          `Next-year growth (%)` = round(next_revenue_growth * 100, 2),
          `12m above SPY (pp)` = round(excess_return_12m * 100, 2),
          `Return status` = return_status
        )
    }, striped = TRUE, hover = TRUE, spacing = "s", na = "NA")

    output$download <- downloadHandler(
      filename = function() paste0("ai-talk-", gsub("[^A-Za-z0-9-]", "-", input$company), ".csv"),
      content = function(file) write_csv(selected_data(), file, na = "NA")
    )
  }
}

shinyApp(ui, server)
