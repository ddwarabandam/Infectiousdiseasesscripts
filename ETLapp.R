# app.R
# ----------------------------------------------------------------------------
# A Shiny app that accepts user-uploaded Measles-like data and runs
# the ETL, descriptive analysis, epidemic curves, R(t), and projection steps,
# with a downloadable report.
# ----------------------------------------------------------------------------

library(shiny)
library(shinythemes)
library(dplyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(EpiEstim)
library(incidence)
library(projections)
library(distcrete)
library(epitrix)
library(tidyr)
library(knitr)
library(kableExtra)
library(readxl)

# We will use rmarkdown::render for downloadable reports
library(rmarkdown)

# ----------------------------------------------------------------------------
# UI
# ----------------------------------------------------------------------------
ui <- fluidPage(
  
  theme = shinytheme("lumen"),
  titlePanel("Measles ETL-Descriptive Analysis, Modeling & Projections"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Upload CSV or Excel file",
                accept = c(".csv", ".xlsx", ".xls")),
      br(),
      p("Acceptable file types: CSV or Excel."),
      p("Data is cleared upon app closure or refresh."),
      
     # hr(),
      
      # Download report
     # radioButtons("report_format", "Download report format:",
     #              choices = c("HTML" = "html", "PDF" = "pdf"),
     #              inline = TRUE),
     # downloadButton("download_report", "Download Report")
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Overview", 
                           h4("Data Preview"),
                           tableOutput("data_head")
                  ),
                  tabPanel("Frequencies",
                           h4("Summary Frequencies"),
                           tableOutput("summary_freq"),
                           h4("Vaccination x Age Group"),
                           tableOutput("cross_tab")
                  ),
                  tabPanel("Epidemic Curve",
                           plotlyOutput("epi_curve")
                  ),
                  tabPanel("Gantt Chart",
                           plotlyOutput("gantt_plot")
                  ),
                  tabPanel("R(t) Estimation",
                           plotlyOutput("rt_plot")
                  ),
                  tabPanel("Projection",
                           plotlyOutput("projection_plot"),
                           plotlyOutput("scenario_plot")
                  )
      )
    )
  )
)

# ----------------------------------------------------------------------------
# Server
# ----------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Reactive expression to read the user's data
  user_data <- reactive({
    req(input$file1)  # Only proceed if a file is uploaded
    
    ext <- tools::file_ext(input$file1$name)
    
    # Try reading data:
    df <- tryCatch({
      if (ext == "csv") {
        read.csv(input$file1$datapath, 
                 stringsAsFactors = FALSE, na.strings = c("", "NA"))
      } else if (ext %in% c("xlsx","xls")) {
        read_excel(input$file1$datapath)
      } else {
        validate("Please upload a .csv or .xlsx file")
      }
    }, error = function(e) {
      # If there's an error reading the file, return NULL or show a message
      validate(paste("Error reading file:", e$message))
      return(NULL)
    })
    
    # Convert any blank strings to NA for easier handling
    df[df == ""] <- NA
    
    df
  })
  
  
  # Reactive expression that does the ETL / cleaning
  measles_data <- reactive({
    # If user hasn't uploaded a file, don't proceed
    req(user_data())
    
    df <- user_data()
    
    # Example: Turn all character NAs/blanks -> "Unknown"
    # But only do so for actual character columns
    char_cols <- names(df)[sapply(df, is.character)]
    for (cc in char_cols) {
      df[[cc]][is.na(df[[cc]])] <- "Unknown"
    }
    
    # If 'age' exists, ensure numeric
    if ("age" %in% names(df)) {
      df$age <- suppressWarnings(as.numeric(df$age))
    }
    
    # Convert known date columns to Date if they exist
    date_vars <- c("onset_date","rash_onset_date","fever_onset_date")
    for (dv in date_vars) {
      if (dv %in% names(df)) {
        df[[dv]] <- suppressWarnings(as.Date(df[[dv]]))
      }
    }
    
    # Create an age_group if age is present
    if ("age" %in% names(df)) {
      df <- df %>%
        mutate(age_group = cut(
          age,
          breaks = c(0, 5, 9, 14, 19, 64, Inf),
          labels = c("0-5", "6-9", "10-14", "15-19", "20-64", "65+"),
          include.lowest = TRUE, right = TRUE
        ))
    }
    
    df
  })
  
  # ----------------------------------------------------------------------------
  # Show data preview
  # ----------------------------------------------------------------------------
  output$data_head <- renderTable({
    df <- measles_data()
    head(df, 10)
  })
  
  
  # ----------------------------------------------------------------------------
  # Summary Frequencies (tableOutput)
  # ----------------------------------------------------------------------------
  output$summary_freq <- renderTable({
    df <- measles_data()
    
    # Arbitrary columns you want frequencies for:
    summary_cols <- c("vacc_status", "final_status", "sex_at_birth",
                      "age_group", "race", "hispanic_latino",
                      "city", "county", "state", "residence_country",
                      "outbreak_related", "recent_travel")
    
    # Build the summary frequency data frame
    summary_freq_df <- data.frame(
      Variable = character(),
      Category = character(),
      Count = numeric(),
      stringsAsFactors = FALSE
    )
    
    for(col_name in summary_cols) {
      if(!col_name %in% names(df)) next  # skip if missing
      
      freq_tbl <- table(df[[col_name]], useNA = "ifany")
      
      tmp <- data.frame(
        Variable = col_name,
        Category = names(freq_tbl),
        Count    = as.numeric(freq_tbl),
        stringsAsFactors = FALSE
      )
      summary_freq_df <- rbind(summary_freq_df, tmp)
    }
    
    if(nrow(summary_freq_df) == 0){
      return(data.frame(Message = "No summary columns found in the dataset."))
    }
    
    # Return a simple data.frame for the table
    summary_freq_df
  })
  
  # ----------------------------------------------------------------------------
  # Vax status x Age Group Cross-tab
  # ----------------------------------------------------------------------------
  output$cross_tab <- renderTable({
    df <- measles_data()
    
    if (!all(c("vacc_status","age_group") %in% names(df))) {
      return(data.frame(Message = "No vacc_status x age_group cross-tab, columns not found."))
    }
    
    tab <- table(df$vacc_status, df$age_group, useNA="ifany")
    as.data.frame(tab)
  })
  
  
  # ----------------------------------------------------------------------------
  # Epidemic Curve (Plotly)
  # ----------------------------------------------------------------------------
  output$epi_curve <- renderPlotly({
    df <- measles_data()
    # If no onset_date, skip
    if(!("onset_date" %in% names(df))) {
      return(plotly::plot_ly() %>% 
               plotly::add_text(x=0, y=0, text="No 'onset_date' column available."))
    }
    
    df_epicurve <- df %>% filter(!is.na(onset_date))
    if(nrow(df_epicurve) == 0){
      return(plotly::plot_ly() %>% 
               plotly::add_text(x=0, y=0, text="No valid onset_date values."))
    }
    
    daily_counts <- df_epicurve %>%
      group_by(onset_date) %>%
      summarise(incidence = n(), .groups = "drop") %>%
      arrange(onset_date)
    
    p_epi <- ggplot(daily_counts, aes(x=onset_date, y=incidence)) +
      geom_col(alpha=0.7) +
      labs(
        title = "Epidemic Curve by Onset Date",
        x = "Onset Date",
        y = "Number of Cases"
      ) +
      theme_minimal()
    
    ggplotly(p_epi)
  })
  
  
  # ----------------------------------------------------------------------------
  # Gantt Chart (Plotly)
  # ----------------------------------------------------------------------------
  output$gantt_plot <- renderPlotly({
    df <- measles_data()
    if(!("rash_onset_date" %in% names(df))) {
      return(plotly::plot_ly() %>% 
               plotly::add_text(x=0, y=0, text="No 'rash_onset_date' column available."))
    }
    
    df_gantt <- df %>% 
      filter(!is.na(rash_onset_date)) %>%
      mutate(
        contagious_start = rash_onset_date - 4,
        contagious_end   = rash_onset_date + 4
      )
    
    if(nrow(df_gantt) == 0){
      return(plotly::plot_ly() %>%
               plotly::add_text(x=0, y=0, text="No valid rash_onset_date for Gantt chart."))
    }
    
    df_gantt <- df_gantt %>% arrange(contagious_start)
    
    p_gantt <- ggplot(df_gantt, aes(y=case_id)) +
      geom_segment(aes(x=contagious_start, xend=contagious_end,
                       y=case_id, yend=case_id),
                   linewidth=4, alpha=0.7) +
      labs(
        title = "Contagious Period (±4 days from Rash Onset)",
        x = "Date",
        y = "Case ID"
      ) +
      theme_minimal()
    
    ggplotly(p_gantt)
  })
  
  
  # ----------------------------------------------------------------------------
  # Time-varying R(t) (Plotly)
  # ----------------------------------------------------------------------------
  output$rt_plot <- renderPlotly({
    df <- measles_data()
    if(!("onset_date" %in% names(df))) {
      return(plotly::plot_ly() %>% 
               plotly::add_text(x=0, y=0, text="No 'onset_date' column available."))
    }
    
    df_rt <- df %>%
      filter(!is.na(onset_date)) %>%
      group_by(onset_date) %>%
      summarise(incidence = n(), .groups="drop") %>%
      arrange(onset_date)
    
    if(nrow(df_rt) < 8){
      return(plotly::plot_ly() %>%
               plotly::add_text(x=0, y=0, text="Not enough data to estimate R(t). Need >= 8 days."))
    }
    
    I <- df_rt$incidence
    t_start <- seq(2, length(I) - 7)
    t_end   <- t_start + 7
    
    mean_si <- 12
    std_si  <- 3
    
    res_R <- estimate_R(
      incid = I,
      method = "parametric_si",
      config = make_config(
        mean_si = mean_si,
        std_si  = std_si,
        t_start = t_start,
        t_end   = t_end
      )
    )
    
    # Build data frame for plotting
    df_R <- data.frame(
      t_start = res_R$R$t_start,
      t_end   = res_R$R$t_end,
      meanR   = res_R$R$`Mean(R)`,
      lower   = res_R$R$`Quantile.0.025(R)`,
      upper   = res_R$R$`Quantile.0.975(R)`
    )
    
    min_date <- min(df_rt$onset_date)
    date_seq <- seq(from = min_date, by = "days", length.out = length(I))
    df_R$mid_date <- date_seq[df_R$t_end]
    
    p_R <- ggplot(df_R, aes(x=mid_date, y=meanR)) +
      geom_line() +
      geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.2) +
      geom_hline(yintercept=1, linetype="dashed") +
      labs(
        title="Time-varying R(t)",
        x="Date",
        y="R(t)"
      ) +
      theme_minimal()
    
    ggplotly(p_R)
  })
  
  
  # ----------------------------------------------------------------------------
  # Projection (Plotly)
  # ----------------------------------------------------------------------------
  output$projection_plot <- renderPlotly({
    df <- measles_data()
    
    if(!("onset_date" %in% names(df))) {
      return(plotly::plot_ly() %>%
               plotly::add_text(x=0, y=0, text="No 'onset_date' column available."))
    }
    
    df_rt <- df %>%
      filter(!is.na(onset_date)) %>%
      group_by(onset_date) %>%
      summarise(incidence = n(), .groups="drop") %>%
      arrange(onset_date)
    
    if(nrow(df_rt) < 1){
      return(plotly::plot_ly() %>%
               plotly::add_text(x=0, y=0, text="Not enough data to run projections."))
    }
    
    # Build incidence object
    incid_obj <- incidence::incidence(
      dates = rep(df_rt$onset_date, df_rt$incidence),
      interval = 1
    )
    
    # Distcrete for generation time (Gamma(12,3))
    gen_time <- distcrete(
      "gamma",
      shape = (12^2)/(3^2),
      rate  = 12/(3^2),
      w=0, interval=1
    )
    
    # Single scenario
    proj_res <- project(
      x = incid_obj,
      R = 1.2,
      si = gen_time,
      n_sim = 100,
      n_days = 14
    )
    
    summ_proj <- summary(proj_res)
    
    p_proj <- plot(proj_res) +
      labs(
        title = "Measles 14-Day Projection (R=1.2)",
        x = "Date",
        y = "Predicted Incidence"
      )
    
    ggplotly(p_proj)
  })
  
  # ----------------------------------------------------------------------------
  # Multiple R scenario projection
  # ----------------------------------------------------------------------------
  output$scenario_plot <- renderPlotly({
    df <- measles_data()
    
    if(!("onset_date" %in% names(df))) {
      return(plotly::plot_ly() %>%
               plotly::add_text(x=0, y=0, text="No 'onset_date' column available."))
    }
    
    df_rt <- df %>%
      filter(!is.na(onset_date)) %>%
      group_by(onset_date) %>%
      summarise(incidence = n(), .groups="drop") %>%
      arrange(onset_date)
    
    if(nrow(df_rt) < 1){
      return(plotly::plot_ly() %>%
               plotly::add_text(x=0, y=0, text="Not enough data to run projections."))
    }
    
    incid_obj <- incidence::incidence(
      dates = rep(df_rt$onset_date, df_rt$incidence),
      interval = 1
    )
    
    gen_time <- distcrete(
      "gamma",
      shape = (12^2)/(3^2),
      rate  = 12/(3^2),
      w=0, interval=1
    )
    
    R_scenarios <- c(1.0, 1.2, 1.4)
    scenario_out <- list()
    
    for(rval in R_scenarios){
      tmp_proj <- project(incid_obj, R=rval, si=gen_time, n_sim=100, n_days=14)
      scenario_out[[as.character(rval)]] <- tmp_proj
    }
    
    scenario_summ <- list()
    for(rval in names(scenario_out)){
      tmp <- summary(scenario_out[[rval]])
      tmp$scenario <- rval
      scenario_summ[[rval]] <- tmp
    }
    all_scen <- do.call(rbind, scenario_summ)
    
    p_scen <- ggplot(all_scen, aes(x=dates, group=scenario)) +
      geom_ribbon(aes(ymin=min, ymax=max, fill=scenario), alpha=0.2) +
      geom_line(aes(y=`quantiles.50%`, color=scenario), size=1) +
      labs(
        title="Measles 14-Day Projection - Multiple Scenarios",
        x="Date",
        y="Predicted Incidence"
      ) +
      theme_minimal()
    
    ggplotly(p_scen)
  })
  
  session$onSessionEnded(function() { # By default, this environment and reactive values vanish, # so data is gone. We do not store anything externally.
    }
  ) 
  
}
    
shinyApp(ui, server)
    
    
