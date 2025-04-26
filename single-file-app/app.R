# ---- Load Required Packages ----
library(shiny)
library(tidyverse)
library(shinydashboard)
library(lubridate)
library(janitor)
library(shinyWidgets)

# ---- Load Datasets ----
load_water_data <- function() {
  actual_shortage <- read_csv("data/actual_water_shortage_level.csv") |> 
    clean_names() |> 
    mutate(start_date = ymd(start_date))
  
  five_year_outlook <- read_csv("data/five_year_water_shortage_outlook.csv") |> clean_names()
  historical_production <- read_csv("data/historical_production_delivery.csv") |> clean_names()
  monthly_water_outlook <- read_csv("data/monthly_water_shortage_outlook.csv") |> clean_names()
  source_name <- read_csv("data/source_name.csv") |> clean_names()
  
  list(
    actual_shortage = actual_shortage,
    five_year_outlook = five_year_outlook,
    historical_production = historical_production,
    monthly_water_outlook = monthly_water_outlook,
    source_name = source_name
  )
}

water_data <- load_water_data()

# ---- UI ----
ui <- fluidPage(
  
  tags$head(tags$style(HTML("
    .value-box-custom .small-box {
      border: 2px solid #333 !important;
      border-radius: 8px;
      padding: 10px;
      margin: 0px !important;
      width: 100%;
      height: 100%;
      box-sizing: border-box;
    }
    .value-box-custom { padding: 10px; }
  "))),
  
  fluidRow(
    column(4, selectInput("dataset_selector", "Select Dataset", choices = names(water_data))),
    column(4, selectInput("org_id", "Select Org ID", choices = NULL)),
    column(4,
           airDatepickerInput("date_picker_start", "Start Date", value = NULL, view = "months", minView = "months", dateFormat = "yyyy-MM"),
           airDatepickerInput("date_picker_end", "End Date", value = NULL, view = "months", minView = "months", dateFormat = "yyyy-MM")
    )
  ),
  
  br(),
  
  # ---- CONDITIONAL PANELS ----
  conditionalPanel(
    condition = "input.dataset_selector == 'five_year_outlook'",
    fluidRow(
      column(6, div(class = "value-box-custom", valueBoxOutput("fiveyr_use"))),
      column(6, div(class = "value-box-custom", valueBoxOutput("fiveyr_supply")))
    ),
    fluidRow(
      column(6, div(class = "value-box-custom", valueBoxOutput("fiveyr_aug"))),
      column(6, div(class = "value-box-custom", valueBoxOutput("fiveyr_red")))
    )
  ),
  
  conditionalPanel(
    condition = "input.dataset_selector == 'monthly_water_outlook'",
    fluidRow(
      column(6, div(class = "value-box-custom", valueBoxOutput("monthly_shortage_value"))),
      column(6, div(class = "value-box-custom", valueBoxOutput("monthly_shortage_months")))
    ),
    fluidRow(
      column(6, div(class = "value-box-custom", valueBoxOutput("monthly_aug_value"))),
      column(6, div(class = "value-box-custom", valueBoxOutput("monthly_aug_months")))
    ),
    fluidRow(
      column(6, div(class = "value-box-custom", valueBoxOutput("monthly_red_value"))),
      column(6, div(class = "value-box-custom", valueBoxOutput("monthly_red_months")))
    )
  ),
  
  conditionalPanel(
    condition = "input.dataset_selector == 'actual_shortage'",
    fluidRow(column(12, div(class = "value-box-custom", valueBoxOutput("average_shortage")))),
    fluidRow(
      lapply(0:6, function(i) column(2, div(class = "value-box-custom", valueBoxOutput(paste0("shortage_level_", i)))))
    )
  ),
  
  conditionalPanel(
    condition = "input.dataset_selector == 'historical_production'",
    fluidRow(
      column(12,
             selectInput("type", "Select Water Type(s)", 
                         choices = c("agriculture", "surface water", "industrial", "other", 
                                     "single-family residential", "commercial/institutional",
                                     "landscape irrigation", "multi-family residential", 
                                     "other pws", "recycled", "sold to another pws", "groundwater wells",
                                     "non-potable (total excluded recycled)", 
                                     "purchased or received from another pws",
                                     "non-potable water sold to another pws"),
                         multiple = TRUE, width = "100%")
      )
    ),
    fluidRow(
      uiOutput("hist_value_boxes")
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  
  # ---- Date Range Helper ----
  date_range <- reactive({
    req(input$dataset_selector)
    df <- water_data[[input$dataset_selector]]
    
    if ("start_date" %in% names(df)) {
      dates <- as.Date(df$start_date)
    } else if ("forecast_start_date" %in% names(df)) {
      dates <- as.Date(df$forecast_start_date)
    } else {
      dates <- NULL
    }
    
    list(
      minDate = min(dates, na.rm = TRUE),
      maxDate = max(dates, na.rm = TRUE)
    )
  })
  
  # ---- Update Inputs when Dataset Changes ----
  observeEvent(input$dataset_selector, {
    df <- water_data[[input$dataset_selector]]
    orgs <- sort(unique(df$org_id))
    updateSelectInput(session, "org_id", choices = orgs, selected = orgs[1])
    
    dr <- date_range()
    updateAirDateInput(session, "date_picker_start",
                       value = dr$minDate,
                       options = list(minDate = dr$minDate, maxDate = dr$maxDate))
    updateAirDateInput(session, "date_picker_end",
                       value = dr$maxDate,
                       options = list(minDate = dr$minDate, maxDate = dr$maxDate))
  })
  
  observeEvent(input$date_picker_start, {
    dr <- date_range()
    start <- as.Date(input$date_picker_start)
    updateAirDateInput(session, "date_picker_end",
                       options = list(minDate = start, maxDate = dr$maxDate))
  })
  
  # ---- Helper Filter Functions ----
  actual_filter <- function(id, date){
    start_month <- format(as.Date(date[1]), "%Y-%m")
    end_month   <- format(as.Date(date[2]), "%Y-%m")
    
    water_data$actual_shortage %>%
      filter(org_id == id) %>%
      mutate(year_month = format(start_date, "%Y-%m")) %>%
      filter(year_month >= start_month, year_month <= end_month)
  }
  
  monthly_filter <- function(id, date){
    start_month <- format(as.Date(date[1]), "%Y-%m")
    end_month   <- format(as.Date(date[2]), "%Y-%m")
    
    water_data$monthly_water_outlook %>%
      filter(org_id == id) %>%
      mutate(year_month = format(forecast_start_date, "%Y-%m")) %>%
      filter(year_month >= start_month, year_month <= end_month)
  }
  
  # ---- Five-Year Outlook Value Boxes ----
  five_filter_function <- function(id, year_range) {
    water_data$five_year_outlook %>%
      filter(org_id == id) %>%
      mutate(forecast_year = year(forecast_start_date)) %>%
      filter(forecast_year >= year_range[1], forecast_year <= year_range[2]) %>%
      pivot_longer(cols = c(starts_with("water"), starts_with("benefit")),
                   names_to = "use_supply_aug_red",
                   values_to = "acre_feet") %>%
      mutate(use_supply_aug_red = factor(
        use_supply_aug_red,
        levels = c("water_supplies_acre_feet", "water_use_acre_feet",
                   "benefit_supply_augmentation_acre_feet", "benefit_demand_reduction_acre_feet"),
        labels = c("Supply", "Use", "Supply Augmentation", "Demand Reduction")
      ))
  }
  
  five_values_function <- function(id, year_range) {
    five_filter_function(id, year_range) %>%
      group_by(use_supply_aug_red) %>%
      summarize(total_value = sum(acre_feet, na.rm = TRUE), .groups = 'drop')
  }
  
  render_five_value_box <- function(label) {
    renderValueBox({
      req(input$org_id, input$date_picker_start, input$date_picker_end)
      start_year <- as.numeric(format(as.Date(input$date_picker_start), "%Y"))
      end_year   <- as.numeric(format(as.Date(input$date_picker_end), "%Y"))
      
      val <- five_values_function(input$org_id, c(start_year, end_year)) %>%
        filter(use_supply_aug_red == label) %>%
        pull(total_value)
      
      valueBox(
        value = ifelse(length(val) > 0, scales::comma(round(val)), "0"),
        subtitle = paste(label, "(Acre Feet)"),
        color = switch(label,
                       "Use" = "purple",
                       "Supply" = "blue",
                       "Supply Augmentation" = "green",
                       "Demand Reduction" = "red")
      )
    })
  }
  
  output$fiveyr_use    <- render_five_value_box("Use")
  output$fiveyr_supply <- render_five_value_box("Supply")
  output$fiveyr_aug    <- render_five_value_box("Supply Augmentation")
  output$fiveyr_red    <- render_five_value_box("Demand Reduction")
  
  # ---- Monthly Water Outlook Value Boxes ----
  # ---- Monthly Water Value Boxes ----
  monthly_values_function <- function(id, date){
    monthly_filter(id, date) %>%
      pivot_longer(cols = c(shortage_surplus_acre_feet, starts_with("benefit")),
                   names_to = "use_supply_aug_red",
                   values_to = "acre_feet") %>%
      filter(is_annual == FALSE, acre_feet != 0) %>%
      group_by(use_supply_aug_red) %>%
      summarize(total_acre_feet = sum(acre_feet, na.rm = TRUE), .groups = "drop")
  }
  
  monthly_months_function <- function(id, date){
    monthly_filter(id, date) %>%
      pivot_longer(cols = c(shortage_surplus_acre_feet, starts_with("benefit")),
                   names_to = "use_supply_aug_red",
                   values_to = "acre_feet") %>%
      filter(is_annual == FALSE, acre_feet != 0) %>%
      group_by(use_supply_aug_red) %>%
      summarize(num_months = n(), .groups = "drop")
  }
  
  # ---- Actual Shortage Value Boxes ----
  output$average_shortage <- renderValueBox({
    req(input$org_id, input$date_picker_start, input$date_picker_end)
    avg <- actual_filter(input$org_id, c(input$date_picker_start, input$date_picker_end)) %>%
      summarize(avg_level = mean(state_standard_shortage_level, na.rm = TRUE)) %>%
      pull(avg_level)
    valueBox(value = ifelse(is.na(avg), 0, round(avg, 2)),
             subtitle = "Average Shortage Level", color = "red")
  })
  
  for (i in 0:6) {
    local({
      level <- i
      output[[paste0("shortage_level_", level)]] <- renderValueBox({
        req(input$org_id, input$date_picker_start, input$date_picker_end)
        count <- actual_filter(input$org_id, c(input$date_picker_start, input$date_picker_end)) %>%
          filter(state_standard_shortage_level == level) %>%
          nrow()
        valueBox(value = count,
                 subtitle = paste("Shortage Level", level),
                 color = "orange")
      })
    })
  }
  
  # ---- Monthly Water Value Boxes continued ----
  render_monthly_value <- function(label, color){
    renderValueBox({
      req(input$org_id, input$date_picker_start, input$date_picker_end)
      value <- monthly_values_function(input$org_id, c(input$date_picker_start, input$date_picker_end)) %>%
        filter(use_supply_aug_red == label) %>%
        pull(total_acre_feet)
      valueBox(value = ifelse(length(value) > 0, scales::comma(value), "0"),
               subtitle = paste(label, "(Acre Feet)"),
               color = color)
    })
  }
  
  render_monthly_months <- function(label, color){
    renderValueBox({
      req(input$org_id, input$date_picker_start, input$date_picker_end)
      months <- monthly_months_function(input$org_id, c(input$date_picker_start, input$date_picker_end)) %>%
        filter(use_supply_aug_red == label) %>%
        pull(num_months)
      valueBox(value = ifelse(length(months) > 0, months, "0"),
               subtitle = paste(label, "(Months)"),
               color = color)
    })
  }
  
  output$monthly_shortage_value <- render_monthly_value("shortage_surplus_acre_feet", "blue")
  output$monthly_shortage_months <- render_monthly_months("shortage_surplus_acre_feet", "teal")
  
  output$monthly_aug_value <- render_monthly_value("benefit_supply_augmentation_acre_feet", "blue")
  output$monthly_aug_months <- render_monthly_months("benefit_supply_augmentation_acre_feet", "teal")
  
  output$monthly_red_value <- render_monthly_value("benefit_demand_reduction_acre_feet", "blue")
  output$monthly_red_months <- render_monthly_months("benefit_demand_reduction_acre_feet", "teal")
  
  
  
  # ---- Historical Production Value Boxes ----
  hist_filt_function <- function(id, date){
    if (length(date) == 2) {
      date <- seq(
        from = ymd(paste0(date[1], "-01")),
        to   = ymd(paste0(date[2], "-01")),
        by   = "1 month"
      ) %>% format("%Y-%m")
    }
    water_data$historical_production %>%
      filter(org_id == id) %>%
      mutate(year_month = format(start_date, "%Y-%m")) %>%
      filter(year_month %in% date)
  }
  
  hist_values_specific <- function(id, date, type){
    hist_filt_function(id, date) %>%
      filter(water_type != "total") %>%
      group_by(water_produced_or_delivered, water_type) %>%
      summarize(total_value = sum(quantity_acre_feet, na.rm = TRUE)) %>%
      filter(water_type %in% type)
  }
  
  output$hist_value_boxes <- renderUI({
    req(input$org_id, input$date_picker_start, input$date_picker_end)
    
    selected_dates <- c(
      format(input$date_picker_start, "%Y-%m"),
      format(input$date_picker_end, "%Y-%m")
    )
    
    if (!is.null(input$type) && length(input$type) > 0) {
      
      values_df <- hist_values_specific(input$org_id, selected_dates, input$type)
      
      value_boxes <- lapply(seq_len(nrow(values_df)), function(i) {
        valueBox(
          value = scales::comma(values_df$total_value[i]),
          subtitle = paste(values_df$water_type[i], "-", values_df$water_produced_or_delivered[i]),
          icon = icon("tint"),
          width = 3
        )
      })
      
      fluidRow(value_boxes)
      
    } else {
      NULL
    }
  })
  
}

# ---- Launch App ----
shinyApp(ui = ui, server = server)
