# load packages
library(tidyverse)

# load data
five_year_water <- read_csv("single-file-app/data/five_year_water_shortage_outlook.csv")

# create value for value boxes

#Function for 5 Year Outlook Filtering
five_filter_function <- function(id, year) {
  
  # If year has two values, assume it's a range
  if (length(year) == 2) {
    year <- seq(from = as.numeric(year[1]), to = as.numeric(year[2]), by = 1)
  }
  
  # Create filtered df from original
  five_year_filter <- water_data$five_year_outlook %>% 
    
    # Filter data to specific org_id
    filter(org_id == id) %>%
    
    mutate(forecast_year = year(forecast_start_date)) %>% 
    
    filter(forecast_year %in% year) %>% 
    
    # Combining use, supply, augmentation, reduction into one columns
    pivot_longer(cols = c(starts_with("water"), starts_with("benefit")),
                 names_to = "use_supply_aug_red",
                 values_to = "acre_feet")
  
  # Renaming & reordering the observations for the plot & table outputs
  five_year_filter$use_supply_aug_red <- factor(five_year_filter$use_supply_aug_red,
                                                levels = c("water_supplies_acre_feet", # reordering observations
                                                           "water_use_acre_feet",
                                                           "benefit_supply_augmentation_acre_feet",
                                                           "benefit_demand_reduction_acre_feet"),
                                                labels = c("Supply", "Use",  # renaming observations
                                                           "Supply Augmentation", "Demand Reduction")) 
  
  return(five_year_filter)
}

# Function for 5 Year Outlook Value calculation
five_values_function <- function(id, year){
  
  # Use filtered data
  five_values <- five_filter_function(id, year)
  
  five_values %>% 
    
    # Group metrics we are interested in 
    group_by(use_supply_aug_red) %>% 
    
    # Summarize the total acre_feet 
    summarize(total_value = sum(acre_feet)) |> 
    
    filter(use_supply_aug_red = "use")
    
  return(use_supply_aug_red$total_value)
  
  
}




##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                  Historical Production Filtering Function                ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


hist_filt_function <- function(id, date){
  
  # If date argument has 2 values, assume it's a start/end range and expand it
  if (length(date) == 2) {
    date <- seq(
      from = lubridate::ymd(paste0(date[1], "-01")), # converts into a Date type
      
      to   = lubridate::ymd(paste0(date[2], "-01")), # converts into a Date type
      
      by   = "1 month" # sequences by month
      
    ) %>% format("%Y-%m") # formats back into Year and month, same as input values
  }
  
  # Start of historical production filtering
  hist_filter <- water_data$historical_production %>% 
    
    # filter to org_id
    filter(org_id == id) %>% 
    
    # Create new forecast year column
    mutate(year_month = format(start_date, "%Y-%m"))  
  
  hist_total <- hist_filter %>% 
    # Group by date 
    group_by(start_date) %>% 
    
    # Append a new row for each start_date & produced_delivered calculating the "total" 
    bind_rows(
      hist_filter %>% 
        group_by(start_date, water_produced_or_delivered) %>% 
        
        # Summarize is producing a new total row by the group by above 
        summarize(water_type = "total",
                  quantity_acre_feet = sum(quantity_acre_feet, na.rm = TRUE),
                  
                  # Using unique returns the "original" observation of these columns,
                  # These are not being changed 
                  start_date = unique(start_date),
                  end_date = unique(end_date),
                  pwsid = unique(pwsid),
                  water_system_name = unique(water_system_name),
                  org_id = unique(org_id),
                  year_month = unique(year_month),
                  .groups = "drop_last")
    ) %>% 
    ungroup() %>% 
    # Filter for a given year range
    filter(year_month %in% date)
} 

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##   Historical Production Total Delivered/Produced Calculation Function    ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

hist_values_function <- function(id, date){
  # Calculating total values for "total produced/delivered"
  hist_values <- hist_filt_function(id, date) %>% 
    
    # Filter out "total" as its not a part of the original data
    filter(water_type != "total") %>% 
    group_by(water_produced_or_delivered) %>% 
    
    # Summing quantity acre-feet for total 
    summarize(total_value = sum(quantity_acre_feet, na.rm = TRUE))
  
  return(hist_values)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##        Historical Production Specific Water Type Value Calculation       ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

hist_values_specific <- function(id, date, type){
  
  # Calculating values for specific water types chosen above 
  hist_values_specific <- hist_filt_function(id, date) %>% 
    
    # Filter out "total" as it's not a part of the original data
    filter(water_type != "total") %>% 
    group_by(water_produced_or_delivered, water_type) %>% 
    
    # Calculating totals and then filtered to chosen water types 
    summarize(total_value = sum(quantity_acre_feet, na.rm = TRUE)) %>% 
    filter(water_type %in% type)
  
  return(hist_values_specific)
}



##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                     Actual Shortage Filtering Function                   ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

actual_filter_function <- function(id, date){
  
  # If date arguemnt has 2 values, assume it's a start/end range and expand it
  if (length(date) == 2) {
    date <- seq(
      from = lubridate::ymd(paste0(date[1], "-01")), # Convert to Date type
      
      to   = lubridate::ymd(paste0(date[2], "-01")), # Convert to Date type
      
      by   = "1 month" # Sequence by month
      
    ) %>% format("%Y-%m") # Format back to Year and month, same as original input
  }
  
  
  # Filter water shortage to Goleta 
  actual_filter <- water_data$actual_shortage %>% 
    filter(org_id == id) %>% 
    
    mutate(year_month = format(start_date, "%Y-%m")) %>% 
    
    filter(year_month %in% date)
  
  return(actual_filter)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                      Actual Water Values Calculation                     ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

actual_values_function <- function(id, date){
  
  # Calculating total values
  actual_values <- actual_filter_function(id, date) %>% 
    summarize(average = mean(state_standard_shortage_level, na.rm = TRUE))
  
  return(actual_values)
}

render_value_box_actual <- function(label) {
  renderValueBox({
    req(input$org_id, input$year_range)
    val <- actual_values_function(input$org_id, input$year_range) |> 
      filter(actual_values == label) |> 
      pull(average)
    
    valueBox(
      value = ifelse(length(val) > 0, scales::comma(round(val, 0)), "0"),
      subtitle = paste(label) |> 
        pull(average),
      icon = NULL,
      color = "purple"
    )
})
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                      Five Year Water Values Calculation                     ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

five_values_function <- function(id, year_range) {
  five_filter_function(id, year_range) %>%
    group_by(use_supply_aug_red) %>%
    summarize(total_value = sum(acre_feet, na.rm = TRUE), .groups = 'drop')
}

render_value_box <- function(label) {
  renderValueBox({
    req(input$org_id, input$year_range)
    val <- five_values_function(input$org_id, input$year_range) %>%
      filter(use_supply_aug_red == label) %>%
      pull(total_value)
    
    valueBox(
      value = ifelse(length(val) > 0, scales::comma(round(val, 0)), "0"),
      subtitle = paste(label, "(Acre Feet)"),
      icon = NULL,
      color = switch(label,
                     "Use" = "purple",
                     "Supply" = "blue",
                     "Supply Augmentation" = "green",
                     "Demand Reduction" = "red")
    )
  })
}

output$fiveyr_value_box_use    <- render_value_box("Use")
output$fiveyr_value_box_supply <- render_value_box("Supply")
output$fiveyr_value_box_aug    <- render_value_box("Supply Augmentation")
output$fiveyr_value_box_red    <- render_value_box("Demand Reduction")


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                      Monthly Water Value Calculation                     ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

monthly_values_function <- function(id, date){
  
  # Calculate total values 
  monthly_values <- monthly_filter(id, date) %>% 
    
    # Pivot columns for easier computation 
    pivot_longer(cols = c(shortage_surplus_acre_feet, starts_with("benefit")),
                 names_to = "use_supply_aug_red",
                 values_to = "acre_feet") %>% 
    
    # Group by new column and summarize total acre-feet
    group_by(use_supply_aug_red) %>% 
    summarize(total = sum(acre_feet, na.rm = TRUE))
  
  return(monthly_values)
}


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                      Monthly Water Filtering Function                    ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

monthly_filter <- function(id, date){
  
  # If date arguemnt has 2 values, assume it's a start/end range and expand it
  if (length(date) == 2) {
    date <- seq(
      from = lubridate::ymd(paste0(date[1], "-01")), # Converts into Date type
      
      to   = lubridate::ymd(paste0(date[2], "-01")), # Converts into Date type
      
      by   = "1 month" # sequences by month 
    ) %>% format("%Y-%m") # Reformats back into year and month, same as input values
  }
  
  # Filter to org_id
  monthly_filter <- water_data$monthly_water_outlook %>% 
    filter(org_id == id) %>% 
    
    # Create new forecast year column
    mutate(year_month = format(forecast_start_date, "%Y-%m")) %>% 
    
    filter(year_month %in% date)
  
  return(monthly_filter)
}

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                 Monthly Water Number of Months Calculation               ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

monthly_months_function <- function(id, date){
  
  monthly_months <- monthly_filter(id, date) %>% 
    
    # Pivot columns for easier computation
    pivot_longer(cols = c(shortage_surplus_acre_feet, starts_with("benefit")),
                 names_to = "use_supply_aug_red",
                 values_to = "acre_feet") %>% 
    
    # Remove total annual observations
    filter(is_annual == FALSE,
           
           # Filter where observations not = 0 
           acre_feet != 0) %>% 
    
    # Group by new column and summarize total acre-feet
    group_by(use_supply_aug_red) %>% 
    summarize(num_months = n())
  
  return(monthly_months)
}






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
  )
)

# ---- Server ----
server <- function(input, output, session) {
  
  # --- Date Range Helper ----
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
  
  # --- Update Inputs when Dataset Changes ----
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
  
  # ---- SAFE Filter Functions ----
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
  
}

# ---- Launch App ----
shinyApp(ui = ui, server = server)











