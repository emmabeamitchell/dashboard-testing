# ---- Load Required Packages ----
library(shiny)
library(tidyverse)
library(shinydashboard)
library(here)
library(janitor)
library(lubridate)

# ---- Load Data on App Startup ----
load_water_data <- function() {
  actual_shortage <- read_csv("data/actual_water_shortage_level.csv") |> clean_names()
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

# Load the datasets into memory
water_data <- load_water_data()

# ---- User Interface ----
ui <- fluidPage(
  
  # Row 1: Dataset and Org Selector
  fluidRow(
    column(
      width = 4,
      selectInput("dataset_selector", "Select Dataset",
                  choices = names(water_data),
                  selected = "five_year_outlook")
    ),
    column(
      width = 4,
      selectInput("org_id", "Select Org ID", choices = NULL)
    ),
    column(
      width = 4,
      sliderInput("year_range", "Select Forecast Year Range",
                  min = 2010, max = 2025, value = c(2010, 2025), step = 1, sep = "")
    )
  ),
  
  # Row 2: Value Boxes for Water Categories
  fluidRow(
    column(width = 3, valueBoxOutput("fiveyr_value_box_use")),
    column(width = 3, valueBoxOutput("fiveyr_value_box_supply")),
    column(width = 3, valueBoxOutput("fiveyr_value_box_aug")),
    column(width = 3, valueBoxOutput("fiveyr_value_box_red"))
  )
)

# ---- Server Logic ----
server <- function(input, output, session) {
  
  # ---- Helper: Filter and Pivot 5-Year Data ----
  five_filter_function <- function(id, year_range = NULL) {
    data <- water_data$five_year_outlook %>%
      filter(org_id == id) %>%
      mutate(forecast_year = year(forecast_start_date))
    
    # Filter only if year range is provided
    if (!is.null(year_range)) {
      data <- data %>% filter(forecast_year >= year_range[1], forecast_year <= year_range[2])
    }
    
    # Reshape and relabel categories
    data %>%
      pivot_longer(
        cols = c(starts_with("water"), starts_with("benefit")),
        names_to = "use_supply_aug_red",
        values_to = "acre_feet"
      ) %>%
      mutate(
        use_supply_aug_red = factor(
          use_supply_aug_red,
          levels = c("water_supplies_acre_feet",
                     "water_use_acre_feet",
                     "benefit_supply_augmentation_acre_feet",
                     "benefit_demand_reduction_acre_feet"),
          labels = c("Supply", "Use", "Supply Augmentation", "Demand Reduction")
        )
      )
  }
  
  # ---- Calculate Total Values for Each Category ----
  five_values_function <- function(id, year_range) {
    five_filter_function(id, year_range) %>%
      group_by(use_supply_aug_red) %>%
      summarize(total_value = sum(acre_feet, na.rm = TRUE), .groups = 'drop')
  }
  
  # ---- Dynamic UI: Update Org ID and Year Range ----
  observeEvent(input$dataset_selector, {
    df <- water_data[[input$dataset_selector]]
    
    # Update org choices
    orgs <- sort(unique(df$org_id))
    updateSelectInput(session, "org_id", choices = orgs, selected = orgs[1])
    
    # If five-year forecast, update year range
    if (input$dataset_selector == "five_year_outlook") {
      yrs <- sort(unique(year(as.Date(df$forecast_start_date))))
      updateSliderInput(session, "year_range",
                        min = min(yrs), max = max(yrs),
                        value = c(min(yrs), max(yrs)))
    }
  })
  
  # ---- Render Value Boxes for Each Category ----
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
                       "Use" = "aqua",
                       "Supply" = "blue",
                       "Supply Augmentation" = "green",
                       "Demand Reduction" = "red")
      )
    })
  }
  
  # Assign each output slot to a value box
  output$fiveyr_value_box_use   <- render_value_box("Use")
  output$fiveyr_value_box_supply <- render_value_box("Supply")
  output$fiveyr_value_box_aug   <- render_value_box("Supply Augmentation")
  output$fiveyr_value_box_red   <- render_value_box("Demand Reduction")
}

# ---- Launch the App ----
shinyApp(ui = ui, server = server)
