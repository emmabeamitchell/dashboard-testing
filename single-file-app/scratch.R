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

















