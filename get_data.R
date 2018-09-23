# Makw Ginobili csv.file --------------------------------------------------
# Load packages -----------------------------------------------------------
library(tidyverse)
library(SportsAnalytics)

# Made list of Ginobili playing year --------------------------------------
playing_year <- map(2002:2017, ~ str_c(str_sub(., -2, -1), str_sub(. + 1, -2, -1), sep = "-")) 

# Fetch NBA data in years Ginobili play -----------------------------------
fetch_ginobil_data <- function(year) {
  fetch_NBAPlayerStatistics(year) %>% 
    tbl_df() %>%   
    mutate(Year = year) %>%   # Create "Year" columns
    select(Year, everything()) %>% 
    filter(Name %in% c("Emanuel Ginobili", "Manu Ginobili"))  # Filter only Ginobili data  
}

list_data_ginobili <- map(playing_year, fetch_ginobil_data)  # List of ginobili data


# Bind data lists ---------------------------------------------------------
# Create NULL tibbles
data_ginobili <- bind_rows(list_data_ginobili)

write_csv(data_ginobili, path = "Ginobili.csv")
