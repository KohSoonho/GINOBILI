# Get Ginobili data -------------------------------------------------------

# Made list of Ginobili playing year --------------------------------------
playing_year <- map(2002:2016, ~ str_c(str_sub(., -2, -1), str_sub(. + 1, -2, -1), sep = "-")) 

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
# It is used in for loops
null_data <- list_data_ginobili[[1]][-1,]  

# Bind tibbles in list_data_ginobili
data_ginobili <- null_data
for(i in seq_along(list_data_ginobili)) {
  data_ginobili[i, ] <- list_data_ginobili[[i]]
}

# Load packages -----------------------------------------------------------
library(shiny)
library(tidyverse)
library(SportsAnalytics)

# Create total row --------------------------------------------------------

total_data <- null_data

# Create total tibble 1 row
# numeric colums -> sum(columns)
# Year -> "Total"
# Else characer of factor -> identical
for(i in seq_along(data_ginobili)) {
  if(colnames(data_ginobili[i]) == "Year") {
    total_data[1, i] <- "Total"
  }else if(identical(class(data_ginobili[[i]]), "integer") == F) {
    total_data[1, i] <- data_ginobili[nrow(data_ginobili), i]
  }else{
    total_data[1, i] <- sum(data_ginobili[i])
  }
}

# add total_data to ginobili_data
data_total_ginobili <- full_join(data_ginobili, total_data)


# Create New Variable -----------------------------------------------------


