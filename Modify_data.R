# Load packages -----------------------------------------------------------
library(tidyverse)

# Get Ginobili data -------------------------------------------------------
data_ginobili <- read_csv("Ginobili.csv")

# Create NULL tibble
null_data <- data_ginobili[0, ]

# Create total row --------------------------------------------------------

# Create total tibble 1 row
total_data <- null_data

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
# Per game stats

# Arrange columns, this order is applyed in NBA2K 
