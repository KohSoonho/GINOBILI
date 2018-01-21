# Load packages -----------------------------------------------------------
library(tidyverse)
library(lazyeval)
library(remoji)

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

# Rename tibble
name_new_list <- c("Season", "League", "Name", "Team", "POS", "GP", "MIN", "FGM", "FGA", 
                  "3PM", "3PA", "FTM", "FTA", "OFER", "REB", "AST", "STL", "TO", 
                  "BLK", "FLS", "Disqualifications", "PTS", "Technicals", "Ejections", 
                  "Flagrant", "GS")

for(i in seq_along(data_total_ginobili)) {
  names(data_total_ginobili)[i] <- name_new_list[i]
}

# Add title column
data_total_ginobili <- data_total_ginobili %>% mutate(Title = "")

add_emoji <- function(title) {
  str_c(emoji("trophy"), title, sep = "")
}

# Title Year
## Unicode of medal
## Gold: \U0001F947, Silver: \U0001F948, Bronze: \U0001F949, Medal: \U0001F396

# NBA-Title
lst_champ <- c("02-03", "04-05", "06-07", "13-14")
lst_allstar <- c("04-05", "10-11")
lst_3rdteam <- c("07-08", "10-11")
lst_six <- "07-08"
lst_all_rockie2nd <- "02-03"

# World Title
lst_gold <- "03-04"
lst_blondz <- "07-08"

for(i in lst_champ) {
  data_total_ginobili[data_total_ginobili$Season == i, "Title"] <- add_emoji("NBA Champion")
}

# Create mutate per function ----------------------------------------------

mutate_per <- function(df, col1, col2, new_col) {
  mutate_call <- lazyeval::interp(~ a / b, 
                                  a = as.name(col1), 
                                  b = as.name(col2))
  df %>% mutate_(.dots = setNames(list(mutate_call), new_col))
}

# Create FG% and 3P% by mutate_per_game -----------------------------------

data_total_ginobili <- data_total_ginobili %>% 
  mutate_per("FGM", "FGA", "FG%") %>% 
  mutate_per("3PM", "3PA", "3P%")   

# Arrange columns, this order is applyed in NBA2K 


# Per game stats ----------------------------------------------------------

# list of stats I want to determine per game stas
basic_stats <- c("MIN", "FGM", "FGA", "3PM", "3PA", "FTM", "FTA", "OFER", "REB", "AST", "STL", "TO", 
"BLK", "FLS", "PTS")

data_per_game_ginobili <- data_total_ginobili  # Copy dataframe

# Replace some stats are devided by GP
for(i in seq_along(basic_stats)) {
  data_per_game_ginobili <- data_per_game_ginobili %>% 
    mutate_per(basic_stats[[i]], "GP", basic_stats[[i]])
}