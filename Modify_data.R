# Load packages -----------------------------------------------------------
library(tidyverse)
library(lazyeval)
library(remoji)
library(magrittr)


# Get Ginobili data -------------------------------------------------------
data_ginobili <- read_csv("Ginobili.csv")


# Create total row --------------------------------------------------------

# Create NULL tibble
null_data <- data_ginobili[0, ]
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

# Remodeling data ------------------------------------------------------------
# Rename tibble
name_new_list <- c("Season", "League", "Name", "Team", "POS", "GP", "MIN", "FGM", "FGA", 
                   "3PM", "3PA", "FTM", "FTA", "OFER", "REB", "AST", "STL", "TO", 
                   "BLK", "FLS", "Disqualifications", "PTS", "Technicals", "Ejections", 
                   "Flagrant", "GS")

for(i in seq_along(data_total_ginobili)) {
  names(data_total_ginobili)[i] <- name_new_list[i]
}

# Create mutate per function ----------------------------------------------

mutate_per <- function(df, col1, col2, new_col, num) {
  mutate_call <- lazyeval::interp(~ round(a / b, digits = num), 
                                  a = as.name(col1), 
                                  b = as.name(col2))
  df %>% mutate_(.dots = setNames(list(mutate_call), new_col))
}

# Create FG%, 3P% and FT% by mutate_per_game -------------------------------
changelist <- map(c("FG", "3P", "FT"), ~ str_c(. , c("M", "A", "%"), sep = ""))
# make list (~M, ~A, ~%)

# loop at FG/3P/FT, by mutate_per
for(i in 1:3) {
  data_total_ginobili %<>% mutate_per(changelist[[i]][1], changelist[[i]][2], changelist[[i]][3], 3) 
}

# Arrange columns, this order is applyed in NBA2K 

data_total_ginobili <- data_total_ginobili %>% 
  mutate(DFER = REB - OFER) %>% 
  select("Season", "Team", "POS", "PTS", "OFER", "DFER", "REB", "AST", "STL", "BLK", "TO", "FGM", "FGA", 
         "FG%", "3PM", "3PA", "3P%", "FTM", "FTA", "FT%", "MIN", "FLS", "GS", "GP")


# Create New Variable -----------------------------------------------------

# Add title column
data_total_ginobili <- data_total_ginobili %>% mutate(Title = "")

## Unicode of medal
## Gold: \U0001F947, Silver: \U0001F948, Bronze: \U0001F949, Medal: \U0001F396

# Make function add emoji + award
add_title <- function(medal) {
  function(title) {
    switch(medal, "trophy" = str_c(emoji("trophy"), title, sep = ""), 
           "gold"   = str_c("\U0001F947", title, sep = ""), 
           "silver" = str_c("\U0001F948", title, sep = ""),
           "bronze" = str_c("\U0001F949", title, sep = ""), 
           "medal"  = str_c("\U0001F396", title, sep = ""))
  }
}

# Make function add award to df
add_title_df <- function(lst, func, theme = "theme") {
  for(i in lst) {
    data_total_ginobili[data_total_ginobili$Season == i, "Title"] <<- 
      data_total_ginobili[data_total_ginobili$Season == i, "Title"] %>% 
      str_c("\n", func(theme), sep = "")
  }
}

lst_title <- list(list(c("02-03", "04-05", "06-07", "13-14"), add_title("trophy"), "NBA Champion"), 
                  list(c("04-05", "10-11"), add_title("medal"), "All Star"), 
                  list(c("07-08", "10-11"), add_title("medal"), "NBA 3rd Team"), 
                  list("07-08", add_title("medal"), "Six Man Award"), 
                  list("02-03", add_title("medal"), "All Rockie 2nd Team"), 
                  list("03-04", add_title("gold"), "Olympic Gold"), 
                  list("07-08", add_title("bronze"), "Olympic Bronze"))

# Add all award to df
for (i in 1:length(lst_title)) {
  add_title_df(lst_title[[i]][[1]], lst_title[[i]][[2]], lst_title[[i]][[3]])
}

# Per game stats ----------------------------------------------------------

# list of stats I want to determine per game stas
basic_stats <- c("MIN", "FGM", "FGA", "3PM", "3PA", "FTM", "FTA", "OFER", "DFER", "REB", "AST", "STL", "TO", 
                 "BLK", "FLS", "PTS")

data_per_game_ginobili <- data_total_ginobili  # Copy dataframe

# Replace some stats are devided by GP
for(i in seq_along(basic_stats)) {
  data_per_game_ginobili <- data_per_game_ginobili %>% 
    mutate_per(basic_stats[[i]], "GP", basic_stats[[i]], 1)
}

