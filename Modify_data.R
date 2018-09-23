# Load packages -------------------------------------------------------------
library(tidyverse)
library(remoji)

# Get Ginobili data ---------------------------------------------------------
data_ginobili <- read_csv("Ginobili.csv")

# Create total row ----------------------------------------------------------
total_data <- data_ginobili %>% 
  map_dfc(~ ifelse(is.numeric(.), sum(.), last(.))) %>% 
  mutate(Year = "Total")

# add total_data to ginobili_data
df_ginobili <- bind_rows(data_ginobili, total_data)

# Remodeling data ------------------------------------------------------------
# Rename tibble

names(df_ginobili) <- c("Season", "League", "Name", "Team", "POS", "GP", "MIN", "FGM", "FGA", 
                        "TPM", "TPA", "FTM", "FTA", "OFER", "REB", "AST", "STL", "TO", 
                        "BLK", "FLS", "Disqualifications", "PTS", "Technicals", "Ejections", 
                        "Flagrant", "GS")

# Create FG%, 3P% and FT% by mutate_per_game ---------------------------------
## I can write this code more efficiently! ##

df_ginobili <- df_ginobili %>%  
  mutate_("FG%" = "round(FGM / FGA, digit = 2)") %>% 
  mutate_("TP%" = "round(TPM / TPA, digit = 2)") %>% 
  mutate_("FT%" = "round(FTM / FTA, digit = 2)") %>% 
  rename("3PM" = "TPM", "3PA" = "TPA", "3P%" = "TP%")
  
# Arrange columns, this order is applyed in NBA2K 
df_ginobili_2k <- df_ginobili %>% 
  mutate(DFER = REB - OFER) %>% 
  select("Season", "Team", "POS", "PTS", "OFER", "DFER", "REB", "AST", "STL", "BLK", "TO", "FGM", "FGA", 
         "FG%", "3PM", "3PA", "3P%", "FTM", "FTA", "FT%", "MIN", "FLS", "GS", "GP")


# Create New Variable --------------------------------------------------------

# Add title column
df_ginobili_2k <- df_ginobili_2k %>% mutate(Title = "")

## Unicode of medal
## Gold: \U0001F947, Silver: \U0001F948, Bronze: \U0001F949, Medal: \U0001F396

# Make function add emoji + award
add_title <- function(medal) {
  function(title) {
    switch(medal, 
           "trophy" = str_c(emoji("trophy"), title, sep = ""), 
           "gold"   = str_c("\U0001F947", title, sep = ""), 
           "silver" = str_c("\U0001F948", title, sep = ""),
           "bronze" = str_c("\U0001F949", title, sep = ""), 
           "medal"  = str_c("\U0001F396", title, sep = ""))
  }
}

# Make function add award to df
add_title_df <- function(lst, func, theme = "theme") {
  for(i in lst) {
    df_ginobili_2k[df_ginobili_2k$Season == i, "Title"] <<- 
      df_ginobili_2k[df_ginobili_2k$Season == i, "Title"] %>% 
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

df_ginobili_2k_per <- df_ginobili_2k  # Copy dataframe

# Replace some stats are devided by GP
for(i in seq_along(basic_stats)) {
  df_ginobili_2k_per[[basic_stats[i]]] = round(df_ginobili_2k[[basic_stats[i]]] / df_ginobili_2k[["GP"]], 1)
}

