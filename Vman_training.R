# Clean up ----
if(!is.null(dev.list())) dev.off()   # Clear plots
rm(list=ls())                        # Clean workspace
cat("\014")                          # Clear console

# Libraries ----
library(tidyverse)
library(rvest)
library(stringr)

# Skill list of the team ----
team_list <- list()

url <- 'https://www.virtualmanager.com/clubs/123675' # My team

webpage <- read_html(url)

rank_data_html <- html_nodes(webpage, '.squad a')

html_txt <- c()
for(i in 1:length(rank_data_html)){
  html_txt <- c(html_txt, xml_attrs(rank_data_html[[i]])[["href"]])
}

remove <- seq(2,length(rank_data_html), by = 2)
html_txt <- html_txt[-remove]

player_urls <- paste('https://www.virtualmanager.com', html_txt, sep = "")


for(url in player_urls){
  
  webpage <- read_html(url)
  
  rank_data_html <- html_nodes(webpage, '.big_name h1 , .position strong , .value')
  
  rank_data <- html_text(rank_data_html)
  rank_data
  
  player_name <- rank_data[1]
  player_pos <- rank_data[2]
  player_total <- rank_data[3] %>% as.numeric()
  
  if(player_pos == "Keeper"){
    handling <- rank_data[6] %>% as.numeric()
    speed <- rank_data[9] %>% as.numeric()
    one_on_one <- rank_data[12] %>% as.numeric()
    acceleration <- rank_data[15] %>% as.numeric()
    diving <- rank_data[18] %>% as.numeric()
    stamina <- rank_data[21] %>% as.numeric()
    aerial <- rank_data[24] %>% as.numeric()
    leader <- rank_data[27] %>% as.numeric()
    passing <- rank_data[30] %>% as.numeric()
    perserverance <- rank_data[33] %>% as.numeric()
    
    avg_stat <- player_total / 10
    
    team_list[[paste(player_name, " - ", player_pos)]] <- tibble(
      "Abilities" = c("Handling", "One-on-one", "Diving", "Aerial", "Passing", 
                      "Speed", "Acceleration", "Stamina", "Leadership", "Perserverance", "Total"), 
      "Stats" = c(handling, one_on_one, diving, aerial, passing, 
                  speed, acceleration, stamina, leader, perserverance, player_total),
      "% above" = sapply(c((handling-avg_stat) / avg_stat * 100, (one_on_one-avg_stat) / avg_stat * 100,
                           (diving-avg_stat) / avg_stat * 100, (aerial-avg_stat) / avg_stat * 100,
                           (passing-avg_stat) / avg_stat * 100, (speed-avg_stat) / avg_stat * 100,
                           (acceleration-avg_stat) / avg_stat * 100, (stamina-avg_stat) / avg_stat * 100,
                           (leader-avg_stat) / avg_stat * 100, (perserverance-avg_stat) / avg_stat * 100, NA), 
                         max, 0)
    )
    
  } else{
    passing <- rank_data[6] %>% as.numeric()
    speed <- rank_data[9] %>% as.numeric()
    finish <- rank_data[12] %>% as.numeric()
    acceleration <- rank_data[15] %>% as.numeric()
    dribbling <- rank_data[18] %>% as.numeric()
    stamina <- rank_data[21] %>% as.numeric()
    tackling <- rank_data[24] %>% as.numeric()
    leader <- rank_data[27] %>% as.numeric()
    set_pieces <- rank_data[30] %>% as.numeric()
    perserverance <- rank_data[33] %>% as.numeric()
    
    avg_stat <- player_total / 10
    
    team_list[[paste0(player_name, " - ", player_pos)]] <- tibble(
      "Abilities" = c("Passing", "Finish", "Dribbling", "Tackling", "Set pieces", 
                      "Speed", "Acceleration", "Stamina", "Leadership", "Perserverance", "Total"), 
      "Stats" = c(passing, finish, dribbling, tackling, set_pieces, 
                  speed, acceleration, stamina, leader, perserverance, player_total),
      "% above" = sapply(c((passing-avg_stat) / avg_stat * 100, (finish-avg_stat) / avg_stat * 100,
                           (dribbling-avg_stat) / avg_stat * 100, (tackling-avg_stat) / avg_stat * 100,
                           (set_pieces-avg_stat) / avg_stat * 100, (speed-avg_stat) / avg_stat * 100,
                           (acceleration-avg_stat) / avg_stat * 100, (stamina-avg_stat) / avg_stat * 100,
                           (leader-avg_stat) / avg_stat * 100, (perserverance-avg_stat) / avg_stat * 100, NA), 
                         max, 0)
    )
    
  }
}

print(team_list)

### Training exercises ----

training_list <- list()

Interval <- c("Speed", "Acceleration", "Stamina")
TeamBuilding <- c("Stamina", "Leadership", "Perserverance")
# Outfield ---
SetPiece <- c("Set pieces", "Finish", "Acceleration")
Technical <- c("Passing", "Finish", "Dribbling", "Tackling", "Set pieces")
Tactical <- c("Passing", "Leadership", "Tackling", "Set pieces")
OneMid <- c("Passing", "Acceleration", "Stamina", "Dribbling")
Offense <- c("Speed", "Perserverance", "Finish", "Dribbling")
Defense <- c("Speed", "Perserverance", "Leadership", "Tackling")
# Keeper ---
SetPiece_Keeper <- c("One-on-one", "Diving", "Aerial", "Acceleration")
Technical_Keeper <- c("Handling", "One-on-one", "Aerial", "Passing")
Tactical_Keeper <- c("One-on-one", "Passing", "Leadership", "Perserverance")
Handling <- c("Handling", "Aerial", "Passing", "Speed")
Physical <- c("Diving", "Speed", "Stamina", "Perserverance")
Crossings <- c("Handling", "Diving", "Acceleration", "Leadership")

for(i in 1:length(team_list)){
  test <- team_list[i][[1]] %>% filter(`% above` < 15) %>% dplyr::select(Abilities) %>% pull()
  if(names(team_list[i]) %>% str_sub(-6,-1) != "Keeper"){
    Exercises <- c("Training match")
    if(all(Interval %in% test)){Exercises <- c(Exercises, "Interval")}
    if(all(TeamBuilding %in% test)){Exercises <- c(Exercises, "Team Building")}
    if(all(SetPiece %in% test)){Exercises <- c(Exercises, "Set Pieces")}
    if(all(Technical %in% test)){Exercises <- c(Exercises, "Technical")}
    if(all(Tactical %in% test)){Exercises <- c(Exercises, "Tactical")}
    if(all(OneMid %in% test)){Exercises <- c(Exercises, "One in the middle")}
    if(all(Offense %in% test)){Exercises <- c(Exercises, "Offense")}
    if(all(Defense %in% test)){Exercises <- c(Exercises, "Defense")}
  } else{
    Exercises <- c("Training match")
    if(all(Interval %in% test)){Exercises <- c(Exercises, "Interval")}
    if(all(TeamBuilding %in% test)){Exercises <- c(Exercises, "Team Building")}
    if(all(SetPiece_Keeper %in% test)){Exercises <- c(Exercises, "Set Pieces Keeper")}
    if(all(Technical_Keeper %in% test)){Exercises <- c(Exercises, "Technical Keeper")}
    if(all(Tactical_Keeper %in% test)){Exercises <- c(Exercises, "Tactical Keeper")}
    if(all(Handling %in% test)){Exercises <- c(Exercises, "Handling")}
    if(all(Physical %in% test)){Exercises <- c(Exercises, "Physical")}
    if(all(Crossings %in% test)){Exercises <- c(Exercises, "Crossings")}
  }
  
  training_list[[names(team_list[i])]] <- Exercises
}

print(training_list)
