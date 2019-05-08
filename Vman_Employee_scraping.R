# Clean up ----
if(!is.null(dev.list())) dev.off()                  # Clear plots
rm(list=ls())                                       # Clean workspace
cat("\014")                                         # Clear console
if (!require("pacman")) install.packages("pacman")  # Installs/loads pacman

pacman::p_load(rvest)

# Test ----
# 
# url <- 'https://www.virtualmanager.com/employees/10648378'
# 
# webpage <- read_html(url)
# 
# rank_data_html <- html_nodes(webpage, '.keepers strong , .speciality strong , .job strong , .outfield strong , .youth_players strong')
# 
# rank_data <- html_text(rank_data_html); rank_data

# Initialization ----

id_min <- 9823626
id_max <- 9828541

# # min <- 10638444 
# min <- 10668444 
# max <- 10676220
id_max-id_min

empl_type = "Scout"
#employee_scraping <- function(empl_type = "Outfield trainer", id_min, id_max){
  
  counter <- 0
  
  if(empl_type == "Outfield trainer"){
    
    trainers <- c()
    
    for(i in id_min:id_max){
      url <- paste('https://www.virtualmanager.com/employees/', i, sep = "")
      #Sys.sleep(0.1)
      webpage <- read_html(url)
      rank_data_html <- html_nodes(webpage, '.keepers strong , .speciality strong , .job strong , .youth_players strong , .outfield strong')
      rank_data <- html_text(rank_data_html)
      n <- length(rank_data) # 5 for keepers
      if(rank_data[1] == "Unemployed" & rank_data[2] == "Trainer" & rank_data[3] == 20 & rank_data[n] == 20) {trainers <- c(trainers, paste("OUTFIELD: Check him:", i, sep = " "))}
      counter <- counter + 1
      print(counter)
    }
    
    return(trainers)
    
  } else if(empl_type == "Keeper trainer"){
    
    trainers <- c()
    
    for(i in id_min:id_max){
      url <- paste('https://www.virtualmanager.com/employees/', i, sep = "")
      #Sys.sleep(0.1)
      webpage <- read_html(url)
      rank_data_html <- html_nodes(webpage, '.keepers strong , .speciality strong , .job strong , .youth_players strong , .outfield strong')
      rank_data <- html_text(rank_data_html)
      n <- length(rank_data) # 5 for keepers
      if(rank_data[1] == "Unemployed" & rank_data[2] == "Trainer" & rank_data[3] == 20 & rank_data[5] == 20) {trainers <- c(trainers, paste("KEEPER: Check him:", i, sep = " "))}
      counter <- counter + 1
      print(counter)
    }
    
    return(trainers)
    
  } else if(empl_type == "Allround trainer"){
    
    trainers <- c()
    
    for(i in id_min:id_max){
      url <- paste('https://www.virtualmanager.com/employees/', i, sep = "")
      #Sys.sleep(0.1)
      webpage <- read_html(url)
      rank_data_html <- html_nodes(webpage, '.keepers strong , .speciality strong , .job strong , .youth_players strong , .outfield strong')
      rank_data <- html_text(rank_data_html)
      n <- length(rank_data) # 5 for keepers
      if(rank_data[1] == "Unemployed" & rank_data[2] == "Trainer" & rank_data[3] == 20 & as.numeric(rank_data[5]) >= 19 & rank_data[n] == 20) {trainers <- c(trainers, paste("ALLROUND: Check him:", i, sep = " "))}
      counter <- counter + 1
      print(counter)
    }
    
    return(trainers)
    
  } else if(empl_type == "Scout"){
    
    scouts <- c()}
  
  for(i in id_min:id_max){
    url <- paste('https://www.virtualmanager.com/employees/', i, sep = "")
    #Sys.sleep(0.1)
    webpage <- read_html(url)
    rank_data_html <- html_nodes(webpage, '.potential_assessment strong , .speciality strong , .job strong , .motivation strong , .discipline strong')
    rank_data <- html_text(rank_data_html)
    n <- length(rank_data) 
    if(rank_data[1] == "Unemployed" & rank_data[2] == "Scout" & rank_data[3] == 20 & as.numeric(rank_data[5]) == 20 & rank_data[n] == 20) {scouts <- c(scouts, paste("SCOUT: Check him:", i, sep = " "))}
    counter <- counter + 1
    print(counter)
  }
  
  return(scouts)
  
# }


# employee_scraping(empl_type = "Scout", id_min = min, id_max = max)
