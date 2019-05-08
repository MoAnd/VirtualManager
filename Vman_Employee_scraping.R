# Clean up ----
if(!is.null(dev.list())) dev.off()   # Clear plots
rm(list=ls())                        # Clean workspace
cat("\014")                          # Clear console


# install.packages("rvest")
library(rvest)
# 
# url <- 'https://www.virtualmanager.com/employees/9594140'
# 
# webpage <- read_html(url)
# 
# rank_data_html <- html_nodes(webpage, '.keepers strong , .speciality strong , .job strong , .outfield strong , .youth_players strong')
# 
# rank_data <- html_text(rank_data_html)

# min <- 10035549 
min <- 9595780
max <- 10676220
max-min
counter <- 0
trainers <- c() #10652691, 10635738, 10675034, 10675137

for(i in min:max){
  url <- paste('https://www.virtualmanager.com/employees/', i, sep = "")
  #Sys.sleep(0.1)
  webpage <- read_html(url)
  rank_data_html <- html_nodes(webpage, '.keepers strong , .speciality strong , .job strong , .youth_players strong , .outfield strong')
  rank_data <- html_text(rank_data_html)
  n <- length(rank_data) # 5 for keepers
  if(rank_data[1] == "Unemployed" & rank_data[2] == "Trainer" & rank_data[3] == 20 & as.numeric(rank_data[5]) >= 18 & rank_data[n] == 20) {trainers <- c(trainers, paste("TJEK HAM:", i, sep = " "))}
  counter <- counter + 1
  print(counter)
}

