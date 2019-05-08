# Clean up ----
if(!is.null(dev.list())) dev.off()   # Clear plots
rm(list=ls())                        # Clean workspace
cat("\014")                          # Clear console


library(tidyverse)
library(rvest)
library(stringr)

url <- 'https://www.virtualmanager.com/players/13372815-morthwyl-anwyl/training'
url <- 'https://www.virtualmanager.com/players/13369255-christoph-bretterhofer/training'
url <- 'https://www.virtualmanager.com/players/13364297-claudio-amendola/training'
url <- 'https://www.virtualmanager.com/players/13339860-safayat-rutherford/training'
webpage <- read_html(url)

rank_data_html <- html_nodes(webpage, '.trainings')

rank_data <- html_text(rank_data_html)
rank_data

training_data <- strsplit(rank_data, "\n") %>% unlist()

indexes <- c(
seq(35, length(training_data), by = 34),
seq(38, length(training_data), by = 34),
seq(42, length(training_data), by = 34)
) %>% sort()

training_data[indexes] %>% str_trim(side = "left")

# training_data[2] %>% str_trim(side = "left")
# training_data[35] %>% str_trim(side = "left")
# training_data[38] %>% str_trim(side = "left")
# training_data[42] %>% str_trim(side = "left")
# training_data[69] %>% str_trim(side = "left")
# training_data[72] %>% str_trim(side = "left")
# training_data[76] %>% str_trim(side = "left")
# training_data[103] %>% str_trim(side = "left")
# training_data[106] %>% str_trim(side = "left")
# training_data[110] %>% str_trim(side = "left")

# 
# html_txt <- c()
# for(i in 1:length(rank_data_html)){
# html_txt <- c(html_txt, xml_attrs(rank_data_html[[i]])[["href"]])
# }
# 
# remove <- seq(2,length(rank_data_html), by = 2)
# html_txt <- html_txt[-remove]
# 
# paste('https://www.virtualmanager.com', html_txt, sep = "")
