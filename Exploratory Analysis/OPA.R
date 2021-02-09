library(tidyverse)
library(httr)
library(dplyr)

# Request OPA for a building-----------------------------------------------
base_url <- "http://api.phila.gov/ais_doc/v1/"
endpoint <- "search/"
address  <- "4701%20PINE%20ST"
key      <- "?gatekeeperKey=6ba4de64d6ca99aa4db3b9194e37adbf"
url <- paste(base_url, endpoint, address, key, sep="")

# response <- httr::GET("http://api.phila.gov/ais_doc/v1/search/1234%20market%20st?gatekeeperKey=6ba4de64d6ca99aa4db3b9194e37adbf")
response <- httr::GET(url)
tidy_res <- httr::content(response, simplifyVector=TRUE)

glimpse(tidy_res$features)
glimpse(tidy_res$features$properties)
glimpse(tidy_res$features$geometry)
glimpse(tidy_res$features$properties$opa_account_num)
tidy_res$features$properties$opa_account_num

#Load data------------------------------------------------------------------
setwd("~/Documents/Practicum")
fire <- read.csv("2015 to 2019 fires for u of pa report run 1620.csv")

library(stringr)

  
  
fire1 = fire %>%
  filter(addr_type ==1)

fire1$address <- paste(ifelse(is.na(fire1$number)==FALSE,fire1$number,''),
                       ifelse(is.na(fire1$st_prefix)==FALSE,fire1$st_prefix,''),
                       "%20",
                       ifelse(is.na(fire1$street)==FALSE,fire1$street,''),
                       "%20",
                       ifelse(is.na(fire1$st_type)==FALSE,fire1$st_type,''), sep = "")

fire2 = fire %>%
  filter(addr_type ==2)

fire2$address <- paste(ifelse(is.na(fire2$xst_prefix)==FALSE,fire2$xst_prefix,''),
                       ifelse(is.na(fire2$xstreet)==FALSE,fire2$xstreet,''),
                       "%20",
                       ifelse(is.na(fire2$xst_type)==FALSE,fire2$xst_type,''),
                       "%20",
                       ifelse(is.na(fire2$st_type)==FALSE,fire2$st_type,''), 
                       "%20",
                       "&",
                       "%20",
                       ifelse(is.na(fire2$st_prefix)==FALSE,fire2$st_prefix,''), 
                       "%20",
                       ifelse(is.na(fire2$st_prefix)==FALSE,fire2$st_type,''),
                       sep = "")  

fireData <- rbind(fire1, fire2)

#Request OPA number for each building-------------------------------------
for (i in (1:10)) {
  address  <- fireData$address[[i]]
  base_url <- "http://api.phila.gov/ais_doc/v1/"
  endpoint <- "search/"
  key      <- "?gatekeeperKey=6ba4de64d6ca99aa4db3b9194e37adbf"
  url <- paste(base_url, endpoint, address, key, sep="")
  response <- httr::GET(url)
  tidy_res <- httr::content(response, simplifyVector=TRUE)
  fireData$opa[[i]] <- tidy_res$features$properties$opa_account_num
}


fireData1 <- fireData[1:100,]

base_url <- "http://api.phila.gov/ais_doc/v1/"
endpoint <- "search/"
key      <- "?gatekeeperKey=6ba4de64d6ca99aa4db3b9194e37adbf"

fireData1 <- fireData1 %>%
  mutate(opa = httr::content(httr::GET(paste(base_url, endpoint, address, key, sep="")),
                             simplifyVector=TRUE)$features$properties$opa_account_num)