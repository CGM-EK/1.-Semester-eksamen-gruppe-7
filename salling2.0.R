library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(RMariaDB)
library(logr)

api_key <- "SG_APIM_EZQHF5FB2Z4JCZY0M68FT9W22VEDWWGPZFF557JPVY61NFM4SHJG"
postnr <- commandArgs(trailingOnly = TRUE)

resp <- GET(
  paste0("https://api.sallinggroup.com/v1/food-waste?zip=",postnr),
  add_headers(Authorization = paste("Bearer", api_key)))
  
data_raw <- content(resp, as = "text", encoding = "UTF-8")
data_json <- fromJSON(data_raw, flatten = TRUE)
  
assign(paste0("postdf"), as.data.frame(data_json))

food_waste_table <- matrix(NA, nrow = 0, ncol = 19)
colnames(food_waste_table) <- c(colnames(postdf[2]), colnames(postdf[[1]][[1]]), "timer_tilbud_er_oppe", "API_kald_tidspunkt")


for(i in 1:nrow(postdf)){ #
  df <- postdf[[1]][[i]]
 
  unix_start <- as.numeric(as.POSIXct(df[["offer.startTime"]], format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"))
  unix_end <- as.numeric(as.POSIXct(df[["offer.endTime"]], format="%Y-%m-%dT%H:%M:%OSZ", tz="UTC"))
  unix_total <- (unix_end-unix_start)/3600
  df <- cbind(df, timer_tilbud_er_oppe=unix_total, API_kald_tidspunkt=Sys.time())

  df[,4] <- ymd_hms(df[,4], tz = "CET")
  df[,5] <- ymd_hms(df[,5], tz = "CET")
  df[,9] <- ymd_hms(df[,9], tz = "CET")
  
  
  food_waste_1 <- cbind(store.id=rep(postdf$store.id[i], times=nrow(df)), df)
  food_waste_table <- rbind(food_waste_table, food_waste_1)

}

stamdata_table <- postdf[1:nrow(postdf), c(2,3,6,8,9,10,11,12)]



con <- dbConnect(MariaDB(),
                 host="localhost",
                 db="salling",
                 user="root",
                 password="Kurt123")

dbWriteTable(con, "food_waste_table", food_waste_table, append=T)
dbWriteTable(con, "stamdata_table", stamdata_table, append=T)



##if (any(postnr <= 9999)){
#  print("cat")
#}

#skriv i logfil
path <- Sys.getenv("HOME")
logfilename <- paste0(path, "/log/log_", as.integer(Sys.time()))
log_open(logfilename)
log_print("Starting")

dbDisconnect(con)
log_close()
