library(DBI)
library(serial)
library(RMariaDB)
library(stringr)
library(stringi)
library(logr)
library(ggplot2)
library(dplyr)

conarduino <- serialConnection(
  name = "mytest",
  #port = "cu.usbserial-146101",
  port = "COM4",
  mode = "9600,n,8,1",
  newline = 1,
  translation = "cr",
  handshake = "xonxoff",
  buffersize = 4096
)

#log_open("log.txt")
open(conarduino)
counter <- 0

arduino_obs <- c()
timestamp <- c()
person_id <- c()


while(TRUE) {
  line <- read.serialConnection(conarduino)
  if(length(line) == 1 && trimws(line) == "1") {
    counter=counter+1
    cat("1\n")
    arduino_obs <- c(arduino_obs, line)
    timestamp <- as.POSIXct(c(timestamp, as.POSIXct(Sys.time(), format="%Y-%m-%dT%H:%M:%OSZ", tz="CET")))
    person_id <- c(person_id, counter)
    
  }
}

arduino_obs <- gsub("\n", "", arduino_obs)

df <- data.frame(person_id, timestamp, person_passeret=as.numeric(arduino_obs))


#51 var det ægte tal

saveRDS(df, "df.rds")
#16 personer i virkeligheden

close(conarduino)



ggplot(df, aes(x = timestamp, y = person_id)) +
  geom_line(size = 1) +
  scale_x_datetime(date_labels = "%H:%M:%S", date_breaks = "60 sec", timezone = "CET")+
  labs(
    title = "Test ved smal passage",
    x = "Tid",
    y = "Antal mennesker ialt"
  )+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))






