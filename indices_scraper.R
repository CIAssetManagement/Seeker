library(dplyr)
library(tidyr) 
library(RMySQL)
library(RSelenium)

###Obteniendo informacion ###
rD <- rsDriver(port = 4568L)
remDr <- rD$client
remDr$navigate("https://mx.investing.com/indices/ipc-historical-data")
Sys.sleep(2)
nivel_ipc <- remDr$findElement("css selector","#curr_table > tbody > tr:nth-child(2) > td:nth-child(2)")$getElementText()

remDr$navigate("https://mx.investing.com/indices/us-spx-500-historical-data")
Sys.sleep(2)
nivel_sp <- remDr$findElement("css selector","#curr_table > tbody > tr:nth-child(2) > td:nth-child(2)")$getElementText()

remDr$navigate("https://mx.investing.com/indices/germany-30-historical-data")
Sys.sleep(2)
nivel_dax <- remDr$findElement("css selector","#curr_table > tbody > tr:nth-child(2) > td:nth-child(2)")$getElementText()

remDr$navigate("https://mx.investing.com/indices/eu-stoxx50-historical-data")
Sys.sleep(2)
nivel_stoxx <- remDr$findElement("css selector","#curr_table > tbody > tr:nth-child(2) > td:nth-child(2)")$getElementText()

remDr$navigate("https://mx.investing.com/indices/nasdaq-composite-historical-data")
Sys.sleep(2)
nivel_nasdaq <- remDr$findElement("css selector","#curr_table > tbody > tr:nth-child(2) > td:nth-child(2)")$getElementText()

remDr$navigate("https://mx.investing.com/currencies/usd-mxn-historical-data")
Sys.sleep(2)
nivel_usdmxn <- remDr$findElement("css selector","#curr_table > tbody > tr:nth-child(2) > td:nth-child(2)")$getElementText()


#### Metiendo a la base de datos ####
dia <- as.character(Sys.Date()-1)
ipc <- c("IPC",dia,gsub(",","",nivel_ipc))

sp <- c("SP500",dia,gsub(",","",nivel_sp))

dax <- c("DAX",dia,gsub(",","",nivel_dax))

stoxx <- c("STOXX50E",dia,gsub(",","",nivel_stoxx))

nasdaq <- c("NASDAQ",dia,gsub(",","",nivel_nasdaq))

usdmxn <- c("USDMXN",dia,gsub(",","",nivel_usdmxn))

general <- rbind(ipc,sp,dax,stoxx,nasdaq,usdmxn)

### Metiendo a la base de datos ###
keys <- readLines('C:/Github/Seeker/keys2.txt')
mydb = dbConnect(MySQL(), user=keys[1], password=keys[2], dbname='mydb', host=keys[3])

for(x in 1:length(general[,1])){
     query <- paste0("INSERT INTO indices (id,fecha,nivel) VALUES ('",paste(general[x,],collapse = "','"),"')")
     dbSendQuery(mydb,query)
}

################################### Varios Datos ################################################################
# numdatos <- 505
# nasdaq <- head(read.csv("C:/Users/JERUIZ/Downloads/Datos históricos NASDAQ Composite.csv",header = TRUE),numdatos)
# 
# fecha <- c()
# for (i in 1:length(nasdaq$ï..Fecha)) {
#   año <- substr(nasdaq$ï..Fecha[i],7,10)
#   mes <- substr(nasdaq$ï..Fecha[i],4,5)
#   dia <- substr(nasdaq$ï..Fecha[i],1,2)
#   fecha <- rbind(fecha,paste0(año,"-",mes,"-",dia))
# }
# nivel <- gsub(",","",nasdaq$Cierre)
# nasdaq <- cbind(id="NASDAQ",fecha,nivel)
# 
# #### Metiendo a la base de datos ####
# keys <- readLines('C:/Github/Seeker/keys2.txt')
# mydb = dbConnect(MySQL(), user=keys[1], password=keys[2], dbname='mydb', host=keys[3])
# 
# for(x in 1:length(nasdaq[,1])){
#      query <- paste0("INSERT INTO indices (id,fecha,nivel) VALUES ('",paste(nasdaq[x,],collapse = "','"),"')")
#      dbSendQuery(mydb,query)
# }
################################################################################################################

remDr$close()
rD$server$stop()

dbDisconnect(mydb)
