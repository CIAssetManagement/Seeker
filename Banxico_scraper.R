###############################################################
#                                                             #
#                Banxico - Banco de México                    #
#                                                             #
###############################################################
file.remove(dir('C:/Users/MATREJO/Downloads', full.names=T, pattern="^Consulta_"))
rD <- rsDriver(port = 4568L)
remDr <- rD$client

#### Tasa Objetivo ####
remDr$navigate("http://www.banxico.org.mx/SieInternet/consultarDirectorioInternetAction.do?accion=consultarCuadroAnalitico&idCuadro=CA51&sectorDescripcion=Precios&locale=es")
Sys.sleep(2)
tasaobjetivo <- remDr$findElement("css selector","#contenedortabla > table:nth-child(3) > tbody > tr:nth-child(11) > td:nth-child(2)")$getElementText()

#### Fondeo Gubernamental ####
fondeoguber <- remDr$findElement("css selector","#contenedortabla > table:nth-child(3) > tbody > tr:nth-child(12) > td:nth-child(8)")$getElementText()

#### UDIs ####
remDr$navigate("http://www.banxico.org.mx/SieInternet/consultarDirectorioInternetAction.do?sector=8&accion=consultarCuadro&idCuadro=CP150&locale=es")
Sys.sleep(2)
remDr$findElement("css selector", "#formatoCSV")$clickElement()

#### Metiendo a la base de datos ####
numdatos <- 20
udis <- tail(read.csv(dir('C:/Users/MATREJO/Downloads', full.names=T, pattern="^Consulta_"),sep = ' '),numdatos)
for(x in udis[,1]){
  datos <- c("UDI",strsplit(as.character(x),",")[[1]])
  datos[2] <- format(as.Date(datos[2],format='%d/%m/%Y') ,'%Y-%m-%d')
  query1 <- paste0("SELECT id, fecha FROM tasas WHERE id ='",datos[1],"' AND fecha = '",datos[2],"'")
  rate <- dbGetQuery(mydb,query1)
  if(length(rate$id) == 0){
    query2 <- paste0("INSERT INTO tasas (id,fecha,nivel) VALUES ('",paste(datos,collapse = "','"),"')")
    dbSendQuery(mydb,query2)
  }
}

dia <- as.character(Sys.Date()-1)
tasaobj <- c("Tasa-Banxico",dia,tasaobjetivo[[1]])
query <- paste0("INSERT INTO tasas (id,fecha,nivel) VALUES ('",paste(tasaobj,collapse = "','"),"')")
dbSendQuery(mydb,query)

fondeogub <- c("Fondeo-GuberMX",dia,fondeoguber[[1]])
query <- paste0("INSERT INTO tasas (id,fecha,nivel) VALUES ('",paste(fondeogub,collapse = "','"),"')")
dbSendQuery(mydb,query)

remDr$close()
rD$server$stop()
