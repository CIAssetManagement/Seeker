###############################################################
#                                                             #
#                Banxico - Banco de México                    #
#                                                             #
###############################################################
rD <- rsDriver(port = 4568L)
remDr <- rD$client

#### Tasa objetivo y fondeo gubernamental ####
remDr$navigate("http://www.banxico.org.mx/SieInternet/consultarDirectorioInternetAction.do?accion=consultarCuadroAnalitico&idCuadro=CA51&sectorDescripcion=Precios&locale=es")
Sys.sleep(2)

#Fechas
fechas <- unlist(remDr$findElement("css selector","#contenedortabla > table:nth-child(3) > tbody > tr:nth-child(12) > td:nth-child(2)")$getElementText())
fechas <- c(fechas,unlist(remDr$findElement("css selector","#contenedortabla > table:nth-child(3) > tbody > tr:nth-child(11) > td:nth-child(1)")$getElementText()))
fechas <- c(fechas,unlist(remDr$findElement("css selector","#contenedortabla > table:nth-child(3) > tbody > tr:nth-child(10) > td:nth-child(1)")$getElementText()))
fechas <- c(fechas,unlist(remDr$findElement("css selector","#contenedortabla > table:nth-child(3) > tbody > tr:nth-child(9) > td:nth-child(1)")$getElementText()))
fechas <- c(fechas,unlist(remDr$findElement("css selector","#contenedortabla > table:nth-child(3) > tbody > tr:nth-child(8) > td:nth-child(1)")$getElementText()))
fechas <- c(fechas,unlist(remDr$findElement("css selector","#contenedortabla > table:nth-child(3) > tbody > tr:nth-child(7) > td:nth-child(2)")$getElementText()))
fechas <- as.Date(fechas, format = '%d/%m/%Y')
fechas <- as.character(fechas)

#Tasa objetivo
tasaobjetivo <- unlist(remDr$findElement("css selector","#contenedortabla > table:nth-child(3) > tbody > tr:nth-child(12) > td:nth-child(3)")$getElementText())
tasaobjetivo <- c(tasaobjetivo,unlist(remDr$findElement("css selector","#contenedortabla > table:nth-child(3) > tbody > tr:nth-child(11) > td:nth-child(2)")$getElementText()))
tasaobjetivo <- c(tasaobjetivo,unlist(remDr$findElement("css selector","#contenedortabla > table:nth-child(3) > tbody > tr:nth-child(10) > td:nth-child(2)")$getElementText()))
tasaobjetivo <- c(tasaobjetivo,unlist(remDr$findElement("css selector","#contenedortabla > table:nth-child(3) > tbody > tr:nth-child(9) > td:nth-child(2)")$getElementText()))
tasaobjetivo <- c(tasaobjetivo,unlist(remDr$findElement("css selector","#contenedortabla > table:nth-child(3) > tbody > tr:nth-child(8) > td:nth-child(2)")$getElementText()))
tasaobjetivo <- c(tasaobjetivo,unlist(remDr$findElement("css selector","#contenedortabla > table:nth-child(3) > tbody > tr:nth-child(7) > td:nth-child(3)")$getElementText()))

#Fondeo Gubernamental
fondeoguber <- unlist(remDr$findElement("css selector","#contenedortabla > table:nth-child(3) > tbody > tr:nth-child(12) > td:nth-child(8)")$getElementText())
fondeoguber <- c(fondeoguber,unlist(remDr$findElement("css selector","#contenedortabla > table:nth-child(3) > tbody > tr:nth-child(11) > td:nth-child(7)")$getElementText()))
fondeoguber <- c(fondeoguber,unlist(remDr$findElement("css selector","#contenedortabla > table:nth-child(3) > tbody > tr:nth-child(10) > td:nth-child(7)")$getElementText()))
fondeoguber <- c(fondeoguber,unlist(remDr$findElement("css selector","#contenedortabla > table:nth-child(3) > tbody > tr:nth-child(9) > td:nth-child(7)")$getElementText()))
fondeoguber <- c(fondeoguber,unlist(remDr$findElement("css selector","#contenedortabla > table:nth-child(3) > tbody > tr:nth-child(8) > td:nth-child(7)")$getElementText()))
fondeoguber <- c(fondeoguber,unlist(remDr$findElement("css selector","#contenedortabla > table:nth-child(3) > tbody > tr:nth-child(7) > td:nth-child(8)")$getElementText()))

#Fondeo Bancario
fondeobanc <- unlist(remDr$findElement("css selector","#contenedortabla > table:nth-child(3) > tbody > tr:nth-child(12) > td:nth-child(7)")$getElementText())
fondeobanc <- c(fondeobanc,unlist(remDr$findElement("css selector","#contenedortabla > table:nth-child(3) > tbody > tr:nth-child(11) > td:nth-child(6)")$getElementText()))
fondeobanc <- c(fondeobanc,unlist(remDr$findElement("css selector","#contenedortabla > table:nth-child(3) > tbody > tr:nth-child(10) > td:nth-child(6)")$getElementText()))
fondeobanc <- c(fondeobanc,unlist(remDr$findElement("css selector","#contenedortabla > table:nth-child(3) > tbody > tr:nth-child(9) > td:nth-child(6)")$getElementText()))
fondeobanc <- c(fondeobanc,unlist(remDr$findElement("css selector","#contenedortabla > table:nth-child(3) > tbody > tr:nth-child(8) > td:nth-child(6)")$getElementText()))
fondeobanc <- c(fondeobanc,unlist(remDr$findElement("css selector","#contenedortabla > table:nth-child(3) > tbody > tr:nth-child(7) > td:nth-child(7)")$getElementText()))

#Data frames
objetivo <- data.frame(id=rep("Tasa-Banxico",length(fechas)),fecha = fechas,nivel = tasaobjetivo,stringsAsFactors = FALSE)
fondeog <- data.frame(id=rep("Fondeo-GuberMX",length(fechas)),fecha = fechas,nivel = fondeoguber,stringsAsFactors = FALSE)
fondeog <- fondeog %>% filter(nivel != "N/E")
fondeob <- data.frame(id=rep("Fondeo-BancarioMX",length(fechas)),fecha = fechas,nivel = fondeobanc,stringsAsFactors = FALSE)
fondeob <- fondeob %>% filter(nivel != "N/E")
#### UDIs ####
remDr$navigate("http://www.banxico.org.mx/SieInternet/consultarDirectorioInternetAction.do?sector=8&accion=consultarCuadro&idCuadro=CP150&locale=es")
Sys.sleep(2)
remDr$findElement("css selector", "#formatoCSV")$clickElement()
Sys.sleep(5)

#### Metiendo a la base de datos ####
numdatos <- 20
udis <- tail(read.csv(dir('C:/Users/MATREJO/Downloads', full.names=T, pattern="^Consulta_"),sep = ' '),numdatos)
#UDI's
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
#Tasa de Fondeo Gubernamental
for(i in seq(1,length(fondeog$id))){
  query1 <- paste0("SELECT id, fecha FROM tasas WHERE id = 'Fondeo-GuberMX' AND fecha ='",fondeog$fecha[i],"'")
  rate <- dbGetQuery(mydb,query1)
  if(length(rate$id) == 0){
    query2 <- paste0("INSERT INTO tasas (id,fecha,nivel) VALUES ('",paste(fondeog[i,],collapse = "','"),"')")
    dbSendQuery(mydb,query2)
  }
}
#Tasa de Fondeo Bancario
for(i in seq(1,length(fondeob$id))){
  query1 <- paste0("SELECT id, fecha FROM tasas WHERE id = 'Fondeo-BancarioMX' AND fecha ='",fondeob$fecha[i],"'")
  rate <- dbGetQuery(mydb,query1)
  if(length(rate$id) == 0){
    query2 <- paste0("INSERT INTO tasas (id,fecha,nivel) VALUES ('",paste(fondeob[i,],collapse = "','"),"')")
    dbSendQuery(mydb,query2)
  }
}
#Tasa Objetivo
for(i in seq(1,length(objetivo$id))){
  query1 <- paste0("SELECT id, fecha FROM tasas WHERE id = 'Tasa-Banxico' AND fecha ='",objetivo$fecha[i],"'")
  rate <- dbGetQuery(mydb,query1)
  if(length(rate$id) == 0){
    query2 <- paste0("INSERT INTO tasas (id,fecha,nivel) VALUES ('",paste(objetivo[i,],collapse = "','"),"')")
    dbSendQuery(mydb,query2)
  }
}
remDr$close()
rD$server$stop()
file.remove(dir('C:/Users/MATREJO/Downloads', full.names=T, pattern="^Consulta_"))
