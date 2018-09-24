###############################################################
#                                                             #
#                Banxico - Banco de México                    #
#                                                             #
###############################################################
rD <- rsDriver(port = 4568L)
remDr <- rD$client

#### Tasa objetivo y fondeo gubernamental ####
remDr$navigate("http://www.banxico.org.mx/SieInternet/consultarDirectorioInternetAction.do?accion=consultarCuadro&idCuadro=CF300&sector=18&locale=es")
Sys.sleep(2)
remDr$findElement("css selector", "#seleccionaTodasSeries")$clickElement()
Sys.sleep(2)
remDr$findElement("css selector", "#nodo_1_12 > td:nth-child(2)")$clickElement()
remDr$findElement("css selector", "#nodo_1_13 > td:nth-child(2)")$clickElement()
remDr$findElement("css selector", "#nodo_1_14 > td:nth-child(2)")$clickElement()
remDr$findElement("css selector", "#nodo_1_15 > td:nth-child(2)")$clickElement()
remDr$findElement("css selector", "#idTablaExportacion > tbody > tr > td:nth-child(5)")$clickElement()
Sys.sleep(5)
file.rename(dir('C:/Users/MATREJO/Downloads', full.names=T, pattern="^Consulta_"),'C:/Users/MATREJO/Downloads/cetes.csv')

remDr$findElement("css selector", "#seleccionaTodasSeries")$clickElement()
Sys.sleep(2)
remDr$findElement("css selector", "#seleccionaTodasSeries")$clickElement()
Sys.sleep(2)
remDr$findElement("css selector", "#nodo_1_0 > td:nth-child(2)")$clickElement()
remDr$findElement("css selector", "#nodo_1_1 > td:nth-child(2)")$clickElement()
remDr$findElement("css selector", "#nodo_1_2 > td:nth-child(2)")$clickElement()
remDr$findElement("css selector", "#nodo_1_3 > td:nth-child(2)")$clickElement()
remDr$findElement("css selector", "#idTablaExportacion > tbody > tr > td:nth-child(5)")$clickElement()
Sys.sleep(5)
file.rename(dir('C:/Users/MATREJO/Downloads', full.names=T, pattern="^Consulta_"),'C:/Users/MATREJO/Downloads/cetes28.csv')


remDr$findElement("css selector", "#seleccionaTodasSeries")$clickElement()
Sys.sleep(2)
remDr$findElement("css selector", "#seleccionaTodasSeries")$clickElement()
Sys.sleep(2)
remDr$findElement("css selector", "#nodo_4_0 > td:nth-child(2)")$clickElement()
remDr$findElement("css selector", "#nodo_4_1 > td:nth-child(2)")$clickElement()
remDr$findElement("css selector", "#nodo_4_2 > td:nth-child(2)")$clickElement()
remDr$findElement("css selector", "#nodo_4_3 > td:nth-child(2)")$clickElement()
remDr$findElement("css selector", "#idTablaExportacion > tbody > tr > td:nth-child(5)")$clickElement()
Sys.sleep(5)
file.rename(dir('C:/Users/MATREJO/Downloads', full.names=T, pattern="^Consulta_"),'C:/Users/MATREJO/Downloads/bonos03.csv')

remDr$findElement("css selector", "#seleccionaTodasSeries")$clickElement()
Sys.sleep(2)
remDr$findElement("css selector", "#seleccionaTodasSeries")$clickElement()
Sys.sleep(2)
remDr$findElement("css selector", "#nodo_4_4 > td:nth-child(2)")$clickElement()
remDr$findElement("css selector", "#nodo_4_5 > td:nth-child(2)")$clickElement()
remDr$findElement("css selector", "#nodo_4_6 > td:nth-child(2)")$clickElement()
remDr$findElement("css selector", "#nodo_4_7 > td:nth-child(2)")$clickElement()
remDr$findElement("css selector", "#idTablaExportacion > tbody > tr > td:nth-child(5)")$clickElement()
Sys.sleep(5)
file.rename(dir('C:/Users/MATREJO/Downloads', full.names=T, pattern="^Consulta_"),'C:/Users/MATREJO/Downloads/bonos35.csv')


#Añadiendo a la base
datos_ <- 20 #Número de datos para añadir a la base, tomar en cuenta que un archivo grande tomará mucho tiempo
#CETES 364
benchmarks <- read.csv('C:/Users/MATREJO/Downloads/cetes.csv',skip = 17,stringsAsFactors = FALSE)
colnames(benchmarks) <- c("fecha","dias","precio_limpio","precio_sucio","tasa")
benchmarks$id <- "CETES-364"
benchmarks$fecha <- as.character(as.Date(benchmarks$fecha,format = "%d/%m/%Y"))
benchmarks <- benchmarks[,c('id','fecha','dias','precio_sucio','precio_limpio','tasa')]
benchmarks <- tail(benchmarks,datos_)
for(i in seq(1,length(benchmarks$fecha),1)){
  query <- paste0("SELECT id, fecha FROM indices WHERE id = 'CETES-364' AND fecha ='",benchmarks$fecha[i],"'")
  rate <- dbGetQuery(mydb,query)
  if(length(rate$id) == 0){
    query2 <- paste0("INSERT INTO indices (id,fecha,dias,precio_sucio,precio_limpio,tasa) VALUES ('",
                     paste(benchmarks[i,],collapse = "','"),"')")
    dbSendQuery(mydb,query2)
  }
}
#CETES 28
benchmarks <- read.csv('C:/Users/MATREJO/Downloads/cetes28.csv',skip = 17,stringsAsFactors = FALSE)
colnames(benchmarks) <- c("fecha","dias","precio_limpio","precio_sucio","tasa")
benchmarks$id <- "CETES-28"
benchmarks$fecha <- as.character(as.Date(benchmarks$fecha,format = "%d/%m/%Y"))
benchmarks <- benchmarks[,c('id','fecha','dias','precio_sucio','precio_limpio','tasa')]
benchmarks <- tail(benchmarks,datos_)
for(i in seq(1,length(benchmarks$fecha),1)){
  query <- paste0("SELECT id, fecha FROM indices WHERE id = 'CETES-28' AND fecha ='",benchmarks$fecha[i],"'")
  rate <- dbGetQuery(mydb,query)
  if(length(rate$id) == 0){
    query2 <- paste0("INSERT INTO indices (id,fecha,dias,precio_sucio,precio_limpio,tasa) VALUES ('",
                     paste(benchmarks[i,],collapse = "','"),"')")
    dbSendQuery(mydb,query2)
  }
}
#MBONOS 0-3
benchmarks <- read.csv('C:/Users/MATREJO/Downloads/bonos03.csv',skip = 17,stringsAsFactors = FALSE)
colnames(benchmarks) <- c("fecha","dias","precio_limpio","precio_sucio","cupon")
benchmarks$id <- "MBONOS-0-3"
benchmarks$fecha <- as.character(as.Date(benchmarks$fecha,format = "%d/%m/%Y"))
benchmarks <- benchmarks[,c('id','fecha','dias','precio_sucio','precio_limpio','cupon')]
benchmarks <- tail(benchmarks,datos_)
benchmarks$tasa <- mapply(yield_to_maturity,as.numeric(benchmarks$dias),as.numeric(benchmarks$cupon),as.numeric(benchmarks$precio_limpio))
benchmarks[,c(6,7)] <- benchmarks[,c(7,6)]
for(i in seq(1,length(benchmarks$fecha),1)){
  query <- paste0("SELECT id, fecha FROM indices WHERE id = 'MBONOS-0-3' AND fecha ='",benchmarks$fecha[i],"'")
  rate <- dbGetQuery(mydb,query)
  if(length(rate$id) == 0){
    query2 <- paste0("INSERT INTO indices (id,fecha,dias,precio_sucio,precio_limpio,tasa,cupon) VALUES ('",
                     paste(benchmarks[i,],collapse = "','"),"')")
    dbSendQuery(mydb,query2)
  }
}
#MBONOS 3-5
benchmarks <- read.csv('C:/Users/MATREJO/Downloads/bonos35.csv',skip = 17,stringsAsFactors = FALSE)
colnames(benchmarks) <- c("fecha","dias","precio_limpio","precio_sucio","cupon")
benchmarks$id <- "MBONOS-3-5"
benchmarks$fecha <- as.character(as.Date(benchmarks$fecha,format = "%d/%m/%Y"))
benchmarks <- benchmarks[,c('id','fecha','dias','precio_sucio','precio_limpio','cupon')]
benchmarks <- tail(benchmarks,datos_)
benchmarks$tasa <- mapply(yield_to_maturity,as.numeric(benchmarks$dias),as.numeric(benchmarks$cupon),as.numeric(benchmarks$precio_limpio))
benchmarks[,c(6,7)] <- benchmarks[,c(7,6)]
for(i in seq(1,length(benchmarks$fecha),1)){
  query <- paste0("SELECT id, fecha FROM indices WHERE id = 'MBONOS-3-5' AND fecha ='",benchmarks$fecha[i],"'")
  rate <- dbGetQuery(mydb,query)
  if(length(rate$id) == 0){
    query2 <- paste0("INSERT INTO indices (id,fecha,dias,precio_sucio,precio_limpio,tasa,cupon) VALUES ('",
                     paste(benchmarks[i,],collapse = "','"),"')")
    dbSendQuery(mydb,query2)
  }
}
Sys.sleep(2)

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
Sys.sleep(6)

#### Metiendo a la base de datos ####
numdatos <- 20
benchmarks <- read.csv(dir('C:/Users/MATREJO/Downloads', full.names=T, pattern="^Consulta_"))

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

file.remove(dir('C:/Users/MATREJO/Downloads', full.names=T, pattern="^Consulta_"))

remDr$close()
rD$server$stop()
