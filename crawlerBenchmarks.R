###############################################################
#                                                             #
#                    Valmer - Benchmarks                      #
#                                                             #
###############################################################

###Obteniendo el archivo de los benchamrks ####
file.remove("C:/Users/MATREJO/Downloads/Benchmarks_SP_Historico_24H.csv") #se borra de descargas 

#Getting into Valmer page
rD <- rsDriver(port = 4568L)
remDr <- rD$client
remDr$navigate("http://www.valmer.com.mx/")
Sys.sleep(5)

#Login
remDr$findElement("class", "ingresarBtn")$clickElement()
remDr$findElement("id", "emailUsuario")$sendKeysToElement(list(keys[6]))
remDr$findElement("id", "contrasenaUsuario")$sendKeysToElement(list(keys[7]))
remDr$findElement("id", "btnIngresar")$clickElement()
Sys.sleep(15)

#### Descarga los archivos de Benchmarks ####
remDr$findElement("id", "lnkBox_395")$clickElement()
Sys.sleep(1)
remDr$findElement("css selector","#nivel1_1443 > span")$clickElement()
Sys.sleep(20)
enterHist<- remDr$findElement("css selector","body > div.nav-area > div > div > ul > li:nth-child(2) > a")
remDr$mouseMoveToLocation(webElement = enterHist)
remDr$findElement("css selector","body > div.nav-area > div > div > ul > li:nth-child(2) > div > ul > li:nth-child(2) > a")$clickElement()
Sys.sleep(40)
remDr$navigate("http://www.valmer.com.mx/en/valmer/bloques_cerrados")
Sys.sleep(10)

#### Cierra sesión ####
remDr$findElement("id", "lnkCerrarSesion")$clickElement()
remDr$findElement("id", "btnCerrarSesionActual")$clickElement()

remDr$close()
rD$server$stop()

#### Metiendo los elementos a la base de datos ####
### Función que alimenta la tabla de prices con los benchmarks del CIGUMP y el CIGULP
datosbench <- read.csv("C:/Users/MATREJO/Downloads/Benchmarks_SP_Historico_24H.csv")
datosbench$FECHA <- paste0(substr(datosbench$FECHA,1,4),"-",substr(datosbench$FECHA,5,6),"-",
                           substr(datosbench$FECHA,7,8))
df_bench <- data.frame(Fecha=datosbench$FECHA,
                       'RC-MBONOS-1-3-YEAR-IND' = datosbench$S.P.BMV.Sovereign.MBONOS.1.3.Year.Bond.Index,
                       'RC-MBONOS-3-5-YEAR-IND' = datosbench$S.P.BMV.Sovereign.MBONOS.3.5.Year.Bond.Index)
df_bench <- melt(df_bench,id.vars = 'Fecha',variable.name = 'Benchmark')
df_benc <- data.frame(id=as.character(df_bench$Benchmark),Fecha=as.character(df_bench$Fecha),Meracdo='MD',
                      Precio1 = as.character(df_bench$value),Precio2 = as.character(df_bench$value),Tasa=0,
                      stringsAsFactors = FALSE)
df_benc$id <- ifelse(df_benc$id == 'RC.MBONOS.1.3.YEAR.IND','RC-MBONOS-1-3-YEAR-IND','RC-MBONOS-3-5-YEAR-IND')
for(i in seq(1,length(df_benc$Fecha),1)){
  query <- paste0("SELECT id FROM prices WHERE id='",df_benc$id[i],"' AND fecha='",df_benc$Fecha[i],"'")
  elemento <- dbGetQuery(mydb,query)
  if(length(elemento$id) == 0){
    query2 <- paste0("INSERT INTO prices (id,fecha,Mercado,Precio_sucio,Precio_limpio,Tasa) VALUES ('",
                     paste(df_benc[i,],collapse = "','"),"')")
    dbSendQuery(mydb,query2)
  }
}

