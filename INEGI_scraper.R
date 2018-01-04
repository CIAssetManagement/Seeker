###############################################################
#                                                             #
#    INEGI -Instituto Nacional de Estadística y Geografía     #
#                                                             #
###############################################################
#Funciones
source("funciones.R",local=FALSE)

file.remove(dir('C:/Users/MATREJO/Downloads', full.names=T, pattern="^INP_INP"),sep = ' ')
rD <- rsDriver(port = 4568L)
remDr <- rD$client

#### INPC ####
remDr$navigate("http://www.inegi.org.mx/sistemas/indiceprecios/Estructura.aspx?idEstructura=112000100010&T=%C3%8Dndices%20de%20Precios%20al%20Consumidor&ST=Principales%20%C3%ADndices")
Sys.sleep(2)
remDr$findElement("css selector","#MainContent_wuc_BarraHerramientas1_Panel1 > div > table > tbody > tr > td:nth-child(2) > a:nth-child(4) > img")$clickElement()
Sys.sleep(2)

#### Metiendo a la base de datos ####
numdatos <- 20
inpc <- tail(read.csv(dir('C:/Users/MATREJO/Downloads', full.names=T, pattern="^INP_INP"),sep = ' '),numdatos)
for(x in inpc[,1]){
  datos <- strsplit(as.character(x),",")[[1]]
  fecha <- fechas(datos[1])
  
  #INPC General
  general <- c("INPC-general",fecha,datos[4])
  query1 <- paste0("SELECT id, fecha FROM inpc WHERE id ='",general[1],"' AND fecha = '",general[2],"'")
  ig <- dbGetQuery(mydb,query1)
  if(length(ig$id) == 0){
    query1 <- paste0("INSERT INTO inpc (id,fecha,nivel) VALUES ('",paste(general,collapse = "','"),"')")
    dbSendQuery(mydb,query1)
  }
  
  #INPC Subyacente
  subyacente <- c("INPC-subyacente",fecha,datos[5])
  query2 <- paste0("SELECT id, fecha FROM inpc WHERE id ='",subyacente[1],"' AND fecha = '",subyacente[2],"'")
  ig <- dbGetQuery(mydb,query2)
  if(length(ig$id) == 0){
    query2 <- paste0("INSERT INTO inpc (id,fecha,nivel) VALUES ('",paste(subyacente,collapse = "','"),"')")
    dbSendQuery(mydb,query2)
  }
  
  #INPC No Subyacente
  nosubyacente <- c("INPC-nosubyacente",fecha,datos[6])
  query3 <- paste0("SELECT id, fecha FROM inpc WHERE id ='",nosubyacente[1],"' AND fecha = '",nosubyacente[2],"'")
  ig <- dbGetQuery(mydb,query3)
  if(length(ig$id) == 0){
    query3 <- paste0("INSERT INTO inpc (id,fecha,nivel) VALUES ('",paste(nosubyacente,collapse = "','"),"')")
    dbSendQuery(mydb,query3)
  }
}

remDr$close()
rD$server$stop()
