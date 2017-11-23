#Function that determines if today is working day
diah <-  function(fecha){
  dia <- "Habil"
  fechabase0 <- as.Date("2017-08-06")
  if(as.integer(fecha - fechabase0 ) %% 7 == 6){dia <- "Inhabil"}
  if(as.integer(fecha - fechabase0 ) %% 7 == 0){dia <- "Inhabil"}
  return(dia)
}

curvas <- function(archivo,tipo=NULL){
  if(tipo %in% c("bonos","cetes")){
    if(tipo == "bonos"){
      df <- data.frame(Plazo=archivo$Plazo,Tasa.Nominal=archivo$Tasa.Bruta)
      w <- "C:/Github/Curvas/Curva_bonos.csv"
      write.csv(df,w,row.names = FALSE)
    }
    if(tipo == "cetes"){
      df <- data.frame(Plazo=archivo$DP.1,Tasa.Nominal=archivo$Tasa.Rendimiento.I)
      w <- "C:/Github/Curvas/Curva_cetes.csv"
      write.csv(df,w,row.names = FALSE)
    }
  } else {
    cat("No se puede definir el tipo de curva")
    df <- c()
  }
}

nodos <- function(archivo){
  clave <- substr(archivo$clave,1,1)
  id <- c()
  
  for (i in seq(1,length(clave),1)){
    if(clave[i] == "U" | clave[i] == "M")
      id <- c(id,"UDI-1")
    if(clave[i] == "C")
      id <- c(id,paste0("CETES-",archivo$plazo[i]))
    if(clave[i] == "T")
      id <- c(id,paste0("TIIE-",archivo$plazo[i]))
  }
  
  #Query
  query <- paste0("INSERT INTO nodos ","(id, fecha, nivel)"," VALUES ",
                  paste(paste(sprintf("('%s','%s','%s')",id,archivo$fecha,archivo$valor_cierre), 
                              collapse = ",")))
  cat("Se agregaron los siguientes nodos: ",paste(id),collapse=",")
  dbSendQuery(mydb,query)
}

###########################################################################
#Código para descargar los archivos a la fecha
###########################################################################

library(dplyr)
library(RMySQL)

###########################################################################
#Date to get the archive.
###########################################################################

#Connection with de DB.

keys <- readLines('keys2.txt')
mydb = dbConnect(MySQL(), user=keys[1], password=keys[2], dbname='mydb', host=keys[3])

date <- Sys.Date()-1
if(diah(date) == "Habil"){
  year <- format(as.Date(date, format="%d/%m/%Y"),"%Y")
  month <- months(as.Date(date, format="%d/%m/%Y"))
  month <- paste(toupper(substr(month, 1, 1)), substr(month, 2, nchar(month)), sep="")
  day <- format(as.Date(date, format="%d/%m/%Y"),"%d-%m-%Y")
  clave <- format(as.Date(date, format="%d/%m/%Y"),"%Y%m%d")
  
  #Alimenta el archivo de curva de Bonos M
  b <- paste0("\\\\192.168.0.223\\VECTORPRECIOS","\\",year,"\\",month," ",year,"\\",day,"\\","curva_bonos_",clave,".txt")
  curva_bonos <- read.delim(b,header=TRUE)
  curvas(curva_bonos,"bonos")
  
  #Alimenta el archivo de curva de CETES
  c <-  paste0("\\\\192.168.0.223\\VECTORPRECIOS","\\",year,"\\",month," ",year,"\\",day,"\\","curva_cetes_",clave,".txt")
  curva_cetes <- read.delim(c,header=TRUE)
  curvas(curva_cetes,"cetes")
  
  #Alimenta la base de datos de nodos (TIIE, CETES y UDI's)
  d <-  paste0("\\\\192.168.0.223\\VECTORPRECIOS","\\",year,"\\",month," ",year,"\\",day,"\\","tiies_",clave,".txt")
  nodo <- read.delim(d,header=TRUE)
  nodos(nodo)
  
}else{
  cat("The day has no price or bond information!!!")
}
dbDisconnect(mydb)
