#Function that determines if today is working day
diah <-  function(fecha){
  dia <- "Habil"
  fechabase0 <- as.Date("2017-08-06")
  if(as.integer(fecha - fechabase0 ) %% 7 == 6){dia <- "Inhabil"}
  if(as.integer(fecha - fechabase0 ) %% 7 == 0){dia <- "Inhabil"}
  return(dia)
}
#Function that finds which bonds are not in the DB already.
findbond <- function(id){
  
  nombres <- c()
  for (x in id){
    query <- paste0("SELECT id FROM bonds WHERE id = '",x,"'")
    dato <- dbGetQuery(mydb,query)
    if(length(dato$id) == 0){
      nombres <- c(nombres,x)
    }
  }
  return(nombres)
}

minimos <- function(renglon){
  return(min(renglon,na.rm=TRUE))
}
#Grades based on Moodys, S&P, Fitch and HR Ratings
calificacion <- function(moodys,sp,fitch,hr){
  moodys <- as.character(moodys)
  sp <- as.character(sp)
  fitch <- as.character(fitch)
  hr <- as.character(hr)
  datos <- c(moodys,sp,fitch,hr)
  valores <- c()
  
  for (x in datos){
    query <- paste0("SELECT Valor FROM calificaciones WHERE Calificadora = '",x,"'")
    val <- dbGetQuery(mydb,query)
    if (length(val$Valor)==0){
      valores <- c(valores,0)
    } else {
      if(length(val$Valor)>1){
        valores <- c(valores,val$Valor[1])
      } else {
        valores <- c(valores,val$Valor)
      }
    }
  }
  valores <- ifelse(valores==0,NA,valores)
  df <- data.frame(matrix(valores,ncol=4,byrow = FALSE))
  calif <- apply(df,1,minimos)
  
  valor <- read.csv("datos.csv",header=FALSE)
  calificaciones <- as.character(valor$V2[match(calif,valor$V1)])
  calificaciones <- ifelse(is.na(calificaciones)==TRUE,"-",calificaciones)
  return(calificaciones)
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

metamorphosis <- function(archivo){
  
  #Archivo must be a txt file
  
  ###########################################################################
  #Data of prices
  ###########################################################################
  
  #Date of the data
  dia <- as.Date(as.character(archivo$FECHA[1]),"%Y%m%d")
  fecha <- as.character(as.Date(as.character(archivo$FECHA),"%Y%m%d"))
  #Type of market
  mercado <- as.character(archivo$MERCADO)
  #Code
  valor <- paste0(archivo$TIPO.VALOR,"-",archivo$EMISORA,"-",archivo$SERIE)
  #Price 
  precio <- archivo$PRECIO.SUCIO
  
  #Writing the new file for prices
  df1 <- data.frame(cbind(fecha,mercado,valor,precio))
  colnames(df1) <- c("Fecha","Mercado","Instrumento","Precio")
  
  query <- paste0("INSERT INTO prices ","(Fecha, Mercado, id, Precio)"," VALUES ",
                  paste(paste(sprintf("('%s','%s','%s','%s')",df1$Fecha,df1$Mercado,df1$Instrumento,df1$Precio), 
                              collapse = ",")))
  dbSendQuery(mydb,query)
  
  ###########################################################################
  #Data of bonds
  ###########################################################################
  
  especiales <- c("BI","I")
  datos <- archivo %>% filter(archivo$MERCADO=="MD" & archivo$TASA.CUPON>0 | archivo$TIPO.VALOR %in% especiales)
  #id
  datos$id <- paste0(datos$TIPO.VALOR,"-",datos$EMISORA,"-",datos$SERIE)
  #Buscando los id's en la base de datos
  nombres <- findbond(datos$id)
  #Agregando los nuevos bonos (si hay nuevos)
  if(length(nombres) != 0){
    #Finding the bonds
    bonos <- datos %>% filter(datos$id %in% nombres)
    #dates
    emision <- as.Date(bonos$FECHA.EMISION,format="%d/%m/%Y")
    emision <- ifelse(is.na(emision)==TRUE,"1900-01-01",as.character(emision))
    vencimiento <- as.Date(as.character(bonos$FECHA.VCTO),format="%d/%m/%Y")
    #frequency of coupons
    frequency <- c()
    for (i in seq(1,length(bonos$FREC..CPN),1)){
      if(is.na(bonos$FREC..CPN[i])==TRUE){
        plazo <- vencimiento[i] - Sys.Date()
      } else {
        plazo <- unlist(strsplit(as.character(bonos$FREC..CPN[i])," "))[2]
      }
      frequency <- c(frequency,plazo)
    }
    #Query
    query <- paste0("INSERT INTO bonds ","(id, FechaEmision,FechaVencimiento,TasaCupon,TipoTasa,SobreTasa,Frecuencia)"," VALUES ",
                    paste(paste(sprintf("('%s','%s','%s','%s','%s','%s','%s')",bonos$id,emision,vencimiento,
                                        bonos$TASA.CUPON,bonos$REGLA.CUPON,bonos$SOBRETASA,frequency), 
                                collapse = ",")))
    dbSendQuery(mydb,query)
    cat("Se agregaron los siguientes bonos: ",paste(nombres),collapse=",")
  } else {cat("No se agregaron bonos")}
  
  #Excel archive of instruments
  df3 <- data.frame(cbind(TipoValor = as.character(archivo$TIPO.VALOR),Emisora = as.character(archivo$EMISORA),
                          Serie = as.character(archivo$SERIE), id = as.character(valor),
                          Moodys=as.character(archivo$MDYS),SP=as.character(archivo$S.P),
                          Fitch=as.character(archivo$CALIFICACION.FITCH)
                          ,HR=as.character(archivo$HR.RATINGS)))
  df3$Calificacion <- calificacion(df3$Moodys,df3$SP,df3$Fitch,df3$HR)
  c <- "C:/Github/Simulador/Instrumentos.csv"
  write.csv(df3,c,row.names = FALSE)
}

###########################################################################
#Código para descargar los archivos a la fecha
###########################################################################

library(xlsx)
library(readxl)
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
  
  #Alimenta la base de datos de bonos y precios
  a <- paste0("\\\\192.168.0.223\\VECTORPRECIOS","\\",year,"\\",month," ",year,"\\",day,"\\","CA_VectorAnalitico",clave,".txt")
  archivo <- read.delim(a,header=TRUE,sep="|")
  metamorphosis(archivo)
  
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
