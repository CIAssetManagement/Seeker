#Function that determines if today is working day
diah <-  function(fecha){
  dia <- "Habil"
  fechabase0 <- as.Date("2017-08-06")
  if(as.integer(fecha - fechabase0 ) %% 7 == 6){dia <- "Inhabil"}
  if(as.integer(fecha - fechabase0 ) %% 7 == 0){dia <- "Inhabil"}
  return(dia)
}

elemento <- function(tv,emisora,serie){
  #Emisoras that won't be found and needn't to be found
  exceptions <- c("CASITA","TOTALES")
  busqueda <- paste0(tv,"-",emisora,"-",serie)
  query <- paste0("SELECT id FROM prices WHERE id = '",busqueda,"'")
  element <- dbGetQuery(mydb, query)
  if(length(element$id) > 0 | emisora %in% exceptions | tv == "CHD"){
    return(as.character(serie))
  } else {
    return(elemento(tv,emisora,paste0("0",serie)))
  }
}


carteras <- function(archivo){
  
  datos <- archivo %>% filter(Fondo != " " & Fondo != "Fondo")
  df <- datos[,c(1,2,3,4,5,9,10)]
  colnames(df) <- c("I","Fondo","TV","Emisora","Serie","Titulos","Costo.Total")
  
  #Function to assure the instruments are right(Covaf does it wrong cause they use xlsx files, not csv)
  serie <- mapply(elemento,df$TV,df$Emisora,df$Serie)
  df$Serie <- mapply(elemento,df$TV,df$Emisora,df$Serie)
  
  df$Costo.Total <- as.numeric(as.character(df$Costo.Total))
  id <- paste0(df$TV,"-",df$Emisora,"-",df$Serie)
  for(i in seq(1,length(id),1)){
    if(!(df$TV[i] %in% c(" ","CHD") | id[i]=="0-CASITA-*")){
      df$Costo.Total[i] <- round(as.numeric(as.character(df$Titulos[i]))*get_prices(Sys.Date()-1,id[i])[1,2],digits=2)
    }
  }
  for(x in unique(df$Fondo)){
    monto <- df %>% filter(Fondo == x & Emisora != "TOTALES") %>% summarise(sum(unlist(Costo.Total)))
    indice1 <- df$Fondo %in% x
    indice2 <- df$Emisora %in% "TOTALES"
    indices <- ifelse(indice1 == TRUE, indice2,indice1)
    df$Costo.Total[indices] <- monto
  }
  df <- data.frame(cbind(I=as.character(df$I),Fondo=as.character(df$Fondo),TV=as.character(df$TV),
                         Emisora=as.character(df$Emisora),Serie=as.character(df$Serie),
                         Titulos=as.numeric(as.character(df$Titulos)),
                         CostoTotal=as.numeric(as.character(df$Costo.Total))))
  
  
  write.xlsx(df,"C:/Github/Simulador/Fondos.xlsx",row.names=FALSE)
  
}

###########################################################################
#Código para descargar los archivos a la fecha
###########################################################################

library(xlsx)
library(readxl)
library(dplyr)
library(RMySQL)
library(FundTools)

#Doesn't allow scientific notation
options(scipen = 999)

###########################################################################
#Date to get the archive.
###########################################################################

#Connection with de DB.

keys <- readLines('keys2.txt')
mydb = dbConnect(MySQL(), user=keys[1], password=keys[2], dbname='mydb', host=keys[3])

date <- Sys.Date()-1
if(diah(date) == "Habil"){
  
  year <- format(as.Date(date, format="%d/%m/%Y"),"%Y")
  month <- format(as.Date(date, format="%d/%m/%Y"),"%m")
  day <- format(as.Date(date, format="%d/%m/%Y"),"%d")
  b <- paste0("C:/Github/Carteras/(Todos)Cartera_0_",day,month,year,".xls")
  archivo <- read.xlsx(b,sheetName = "Cartera",header=TRUE)
  
  #Generando el archivo de Fondos
  carteras(archivo)
  
}else{
  cat("The day has no price or bond information!!!")
}
dbDisconnect(mydb)
