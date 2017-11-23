#Function that determines if today is working day
diah <-  function(fecha){
  dia <- "Habil"
  fechabase0 <- as.Date("2017-08-06")
  if(as.integer(fecha - fechabase0 ) %% 7 == 6){dia <- "Inhabil"}
  if(as.integer(fecha - fechabase0 ) %% 7 == 0){dia <- "Inhabil"}
  return(dia)
}
#Descartando la notación científica
options(scipen = 999)

comparableacciones <- function(vector,instrumentos){
  tv <- instrumentos %>% filter(TIPO.VALOR == as.character(unlist(vector[1])))
  datos <- tv
  if(length(tv$id) > 1){
    sector <- tv %>% filter(SECTOR == as.character(unlist(vector[4])))
    
    if(length(sector$id) == 1)
      return(sector$id)
    
    if(length(sector$id) > 1){
      bursatilidad <- sector %>% filter(BURSATILIDAD == as.character(unlist(vector[9])))
      
      if(length(bursatilidad$id) > 1)
        datos <- bursatilidad
      
      if(length(bursatilidad$id) == 1)
        return(bursatilidad$id)
      
    } else {
      if(length(sector$id) == 1)
        return(sector$id)
    }
  }
  minimos <- abs(datos$MONTO.EMITIDO - as.numeric(unlist(vector[5])))
  indice <- which(minimos == min(minimos))
  elementos <- datos$id[indice]
  return(elementos[length(elementos)])
}

comparablebonos <- function(vector,instrumentos) {
    tv <- instrumentos %>% filter(TIPO.VALOR == as.character(unlist(vector[1])))
    datos <- tv
    if(length(tv$id)>1){
      emisora <- tv %>% filter(EMISORA == as.character(unlist(vector[2])))
      if(length(emisora$id) > 1)
        datos <- emisora
      
      if(length(emisora$id) == 1)
        return(emisora$id)
      
    } else {
      if(length(tv$id) == 1)
        return(tv$id)
    }
    numero <- sum(ifelse(datos$TASA.CUPON == 0,1,0))/length(datos$TASA.CUPON)
    if(numero == 1){
      minimos <- abs(datos$MONTO.EMITIDO - as.numeric(unlist(vector[5])))
      indice <- which(minimos == min(minimos,na.rm = TRUE))
      elementos <- datos$id[indice]
      return(elementos[length(elementos)])
    } else {
      minimos <- abs(datos$TASA.CUPON - as.numeric(unlist(vector[8])))
      indice <- which(minimos == min(minimos,na.rm = TRUE))
      elementos <- datos$id[indice]
      return(elementos[length(elementos)])
    }
}

comparacion <- function(fondos,instrumentos){
  comparables <- c()
  instrumentos <- instrumentos[c(2,3,4,11,12,14,22,23,39,77)]
  instrumentos2 <- instrumentos[!(instrumentos$id %in% fondos$id),]
  for (i in seq(1,length(fondos$TV),1)){
    fondosc <- instrumentos[instrumentos$id == fondos$id[i],]
    if(length(fondosc$TIPO.VALOR)>0){
      if(fondosc$TIPO.VALOR %in% tvbonos){
        elemento <- comparablebonos(fondosc,instrumentos2)
      } else {
        if(fondosc$TIPO.VALOR %in% tvacciones){
          elemento <- comparableacciones(fondosc,instrumentos2)
        } else {
          elemento <- fondos$id[i]
          }
        } 

    } else {
      elemento <- fondos$id[i]
      }
    
    print.default(c(fondos$id[i],elemento))
    comparables <- c(comparables,elemento)
  }
  return(comparables)
}

library(xlsx)
library(readxl)
library(dplyr)
tvbonos <- c('90','91','92','93','94','95','97','BI','CD',
             'F','IM','IQ','IS','I','LD','M','S')
tvacciones <- c('0','1','3')

date <- Sys.Date()-1
if(diah(date) == "Habil"){
  year <- format(as.Date(date, format="%d/%m/%Y"),"%Y")
  month <- months(as.Date(date, format="%d/%m/%Y"))
  month <- paste(toupper(substr(month, 1, 1)), substr(month, 2, nchar(month)), sep="")
  day <- format(as.Date(date, format="%d/%m/%Y"),"%d-%m-%Y")
  clave <- format(as.Date(date, format="%d/%m/%Y"),"%Y%m%d")
  
  a <- paste0("\\\\192.168.0.223\\VECTORPRECIOS","\\",year,"\\",month," ",year,"\\",day,"\\","CA_VectorAnalitico",clave,".txt")
  instrumentos <- read.delim(a,header=TRUE,sep="|")
  instrumentos$id <- paste0(instrumentos$TIPO.VALOR,"-",instrumentos$EMISORA,"-",instrumentos$SERIE)
  
  fondos <- data.frame(read_xlsx("C:/Github/Simulador/Fondos.xlsx"))
  fondos$id <- paste0(fondos$TV,"-",fondos$Emisora,"-",fondos$Serie)
  
  fondos$Comparable <- comparacion(fondos,instrumentos)
  fondos$id <- NULL
  fondos$Comparable <- ifelse(fondos$Comparable == "NA-TOTALES-NA","",fondos$Comparable)
  fondos[is.na(fondos) == TRUE] <- ""
  write.xlsx(fondos,"C:/Github/Simulador/Fondos.xlsx",row.names=FALSE)
  
}else{
  cat("The day has no price or bond information!!!")
}
