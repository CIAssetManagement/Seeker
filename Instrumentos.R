#Funciones
source("funciones.R",local=FALSE)

###########################################################################
#Código para descargar los archivos a la fecha
###########################################################################

library(xlsx)
library(readxl)
library(dplyr)
library(tidyr)
library(RMySQL)
library(FundTools)
library(RSelenium)

#Doesn't allow scientific notation
options(scipen = 999)

###########################################################################
#Dia para obtener los archivos a leer
###########################################################################

#Connection with de DB.

keys <- readLines('C:/Github/Seeker/keys2.txt')
mydb = dbConnect(MySQL(), user=keys[1], password=keys[2], dbname='mydb', host=keys[3])

date <- Sys.Date()-1
if(diah(date) == "Habil"){
  year <- format(as.Date(date, format="%d/%m/%Y"),"%Y")
  month <- months(as.Date(date, format="%d/%m/%Y"))
  month <- paste(toupper(substr(month, 1, 1)), substr(month, 2, nchar(month)), sep="")
  day <- format(as.Date(date, format="%d/%m/%Y"),"%d-%m-%Y")
  clave <- format(as.Date(date, format="%d/%m/%Y"),"%Y%m%d")
  
  #Alimenta la base de datos de bonos y precios
  a <- paste0("//192.168.0.223/VECTORPRECIOS","/",year,"/",month," ",year,"/",day,"/","CA_VectorAnalitico",clave,".txt")
  archivo <- read.delim(a,header=TRUE,sep="|")
  metamorphosis(archivo)
  
  #Alimenta la base de datos de tasas
  b <-  paste0("//192.168.0.223/VECTORPRECIOS","/",year,"/",month," ",year,"/",day,"/","tiies_",clave,".txt")
  nodo <- read.delim(b,header=TRUE)
  nodo <- nodo %>% filter(clave != "UDI")
  nodos(nodo)
  
  #Generando el archivo de Fondos
  c <- paste0("C:/Github/Carteras/(Todos)Cartera_0_",gsub("-","",day),".xls")
  archivo <- read.xlsx(c,sheetName = "Cartera",header=TRUE)
  carteras(archivo)
  
  #Scrapeando el FRED, Banxico e INEGI
  rD <- rsDriver(port = 4568L)
  remDr <- rD$client
  source("FRED_scraper.R",local = FALSE)
  source("Banxico_scraper.R",local = FALSE)
  #source("INEGI_scraper.R",local = FALSE)
  remDr$close()
  
}else{
  cat("The day has no price or bond information!!!")
}
dbDisconnect(mydb)
