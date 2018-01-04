#Funciones
source("funciones.R",local=FALSE)

###########################################################################
#Librerías
###########################################################################

library(xlsx)
library(readxl)
library(dplyr)
library(tidyr)
library(RMySQL)
library(FundTools)
library(RSelenium)
library(rdrop2)

#Doesn't allow scientific notation
options(scipen = 999)

###########################################################################
#Obteniendo los datos del día
###########################################################################

#Connection with de DB.
keys <- readLines('C:/Github/Seeker/keys2.txt')
mydb = dbConnect(MySQL(), user=keys[1], password=keys[2], dbname='mydb', host=keys[3])

#Dias festivos
festivos <- drop_read_csv('Carpeta del equipo CIEstrategias/festivos.csv',header=TRUE,stringsAsFactors = FALSE)
festivos$dias <- as.Date(festivos$dias,format="%d/%m/%Y")
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
  
  #Generando el archivo de Fondos
  c <- paste0("C:/Github/Carteras/(Todos)Cartera_0_",gsub("-","",day),".xls")
  archivo <- read.xlsx(c,sheetName = "Cartera",header=TRUE)
  carteras(archivo)
  
  #Alimenta la base de datos de tasas
  b <-  paste0("//192.168.0.223/VECTORPRECIOS","/",year,"/",month," ",year,"/",day,"/","tiies_",clave,".txt")
  nodo <- read.delim(b,header=TRUE)
  nodo <- nodo %>% filter(clave != "UDI")
  nodos(nodo)
  
  #Scrapeando el FRED, Banxico e INEGI
  source("FRED_scraper.R",local = FALSE)
  source("Banxico_scraper.R",local = FALSE)
  source("INEGI_scraper.R",local = FALSE)
  
}else{
  cat("The day has no price or bond information!!!")
}
dbDisconnect(mydb)
