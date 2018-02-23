#Funciones
source("funciones.R",local=FALSE)

###########################################################################
#Librerías
###########################################################################

library(xlsx)
library(readxl)
library(dplyr)
library(reshape2)
library(tidyr)
library(RMySQL)
library(FundTools)
library(RSelenium)
library(rdrop2)
library(rsconnect)

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
  a <- paste0("//192.168.0.223/vectordeprecios","/",year,"/",month," ",year,"/",day,"/","CA_VectorAnalitico",clave,".txt")
  archivo <- read.delim(a,header=TRUE,sep="|")
  d <- paste0("//192.168.0.223/vectordeprecios","/",year,"/",month," ",year,"/",day,"/",clave,"_24.xls")
  referencia <- read_excel(d,sheet = "VECTOR")
  metamorphosis(archivo,referencia)
  
  #Obtiene las posiciones de los contratos 50901, 50903, 50905 y 50499
  system(paste0('"C:/Program Files (x86)/FIDEM/AM/AmConsola.exe" "2|cism_batch|Nueva2015|4|',
                50499,',',clave,',',clave,'|C:/Github/Carteras_Portafolios|IMSS|"'))
  p1 <- read_xls('C:/Github/Carteras_Portafolios/IMSS.xls')
  portafolios(p1)
  system(paste0('"C:/Program Files (x86)/FIDEM/AM/AmConsola.exe" "2|cism_batch|Nueva2015|4|',
                50901,",",clave,",",clave,'|C:/Github/Carteras_Portafolios|Conservador|"'))
  p2 <- read_xls('C:/Github/Carteras_Portafolios/Conservador.xls')
  portafolios(p2)
  system(paste0('"C:/Program Files (x86)/FIDEM/AM/AmConsola.exe" "2|cism_batch|Nueva2015|4|',
                50903,",",clave,",",clave,'|C:/Github/Carteras_Portafolios|Moderado|"'))
  p3 <- read_xls('C:/Github/Carteras_Portafolios/Moderado.xls')
  portafolios(p3)
  system(paste0('"C:/Program Files (x86)/FIDEM/AM/AmConsola.exe" "2|cism_batch|Nueva2015|4|',
                50905,",",clave,",",clave,'|C:/Github/Carteras_Portafolios|Agresivo|"'))
  p4 <- read_xls('C:/Github/Carteras_Portafolios/Agresivo.xls')
  portafolios(p4)
  
  #Arreglando las carteras de los fondos
  car <- read_xls(paste0("C:/Github/Carteras/(Todos)Cartera_0_",gsub("-","",day),".xls"))
  carteras(car,date)
  
  #Scrapeando el FRED, Banxico, INEGI y Valmer
  source("FRED_scraper.R",local = FALSE)
  source("Banxico_scraper.R",local = FALSE)
  if(substr(date,9,10) == 22)
    source("INEGI_scraper.R",local = FALSE)
  source("crawlerBenchmarks.R",local = FALSE)
  
  #Alimenta la base de datos de tasas
  b <-  paste0("//192.168.0.223/vectordeprecios","/",year,"/",month," ",year,"/",day,"/","tiies_",clave,".txt")
  nodo <- read.delim(b,header=TRUE)
  bonos <- read_excel(d,sheet = "Curva Bonos")
  ums <- read_excel(d,sheet = "UMS")
  cetes <- read_excel(d,sheet = "Curva_Cetes") 
  libor <- read_excel(d,sheet = "Curva Libor")
  reales <- read_excel(d,sheet = "Tasa Real Bruta")
  nodo <- nodo %>% filter(clave == "TIIE")
  nodos(date,nodo,bonos,cetes,ums,libor,reales)
  
  #Generando archivos para comparador de fondos
  comparador(date)
  deployApp('C:/Github/ComparadorFondos',appName = 'comparadorfondos',launch.browser = FALSE)
  y
  
}else{
  cat("The day has no price or bond information!!!")
}
dbDisconnect(mydb)
