###############################################################
#                                                             #
#  FRED - FEDERAL RESERVE ECONOMIC DATA (FED de St. Louis)    #
#                                                             #
###############################################################

#### Eliminando los archivos anteriores ####
file.remove("C:/Users/MATREJO/Downloads/USD1MTD156N.csv")
file.remove("C:/Users/MATREJO/Downloads/USD3MTD156N.csv")
file.remove("C:/Users/MATREJO/Downloads/USD6MTD156N.csv")
file.remove("C:/Users/MATREJO/Downloads/USD12MD156N.csv")
file.remove("C:/Users/MATREJO/Downloads/DFEDTARU.csv")
file.remove("C:/Users/MATREJO/Downloads/FEDFUNDS.csv")
file.remove("C:/Users/MATREJO/Downloads/DGS5.csv")
file.remove("C:/Users/MATREJO/Downloads/DGS2.csv")
file.remove("C:/Users/MATREJO/Downloads/DGS10.csv")

rD <- rsDriver(port = 4568L)
remDr <- rD$client


#### LIBOR de 12 meses ####
remDr$navigate("https://fred.stlouisfed.org/series/USD12MD156N")
Sys.sleep(3)
remDr$findElement("css selector","#download-button")$clickElement()
remDr$findElement("css selector","#download-data-csv")$clickElement()

#### LIBOR  de 6 meses ####
remDr$navigate("https://fred.stlouisfed.org/series/USD6MTD156N")
Sys.sleep(3)
remDr$findElement("css selector","#download-button")$clickElement()
remDr$findElement("css selector","#download-data-csv")$clickElement()

#### LIBOR  de 3 meses ####
remDr$navigate("https://fred.stlouisfed.org/series/USD3MTD156N")
Sys.sleep(3)
remDr$findElement("css selector","#download-button")$clickElement()
remDr$findElement("css selector","#download-data-csv")$clickElement()

#### LIBOR  de 1 mes ####
remDr$navigate("https://fred.stlouisfed.org/series/USD1MTD156N")
Sys.sleep(3)
remDr$findElement("css selector","#download-button")$clickElement()
remDr$findElement("css selector","#download-data-csv")$clickElement()

#### TREASURY de 10 años ####
remDr$navigate("https://fred.stlouisfed.org/series/DGS10")
Sys.sleep(3)
remDr$findElement("css selector","#download-button")$clickElement()
remDr$findElement("css selector","#download-data-csv")$clickElement()

#### TREASURY de 5 años ####
remDr$navigate("https://fred.stlouisfed.org/series/DGS5")
Sys.sleep(3)
remDr$findElement("css selector","#download-button")$clickElement()
remDr$findElement("css selector","#download-data-csv")$clickElement()

#### TREASURY de 2 años ####
remDr$navigate("https://fred.stlouisfed.org/series/DGS2")
Sys.sleep(3)
remDr$findElement("css selector","#download-button")$clickElement()
remDr$findElement("css selector","#download-data-csv")$clickElement()

#### FED techo de tasa objetivo ####
remDr$navigate("https://fred.stlouisfed.org/series/DFEDTARU")
Sys.sleep(3)
remDr$findElement("css selector","#download-button")$clickElement()
remDr$findElement("css selector","#download-data-csv")$clickElement()


#### Metiendo a la base de datos ####
Sys.sleep(2)
numdatos <- 20
fechas <- as.character(seq.Date(Sys.Date()-15,Sys.Date()-1,1))

libor1 <- tail(read.csv("C:/Users/MATREJO/Downloads/USD1MTD156N.csv",header = TRUE,
                        colClasses = c("character","character")),numdatos) %>%
  data.frame("LIBOR-1m",.,stringsAsFactors = FALSE)
colnames(libor1) <- c("id","fecha","valor")

libor3 <- tail(read.csv("C:/Users/MATREJO/Downloads/USD3MTD156N.csv",header = TRUE,
                        colClasses = c("character","character")),numdatos) %>%
  data.frame("LIBOR-3m",.,stringsAsFactors = FALSE)
colnames(libor3) <- c("id","fecha","valor")

libor6 <- tail(read.csv("C:/Users/MATREJO/Downloads/USD6MTD156N.csv",header = TRUE,
                        colClasses = c("character","character")),numdatos) %>%
  data.frame("LIBOR-6m",.,stringsAsFactors = FALSE)
colnames(libor6) <- c("id","fecha","valor")

libor12 <- tail(read.csv("C:/Users/MATREJO/Downloads/USD12MD156N.csv",header = TRUE,
                         colClasses = c("character","character")),numdatos) %>%
  data.frame("LIBOR-12m",.,stringsAsFactors = FALSE)
colnames(libor12) <- c("id","fecha","valor")

t10 <- tail(read.csv("C:/Users/MATREJO/Downloads/DGS10.csv",header = TRUE,
                     colClasses = c("character","character")),numdatos) %>%
  data.frame("Treasury-10y",.,stringsAsFactors = FALSE)
colnames(t10) <- c("id","fecha","valor")

t5 <- tail(read.csv("C:/Users/MATREJO/Downloads/DGS5.csv",header = TRUE,
                    colClasses = c("character","character")),numdatos) %>%
  data.frame("Treasury-5y",.,stringsAsFactors = FALSE)
colnames(t5) <- c("id","fecha","valor")

t2 <- tail(read.csv("C:/Users/MATREJO/Downloads/DGS2.csv",header = TRUE,
                    colClasses = c("character","character")),numdatos) %>%
  data.frame("Treasury-2y",.,stringsAsFactors = FALSE)
colnames(t2) <- c("id","fecha","valor")

objetivofed <- tail(read.csv("C:/Users/MATREJO/Downloads/DFEDTARU.csv",header = TRUE,
                             colClasses = c("character","character")),numdatos) %>%
  data.frame("Tasa-FED",.,stringsAsFactors = FALSE)
colnames(objetivofed) <- c("id","fecha","valor")

df <- rbind(libor1,libor3,libor6,libor12,t10,t5,t2,objetivofed)
df <- df %>% filter(valor != ".")

for(i in seq(1,length(df$id),1)){
  query1 <- paste0("SELECT id, fecha FROM tasas WHERE id ='",df$id[i],"' AND fecha = '",df$fecha[i],"'")
  rate <- dbGetQuery(mydb,query1)
  if(length(rate$id) == 0){
    query2 <- paste0("INSERT INTO tasas (id,fecha,nivel) VALUES ('",paste(df[i,],collapse = "','"),"')")
    dbSendQuery(mydb,query2)
  }
}

remDr$close()
rD$server$stop()
