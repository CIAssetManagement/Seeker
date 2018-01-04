
######################## Funciones que agregan elementos a las tablas ###################################

### Función que alimenta la tabla de prices con los precios y niveles de los instrumentos en la BMV.
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
  #Dirty Price 
  precio_sucio <- archivo$PRECIO.SUCIO
  #Clean Price
  precio_limpio <- archivo$PRECIO.LIMPIO
  #Rate
  tasa <- archivo$TASA.DE.RENDIMIENTO
  
  #Writing the new file for prices
  df1 <- data.frame(cbind(valor,fecha,mercado,precio_sucio,precio_limpio,tasa))
  colnames(df1) <- c("Instrumento","Fecha","Mercado","Precio_sucio","Precio_limpio","Tasa")
  query <- paste0("INSERT INTO prices ","(id,fecha, Mercado,Precio_sucio,Precio_limpio,Tasa)"," VALUES ",
                  paste(paste(sprintf("('%s','%s','%s','%s','%s','%s')",df1$Instrumento,df1$Fecha,df1$Mercado,
                                      df1$Precio_sucio,df1$Precio_limpio,df1$Tasa), 
                              collapse = ",")))
  dbSendQuery(mydb,query)
  
  ###########################################################################
  #Data of bonds
  ###########################################################################
  
  # especiales <- c("BI","I")
  # datos <- archivo %>% filter(archivo$MERCADO=="MD" & archivo$TASA.CUPON>0 | archivo$TIPO.VALOR %in% especiales)
  # #id
  # datos$id <- paste0(datos$TIPO.VALOR,"-",datos$EMISORA,"-",datos$SERIE)
  # #Buscando los id's en la base de datos
  # nombres <- findbond(datos$id)
  # #Agregando los nuevos bonos (si hay nuevos)
  # if(length(nombres) != 0){
  #   #Finding the bonds
  #   bonos <- datos %>% filter(datos$id %in% nombres)
  #   #dates
  #   emision <- as.Date(bonos$FECHA.EMISION,format="%d/%m/%Y")
  #   emision <- ifelse(is.na(emision)==TRUE,"1900-01-01",as.character(emision))
  #   vencimiento <- as.Date(as.character(bonos$FECHA.VCTO),format="%d/%m/%Y")
  #   #frequency of coupons
  #   frequency <- c()
  #   for (i in seq(1,length(bonos$FREC..CPN),1)){
  #     if(is.na(bonos$FREC..CPN[i])==TRUE){
  #       plazo <- vencimiento[i] - Sys.Date()
  #     } else {
  #       plazo <- unlist(strsplit(as.character(bonos$FREC..CPN[i])," "))[2]
  #     }
  #     frequency <- c(frequency,plazo)
  #   }
  #   #Query
  #   query <- paste0("INSERT INTO bonds ","(id, FechaEmision,FechaVencimiento,TasaCupon,TipoTasa,SobreTasa,Frecuencia)"," VALUES ",
  #                   paste(paste(sprintf("('%s','%s','%s','%s','%s','%s','%s')",bonos$id,emision,vencimiento,
  #                                       bonos$TASA.CUPON,bonos$REGLA.CUPON,bonos$SOBRETASA,frequency), 
  #                               collapse = ",")))
  #   dbSendQuery(mydb,query)
  #   cat("Se agregaron los siguientes bonos: ",paste(nombres),collapse=",")
  # } else {cat("No se agregaron bonos")}
  # 
  # #Excel archive of instruments
  # df3 <- data.frame(cbind(TipoValor = as.character(archivo$TIPO.VALOR),Emisora = as.character(archivo$EMISORA),
  #                         Serie = as.character(archivo$SERIE), id = as.character(valor),
  #                         Moodys=as.character(archivo$MDYS),SP=as.character(archivo$S.P),
  #                         Fitch=as.character(archivo$CALIFICACION.FITCH)
  #                         ,HR=as.character(archivo$HR.RATINGS)))
  # df3$Calificacion <- calificacion(df3$Moodys,df3$SP,df3$Fitch,df3$HR)
  # c <- "C:/Github/Funds/Instrumentos.csv"
  # write.csv(df3,c,row.names = FALSE)
}
###

### Función que agrega los nodos de CETES y TIIES a la tabla tasas
nodos <- function(archivo){
  clave <- substr(archivo$clave,1,1)
  id <- c()
  
  for (i in seq(1,length(clave),1)){
    if(clave[i] == "C")
      id <- c(id,paste0("CETES-",archivo$plazo[i]))
    if(clave[i] == "T")
      id <- c(id,paste0("TIIE-",archivo$plazo[i]))
  }
  archivo$fecha <- format(as.Date(archivo$fecha,format='%d/%m/%Y'),'%Y-%m-%d')
  #Query
  query <- paste0("INSERT INTO tasas ","(id, fecha, nivel)"," VALUES ",
                  paste(paste(sprintf("('%s','%s','%s')",id,archivo$fecha,archivo$valor_cierre), 
                              collapse = ",")))
  cat("Se agregaron los siguientes nodos: ",paste(id),collapse=",")
  dbSendQuery(mydb,query)
}
###

########################################## Funciones de soporte #########################################

# Función que determina si el día es hábil o inhábil.
diah <-  function(fecha){
  dia <- "Habil"
  fechabase0 <- as.Date("2017-08-06")
  if(as.integer(fecha - fechabase0 ) %% 7 == 6){dia <- "Inhabil"}
  if(as.integer(fecha - fechabase0 ) %% 7 == 0){dia <- "Inhabil"}
  if(fecha %in% festivos$dias){dia <- "Inhabil"}
  return(dia)
}

# Función que encuentra los mínimos en un renglón quitando los NA's.
minimos <- function(renglon){
  return(min(renglon,na.rm=TRUE))
}

# Calificaciones basadas en Moodys, S&P, Fitch y HR Ratings.
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

# Función que obtiene los bonos que no se encuentran aún en la tabla de bonds.
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

# Función que ajusta el número de 0's a la izquierda ya que Covaf utiliza xlsx y no csv.
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

# Función que crea el archivo de carteras diario.
carteras <- function(archivo){
  
  datos <- archivo %>% filter(Fondo != " " & Fondo != "Fondo")
  df <- datos[,c(1,2,3,4,5,9,10)]
  colnames(df) <- c("I","Fondo","TV","Emisora","Serie","Titulos","Costo.Total")
  
  #Function to assure the instruments are right(Covaf does it wrong cause they use xlsx files, not csv)
  serie <- mapply(elemento,df$TV,df$Emisora,df$Serie)
  df$Serie <- mapply(elemento,df$TV,df$Emisora,df$Serie)
  
  df$Costo.Total <- as.numeric(as.character(df$Costo.Total))
  id <- as.character(paste0(df$TV,"-",df$Emisora,"-",df$Serie))
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
  df$Fondo <- paste0("'",df$Fondo)
  df$Serie <- paste0("'",df$Serie)
  
  
  write.csv(df,"C:/Github/Funds/Fondos.csv")
}

#Función que regresa formatos de fecha para los datos del INEGI
fechas <- function(dato){
  componentes <- strsplit(dato," ")[[1]]
  mes <- switch(componentes[2],"Ene"="01","Feb"="02","Mar"="03","Abr"="04","May"="05","Jun"="06",
                "Jul"="07","Ago"="08","Sep"="09","Oct"="10","Nov"="11","Dic"="12")
  if(substr(componentes[1],1,1) == "2"){
    dia <- as.Date(paste0(componentes[3],"-",mes,"-","01"))
    fecha <- seq(dia, length=2, by="1 month") - 1
    fecha <- as.character(fecha[2])
  } else {
    fecha <- paste0(componentes[3],"-",mes,"-","15")
  }
  return(fecha)
}