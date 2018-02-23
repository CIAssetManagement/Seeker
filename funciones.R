
######################## Funciones que agregan elementos a las tablas ###################################

### Función que alimenta la tabla de prices con los precios y niveles de los instrumentos en la BMV.
metamorphosis <- function(archivo,referencia){
  
  #Archivo must be a txt file
  
  ###########################################################################
  #Data of prices
  ###########################################################################
  
  #Date of the data
  dia <- as.Date(as.character(archivo$FECHA[1]),"%Y%m%d")
  fecha <- as.character(as.Date(as.character(archivo$FECHA),"%Y%m%d"))
  if(length(fecha) == 0)
    fecha <- as.character(as.Date(as.character(archivo$Fecha),"%Y%m%d"))
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
  
  especiales <- c("BI","I","MC")
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
    if(length(vencimiento) == 0)
     vencimiento <- as.Date(as.character(bonos$FECHA.DE.VENCIMIENTO),format="%d/%m/%Y")
    monto_emitido <- as.numeric(bonos$MONTO.EMITIDO)
    monto_emitido <- ifelse(is.na(monto_emitido) == TRUE,0,monto_emitido)
    #Reference rate
    tasa_referencia <- c()
    for(i in seq(1,length(bonos$FREC..CPN),1)){
      indiceref <- which(paste0(referencia$TV,"-",referencia$EMISION,"-",referencia$SERIE) == bonos$id[i])
      tasa_referencia <- c(tasa_referencia,as.character(referencia$`TASA DE REFERENCIA`[indiceref]))
    }
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
    query <- paste0("INSERT INTO bonds ","(id, FechaEmision,FechaVencimiento,TasaCupon,TipoTasa,SobreTasa,Frecuencia,MontoEmitido)",
                    " VALUES ",paste(paste(sprintf("('%s','%s','%s','%s','%s','%s','%s','%s')",bonos$id,emision,vencimiento,
                                        bonos$TASA.CUPON,tasa_referencia,bonos$SOBRETASA,frequency,monto_emitido),
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
  c <- "C:/Github/Funds/Instrumentos.csv"
  write.csv(df3,c,row.names = FALSE)
}
###

### Función que agrega los nodos de la curva de CETES, BONOS y algunos nodos de la TIIE a la tabla tasas
nodos <- function(fecha,nodo,bonos,cetes,ums,libor,reales){
  clave <- substr(nodo$clave,1,1)
  id <- c()
  
  for (i in seq(1,length(clave),1)){
    id <- c(id,paste0("TIIE-",nodo$plazo[i]))
  }
  nodo$fecha <- format(as.Date(nodo$fecha,format='%d/%m/%Y'),'%Y-%m-%d')
  #Query
  query <- paste0("INSERT INTO tasas ","(id, fecha, nivel)"," VALUES ",
                  paste(paste(sprintf("('%s','%s','%s')",id,nodo$fecha,nodo$valor_cierre), 
                              collapse = ",")))
  cat("Se agregaron los siguientes nodos: ",paste(id),collapse=",")
  dbSendQuery(mydb,query)
  #CETEs
  df <- data.frame(id=paste0("CETES-",cetes$X__4),fecha=rep(fecha,length(cetes$X__4)),
                   nivel=cetes$X__6)
  query <- paste0("INSERT INTO tasas ","(id, fecha, nivel)"," VALUES ",
                  paste(paste(sprintf("('%s','%s','%s')",df$id,df$fecha,df$nivel), 
                              collapse = ",")))
  dbSendQuery(mydb,query)
  #Bonos
  df2 <- data.frame(id=paste0("BONOS-",bonos$Plazo),fecha=rep(fecha,length(bonos$Plazo)),
                   nivel=bonos$X__2)
  query <- paste0("INSERT INTO tasas ","(id, fecha, nivel)"," VALUES ",
                  paste(paste(sprintf("('%s','%s','%s')",df2$id,df2$fecha,df2$nivel), 
                              collapse = ",")))
  dbSendQuery(mydb,query)
  #UMS
  df3 <- data.frame(id=paste0("UMS-",ums$Plazo),fecha=rep(fecha,length(ums$Plazo)),
                    nivel=ums$Tasa)
  query <- paste0("INSERT INTO tasas ","(id, fecha, nivel)"," VALUES ",
                  paste(paste(sprintf("('%s','%s','%s')",df3$id,df3$fecha,df3$nivel), 
                              collapse = ",")))
  dbSendQuery(mydb,query)
  #Libor
  df4 <- data.frame(id=paste0("LIBOR-",libor$Plazo),fecha=rep(fecha,length(libor$Plazo)),
                    nivel=libor$X__1)
  query <- paste0("INSERT INTO tasas ","(id, fecha, nivel)"," VALUES ",
                  paste(paste(sprintf("('%s','%s','%s')",df4$id,df4$fecha,df4$nivel), 
                              collapse = ",")))
  dbSendQuery(mydb,query)
  #Tasa Real
  df5 <- data.frame(id=paste0("TREAL-",reales$Plazo),fecha=rep(fecha,length(reales$Plazo)),
                    nivel=reales$X__1)
  query <- paste0("INSERT INTO tasas ","(id, fecha, nivel)"," VALUES ",
                  paste(paste(sprintf("('%s','%s','%s')",df5$id,df5$fecha,df5$nivel), 
                              collapse = ",")))
  dbSendQuery(mydb,query)
}

### Función que agrega las carteras de los contratos
portafolios <- function(archivo){
  archivo <- archivo %>% filter(Importe > 0)
  efectivo <- archivo %>% filter(Tipo %in% c('E','R')) %>% summarise(Efectivo=sum(Importe))
  archivo <- archivo %>% filter(Tipo %in% c('D','E'))
  archivo[is.na(archivo) == TRUE] <- ''
  archivo$Fecha <- as.Date(archivo$Fecha,format = '%d/%M/%Y')
  indices <- which(archivo$Emisora == 'EFECTIVO')
  archivo$Importe[indices[1]] <- efectivo$Efectivo
  if(length(indices) > 1)
    archivo <- archivo[-indices[-1],]
  instrumento <- paste0(archivo$TipoValor,"-",archivo$Emisora,"-",gsub("'","",archivo$Serie))
  query <- paste0("INSERT INTO portafolios (fecha,contrato,instrumento,titulos,costo,monto) VALUES ",
                  paste(paste(sprintf("('%s','%s','%s','%s','%s','%s')",archivo$Fecha,archivo$CContrato,
                                      instrumento,archivo$Titulos,archivo$Costo,archivo$Importe),collapse = ",")))
  dbSendQuery(mydb,query)
}

### Función que llena la tabla de comisiones y obtiene los archivos necesarios para la app comparador de fondos.
comparador <- function(fecha){
  
  #Metiendo a la base de datos las comisiones
  comisiones <- read_excel('C:/Users/MATREJO/Downloads/economatica.xlsx',skip = 2)
  instrumentos <- drop_read_csv('Carpeta del equipo CIEstrategias/Instrumentos.csv',header = TRUE,stringsAsFactors = FALSE)
  id <- c()
  indices <- c()
  for(i in seq(1,length(comisiones$Nombre),1)){
    indices1 <- which(instrumentos$Emisora == toupper(comisiones$Nombre[i]))
    indices2 <- which(instrumentos$Serie == comisiones$Clase[i])
    indice <- intersect(indices1,indices2)
    if(length(indice) != 0){
      id <- c(id, instrumentos$id[indice])
      indices <- c(indices,i)
    }
  }
  datos <- data.frame(date, id, comisiones[indices,15])
  colnames(datos) <- c('fecha','id','comisiones')
  datos$comisiones[which(datos$comisiones == "-")] <- "0"
  datos$comisiones <- ifelse(datos$id == "51-INVEXCP-A",as.numeric(datos$comisiones)*100,datos$comisiones)
  query <- paste0("INSERT INTO comisiones (fecha,fondo,comision_admin) VALUES ",
                  paste(paste(sprintf("('%s','%s','%s')", datos$fecha,datos$id,datos$comisiones)
                              ,collapse = ",")))
  dbSendQuery(mydb,query)
  
  #Creando los archivos que necesita la app comparador de fondos
  datos <- read_excel("C:/Github/ComparadorFondos/comparables.xlsx")
  bench_tasas <- c('Fondeo-GuberMX','CETES-364')
  ciusdclaves <- c('*CSP-MXPUSDS-V48','51-+CIUSD-A','51-INVEXCO-A','51-+TASAUS-A','52-SURUSD-A','51-SBANKDL-A',
                   '51-NTEDLS-A')
  tasas <- datos[which(datos$Clave %in% bench_tasas),]
  ciusd <- datos[which(datos$Clave %in% ciusdclaves),]
  datos <- datos[-which(datos$Clave %in% bench_tasas | datos$Clave %in% ciusdclaves),]
  query <- paste0("SELECT id,fecha,Precio_limpio FROM prices WHERE id IN ('",paste(datos$Clave,collapse = "','"),"')")
  data <- dbGetQuery(mydb,query)
  query <- paste0("SELECT id,fecha,nivel FROM tasas WHERE id IN ('",paste(tasas$Clave,collapse = "','"),"')")
  data2 <- dbGetQuery(mydb,query)
  colnames(data2) <- c('id','fecha','Precio_limpio')
  query <- paste0("SELECT id,fecha,Precio_limpio FROM prices WHERE id IN ('",paste(ciusd$Clave,collapse = "','"),"')")
  data3 <- dbGetQuery(mydb,query)
  indiceprecio <- which(data3$id == "*CSP-MXPUSDS-V48")
  for(i in seq(1,length(data3$id),1)){
    if(data3$id[i] != "*CSP-MXPUSDS-V48"){
      indicefechas <- which(data3$fecha == data3$fecha[i])
      indicesusd <- intersect(indicefechas,indiceprecio)
      if(length(indicesusd) != 0)
        data3$Precio_limpio[i] <- data3$Precio_limpio[i]/data3$Precio_limpio[indicesusd]
    } else {
      data3$fecha[i] <- as.character(diausd(as.Date(data3$fecha[i])))
    }
  }
  data <- rbind(data,data2,data3)
  write.csv(data,'C:/Github/ComparadorFondos/precios.csv',row.names = FALSE)
  
  query <- paste0("SELECT fecha,fondo,comision_admin FROM comisiones WHERE fondo IN ('",
                  paste(unique(data$id),collapse = "','"),"')")
  data <- dbGetQuery(mydb,query)
  
  write.csv(data,'C:/Github/ComparadorFondos/comisiones.csv',row.names = FALSE)
  isr <- read_excel('C:/Github/ComparadorFondos/isr.xlsx')
  isr <- data.frame(rbind(isr,data.frame(Fecha = as.character(fecha), ISR = as.character(isr$ISR[length(isr$ISR)]))))
  archivom <- 'C:/Github/ComparadorFondos/isr.xlsx'
  write.xlsx2(isr,archivom,col.names = TRUE,row.names = FALSE,append = FALSE)
}

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
carteras <- function(archivo,fecha){
  
  datos <- archivo %>% filter(Fondo != " " & Fondo != "Fondo")
  df <- datos[,c(1,2,3,4,5,9,10)]
  df[is.na(df)] <- ""
  colnames(df) <- c("I","Fondo","TV","Emisora","Serie","Titulos","Costo.Total")
  
  #Function to assure the instruments are right(Covaf does it wrong cause they use xlsx files, not csv)
  df$Serie <- mapply(elemento,df$TV,df$Emisora,df$Serie)
  
  df$Costo.Total <- as.numeric(as.character(df$Costo.Total))
  id <- as.character(paste0(df$TV,"-",df$Emisora,"-",df$Serie))
  for(i in seq(1,length(id),1)){
    if(!(df$TV[i] %in% c(" ","CHD") | id[i]=="0-CASITA-*" | df$Emisora[i] == "TOTALES")){
      df$Costo.Total[i] <- round(as.numeric(as.character(df$Titulos[i]))*get_prices(fecha,id[i])[1,2],digits=2)
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

#Función que obtiene los archivos necesarios para la app de portafolios
portafolios2 <- function(fecha){
  v_fechas <- seq(as.Date(paste0(as.numeric(substr(fecha,1,4))-1,"-01-01")),fecha,by = "1 day")
  #Cartera del día
  query <- paste0("SELECT * FROM portafolios WHERE fecha = '",fecha,"'")
  datos <- dbGetQuery(mydb,query)
  write.csv(datos,"C:/Github/Portafolios/Carteras_imss.csv",row.names = FALSE)
  #calificaciones de los instrumentos
  datos <- datos %>% filter(instrumento != '-EFECTIVO-')
  instrumentos <- unique(datos$instrumento)
  inst_imss <- drop_read_csv('Carpeta del equipo CIEstrategias/Instrumentos.csv',header=TRUE,stringsAsFactors = FALSE)
  posibles <- drop_read_csv('Carpeta del equipo CIEstrategias/mercados.csv',header=TRUE,stringsAsFactors = FALSE)
  posibles <- unique(posibles$imss)
  inst_imss <- inst_imss %>% filter(TipoValor %in% posibles)
  write.csv(inst_imss,"C:/Github/Portafolios/Calificaciones.csv",row.names = FALSE)
  #Precios de los instrumentos
  instrumentos <- c(instrumentos,inst_imss$id)
  query2 <- paste0("SELECT id,fecha,Precio_limpio,Tasa FROM prices WHERE id IN ('",paste(instrumentos,collapse = "','")
                   ,"') AND fecha IN ('",paste(v_fechas,collapse = "','"),"')")
  precios <- dbGetQuery(mydb,query2)
  write.csv(precios,"C:/Github/Portafolios/Precios.csv",row.names = FALSE)
  query3 <- paste0("SELECT * FROM bonds WHERE id IN ('",paste(instrumentos,collapse = "','"),"')")
  bonos <- dbGetQuery(mydb,query3)
  #Información de los bonos
  write.csv(bonos,"C:/Github/Portafolios/Bonos.csv",row.names = FALSE)
  #Saldo histórico de las carteras
  query4 <- paste0("SELECT fecha,contrato,monto FROM portafolios WHERE fecha IN ('",paste(v_fechas,collapse = "','"),
                   "')")
  historico <- dbGetQuery(mydb,query4)
  historico <- historico %>% group_by(fecha, contrato) %>% summarise(Monto =sum(monto))
  write.csv(historico,"C:/Github/Portafolios/historico.csv",row.names = FALSE)
}

#Función que suma un dia para el tipo de cambio
diausd <- function(fecha){
  fecha <- fecha +1
  fechabase0 <- as.Date("2017-08-06")
  if(as.integer(fecha - fechabase0 ) %% 7 == 6 | as.integer(fecha - fechabase0 ) %% 7 == 0 | fecha %in% festivos$dias){
    return(diausd(fecha))
  } else {
      return(fecha)
    }
}
