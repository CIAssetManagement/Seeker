library(RMySQL)

#Connection to the database
keys <- readLines('keys2.txt')
mydb = dbConnect(MySQL(), user=keys[1], password=keys[2], dbname='mydb', host=keys[3])
query <- "SELECT * FROM bonds"

#Data to be clustered
datos <- dbGetQuery(mydb,query)
dbDisconnect(mydb)
emisora <-  c()
for (x in datos$id){
  emisora <- c(emisora,strsplit(x,"-")[[1]][2])
}
datos$Emisora <- emisora

#Cleaning the dat
datosn <- datos[c(4,6,7)]
datosn$Fecha <- as.numeric(as.Date(datos$FechaVencimiento)-as.Date("1990-01-01"))
#Emisoras
emisoras <- unique(datos$Emisora)
i <- 0
for (x in emisoras){
  indices <- which(datos$Emisora == x)
  emisora[indices] <- i
  i <- i + 1
}
datosn$Emisora <- as.numeric(emisora)
desv <- sd(datosn$Emisora)
media <- mean(datosn$Emisora)
#Tasas
tasas <- unique(datos$TipoTasa)
tasa <- rep(0,length(datos$id))
i <- 0
for (x in tasas){
  indices <- which(datos$TipoTasa == x)
  tasa[indices] <- i
  i <- i + 1
}
datosn$TipoTasa <- as.numeric(tasa)
datosn <- data.frame(scale(datosn))
datosn$Emisora <- desv*datosn$Emisora+media

#K-Means
set.seed(1345)
wss <- sapply(1:150, function(k){kmeans(datosn, k, nstart=50,iter.max = 20 )$tot.withinss})
wss
plot(1:150, wss,type="b", pch = 19, frame = FALSE, xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
#Selecting the number of clusters
cluster <- sum(ifelse((wss/wss[1])>0.001,1,0))
kmeans(datosn,cluster,iter.max=20,nstart=50)
