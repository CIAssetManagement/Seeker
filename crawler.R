library(RSelenium)

#Getting into Valmer page
rD <- rsDriver(port = 4568L)
remDr <- rD$client
remDr$navigate("http://www.valmer.com.mx/es/")
ingresar <- remDr$findElement(using = "class", "ingresarBtn")
ingresar$clickElement()
Sys.sleep(3)

#Providing user
email_usuario <- remDr$findElement("id", "emailUsuario")
email_usuario$sendKeysToElement(list(keys[1]))

#Providing password
pwd <- remDr$findElement("id", "contrasenaUsuario")
pwd$sendKeysToElement(list(keys[2]))

#Enter to the site
enterBtn <- remDr$findElement("class", "btnEnviar")
enterBtn$clickElement()
Sys.sleep(3)

#Enter in Historicos
lnk_box_bd <- remDr$findElement("id", "lnkBox_383")
lnk_box_bd$clickElement()
Sys.sleep(1)

#Enter in Vector
vectorBtn <- remDr$findElement("id", "nivel1_384")
vectorBtn$clickElement()
Sys.sleep(1)

#Enter in Vector Completo
vectorBtn2 <- remDr$findElement("id", "nivel2_385")
vectorBtn2$clickElement()
Sys.sleep(1)

#Setting the initial date
fechaFin <- remDr$findElement("css selector", "dl:nth-child(1) .ui-datepicker-trigger")
fechaFin$clickElement()
Sys.sleep(1)
#Day
dayInicio <- remDr$findElement("class","ui-datepicker-days-cell-over")
dayInicio$clickElement()
Sys.sleep(1)

#Setting the final date
fechaFin <- remDr$findElement("css selector", "dl:nth-child(2) .ui-datepicker-trigger")
fechaFin$clickElement()
Sys.sleep(1)
#Day
dayInicio <- remDr$findElement("class","ui-datepicker-days-cell-over")
dayInicio$clickElement()
Sys.sleep(1)

#Submitting the request
submitBtn <- remDr$findElement("class", "btn-submit")
submitBtn$clickElement()
Sys.sleep(5)

#Downloading Elements
download <- remDr$findElements("class","download-box")
down$clickElement()
Sys.sleep(20)

#Saving elements in a different directory
fecha <- as.Date(Sys.Date())
foldersCab(from = keys[3],to = paste0(keys[4],"/","Vector_",as.character(fecha),".csv"))

#Sign out Valmer
csesion <- remDr$findElement("xpath","//*[contains(text(), 'CERRAR SESIÓN')]")
csesion$clickElement()
remDr$close()