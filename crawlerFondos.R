library(RSelenium)

file.remove("C:/Users/JERUIZ/Downloads/Resumen_Operaciones.csv")
user <- Sys.getenv("ROUSER")
password <- Sys.getenv("ROPWD")

#Getting into Valmer page
rD <- rsDriver(port = 4568L)
remDr <- rD$client
remDr$navigate("http://192.168.0.222:8081/ConsultaCI/login.aspx")

#Providing user
email_usuario <- remDr$findElement("id", "Login1_UserName")
email_usuario$sendKeysToElement(list(user))

#Providing password
pwd <- remDr$findElement("id", "Login1_Password")
pwd$sendKeysToElement(list(password))

#Enter to the site
enterBtn <- remDr$findElement("id", "Login1_LoginButton")
enterBtn$clickElement()
Sys.sleep(1)

#Enter to the 'Repostes'
remDr$navigate("http://192.168.0.222:8081/ConsultaCI/cism.aspx")

#Enter in reporte 
selectorRep <- remDr$findElement("id", "ctl00_ContentPlaceHolder1_cmblista")
selectorRep$clickElement()
Sys.sleep(1)
 
#Enter in Vector Completo
resumenBtn <- remDr$findElement("xpath", "//*[@value='resumen']")
resumenBtn$clickElement()
Sys.sleep(1)

#Generate report
genReporte <- remDr$findElement("id", "ctl00_ContentPlaceHolder1_btnmuestra")
genReporte$clickElement()
Sys.sleep(3)

#close session 
csesion <- remDr$findElement("xpath","//*[contains(text(), 'Cerrar Sesion')]")
csesion$clickElement()

remDr$close()
 