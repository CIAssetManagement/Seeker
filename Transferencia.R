library(rdrop2)

#Copiando el archivo de Fondos
drop_upload(file = "C:/Github/Funds/Fondos.csv",path = 'Carpeta del equipo CIEstrategias',
            mode = "overwrite",verbose = TRUE)

#Copiando el archivo de instrumentos
drop_upload(file = "C:/Github/Funds/Instrumentos.csv",path = 'Carpeta del equipo CIEstrategias',
            mode = "overwrite",verbose = TRUE)