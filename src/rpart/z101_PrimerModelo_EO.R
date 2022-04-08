#Arbol elemental con libreria  rpart
#Debe tener instaladas las librerias  data.table  ,  rpart   y rpart.plot

#cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")
library(dplyr) 
library("zoo")

#Aqui se debe poner la carpeta de SU computadora local
setwd("C:\\Users\\oliva\\OneDrive\\Desktop\\DS\\Austral\\08 - Labo1\\labo")  #Establezco el Working Directory

#cargo los datos de 202011 que es donde voy a ENTRENAR el modelo
dtrain  <- fread("./datasets/paquete_premium_202011.csv")


#genero el modelo,  aqui se construye el arbol
modelo  <- rpart("clase_ternaria ~ .",  #quiero predecir clase_ternaria a partir de el resto de las variables
                 data = dtrain,
                 xval = 2,
                 cp = -0.045,   #esto significa no limitar la complejidad de los splits
                 minsplit = 80,     #minima cantidad de registros para que se haga el split
                 minbucket = 1,     #tamaño minimo de una hoja
                 maxdepth=   8,       #profundidad maxima del arbol
                 )   


#grafico el arbol
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)

#xcompact = FALSE, ycompact = FALSE
#Ahora aplico al modelo  a los datos de 202101  y genero la salida para kaggle

#cargo los datos de 202011, que es donde voy a APLICAR el modelo
dapply  <- fread("./datasets/paquete_premium_202101.csv")

#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, dapply , type = "prob")

#prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
#cada columna es el vector de probabilidades 

#agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[ , prob_baja2 := prediccion[, "BAJA+2"] ]

#solo le envio estimulo a los registros con probabilidad de BAJA+2 mayor  a  1/60
dapply[ , Predicted  := as.numeric(prob_baja2 > 1/60) ]

#genero un dataset con las dos columnas que me interesan
entrega  <- dapply[   , list(numero_de_cliente, Predicted) ] #genero la salida

#genero el archivo para Kaggle
#creo la carpeta donde va el experimento
dir.create( "./exp/" ) 
dir.create( "./exp/KA2001" ) 

fwrite( entrega, 
        file= "./exp/KA2001/K101_001.csv", 
        sep= "," )







-----------------------------------------
count <- dapply %>% 
  group_by(clase_ternaria) %>% 
  count(clase_ternaria)

count <- dapply %>% 
  group_by(Predicted) %>% 
  count(Predicted)

prediccion <- prediccion %>% 
  arrange(desc(BAJA+2))