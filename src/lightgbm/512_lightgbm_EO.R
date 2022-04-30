# LightGBM  cambiando algunos de los parametros

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection


#uso esta semilla para los canaritos
set.seed(525713)

require("data.table")
require("lightgbm")

#Aqui se debe poner la carpeta de la computadora local
setwd("C:\\Users\\oliva\\OneDrive\\Desktop\\DS\\Austral\\08 - Labo1")    #Establezco el Working Directory

#cargo el dataset donde voy a entrenar
dataset  <- fread("./labo/datasets/paquete_premium_202011.csv", stringsAsFactors= TRUE)


#paso la clase a binaria que tome valores {0,1}  enteros
dataset[ , clase01 := ifelse( clase_ternaria %in%  c("BAJA+2","BAJA+1"), 1L, 0L) ]

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01") )


#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ , campos_buenos, with=FALSE]),
                        label= dataset$clase01 )

#genero el modelo con los parametros por default
modelo  <- lgb.train( data= dtrain,
                      param= list( objective=        "binary",
                                   metric="custom",
                                   first_metric_only=TRUE,
                                   boost_from_average=TRUE,
                                   feature_pre_filter=FALSE,
                                   verbosity=-100,
                                   seed = 999983,
                                   max_depth=-1,
                                   min_gain_to_split=0,
                                   max_bin=31,
                                   num_iterations=501,
                                   force_row_wise=TRUE,
                                   learning_rate=0.011284351,
                                   feature_fraction = 0.588206711,
                                   min_data_in_leaf=4359,
                                   num_leaves=18
                      ) 
                    )

#aplico el modelo a los datos sin clase
dapply  <- fread("./labo/datasets/paquete_premium_202101.csv")

#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, 
                        data.matrix( dapply[, campos_buenos, with=FALSE ])                                 )


#Genero la entrega para Kaggle
entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                 "Predicted"= prediccion > 0.014946321)  ) #genero la salida

dir.create( "./labo/exp/",  showWarnings = FALSE ) 
dir.create( "./labo/exp/KA2512/", showWarnings = FALSE )
archivo_salida  <- "./labo/exp/KA2512/KA_512_001.csv"

#genero el archivo para Kaggle
fwrite( entrega, 
        file= archivo_salida, 
        sep= "," )

