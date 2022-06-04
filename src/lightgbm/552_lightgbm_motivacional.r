# LightGBM  Motivacional
# para motivar a los alumnos a hacer la  "Tarea Hogar DOS"
# viendo que desde el inicio de la tarea logran ganancias superadoras
# la salida queda en  "./labo/exp/KA552/KA_552_001.csv"

#los DOS puntos novedosos que se ven en este script
# 1. Se entrena  con  POS = { BAJA+1, BAJA+2 }    los BAJA+1 en realidad estan mas enfermos que los BAJA+2
#    Era forzar mucho al algoritmo agrupar los BAJA¿1 con los CONTINUA 
# 2. El punto anterior obliga a buscar una probabilidad de corte DISTINTA  a 1/60

# utilizar la primer semilla propia

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("lightgbm")

<<<<<<< HEAD

ksemilla  <- 236087  #poner aqui la PRIMERA de sus cinco semillas

#Aqui se debe poner la carpeta de la computadora local
setwd("C:\\Users\\Gonzalo\\Desktop\\MMD\\8- Mineria Aplicada a Finanzas\\Git Clone\\")   #Establezco el Working Directory

#cargo el dataset donde voy a entrenar
dataset  <- fread("C:\\Users\\Gonzalo\\Desktop\\paquete_premium.csv", stringsAsFactors= TRUE)
dataset = dataset[1:1600000,]
=======
ksemilla  <- 236087  #poner aqui la PRIMERA de sus cinco semillas


f_dowle2 = function(DT) {
  for (i in names(DT))
    DT[is.na(get(i)), (i):=1]
}


#Aqui se debe poner la carpeta de la computadora local
setwd("C:\\Users\\Gonzalo\\Desktop\\MMD\\Git Clone\\")   #Establezco el Working Directory

#cargo el dataset donde voy a entrenar
dataset  <- fread("../datasets/paquete_premium.csv", stringsAsFactors= TRUE) #_premium_202011
# median_m = median(dataset[!is.na(get('Master_mlimitecompra')),Master_mlimitecompra])
# median_v = median(dataset[!is.na(get('Visa_mlimitecompra')),Visa_mlimitecompra])
# dataset[is.na(get('Master_mlimitecompra')),Master_mlimitecompra:=median_m]
# dataset[is.na(get('Visa_mlimitecompra')),Visa_mlimitecompra:=median_v]
# dataset[ , tarjetas_limite_suma := ifelse(Master_mlimitecompra+Visa_mlimitecompra<=680000, 1L, 0L)]
#f_dowle2(dataset)

# b2=dataset[clase_ternaria == 'BAJA+2']
# b1=dataset[clase_ternaria == 'BAJA+1']
# cont=dataset[clase_ternaria == 'CONTINUA']
# summary(b2)
# summary(b1)
# summary(cont)
# b2_df = data.frame(unclass(summary(b2)), check.names = FALSE, stringsAsFactors = FALSE)
# b1_df = data.frame(unclass(summary(b1)), check.names = FALSE, stringsAsFactors = FALSE)
# cont_df = data.frame(unclass(summary(cont)), check.names = FALSE, stringsAsFactors = FALSE)

>>>>>>> 08da12797827cabda4405bcf4e20cc7d7f09620b

#paso la clase a binaria que tome valores {0,1}  enteros
#set trabaja con la clase  POS = { BAJA+1, BAJA+2 } 
#esta estrategia es MUY importante
dataset[ , clase01 := ifelse( clase_ternaria %in%  c("BAJA+2","BAJA+1"), 1L, 0L) ]

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01") )


#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ , campos_buenos, with=FALSE]),
                        label= dataset$clase01 )

<<<<<<< HEAD
=======
#parámetro
>>>>>>> 08da12797827cabda4405bcf4e20cc7d7f09620b

#genero el modelo con los parametros por default
#estos hiperparametros  salieron de una Optmizacion Bayesiana
modelo  <- lgb.train( data= dtrain,
                      param= list( objective=        "binary",
                                   metric= "custom",
                                   first_metric_only= TRUE,
                                   boost_from_average= TRUE,
                                   feature_pre_filter= FALSE,
                                   verbosity= -100,
                                   prob_corte = 0.0155600466390736,
                                   max_bin=              31,
                                   learning_rate=         0.0101202577405108, #0.067
                                   num_iterations=      535,#128
                                   num_leaves=          162, #100
                                   min_data_in_leaf=   4111, #1700
                                   feature_fraction=      0.523609414306473, #0.37
                                   seed=               ksemilla   #aqui se utiliza SU primer semilla
<<<<<<< HEAD
                      )
                    )

#aplico el modelo a los datos sin clase
dapply  <- fread("C:\\Users\\Gonzalo\\Desktop\\datasets_paquete_premium_202101.csv")
=======
                                  )
                    )


#aplico el modelo a los datos sin clase
dapply  <- fread("../datasets/paquete_premium_202101.csv") #

### Pruebas sin sentido de Feature Eng.
#f_dowle2(dapply)
# median_m = median(dapply[!is.na(get('Master_mlimitecompra')),Master_mlimitecompra])
# median_v = median(dapply[!is.na(get('Visa_mlimitecompra')),Visa_mlimitecompra])
# dapply[is.na(get('Master_mlimitecompra')),Master_mlimitecompra:=median_m]
# dapply[is.na(get('Visa_mlimitecompra')),Visa_mlimitecompra:=median_v]
# dapply[ , tarjetas_limite_suma := ifelse(Master_mlimitecompra+Visa_mlimitecompra<=680000, 1L, 0L)]
>>>>>>> 08da12797827cabda4405bcf4e20cc7d7f09620b

#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, 
                        data.matrix( dapply[, campos_buenos, with=FALSE ])                                 )


#Genero la entrega para Kaggle
#Atencion ya NO corto por  1/60,  sino que busque el punto de corte optimo
entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                 "Predicted"= as.integer(prediccion > 0.023)   ) ) #ATENCION  no es  1/60

#guardo el resultado
#creo las carpetas
dir.create( "labo/exp/",  showWarnings = FALSE ) 
dir.create( "labo/exp/KA5520/", showWarnings = FALSE )
setwd( "labo/exp/KA5520/" )

archivo_salida  <- "KA_552_best.csv"

#genero el archivo para Kaggle
fwrite( entrega, 
        file= archivo_salida, 
        sep= "," )


#ahora imprimo la importancia de variables
tb_importancia  <-  as.data.table( lgb.importance(modelo) ) 
archivo_importancia  <- "552_importancia_001.txt"

fwrite( tb_importancia, 
        file= archivo_importancia, 
        sep= "\t" )


#cuento cuantos 1's tiene la prediccion
#cuantos estimulos estoy enviando para retener clientes
entrega[  , sum( Predicted ) ]
