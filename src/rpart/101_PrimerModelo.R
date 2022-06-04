#Arbol elemental con libreria  rpart
#Debe tener instaladas las librerias  data.table  ,  rpart   y rpart.plot

#cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

#Aqui se debe poner la carpeta de SU computadora local
<<<<<<< HEAD
setwd("C:\\Users\\Gonzalo\\Desktop\\MMD\\8- Mineria Aplicada a Finanzas\\Git Clone\\labo\\src\\rpart")  #Establezco el Working Directory

#cargo los datos de 202011 que es donde voy a ENTRENAR el modelo
dtrain  <- fread("C:\\Users\\Gonzalo\\Desktop\\MMD\\8- Mineria Aplicada a Finanzas\\Datasets/paquete_premium_202011.csv")
=======
setwd("C:\\Users\\Gonzalo\\Desktop\\MMD\\Git Clone\\labo\\src\\rpart")  #Establezco el Working Directory

#cargo los datos de 202011 que es donde voy a ENTRENAR el modelo
dtrain  <- fread("C:\\Users\\Gonzalo\\Desktop\\MMD\\Datasets/paquete_premium_202011.csv")
>>>>>>> 08da12797827cabda4405bcf4e20cc7d7f09620b

#genero el modelo,  aqui se construye el arbol
modelo  <- rpart("clase_ternaria ~ .",  #quiero predecir clase_ternaria a partir de el resto de las variables
                 data = dtrain,
                 xval=0,
<<<<<<< HEAD
                 cp=        -944367597800102,   #esto significa no limitar la complejidad de los splits #prueba -1
                 minsplit=  2140,     #minima cantidad de registros para que se haga el split (80)M90 Prueba 300 
                 minbucket=  524,     #tamaño minimo de una hoja (1)M10
                 maxdepth=   20 )    #profundidad maxima del arbol (4)M5

#con las primeras pruebas (-1,1000,355,6)
=======
                 cp=        -0.3,   #esto significa no limitar la complejidad de los splits
                 minsplit=  90,     #minima cantidad de registros para que se haga el split (80)M90
                 minbucket=  10,     #tamaño minimo de una hoja (1)M10
                 maxdepth=   5 )    #profundidad maxima del arbol (4)M5


>>>>>>> 08da12797827cabda4405bcf4e20cc7d7f09620b
#grafico el arbol
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)


#Ahora aplico al modelo  a los datos de 202101  y genero la salida para kaggle

#cargo los datos de 202011, que es donde voy a APLICAR el modelo
<<<<<<< HEAD
dapply  <- fread("C:\\Users\\Gonzalo\\Desktop\\MMD\\8- Mineria Aplicada a Finanzas\\Datasets/paquete_premium_202101.csv")
=======
dapply  <- fread("C:\\Users\\Gonzalo\\Desktop\\MMD\\Datasets/paquete_premium_202101.csv")
>>>>>>> 08da12797827cabda4405bcf4e20cc7d7f09620b

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
<<<<<<< HEAD
dir.create( "C:\\Users\\Gonzalo\\Desktop\\MMD\\8- Mineria Aplicada a Finanzas\\Git Clone\\labo\\exp/") 
dir.create( "C:\\Users\\Gonzalo\\Desktop\\MMD\\8- Mineria Aplicada a Finanzas\\Git Clone\\labo\\exp/KA2001") 

fwrite( entrega, 
        file= "C:\\Users\\Gonzalo\\Desktop\\MMD\\8- Mineria Aplicada a Finanzas\\Git Clone\\labo\\exp//KA2001/K101_001.csv", 
=======
dir.create( "C:\\Users\\Gonzalo\\Desktop\\MMD\\Git Clone\\labo\\exp/") 
dir.create( "C:\\Users\\Gonzalo\\Desktop\\MMD\\Git Clone\\labo\\exp/KA2001") 

fwrite( entrega, 
        file= "C:\\Users\\Gonzalo\\Desktop\\MMD\\Git Clone\\labo\\exp//KA2001/K101_001.csv", 
>>>>>>> 08da12797827cabda4405bcf4e20cc7d7f09620b
        sep= "," )
