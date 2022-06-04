#limpio la memoria
rm( list=ls() )
gc()

library("data.table")
library("rpart")
library("rpart.plot")

setwd("C:\\Users\\Gonzalo\\Desktop\\MMD\\8- Mineria Aplicada a Finanzas\\Git Clone\\labo\\src\\RepensandoOverfitting")  #establezco la carpeta donde voy a trabajar
#cargo el dataset
dataset  <- fread("C:\\Users\\Gonzalo\\Desktop\\MMD\\8- Mineria Aplicada a Finanzas\\Datasets/paquete_premium_202011.csv")

#uso esta semilla para los canaritos
set.seed(236087)

#agrego una variable canarito, random distribucion uniforme en el intervalo [0,1]
dataset[ ,  canarito1 :=  runif( nrow(dataset) ) ]

#agrego los siguientes canaritos
for( i in 1:100 ) dataset[ , paste0("canarito", i ) :=  runif( nrow(dataset)) ]


#Primero  veo como quedan mis arboles
modelo  <- rpart(formula= "clase_ternaria ~ . ",
                 data= dataset[,],
                 model= TRUE,
                 xval= 0,
                 cp=        -9443675978001020, 
                 minsplit=  2140,
                 minbucket=  524,
                 maxdepth= 20)

#creo la carepta donde guardo el resultado
dir.create( "C://Users//Gonzalo//Desktop//MMD//8- Mineria Aplicada a Finanzas//Git Clone//labo///exp/",  showWarnings = FALSE ) 
dir.create( "C://Users//Gonzalo//Desktop//MMD//8- Mineria Aplicada a Finanzas//Git Clone//labo///exp//ST5210/", showWarnings = FALSE )
setwd("C://Users//Gonzalo//Desktop//MMD//8- Mineria Aplicada a Finanzas//Git Clone//labo///exp//ST5210\\")   #Establezco el Working Directory DEL EXPERIMENTO

#genero la imagen del arbol
pdf( file= "arbol_canaritos_mejor100.pdf", width=20, height=4)
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
dev.off()
