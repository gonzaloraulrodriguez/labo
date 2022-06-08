# XGBoost  sabor original ,  cambiando algunos de los parametros

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("xgboost")

#Aqui se debe poner la carpeta de la computadora local
setwd("C:\\Users\\Gonzalo\\Desktop\\MMD\\8- Mineria Aplicada a Finanzas\\Git Clone")   #Establezco el Working Directory

#cargo el dataset donde voy a entrenar
dataset  <- fread("../datasets/paquete_premium_202011.csv", stringsAsFactors= TRUE)


#paso la clase a binaria que tome valores {0,1}  enteros
dataset[ , clase01 := ifelse( clase_ternaria=="BAJA+2", 1L, 0L) ]

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01") )


#dejo los datos en el formato que necesita XGBoost
dtrain  <- xgb.DMatrix( data= data.matrix(  dataset[ , campos_buenos, with=FALSE]),
                        label= dataset$clase01 )

#genero el modelo con los parametros por default
modelo  <- xgb.train( data= dtrain,
                      param= list( objective=       "binary:logistic",
                                   gamma	= 0,
                                   alpha	=	0,
                                   lambda	=	0,
                                   subsample	=	1,
                                   tree_method	=	"auto",
                                   grow_policy	= "depthwise",
                                   max_bin	=	256,
                                   max_leaves =	0,
                                   scale_pos_weight	= 1,	
                                   eta		= 0.0103328555076991,
                                   colsample_bytree	=	0.537458968187861,
                                   min_child_weight		= 10,
                                   max_depth		= 18,
                                   base_score= mean( getinfo(dtrain, "label"))
                                   ),
                      nrounds= 218
                    )


#aplico el modelo a los datos sin clase
dapply  <- fread("../datasets/paquete_premium_202101.csv")

#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, 
                        data.matrix( dapply[, campos_buenos, with=FALSE ]) )


#Genero la entrega para Kaggle
entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                 "Predicted"= as.integer( prediccion > 0.0135108388703657 ) )  ) #1/60#genero la salida

dir.create( "labo/exp/",  showWarnings = FALSE ) 
dir.create( "labo/exp/KA5610/", showWarnings = FALSE )
archivo_salida  <- "labo/exp/KA5610/KA_561_tarea_dos_op.csv"

#genero el archivo para Kaggle
fwrite( entrega, 
        file= archivo_salida, 
        sep= "," )
