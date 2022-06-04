#Intento de Solucion del desafio  15k
#que NO logra solucionarlo, una que falta una idea fundamental, una chispa, un Momento Eureka
#pero crea estructura sobre la cual trabajar

#limpio la memoria
rm( list=ls() )
gc()

require("data.table")

ftirar  <- function(prob, qty)
{
  return(sum(runif(qty) < prob))
}

#variables globales que usan las funciones gimnasio_xxxx
GLOBAL_jugadores  <- c()
GLOBAL_tiros_total  <- 0

#Crea el juego
#a cada jugador se le pone un numero de 1 a 100 en la espalda
#debajo de ese numero esta el indice_de_enceste  que NO puede ser visto por el cazatalentos
gimnasio_init  <- function() 
{
  GLOBAL_jugadores  <<-  sample(c((501:599)/1000 , 0.70))
  GLOBAL_tiros_total  <<- 0
}

#se le pasa un vector con los IDs de los jugadores y la cantidad de tiros a realizar
#devuelve en un vector cuantos aciertos tuvo cada jugador
gimnasio_tirar  <- function(  pids,  pcantidad )
{
  GLOBAL_tiros_total  <<-  GLOBAL_tiros_total + length( pids )*pcantidad
  res  <- mapply(  ftirar, GLOBAL_jugadores[pids], pcantidad )

  return( res )
}

#El cazatalentos decide a que jugador llevarse
#devuelve la cantidad de tiros libres y si le acerto al verdadero_mejor o no
gimnasio_veredicto  <- function( pid )
{
  return( list("tiros_total"= GLOBAL_tiros_total, 
               "acierto"=     as.integer( GLOBAL_jugadores[pid]==0.70) ))
}
#------------------------------------------------------------------------------

Estrategia_C  <- function()
{
  #Estrategia
  #Se juegan varias rondas
  #En cada ronda, los jugadores que participan, tiran 70 tiros
  #De una ronda a la otra, solo pasan los que tuvieron igual o mayor aciertos a la mediana de aciertos de la ronda anterior
  #Se elige el mejor jugador de la sexta ronda

  gimnasio_init()

  #Esta el la planilla del cazatalentos
  #el id es el numero que tiene en la espalda cada jugador
  planilla_cazatalentos  <- data.table( "id"= 1:100 )

  #Ronda 1  ------------------------------------------------------
  #tiran los 100 jugadores es decir 1:100   70  tiros libres cada uno
  ids_juegan1  <- 1:100   #los jugadores que participan en la ronda,
  tiradas_1 = 415
  planilla_cazatalentos[ ids_juegan1,  tiros1 := tiradas_1 ]  #registro en la planilla que tiran 70 tiros
  resultado1  <- gimnasio_tirar( ids_juegan1, tiradas_1)
  planilla_cazatalentos[ ids_juegan1,  aciertos1 := resultado1 ]  #registro en la planilla
  
  #Ronda 2 -------------------------------------------------------
  #setorder(planilla_cazatalentos,-'aciertos1')
  # = as.matrix(planilla_cazatalentos[1:60,1])[,1]
  ids_juegan2  <- planilla_cazatalentos[ ids_juegan1 ][ aciertos1 >= 32, id ]
  tiradas_2 = 100
  planilla_cazatalentos[ ids_juegan2,  tiros2 := tiradas_2 ]  #registro en la planilla que tiran 70 tiros
  resultado2  <- gimnasio_tirar( ids_juegan2, tiradas_2)
  planilla_cazatalentos[ ids_juegan2,  aciertos2 := resultado2 ]  #registro en la planilla

  #Ronda 3 -------------------------------------------------------
  setorder(planilla_cazatalentos,-'aciertos2')
  ids_juegan3 = as.matrix(planilla_cazatalentos[1:35,1])[,1]
  tiradas_3 = 40
  planilla_cazatalentos[ ids_juegan3,  tiros3 := tiradas_3 ]  #registro en la planilla que tiran 70 tiros
  resultado3  <- gimnasio_tirar( ids_juegan3, tiradas_3)
  planilla_cazatalentos[ ids_juegan3,  aciertos3 := resultado3 ]  #registro en la planilla  
  #Epilogo
  #El cazatalentos toma una decision, elige al que mas aciertos tuvo en la ronda2
  pos_mejor <-  planilla_cazatalentos[ , which.max(aciertos3) ]
  id_mejor  <-  planilla_cazatalentos[ pos_mejor, id ]

  #Finalmente, la hora de la verdadero_mejor
  #Termino el juego
  veredicto  <- gimnasio_veredicto( id_mejor )
  
  return( veredicto )
}
#------------------------------------------------------------------------------

#Aqui hago la Estimacion Montecarlo del porcentaje de aciertos que tiene la estrategia A

set.seed(236087)  #debe ir una sola vez, ANTES de los experimentos

tabla_veredictos  <- data.table(  tiros_total=integer(),  acierto=integer() )

for( experimento  in  1:10000 )
{
  if( experimento %% 1000 == 0 )  cat( experimento, " ")  #desprolijo, pero es para saber por donde voy

  veredicto  <- Estrategia_C()
  
  tabla_veredictos  <- rbind( tabla_veredictos, veredicto )
}

cat("\n")

tiros_total  <-  tabla_veredictos[  , max( tiros_total) ]
tasa_eleccion_correcta  <-  tabla_veredictos[  , mean( acierto) ]

tiros_total 
tasa_eleccion_correcta
#tiros_total: 18470
#tasa_eleccion_correcta: 0.9294 Esto es con "Cali Sumpoint" con 0.7. 
#Que pasa si este pibe es menos bueno?
#Si Cali Sumpoint se quiebra un uña y tira con 0.65, estamos realmente complicados... 0.5268

