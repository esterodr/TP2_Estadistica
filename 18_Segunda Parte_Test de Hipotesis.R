########################################################

## TP en RStudio de Estadística para Economistas I #################

## Cátedra de Tamara Burdisso – FCE-UNLP ##################

## Este ejercicio fuer elaborado por Esteban Rodriguez ########

## para el curso de Estadística FCE-UBA de Tamara Burdisso ##########

##durante los años 2018 a 2022   ##################

########################################################

## Segunda Parte - Ejercicio 18 #########################

########################################################

## Test de Hipótesis

#' En la primera parte del trabajo práctico se realizaron varias simulaciones 
#' de procesos aleatorios intentando ver si se obtenían resultados consistentes 
#' con los predichos por la teoría. En ese momento, no contábamos con herramientas 
#' muy formales para afirmar si el resultado obtenido estaba “cerca” o “lejos” 
#' del teórico, más allá de lo que podíamos evaluar mediante el valor z.

#' En este ejercicio, volveremos a evaluar los resultados de las simulaciones ya 
#' realizadas mediante la realización de test de hipotesis. Para ello, será necesario 
#' que tenga a mano la primera parte de su TP, ya que deberá introducir los resultados 
#' allí obtenidos en el código de R.


## Ejercicio 18 a)

#' En el Ejercicio 8.c) se lanzó un millón de veces un dado supuestamente equilibrado, 
#' calculándose la proporción de veces que salieron el número 1 y el 6. 
#' Casi con seguridad, las proporciones obtenidas no fueron exactamente iguales, 
#' por lo que realizaremos un Test de Hipótesis para diferencia de Proporciones para 
#' ver si podemos confiar en que el dado estaba efectivamente equilibrado. Las hipótesis 
#' del test son las siguientes
#' H_0:p_1-p_6=0
#' H_1:p_1-p_6≠0

#' ¿Cuáles fueron la proporción muestrales de unos y seis 
#' que se obtuvieron en el Ejercicio 8.c)?
  

###### Ejecutar desde aqui #################################
prop_1 <-   ### COMPLETAR
prop_6 <-   ### COMPLETAR
###### Hasta aqui ##########################################


#' ¿Qué supuestos son necesarios para realizar el test de hipótesis? ¿Se cumplen? 

#' ¿Qué distribución utilizará? 
 
#' En este test se supone que ambas proporciones son iguales, para lo cual es 
#' necesario calcular un promedio ponderado de las proporciones muestrales. 
#' ¿Cuál es el valor de p̂_0?


###### Ejecutar desde aqui #################################
prop_0 <-   (prop_1+prop_6)*1000000/2000000
prop_0
###### Hasta aqui ##########################################


#' ¿Cuál es el z observado correspondiente al resultado de la simulación?


###### Ejecutar desde aqui #################################
z_obs <- (prop_1-prop_6)/sqrt(2*prop_0*(1-prop_0)/1000000)
z_obs
###### Hasta aqui ##########################################


#' Cuál es el p-value de este resultado


###### Ejecutar desde aqui #################################
p_value <- ifelse(z_obs<0,2*pnorm(z_obs),2*pnorm(z_obs,lower.tail = FALSE))
p_value
###### Hasta aqui ##########################################


#' En base a la respuesta anterior, considere si hay evidencia suficiente 
#' para rechazar o no la hipótesis nula. Justifique en interprete en el 
#' contexto del problema.


## Ejercicio 18 b)

#' En el Ejercicio 8.d) se lanzó un millón de veces un dado supuestamente cargado a 
#' favor del 6, calculándose la proporción de veces que salieron el número 1 y el 6. 
#' Casi con seguridad, se obtuvo una mayor proporción de 6 que de 1, pero requerimos 
#' de una prueba más rigurosa para saber si efectivamente esto es evidencia de que 
#' el dado estaba cargado. Para ello, realizaremos un Test de Hipótesis para diferencia 
#' de Proporciones con las siguientes hipótesis:
#' H_0:p_1-p_6=0
#' H_1:p_1-p_6≠0

#' ¿Cuáles fueron la proporción muestrales de unos y seis 
#' que se obtuvieron en el Ejercicio 8.d)?


###### Ejecutar desde aqui #################################
prop_1 <- ### COMPLETAR
prop_6 <- ### COMPLETAR
###### Hasta aqui ##########################################


#' ¿Qué supuestos son necesarios para realizar el test de hipótesis? ¿Se cumplen? 

#' ¿Qué distribución utilizará? 

#' En este test se supone que ambas proporciones son iguales, para lo cual es 
#' necesario calcular un promedio ponderado de las proporciones muestrales. 
#' ¿Cuál es el valor de p̂_0?


###### Ejecutar desde aqui #################################
prop_0 <-   (prop_1+prop_6)*1000000/2000000
prop_0
###### Hasta aqui ##########################################


#' ¿Cuál es el z observado correspondiente al resultado de la simulación?


###### Ejecutar desde aqui #################################
z_obs <- (prop_1-prop_6)/sqrt(2*prop_0*(1-prop_0)/1000000)
z_obs
###### Hasta aqui ##########################################


#' Cuál es el p-value de este resultado


###### Ejecutar desde aqui #################################
p_value <- ifelse(z_obs<0,2*pnorm(z_obs),2*pnorm(z_obs,lower.tail = FALSE))
p_value
###### Hasta aqui ##########################################


#' En base a la respuesta anterior, considere si hay evidencia suficiente 
#' para rechazar o no la hipótesis nula. Justifique en interprete en el 
#' contexto del problema.

