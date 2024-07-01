########################################################

## TP en RStudio de Estadística para Economistas I #################

## Cátedra de Tamara Burdisso – FCE-UNLP ##################

## Este ejercicio fuer elaborado por Esteban Rodriguez ########

## para el curso de Estadística FCE-UBA de Tamara Burdisso ##########

##durante los años 2018 a 2022   ##################

########################################################

## Segunda Parte - Ejercicio 17 #########################

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


## Ejercicio 17 a)

#' En el Ejercicio 5 de la primera parte del TP se simuló un mazo de naipes españolas 
#' (40 cartas). Allí se afirmaba que la probabilidad teórica de que, en una mano de 
#' tres cartas, salga el 1 de espadas es p=0,075. Veamos si los resultados obtenidos 
#' mediante una simulación de 1 millón de manos permiten respaldar esta hipótesis. 
#' Definimos las hipótesis nula y alternativa para un test bilateral:
#' H_0:p=0,075
#' H_1:p≠0,075

#' ¿Qué supuestos son necesarios para realizar el test de hipótesis? ¿Se cumplen? 

#' ¿Qué distribución utilizará? 

#' En base a la simulación de 1 millón de manos realizada en el Ejercicio 5, 
#' ¿cuál fue la cantidad de manos que contenían al 1 de espadas?


# COMPLETAR CON EL RESULTADO DEL EJERCICIO 5 DE LA PRIMERA PARTE DEL TP


###### Ejecutar desde aqui #################################
manos_con_1_de_espadas <-      ### COMPLETAR
###### Hasta aqui ##########################################

#' De esta forma, a partir de esa muestra obtuvimos una proporción muestral de:


###### Ejecutar desde aqui #################################
n <- 1000000
p_muestral <- manos_con_1_de_espadas/n
p_muestral
###### Hasta aqui ##########################################


#' ¿Cuál es el z observado correspondiente a este resultado?


###### Ejecutar desde aqui #################################
z_obs <- (p_muestral-0.075)/sqrt(0.075*(1-0.075)/n)
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



## Ejercicio 17 b)

#' En el Ejercicio 7.c) de la primera parte del TP se simuló el lanzamiento de un 
#' millón de monedas y se calculó la proporción de veces que se obtuvo una “cara” 
#' como resultado. Se suponía que la moneda estaba equilibrada, es decir, que se 
#' deberían obtener resultados en torno a 50% de caras y 50% de cecas. En términos 
#' de hipótesis, este supuesto lo podemos expresar de la siguiente forma.
#' H_0:p=0,5
#' H_1:p≠0,5

#' Veamos si los resultados obtenidos en la simulación de 1 millón de lanzamientos 
#' de monedas del Ejercicio 7.c) refuta o no la hipótesis nula.

#' ¿Cuál fue la proporción muestral de caras que se obtuvo en el Ejercicio 7.c)?


# COMPLETAR CON EL RESULTADO DEL EJERCICIO 7c DE LA PRIMERA PARTE DEL TP


###### Ejecutar desde aqui #################################
p_muestral <-    ### COMPLETAR
###### Hasta aqui ########################################## 


#' ¿Qué supuestos son necesarios para realizar el test de hipótesis? ¿Se cumplen? 

#' ¿Qué distribución utilizará? 

#' ¿Cuál es el z observado correspondiente al resultado de la simulación?


###### Ejecutar desde aqui #################################
z_obs <- (p_muestral-0.5)/sqrt(0.5*(1-0.5)/1000000)
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



## Ejercicio 17 c)

#' En el Ejercicio 7.d) de la primera parte del TP se simuló el lanzamiento de un 
#' millón de monedas y se calculó la proporción de veces que se obtuvo una “cara” 
#' como resultado. Se suponía que la moneda estaba cargada a favor de las caras, 
#' pero vamos a hacer de cuenta que nadie nos avisó de esto. Es decir, vamos a 
#' suponer que la moneda estaba equilibrada y ver si los resultados obtenidos nos 
#' obligan a cambiar de opinión. En términos de hipótesis, este supuesto lo podemos 
#' expresar de la siguiente forma.
#' H_0:p=0,5
#' H_1:p≠0,5

#' ¿Cuál fue la proporción muestral de caras que se obtuvo en el Ejercicio 7.d)?


# COMPLETAR CON EL RESULTADO DEL EJERCICIO 7c DE LA PRIMERA PARTE DEL TP


###### Ejecutar desde aqui #################################
p_muestral <-    ### COMPLETAR
###### Hasta aqui ########################################## 


#' ¿Qué supuestos son necesarios para realizar el test de hipótesis? ¿Se cumplen? 

#' ¿Qué distribución utilizará? 

#' ¿Cuál es el z observado correspondiente al resultado de la simulación?


###### Ejecutar desde aqui #################################
z_obs <- (p_muestral-0.5)/sqrt(0.5*(1-0.5)/1000000)
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