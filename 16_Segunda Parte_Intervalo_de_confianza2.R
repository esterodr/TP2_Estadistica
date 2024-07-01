########################################################

## TP en RStudio de Estadística para Economistas I #################

## Cátedra de Tamara Burdisso – FCE-UNLP ##################

## Este ejercicio fuer elaborado por Esteban Rodriguez ########

## para el curso de Estadística FCE-UBA de Tamara Burdisso ##########

##durante los años 2018 a 2022   ##################

########################################################

## Segunda Parte - Ejercicio 16 #########################

########################################################

## Intervalos de Confianza

#' En la primera parte del trabajo práctico se realizaron varias simulaciones de 
#' procesos aleatorios intentando ver si se obtenían resultados consistentes con 
#' los predichos por la teoría. En ese momento, no contábamos con herramientas muy 
#' formales para afirmar si el resultado obtenido estaba “cerca” o “lejos” del 
#' teórico, más allá de lo que podíamos evaluar mediante el valor z.

#' En este ejercicio, volveremos a evaluar los resultados de las simulaciones ya 
#' realizadas mediante la construcción de intervalos de confianza. Para ello, será 
#' necesario que tenga a mano la primera parte de su TP, ya que deberá introducir 
#' los resultados allí obtenidos en el código de R.


##' Ejercicio 16 a)

#' En el Ejercicio 5 de la primera parte del TP se simuló un mazo de naipes españolas 
#' (40 cartas) y se calculó la probabilidad teórica de que, en una mano de tres cartas, 
#' salga el 1 de espadas. La probabilidad calculada para este evento fue: p=0,075.

#' Se intentó estimar este valor mediante la simulación de 1 millón de manos, dando 
#' como resultado que la cantidad de manos que contenían al 1 de espadas fue:
  
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


#' Este resultado muestral, ¿avala o rechaza la idea de que el verdadero valor 
#' de la proporción es p=0,075? Para dar una respuesta formal a esta pregunta, 
#' utilicemos el resultado muestral para construir intervalos de confianza para 
#' la proporción poblacional. Utilizaremos niveles de confianza del 90%, 95% y 99%.

#' ¿Qué supuestos son necesarios para construir el intervalo de confianza? ¿Se cumplen? 
  
#' ¿Qué distribución utilizará? 
  
#' ¿Cuáles son los valores críticos (z o t) para cada nivel de significatividad?
  
#' ¿Cuál es el margen de error para cada nivel de confianza?
#' INTENTE RESOLVER ANALÍTICAMENTE. UTILICE LOS RESULTADOS DE R PARA COMPROBAR.


###### Ejecutar desde aqui #################################
ME_90 <- qnorm(0.95)*sqrt(p_muestral*(1-p_muestral)/n)
ME_95 <- qnorm(0.975)*sqrt(p_muestral*(1-p_muestral)/n)
ME_99 <- qnorm(0.995)*sqrt(p_muestral*(1-p_muestral)/n)
paste("El ME para el IC al 90% es:",ME_90)
paste("El ME para el IC al 95% es:",ME_95)
paste("El ME para el IC al 99% es:",ME_99)
###### Hasta aqui ##########################################


#' ¿Cuál es el intervalo de confianza para la proporción poblacional al 
#' 90%, 95% y 99% respectivamente?
#' INTENTE RESOLVER ANALÍTICAMENTE. UTILICE LOS RESULTADOS DE R PARA COMPROBAR.


###### Ejecutar desde aqui #################################
paste("El IC al 90% es: (",p_muestral-ME_90," , ", p_muestral+ME_90,")")
paste("El IC al 95% es: (",p_muestral-ME_95," , ", p_muestral+ME_95,")")
paste("El IC al 99% es: (",p_muestral-ME_99," , ", p_muestral+ME_99,")")
###### Hasta aqui ##########################################


#' ¿Se encuentra el valor teórico contenido por estos intervalos de confianza?
  
#' En base a los resultados obtenidos, ¿considera que la probabilidad teórica de 
#' obtener un 1 de espadas en una mano de tres cartas estuvo bien calculada?
  

##' Ejercicio 16 b)

#' En el Ejercicio 7.c) de la primera parte del TP se simuló el lanzamiento de 
#' un millón de monedas y se calculó la proporción de veces que se obtuvo una 
#' “cara” como resultado. Como se suponía que la moneda estaba equilibrada, 
#' la teoría sugiere que se debería obtener una proporción cercana a p=0,5.

#' ¿Cuál fue la proporción muestral de caras que se obtuvo en el Ejercicio 7.c)?
  
# COMPLETAR CON EL RESULTADO DEL EJERCICIO 7c DE LA PRIMERA PARTE DEL TP


###### Ejecutar desde aqui #################################
p_muestral <-      ### COMPLETAR
###### Hasta aqui ##########################################


#' Este resultado muestral, ¿avala o rechaza la idea de que la moneda está equilibrada? 
#' Para dar una respuesta formal a esta pregunta, utilicemos el resultado muestral para 
#' construir intervalos de confianza para la proporción poblacional. Utilizaremos niveles 
#' de confianza del 90%, 95% y 99%.

#' ¿Qué supuestos son necesarios para construir el intervalo de confianza? ¿Se cumplen? 

#' ¿Qué distribución utilizará? 

#' ¿Cuáles son los valores críticos (z o t) para cada nivel de significatividad?

#' ¿Cuál es el margen de error para cada nivel de confianza?
#' INTENTE RESOLVER ANALÍTICAMENTE. UTILICE LOS RESULTADOS DE R PARA COMPROBAR.


###### Ejecutar desde aqui #################################
ME_90 <- qnorm(0.95)*sqrt(p_muestral*(1-p_muestral)/n)
ME_95 <- qnorm(0.975)*sqrt(p_muestral*(1-p_muestral)/n)
ME_99 <- qnorm(0.995)*sqrt(p_muestral*(1-p_muestral)/n)
paste("El ME para el IC al 90% es:",ME_90)
paste("El ME para el IC al 95% es:",ME_95)
paste("El ME para el IC al 99% es:",ME_99)
###### Hasta aqui ##########################################


#' ¿Cuál es el intervalo de confianza para la proporción poblacional al 
#' 90%, 95% y 99% respectivamente?
#' INTENTE RESOLVER ANALÍTICAMENTE. UTILICE LOS RESULTADOS DE R PARA COMPROBAR.


###### Ejecutar desde aqui #################################
paste("El IC al 90% es: (",p_muestral-ME_90," , ", p_muestral+ME_90,")")
paste("El IC al 95% es: (",p_muestral-ME_95," , ", p_muestral+ME_95,")")
paste("El IC al 99% es: (",p_muestral-ME_99," , ", p_muestral+ME_99,")")
###### Hasta aqui ##########################################


#' ¿Se encuentra el valor teórico contenido por estos intervalos de confianza?

#' En base a los resultados obtenidos, ¿considera que la moneda efectivamente 
#' era equilibrada?



##' Ejercicio 16 c)

#' En el Ejercicio 7.d) de la primera parte del TP se simuló el lanzamiento de un 
#' millón de monedas y se calculó la proporción de veces que se obtuvo una “cara” 
#' como resultado. Se suponía que la moneda estaba cargada a favor de las caras, 
#' es decir, que la proporción teórica de caras que se debería obtener es mayor al 50%.
#' Veamos si los resultados obtenidos avalan este supuesto.

#' ¿Cuál fue la proporción muestral de caras que se obtuvo en el Ejercicio 7.d)?
  

# COMPLETAR CON EL RESULTADO DEL EJERCICIO 7d DE LA PRIMERA PARTE DEL TP


###### Ejecutar desde aqui #################################
p_muestral <-      ### COMPLETAR
  ###### Hasta aqui ##########################################


#' Este resultado muestral, ¿avala o rechaza la idea de que la moneda está cargada? 
#' Para dar una respuesta formal a esta pregunta, utilicemos el resultado muestral para 
#' construir intervalos de confianza para la proporción poblacional. Utilizaremos niveles 
#' de confianza del 90%, 95% y 99%.

#' ¿Qué supuestos son necesarios para construir el intervalo de confianza? ¿Se cumplen? 

#' ¿Qué distribución utilizará? 

#' ¿Cuáles son los valores críticos (z o t) para cada nivel de significatividad?

#' ¿Cuál es el margen de error para cada nivel de confianza?
#' INTENTE RESOLVER ANALÍTICAMENTE. UTILICE LOS RESULTADOS DE R PARA COMPROBAR.


###### Ejecutar desde aqui #################################
ME_90 <- qnorm(0.95)*sqrt(p_muestral*(1-p_muestral)/n)
ME_95 <- qnorm(0.975)*sqrt(p_muestral*(1-p_muestral)/n)
ME_99 <- qnorm(0.995)*sqrt(p_muestral*(1-p_muestral)/n)
paste("El ME para el IC al 90% es:",ME_90)
paste("El ME para el IC al 95% es:",ME_95)
paste("El ME para el IC al 99% es:",ME_99)
###### Hasta aqui ##########################################


#' ¿Cuál es el intervalo de confianza para la proporción poblacional al 
#' 90%, 95% y 99% respectivamente?
#' INTENTE RESOLVER ANALÍTICAMENTE. UTILICE LOS RESULTADOS DE R PARA COMPROBAR.


###### Ejecutar desde aqui #################################
paste("El IC al 90% es: (",p_muestral-ME_90," , ", p_muestral+ME_90,")")
paste("El IC al 95% es: (",p_muestral-ME_95," , ", p_muestral+ME_95,")")
paste("El IC al 99% es: (",p_muestral-ME_99," , ", p_muestral+ME_99,")")
###### Hasta aqui ##########################################


#' En base a los resultados obtenidos, ¿considera que la moneda efectivamente estaba 
#' cargada a favor de las caras?

