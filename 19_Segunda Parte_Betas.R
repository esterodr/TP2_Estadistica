########################################################

## TP en RStudio de Estadística para Economistas I #################

## Cátedra de Tamara Burdisso – FCE-UNLP ##################

## Este ejercicio fuer elaborado por Esteban Rodriguez ########

## para el curso de Estadística FCE-UBA de Tamara Burdisso ##########

##durante los años 2018 a 2022   ##################

########################################################

## Segunda Parte - Ejercicio 19 #########################

########################################################

##' El "Beta" de un activo financiero

##' La economía financiera ha desarrollado algunas medidas y métodos de análisis para ayudar a los inversores 
##' a medir y a controlar el riesgo financiero en el desarrollo de carteras de inversión. El riesgo se puede 
##' dividir en riesgo diversificable y riesgo no diversificable. El *riesgo diversificable* es el riesgo que 
##' entrañan determinadas empresas y sectores e incluye los conflictos laborales, la nueva competencia, los 
##' cambios del mercado de consumidores y otros muchos factores. Este riesgo se puede controlar por medio de 
##' una cartera mayor e incluyendo activos cuyos rendimientos tengan *correlaciones negativas*. 
##' 
##' El *riesgo no diversificable* es el riesgo es el riesgo que entraña el conjunto de la economía. Los cambios 
##' económicos provocados por los ciclos económicos, las crisis internacionales, los cambios de la demanda 
##' mundial de energía u otros factores afectan a todas las empresas, pero no producen el mismo efecto en todas
##' ellas. Una medida de qué tan relevante es el riesgo no diversificable para determinada empresa se obtiene 
##' por medio del coeficiente beta, como se analiza a continuación.
##' 
##' *Utilizando el modelo CAPM para el cálculo del coeficiente beta*
##' 
##' Una idea fundamental detrás de las finanzas es que un inversor necesita de un incentivo financiero para que 
##' esté dispuesto a tomar un riesgo. Dicho de otra manera, el retorno esperado de una inversión riesgosa (R), 
##' debe ser superior al retorno de una inversión segura o libre de riesgo (R0). Es decir, el *retorno excedente* 
##' *esperado* (R - R0) de una inversión riesgosa, como comprar acciones de determinada empresa, debe ser positivo.
##' 
##' El *Modelo de valoración de activos financieros* (CAPM, por sus siglas en inglés) formaliza esta idea. De 
##' acuerdo con el CAPM, el *retorno excedente esperado* de un activo es proporcional al retorno excedente esperado 
##' de una cartera con todos los activos disponibles: la cartera del mercado. Esto es, el CAPM dice que:
  
##' 
##' (R - R0) = β * (Rm - R0)
##' 
##' donde Rm es el retorno excedente esperado de la cartera del mercado y *β* es el coeficiente de la regresión
##' lineal de (R - R0) sobre (Rm - R0). Es decir, el coeficiente β de una empresa es el coeficiente de la pendiente 
##' que se obtiene cuando se realiza una regresión del rendimiento excedente de una empresa con respecto a los 
##' rendimientos excedentes de un índice de mercado.
##' 
##' Este coeficiente β indica la sensibilidad de los rendimientos de una empresa a los rendimientos totales del 
##' mercado. En la mayoría de los casos, el coeficiente será positivo, pero en algunos, los rendimientos de una 
##' empresa varían en sentido contrario al del conjunto de la economía. Si los rendimientos de la empresa siguen 
##' al mercado exactamente, tendremos β=1 . Por su parte,, una acción con β<1 es menos sensible a los rendimientos 
##' del mercado y una acción con β>1 es más sensible que el resto del mercado. Cuando el valor de beta es más alto, 
##' el rendimiento exigido a la inversión tiene que ser mayor. Este mayor rendimiento exigido tendría en cuenta el 
##' hecho de que en el rendimiento de las acciones influye más el riesgo de mercado no diversificable. La 
##' diversificación por medio de carteras mayores no puede tener en cuenta los cambios generales del mercado.
##' 
##' El cálculo de los *β* es una práctica usual en finanzas y pueden encontrarse distintas estimaciones en la web.
##' Por ejemplo, en el siguiente link se encuentran los β estimados para distintas acciones del mercado de EEUU:
##' http://www.abg-analytics.com/stock-betas.shtml
##' 
##' Por lo general, como inversión segura o libre de riesgo se suelen tomar los bonos de corto plazo del Tesoro
##' de los EEUU, siendo su retorno el valor R0 utilizado para calcular el retorno excedente. Pero dependiendo de
##' los mercados a los que acceda el inversor, pueden tomarse otros activos.
##' 
##' En este ejercicio, calcularemos el *β* de dos acciones del mercado argentino: Banco Galicia e YPF.
##' Como cartera del mercado consideraremos al índice MERVAL, el cual está compuesto por las principales acciones del
##' mercado local. Como activo libre de riesgo, consideraremos un plazo fijo en pesos, estimando su rendimiento a
##' través de la tasa BADLAR publicada por el BCRA, y que es la tasa de referencia para los plazos fijos mayores a 
##' 1 millón de pesos. Trabajaremos con datos de frecuencia mensual.

##' Antes de comenzar a trabajar, veamos un gráfico de cada una de las series:


###### Ejecutar desde aqui #################################
library(tidyverse)
library(lubridate)
load("./archivos/datos_beta.Rda")
datos %>% gather(Activo, Cotizacion, -Fecha) %>%
  ggplot(aes(x=Fecha, y=Cotizacion)) +
  geom_line() +
  ylab("") +
  facet_wrap(.~Activo, scales="free") +
  theme_classic() +
  ggtitle("Cotizaciones")
###### Hasta aqui ##########################################


##' Tenga en cuenta que la BADLAR está en % (es una tasa), mientras que el resto de 
##' los activos está en pesos.

##' Se observa que hay datos desde 2005 hasta la actualidad, y que el mercado ha pasado
##' a lo largo de esos años por distintos niveles de volatilidad.
##' Trabajaremos primero con las series completas, enfocándonos luego en un período más 
##' corto para ver si cambia en algo el análisis.

##' Calculamos las variaciones porcentuales mensuales de cada acción. Para la tasa BADLAR, 
##' como está expresada en términos anuales, calculamos el rendimiento mensual implícito 
##' en esas tasas.


###### Ejecutar desde aqui #################################
rendimientos <- data.frame(Fecha = datos$Fecha[2:nrow(datos)],
                           MERVAL = 100*(diff(datos$MERVAL)/datos$MERVAL[1:nrow(datos)-1]),
                           GGAL = 100*(diff(datos$GGAL)/datos$GGAL[1:nrow(datos)-1]),
                           YPFD = 100*(diff(datos$YPFD)/datos$YPFD[1:nrow(datos)-1]),
                           BADLAR = 100*((1+datos$BADLAR/100)^(1/12)-1)[2:nrow(datos)])
###### Hasta aqui ##########################################


##' Veamos ahora un gráfico de estos rendimientos:


###### Ejecutar desde aqui #################################
rendimientos %>% gather(Activo, Rendimientos, -Fecha) %>%
  ggplot(aes(x=Fecha, y=Rendimientos)) +
  geom_line() +
  ylab("") +
  facet_wrap(.~Activo, scales="free") +
  theme_classic() +
  ggtitle("Rendimientos")
###### Hasta aqui ##########################################


##' ¿Qué comportamientos observa en los rendimientos de las acciones por un lado y en
##' la tasa de plazos fijos BADLAR por el otro?

##' Calculemos ahora los *rendimientos excedentes* de cada acción. Es decir, al rendimiento
##' de cada activo le restamos el rendimiento del activo libre de riesgo (plazo fijo)


###### Ejecutar desde aqui #################################
rendimientos_exc <- data.frame(Fecha = rendimientos$Fecha,
                               MERVAL = rendimientos$MERVAL-rendimientos$BADLAR,
                               GGAL = rendimientos$GGAL-rendimientos$BADLAR,
                               YPFD = rendimientos$YPFD-rendimientos$BADLAR)
###### Hasta aqui ##########################################


##' Grafiquemos estos rendimientos excedentes:


###### Ejecutar desde aqui #################################
rendimientos_exc %>% gather(Activo, Rendimientos, -Fecha) %>%
  ggplot(aes(x=Fecha, y=Rendimientos)) +
  geom_line() +
  geom_hline(yintercept = 0) +
  facet_wrap(.~Activo) +
  theme_classic() +
  ggtitle("Rendimientos Excedentes")
###### Hasta aqui ##########################################


##' ¿Observa alguna diferencia entre cada activo?

##' Veamos ahora cómo se relaciona el rendimiento excedente de cada una de las acciones
##' con el rendimiento excedente del MERVAL
##' Comencemos con el Banco Galicia, calculando la regresión lineal mediante *Mínimos Cuadrados Ordinarios*


###### Ejecutar desde aqui #################################
MCO_GGAL <- lm(rendimientos_exc$GGAL ~ rendimientos_exc$MERVAL)
summary(MCO_GGAL)
###### Hasta aqui ##########################################


##' En los resultados de la regresión observará una tabla con dos coeficientes: 
##' (Intercept), correspondiente al α, y (rendimientos_exc$MERVAL), correspondiente al β. 
##' Bajo la columna “Estimate” encontrará el valor estimado para el coeficiente y en la 
##' última columna de la tabla está el valor-p del test de hipótesis cuya hipótesis nula es 
##' que el coeficiente es igual a 0. Si al lado del valor-p aparecen asteriscos (*, **, o ***),
##' significa que la hipótesis nula se rechaza con un nivel de significatividad del 5%, 1% y 0,1% 
##' respectivamente. 

##' ¿Qué coeficientes resultan significativos?
##' ¿Cuál es el valor del Beta para el Banco Galicia?
##' ¿Cuál es el valor del R2 ajustado (Adjusted R-squared)?

##' Realice el gráfico de dispersión entre ambas variables, junto con la recta de regresión


###### Ejecutar desde aqui #################################
rendimientos_exc %>% mutate(GGAL_fit = predict(MCO_GGAL)) %>%
  ggplot() +
  geom_point(aes(x=MERVAL, y=GGAL)) +
  geom_line(aes(x=MERVAL, y=GGAL_fit), color="red") +
  theme_classic() +
  ggtitle("Rendimientos Excedentes GGAL vs MERVAL")
###### Hasta aqui ##########################################


##' ¿Considera que la regresión lineal es útil para explicar el retorno excedente del Banco Galicia
##' en base al retorno excedente del MERVAL?

##' ¿Observa algún valor claramente atípico?

##' La estimación de los Betas y de la recta de regresión pueden verse afectadas por la existencia de 
##' valores atípicos. Realicemos nuevamente la estimación por MCO, pero excluyendo previamente 
##' algunas observaciones que podrían ser atípicas.


###### Ejecutar desde aqui #################################
rendimientos_exc_aj <- rendimientos_exc %>% filter(MERVAL>-25)
MCO_GGAL_aj <- lm(rendimientos_exc_aj$GGAL ~ rendimientos_exc_aj$MERVAL)
summary(MCO_GGAL_aj)
###### Hasta aqui ##########################################


##' ¿Observa resultados muy diferentes?

##' Restrinjamos el análisis a un período donde las tasas de interés fueron relativamente estables.
##' Por ejemplo, consideremos el período 2007-2012


###### Ejecutar desde aqui #################################
rendimientos_exc_restr <- rendimientos_exc %>% filter(Fecha>="2007-01-01" & Fecha<"2013-01-01")
##' Calculemos la regresión lineal por MCO
MCO_GGAL_restr <- lm(rendimientos_exc_restr$GGAL ~ rendimientos_exc_restr$MERVAL)
summary(MCO_GGAL_restr)
###### Hasta aqui ##########################################


###### Ejecutar desde aqui #################################
rendimientos_exc_restr %>% mutate(GGAL_fit = predict(MCO_GGAL_restr)) %>%
  ggplot() +
  geom_point(aes(x=MERVAL, y=GGAL)) +
  geom_line(aes(x=MERVAL, y=GGAL_fit), color="red") +
  theme_classic() +
  ggtitle("Rendimientos Excedentes GGAL vs MERVAL (2007-2012)")
###### Hasta aqui ##########################################


##' ¿Observa resultados muy diferentes? En base a todos estos resultados, ¿qué tan estable
##' considera que es el Beta de la acción del Banco Galicia?

##' Repitamos el ejercicio, pero ahora para YPF. Comencemos estimando la regresión para todo
##' el período, incluyendo todas las observaciones.


###### Ejecutar desde aqui #################################
MCO_YPFD <- lm(rendimientos_exc$YPFD ~ rendimientos_exc$MERVAL)
summary(MCO_YPFD)
###### Hasta aqui ##########################################


##' ¿Qué coeficientes resultan significativos?
##' ¿Cuál es el valor del Beta para YPF?
##' ¿Cuál es el valor del R2 ajustado?
##' Compare sus resultados con los obtenidos para el Banco Galicia.

##' Realice el gráfico de dispersión entre ambas variables, junto con la recta de regresión


###### Ejecutar desde aqui #################################
rendimientos_exc %>% mutate(YPFD_fit = predict(MCO_YPFD)) %>%
  ggplot() +
  geom_point(aes(x=MERVAL, y=YPFD)) +
  geom_line(aes(x=MERVAL, y=YPFD_fit), color="red") +
  theme_classic() +
  ggtitle("Rendimientos Excedentes YPFD vs MERVAL")
###### Hasta aqui ##########################################


##' ¿Considera que la regresión lineal es útil para explicar el retorno excedente de YPF
##' en base al retorno excedente del MERVAL?

##' ¿Observa algún valor atípico?

##' Como ya dijimos, la estimación de los Betas y de la recta de regresión pueden verse afectadas 
##' por la existencia de valores atípicos. Realicemos nuevamente la estimación por MCO, pero 
##' excluyendo previamente algunas observaciones que parecen ser atípicas.


###### Ejecutar desde aqui #################################
MCO_YPFD_aj <- lm(rendimientos_exc_aj$YPFD ~ rendimientos_exc_aj$MERVAL)
summary(MCO_YPFD_aj)
###### Hasta aqui ##########################################


##' ¿Observa resultados muy diferentes?

##' Restrinjamos el análisis a un período donde las tasas de interés fueron relativamente estables.
##' Por ejemplo, consideremos el período 2007-2012


###### Ejecutar desde aqui #################################
rendimientos_exc_restr <- rendimientos_exc %>% filter(Fecha>="2007-01-01" & Fecha<"2013-01-01")
##' Calculemos la regresión lineal por MCO
MCO_YPFD_restr <- lm(rendimientos_exc_restr$YPFD ~ rendimientos_exc_restr$MERVAL)
summary(MCO_YPFD_restr)
###### Hasta aqui ##########################################


###### Ejecutar desde aqui #################################
rendimientos_exc_restr %>% mutate(YPFD_fit = predict(MCO_YPFD_restr)) %>%
  ggplot() +
  geom_point(aes(x=MERVAL, y=YPFD)) +
  geom_line(aes(x=MERVAL, y=YPFD_fit), color="red") +
  theme_classic() +
  ggtitle("Rendimientos Excedentes YPFD vs MERVAL (2007-2012)")
###### Hasta aqui ##########################################


##' En base a todos estos resultados, ¿qué tan estable considera que es el Beta de la acción del YPF?

##' Si tuviera que considerar invertir en acciones del Banco Galicia o en YPF, ¿qué acción considera 
##' más riesgosa? ¿Con cuál esperaría un retorno mayor?

##' Fin del ejercicio. Borramos la memoria.
rm(list=ls())
