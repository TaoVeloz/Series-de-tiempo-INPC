
library(tidyverse)
library(tseries)     
#library(help="tseries")
library(forecast)
library(ggfortify)

library(xts)
library(aTSA)
library(fable)
library(tsibble)
library(plyr)

library(reshape)
library(RCurl)
library(tsbox)
library(readxl)



datos <- read_excel("INPC_1969_2022.xlsx", 
                    col_types = c("date", "numeric"))

datos <- ts(datos$Indice_Mensual, st = c(1969,1),end = c(2020,12),freq = 12)
plot(datos)


summary(datos)
sd(datos)
var(datos)

dif_datos <- diff(datos)
log_datos <- log(datos)


ggplot2::autoplot(dif_datos, ts.colour = "blue",
                  ts.linetype = "dashed") +
  ggtitle("INPC Primera Diferencia") +
  xlab("Año") + ylab("Variacion Indice") + 
  theme(plot.title = element_text(hjust = 0.5))

 # Se puede ver una inlfacion estable
adf.test(log_dif_datos)

# la serie es estable 

library(latticeExtra)
library(RColorBrewer)
library(lattice)


asTheEconomist(xyplot(datos,
                      main="INPC 1969-2022")
               ,xlab="Año_mes",ylab="Indice")

datos.descompuesta<-decompose(datos, type = "additive")
autoplot(datos.descompuesta)


# Una forma de hacer estacionaria una serie es trabajar 
# con las diferencias
pacf(dif_datos)
acf(diff(datos))

#test de Dickey fuller
adf.test(diff(datos))

# el resultado nos indica que hay una estructura en la serie  
#  estacionaria (el p-valor cae a casi 0)


# Se puede decir que la hicimos una serie estacionaria, 
 # la prueba estadística lo comprueba.


# ARIMA -------------------------------------------------------------------

modelo1 <-arima(datos, order = c(6,1,7), method = "ML");modelo1
Box.test(residuals(modelo1), type = "Ljung-Box")

modelo2 <-arima(datos, order = c(3,1,4), method = "ML");modelo2
Box.test(residuals(modelo2), type = "Ljung-Box")


modelo3 <-arima(datos, order = c(2,1,3), method = "ML");modelo3
Box.test(residuals(modelo3), type = "Ljung-Box")# jalo

modelo4 <-arima(datos, order = c(1,1,2), method = "ML");modelo4
Box.test(residuals(modelo4), type = "Ljung-Box")

modelo5 <-arima(datos, order = c(1,1,1));modelo5
Box.test(residuals(modelo5), type = "Ljung-Box")
predicc_m5 <- forecast::forecast(modelo7,h=36);predicc_m5

modelo6 <-arima(datos, order = c(0,1,1));modelo6
Box.test(residuals(modelo6), type = "Ljung-Box")

modelo7 <-arima(datos, order = c(1,1,0));modelo7
Box.test(residuals(modelo7), type = "Ljung-Box")# that's good


a <-ts.diag(modelo7);a
tsdiag(modelo7)
error <- residuals(modelo7)
plot(error)
library(forecast)

predicc_m7 <- forecast::forecast(modelo7,h=36);predicc_m7




predicc_m4 <- forecast::forecast(modelo4,h=36);predicc_m4
predicc_m <- forecast::forecast(propuesta.auto,h=36);predicc_m

b <- as.t(predicc_m4)
write.table(predicc_m4)
write_csv(predicc_m4, file = "prediccion_modelo4")



propuesta.auto<-auto.arima(datos)
propuesta.auto
Box.test(residuals(propuesta.auto), type = "Ljung-Box")



# serie energeticos -------------------------------------------------------


energeticos_ <- read_excel("INPC_1969_2022.xlsx", 
                           sheet = "Hoja2", col_types = c("date", "numeric"))

view(energeticos_)
energeticos_ <- ts(energeticos_$Indice_energeticos, st = c(1982,1),end = c(2020,12),freq = 12)
plot(energeticos_)


summary(energeticos_)
sd(energeticos_)
var(energeticos_)

dif_energeticos_ <- diff(energeticos_)
log_energeticos_ <- log(energeticos_)


ggplot2::autoplot(dif_energeticos_, ts.colour = "blue",
                  ts.linetype = "dashed") +
  ggtitle("INPC Energeticos Primera Diferencia") +
  xlab("Año") + ylab("Variacion Indice") + 
  theme(plot.title = element_text(hjust = 0.5))


adf.test(dif_energeticos_)
adf.test(log_energeticos_)

# la serie es estable 

library(latticeExtra)
library(RColorBrewer)
library(lattice)


asTheEconomist(xyplot(energeticos_,
                      main="INPC 1982-2020 Componente_Energeticos")
               ,xlab="Año_mes",ylab="Indice")

energeticos_descompuesta<-decompose(energeticos_, type = "additive")
autoplot(energeticos_descompuesta)


# Una forma de hacer estacionaria una serie es trabajar 
# con las diferencias
pacf(dif_energeticos_)
acf(diff(energeticos_))

# el resultado nos indica que hay una estructura en la serie  
#  estacionaria (el p-valor cae a casi 0)

# Se puede decir que la hicimos una serie estacionaria, 
# la prueba estadística lo comprueba.

automodelo <-auto.arima(energeticos_);automodelo
Box.test(residuals(automodelo), type = "Ljung-Box")

modelo415 <-arima(energeticos_, order = c(4,1,5), method = "ML");modelo415
Box.test(residuals(modelo415), type = "Ljung-Box")# si jala

tsdiag(modelo415)
error <- residuals(modelo415)
plot(error)
library(forecast)

predicc_m415 <- forecast::forecast(modelo415,h=36);predicc_m415

modelo213 <-arima(energeticos_, order = c(0,1,0), method = "ML");modelo213
Box.test(residuals(modelo213), type = "Ljung-Box")# si jala

predicc_m213 <- forecast::forecast(modelo213,h=36);predicc_m213




# Cointegración -----------------------------------------------------------



datos_ <- read_excel("INPC_1969_2022.xlsx", 
                           sheet = "Hoja1", col_types = c("date", "numeric"))

y <-ts(datos_$Indice_Mensual, st = c(1982,1), end = c(2020,12),
       freq = 12)

PP.test(y)#
PP.test(energeticos_)

dif_y <- diff(y)
# Las series con la primera diferencia son estacionarias
PP.test(dif_y)
PP.test(dif_energeticos_)

y <- dif_y
x <- dif_energeticos_

bfx <- as.matrix(cbind(y,x))
po.test(bfx)

#Peter C. B. Phillips y Sam Ouliaris (1990) muestran que las 
#pruebas de raíz unitaria basadas en residuos aplicadas a 
#los residuos de cointegración estimados no tienen las 
#distribuciones habituales de Dickey-Fuller bajo la hipótesis nula
#de no cointegración.

#Debido al fenómeno de regresión espuria bajo la hipótesis nula, 
#la distribución de estas pruebas tiene distribuciones asintóticas  
# que dependen de

#el número de términos de tendencias deterministas y
#el número de variables con las que se prueba la cointegración.

#En este caso p_value < 0.05 por lo tanto rechazamos la hipótesis
# nula, existe evidencia para sostener que las series son cointegradas, 
#es decir que hay una tendencia a largo plazo.



mmC1 <- lm(formula= y ~ x)
summary(mmC1)

residuales <- mmC1$residuals
summary(residuales)


residualPlot(mmC1, y_axis = c(-1,1))
autoplot(mmC1)


adf.test(residuales)

library(urca)
yy<-ur.df(residuales, type="trend",selectlags="AIC")

summary(yy)



