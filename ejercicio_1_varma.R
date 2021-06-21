################################################################################
#                                                                              #
#                     Modelos ARMA(p,q) multivariados                          # 
#                                                                              #
################################################################################
# 
# Limpiar el ambiente
#
rm(list=ls())

# Establecer working directory
setwd("C:\\Users\\miguel.villalobos\\OneDrive - Red de Universidades Anáhuac\\DataSets\\SeriesTiempo")

# Instalar paquetes y librerías requeridos


if(!require(astsa)) {install.packages("astsa")}
library(astsa)
if(!require(readxl)) {install.packages("readxl")}
library(readxl)
if(!require(ModelMetrics)) {install.packages("ModelMetrics")}
library(ModelMetrics)
if(!require(vars)) install.packages("vars")
library(vars)
if(!require(MTS)) install.packages("MTS")
library(MTS)
if(!require(dplyr)) install.packages("dplyr")
library(dplyr)
if(!require(lubridate)) install.packages("lubridate")
library(lubridate)
if(!require(anytime)) install.packages("anytime")
library(tseries)
if(!require(tseries)) install.packages("tseries")
library(xts)
if(!require(xts)) install.packages("xts")
library(forecast)
if(!require(forecast)) install.packages("forecast")
library(TSstudio)
if(!require(TSstudio)) install.packages("TSstudio")

#
#
# Modelar los datos en econ5 (que viene con el paquete ASTSA) 
# contiene datos trimestrales de desempleo, GNP, Consumo e inversión
# gubernamental y privada del tercer cuarto de 1948 al segundo cuarto
# de 1988.
#
#data(econ5)
#str(econ5)

tasa_des = read_excel("data.xlsx", sheet = "tasa_des")#05-01 al 20-01
ahorro = read_excel("data.xlsx", sheet = "ahorro")  #93-01 al 20-04
pib = read_excel("data.xlsx", sheet = "pib") #80-01 al 21-01

###Filtramos los datos

tasa_des$periodo = anytime(tasa_des$periodo)
ahorro$periodo = anytime(ahorro$periodo)
pib$periodo = anytime(pib$periodo)

## ajustando tiempo

tasa_des_vf = tasa_des %>%
  filter(periodo > "2004-04-01" & periodo <= "2020-01-01")
ahorro_vf = ahorro %>%
  filter(periodo > "2004-04-01" & periodo <= "2020-01-01")
pib_vf = pib %>%
  filter(periodo > "2004-04-01" & periodo <= "2020-01-01")


####



# unemp = Unemployment            
# PIB = PIB
# ahorro = %ahorro
# unemployment = tasa de desempleo 
#
#
# Como primer paso calculamos el logaritmo de las series y eliminamos
# la tendencia
#
dfecon = as.data.frame(cbind(pib_vf, tasa_des_vf$tasa_desocupacion, ahorro_vf$ahorro))
colnames(dfecon) <- c("periodo", "pib", "tasa_desocupacion", "ahorro")
head(dfecon)
dfecon$periodo <- NULL
dfecon = log(dfecon)


#Lo guardamos como tasa de desempleo

ut = resid(trend.ut  <-  lm(tasa_desocupacion~time(tasa_desocupacion), data = dfecon, 
                         na.action=NULL))
##Lo guardamos con PIB

gdp = resid(trend.gt  <-  lm(pib~time(pib), data = dfecon, 
                         na.action = NULL))
##Lo guardamos como ahorro

sav = resid(trend.ct  <-  lm(ahorro~time(ahorro), data = dfecon, 
                         na.action = NULL))
#
# Creamos una matriz con las series
#
#x = t(matrix(cbind(ut, gdp, sav), ncol=3))
x = cbind(ut, gdp, sav)

head(x)
#

#Identificar p y q. PAQUETE MTS CSM

Eccm((x))


# Definimos el modelo que vamos a ajustar
#
library(marima)
modelo = define.model (kvar = 3, ar=c(1), ma=c(1)) #Definimos ARMA(2,1)
names(modelo)
arp = modelo$ar.pattern
arp
map = modelo$ma.pattern
map
#

# Ajustamos el modelo con la función marima
#
modelo.marma = marima(x, ar.pattern = arp, ma.pattern = map,
                      means=c(1,1,1), penalty = 1)
#
# Coeficientes autoregresivos
#
modelo.marma
short.form(modelo.marma$ar.estimates, leading=FALSE)
#short.form(modelo.marma$ar.fvalues, leading=FALSE)
short.form(modelo.marma$ar.pvalues, leading=FALSE)
#
# Coeficientes de promedios móvles
#
short.form(modelo.marma$ma.estimates,leading=FALSE)
#short.form(modelo.marma$ma.fvalues,leading=FALSE)
short.form(modelo.marma$ma.pvalues,leading=FALSE)
#
# Alternativamente podemos usar print
#
print(modelo.marma, estimaes = TRUE, pvalues=TRUE, 
      fvalues = FALSE)
#
# Análisis de los residuales
#
residuales = t(resid(modelo.marma))
plot.ts(residuales)
acf(residuales, na.action = na.pass)
str(residuales)
#
# Cuando cuando la dimensión k es muy grande, resulta muy
# útil visualizar la versión simplificada de las matrices de
# autocorrelaciones cruzadas para diferentes lags.
#  
# Hacemos esto con la función ccm del paquete MTS.
#
residuales = as.matrix (na.omit(residuales))
ccm(as.matrix(residuales))
# 
# para llevar a cabo la prueba de cero cros-correlaciones de los
# residuales, utilizamos la prueba multivariad de Ljunt-Box (definida
# en las notas).  Para llevar a cabo la prueba utilizamos la función
# mq() del paquete MTS
#
mq(residuales)
#
#
# Valores estimados de PIB
#

pib_ts = ts(pib_vf, start=c(2005,01), frequency  = 4)

pred = fitted(modelo.marma)
str(t(pred))
pred = ts(t(pred)[,1], start=start(pib_ts), freq=frequency(pib_ts)) +
  trend.ut$coefficients[1] + trend.ut$coefficients[2]*time(pib_ts)
plot(pred, ylab="PIB", xlab="Tiempo", 
     lwd=2, col=4)
#
# Pronósticos
#
x1 = matrix(data=NA, nrow=length(ut)+20, ncol=3)
x1[1:length(ut),] = x

fcst = arma.forecast(series= x1, marima=modelo.marma, 
                     nstart = 61, nstep=20)
names(fcst)
str(fcst$forecasts)

predict.ut = fcst$forecasts[1, 62:81]
stdv.ut = sqrt(fcst$pred.var[1,1,])
#
# Calcular límites inferior y superior al 90% de confianza
#
ul.ut = predict.ut + 1.645*stdv.ut
ll.ut = predict.ut - 1.645*stdv.ut
out.ut = rbind (predict.ut, ul.ut, ll.ut)
out.ut
#
# Graficar los resultados
#
ut.pred = ts(fcst$forecasts[1,], start=start(ut), 
                freq=frequency(ut))

fechas = time(ut.pred)

plot(fechas, fcst$forecasts[1,], type="l", col="blue",
     xlab="Tiempo", ylab="PIB",
     main = "Pronóstico de PIB")

fechas.fcst = fechas[62:81]
lines(fechas.fcst, predict.ut, type="l", col="black")
lines(fechas.fcst, ul.ut, type="l", col="red")
lines(fechas.fcst, ll.ut, type="l", col="red")




