################################################################################
#                                                                              #
#              Modelos de Series de Tiempo Multivariadas                       # 
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
library(astsa)
library(xts)
library(forecast)
library(readxl)
library(ModelMetrics)
library(TSstudio)



#
# Modelar las series del estudio de mortalidad cardiovascular en Los Angeles
# x1(t) = mortalidad cardiovascular (cmort)
# x2(t) = temperatura (tempr)
# x3(t) = partículas de contaminación (part)
#
#

##Juntamos los datos

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

### quitando periodo


tasa_des_vf$periodo <- NULL
ahorro_vf$periodo <- NULL
pib_vf$periodo <- NULL

###convertimos a time series los datos

tasa_des_ts = ts(tasa_des_vf, start=c(2005,01), frequency  = 4)
ahorro_ts = ts(ahorro_vf, start=c(2005,01), frequency  = 4)
pib_ts = ts(pib_vf, start=c(2005,01), frequency  = 4)


x = cbind(tasa_des_ts, ahorro_ts, pib_ts)
plot(x)

# Modelamos la serie multivariada con la función VAR
#
install.packages("vars")
library(vars)
lagselect <- VARselect(x, lag.max = 15, type = "const")
lagselect$selection


fit.x = VAR(x, p=1, type="both") #"both" ajusta constante y tendencia
summary(fit.x)
#
# Seleccionar el orden p del modelo
#
library(vars)
VARselect(x, lag.max = 10, type="both") #both ajusta constante y tendencia
#
fit.BIC = VAR(x, p=1, type="both")
summary (fit.BIC)
#
# Analizamos los residuales
#
acf(resid(fit.BIC))
#
# Para ver los valores, por ejemplo lags 0-12
#
acf(resid(fit.BIC), 12)$acf
#
# Prueba Q multivariada
#
serial.test(fit.BIC, lags.pt=12, type="PT.adjusted")
#
residuales = (resid(fit.BIC))
str(residuales)
# 
# Alternativamente, para llevar a cabo la prueba de cero 
# correlaciones cruzadas de los residuales, utilizamos la 
# prueba multivariad de Ljunt-Box (definida en las notas)
# utilizando la función mq() del paquete MTS
#
MTS::mq(residuales, lag=12)
#
# Predicción
#
fit.pred = predict(fit.BIC, n.ahead=24, ci=0.95) #4 semanas
fanchart(fit.pred)  # grafica predicciones y error
