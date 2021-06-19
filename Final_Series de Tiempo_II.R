# Establecer working directory
setwd("C:/Users/rmartinez/Desktop/Maestria/Trimeste V/Series de Tiempo")

# Instalar paquetes y librerías requeridos
library(tseries)
library(astsa)
library(xts)
library(forecast)
library(readxl)
library(ModelMetrics)
library(TSstudio)
standev = function(x) {sd(x,na.rm=TRUE)}

Gastos = read_xlsx("Equipo5.xlsx", sheet = "Hoja1")

# Revisamos estructura, serie inicia en 01/97 y termina 01/21
head(Gastos)
tail(Gastos)

# Establecemos Serie de Tiempo
Gastos1 = ts(Gastos$Fatalities, start=c(1966,01), frequency = 1)

# Visualizamos comportamiento
plot(Gastos1, main="US Motor Vehicle Fatalities de 1966 a 2012")
acf2(Gastos1)

# Por la tendencia presentada tomamos una diferencia
# La serie es estacionaria y fluctua claramente en una media
Gastos1_diff <- diff(Gastos1)
plot(cbind(Gastos1,Gastos1_diff))
acf2(Gastos1_diff)

auto.arima(Gastos1)

#?sarima
# xdata	- univariate time series
# p - AR order (must be specified)
# d	- difference order (must be specified)
# q	- MA order (must be specified)
# P	- SAR order; use only for seasonal models
# D	- seasonal difference; use only for seasonal models
# Q	- SMA order; use only for seasonal models
# S	- seasonal period; use only for seasonal models

mod1 = sarima(Gastos1, p=0, d=1, q=1)
mod1

# Realizamos pronostico con modelo elegido
pronosticos = sarima.for(Gastos1_diff, n.ahead=12, 0,1,1,0,0,0,0, plot.all=TRUE)

# Dividimos datos en conjunto de Entrenamiento y Prueba
Gastos1_diff.train = window(Gastos1_diff, end=c(2008))
Gastos1_diff.test = window(Gastos1_diff, start=c(2008))

mod.train = sarima(Gastos1_diff.train, 0,1,1,0,0,0,0)
mod.train

fcst.Gastos1_lg.test = sarima.for(Gastos1_diff.train, n.ahead=7, 0,1,1,0,0,0,0,
                                  plot.all=TRUE)