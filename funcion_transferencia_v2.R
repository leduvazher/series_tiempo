################################################################################
#                    Modelos de función de transferencia                       #
################################################################################
#
# 
# Limpiar el ambiente
#
rm(list=ls())

# Establecer working directory
setwd("C:\\Users\\miguel.villalobos\\OneDrive - Red de Universidades Anáhuac\\DataSets\\SeriesTiempo")

# Instalar paquetes y librerías requeridos


if(!require(astsa)) install.packages("astsa")
library(astsa)
if(!require(TSA)) install.packages("TSA")
library(TSA)
if(!require(readxl)) install.packages("readxl")
library(readxl)
if(!require(forecast)) install.packages("forecast")
library(forecast)

standev = function(x) {sd(x,na.rm=TRUE)}
#
#
# Función para obtener la gráfica 4 en de los residuales
#         Esta función toma como entrada los valores residuales
#         del modelo genera la gráfica Normal de los residuales, 
#         residuales vs tiempo y las funciones de autocorrelación y
#         autocorrelación parcial de los residuales.
#
CuatroEnUno = function(residuales){
  par(mfrow=c(2,2), oma=c(0,0,0,0))
  qqnorm(residuales, xlab="Percentiles teóricos",
         ylab="Percentiles Observados", 
         main = "Grafica Normal de los Residuales")
  qqline(residuales, distribution=qnorm)
  plot(residuales, type="l", xlab="Tiempo", ylab = "Residuales",
       main = "Residuales vs Tiempo")
  acf(residuales, lag.max=25, type="correlation", 
      main = "FAC de los Residuales")
  acf(residuales, lag.max=25, type="partial", 
      main = "FACP de los Residuales")
  par(mfrow=c(1,1))
}

setwd("C:\\Users\\miguel.villalobos\\OneDrive - Red de Universidades Anáhuac\\DataSets\\SeriesTiempo")

#
# 
# Leer los datos de temperatura y viscosidad
#ChemProc = read_excel("data.xlsx", sheet = "ejercicio2")


TempVisc = read_excel("data.xlsx", sheet = "ejercicio2")
# 
# La variables dependiente es y(t) = viscosidad y la variable
# predictora es la temperatura (x(t))
#
xt = ts(TempVisc$annual_unemployment_rate)
yt = ts(TempVisc$fatalities)
par(mfrow=c(2,1))
plot(xt, type="l", xlab="Tiempo", 
     ylab=expression(italic(x[italic(t)])))
plot(yt, type="l", xlab="Tiempo",
     ylab=expression(italic(y[italic(t)])))
par(mfrow=c(1,1))
# 
# Pre-blanqueado o prewhitening:
# En este paso determiamos un posible modelo para la variable de 
# entrada x(t) (temperatura).  Para hacerlo procedemos a analizar su
# FAC y FACP
#
acf2(xt)
#
# Ajustamos un modelo AR(1)
# 
xt.fit = arima(xt, order=c(2,0,0), include.mean = FALSE)
xt.fit
#
# Para llevar a cabo el análisis de residuales
#
res.xt.fit = (residuals(xt.fit))
#
# 
# Analizamos la gráfica 4 en 1 de los residuales
#
CuatroEnUno(res.xt.fit)
#
# Elmodelo parece adecuado por tanto alpha(t) = (1-.73B)x(t)
#
# Hacemos el preblanqueado aplicando el operador (1-.73B) a
# x(t) y y(t)
#
n=length(xt)
alphat = xt[2:n] - (-0.2932)*xt[1:(n-1)]
betat = yt[2:n] - (-0.2932)*yt[1:(n-1)]
#
# Calculamos la FCC entre alpha y beta
#
rab = ccf(betat, alphat, main="FCC de alpha(t) y beta(t)",
          ylab="FCC")
abline(v=0, col = "blue")
abline(h=0, col="blue")
#
#
# Calculamos los estimadores de v(t)
#
vhat = sqrt(var(betat)/var(alphat))*rab$acf
n1 = length(vhat)
par(mar=c(4,5,4,4))
plot(seq(-(n1-1)/2,(n1-1)/2,1), vhat, type="h",xlab = "Lag",
     ylab=expression(italic(hat(v)[italic(j)])), cex = 0.7)
abline(v=0, col="blue")
abline(h=0, col="blue")
#
# Cómo b=3, vhat(0) corresponde al elemento (n1-1)/2+1 de vhat
# vhat(3) = vhat[(n-1)/2+1+3]
str(vhat)

str(vhat)
origen = (n1-1)/2+1
vhat_1 = vhat[origen+1]
vhat_1
vhat_2 = vhat[origen+2]
vhat_2



# Una vez que obtenemos los estimadores de w0, delta1 y delta2 utilizamos
# La diferencia entre y(t) y yhat(t) N(t) y lo modelamos
#
w0 = -1220.415
d1 = 1.82979
d2 = 0.48
Nhat = rep(0, times=n)
for (i in 4:n) {
  Nhat[i] = -yt[i]+d1*(Nhat[i-1]-yt[i-1])+
    d2*(Nhat[i-2]-yt[i-2])+w0*xt[i-3]
}
Nhat = Nhat[4:n]
plot(Nhat, type="l", xlab="Tiempo",
     ylab=expression(italic(hat(N)[italic(t)])))
#
# Determinamos un modelo para Nhat
#

acf2(Nhat)

Nhat.fit2 = arima(Nhat, order=c(4,0,0), 
                  include.mean = FALSE)
Nhat.fit2
res.Nhat.fit2 = residuals(Nhat.fit2)
CuatroEnUno (res.Nhat.fit2)

#
# Ahora llevamos a cabo la estimación del modelo completo
#
ts.xt = ts(xt)
lag3.xt = lag(ts.xt, -3)
ts.yt = ts(yt)
df = cbind(ts.xt, lag3.xt, ts.yt)
str(df)
dimnames(df)[[2]] = cbind("xt", "lag3.xt", "yt")
head(df)
df=na.omit(as.data.frame(df))
#
# En la función arimax order determina el modelo para el
# componente de error, es decir el orden del modelo ARMA
# para viscosidad (y(t) en la notación de la presentación
# si no tuviéramos una variable predictora temperatura(t)
# (x(t) en la notación de la presentación)
#
# xtransf es el input para la función de transferencia
# es decir x(t) en la notación de las notas.
#
# El parámetro transfer indica los órdenes (r,s) de la
# función de transferencia (nu(t)).
#
library(TSA)
visc.tf = arimax(df$yt, order=c(2,0,0), 
                 xtransf=data.frame(df$lag3.xt),
                 transfer=list(c(2,0)), include.mean=FALSE)
visc.tf
#
# El estimador de phi2 no es significativo por lo que estimamos
# el modelo sin este término y nalizamos los residuales
#
visc.tf = arimax(df$yt, order=c(1,0,0), 
                 xtransf=data.frame(df$lag3.xt),
                 transfer=list(c(2,0)), include.mean=FALSE)
visc.tf
#
#
# Analizamos los residuales
#
res.visc.tf = visc.tf$residuals
CuatroEnUno (res.visc.tf)
#
# Ahora analizamos la función de correlación cruzada
# entre los residuales del modelo de función de 
# transferencia y alpha(t) 
#
n=length(res.visc.tf)
nalpha = length(alphat)
ccf(res.visc.tf, alphat[(nalpha-n+1):nalpha], 
    main="FCC de alpha(t) y los res.visc.tf")
abline(v=0, col="blue")
abline(h=0, col="blue")
#
# Como se puede observar no hay indicación de que el modelo sea inadecuado
# 

