# practica-21
################### Ejercicio en clase 
####1) precios diarios del dólar últimos 6 meses 

#### 1) obtener las series
dol<- read.csv(file.choose())
ica<- read.csv(file.choose())
pib<- read.csv(file.choose())
bim<- read.csv(file.choose())
sal<- read.csv(file.choose())

#### 2) convertirlas en series de tiempo y graficarlas
td<-ts(dol, start=c(2016, 208), frequency=252)
td


tica<-ts(ica, start=c(2016, 001), frequency=252)
tpib<-ts(pib, start=2000, frequency=1)
tbim<-ts(bim, start=c(2016, 001), frequency=252)
tsal<-ts(sal, start=1993, frequency=1)
tsal
  
plot(tdol, col="blue")
plot(tica, col="blue")
plot(tpib, col="blue")
plot(tbim, col="blue")
plot(tsal, col="blue")


#### 3) analizar las gráficas y decidir qué modelo usar, modelos simples (media, ingenuo, etc), holt, holt amortiguado, holt Winter

installed.packages("forecast")
require(forecast)

install.packages("fpp")
require(fpp)

##################
####PRONOSTICOS###
##################

####MODELOS SIMPLES
idol<- naive (tdol, h=10) ########INGENUO######
pdol<- meanf(tdol, h=10)########PROMEDIO######
indol<- snaive (tdol, h=10)########INGENUO ESTACIONAL######
ddol<- rwf(tdol, h=10, drift=TRUE)########DERIVA######

x11()
plot(pdol, main="Pronostico Dolar",col=1)
lines(idol$mean, col=5)##mean=pronostico
lines(indol$mean, col=6)
lines(ddol$mean, col=7)
lines(td)
legend("topleft", lty=1, col=c(1,5,6,7),
       legend=c("media", "ingenuo", "ingenuo estacional", "deriva"))


####MODELOS HOLT
mod1<-holt(tdol, alpha=.8, beta=.2, initial="simple", h=10)#holt lineal simple
mod2<- holt(tdol, alpha=.8, beta=.2, initial="simple", exponential=TRUE, h=10)#holt lineal exponencial

x11()
plot(mod2, main= "Pronostico Dolar")
lines(mod2$mean, col=5, type="o", pch=3)
lines(mod1$mean, col=6)
legend("topleft", lty=1, col=c(5,6), pch=3,
       legend=c ("exponencial","simple"))

####MODELOS HOLT AMORTIGUADO
mod3<- holt(tdol, damped=TRUE)#Tendencia Aditiva amortiguado
mod4<- holt(tdol, exponencial=TRUE, damped=T)#Tendencia multiplicativa amortiguado

x11()
plot(mod4, main= "Pronostico Dolar")
lines(mod4$mean, col=2, type"o", pch=8)
lines(mod3$mean, col=1, type"o", pch=8)
legend("topleft", lty=1, col=c( 6,2), pch=8,
       legend=c("multiplicativo", "aditivo" ))

####MODELO HOTL WINTER

#holt winter
hw1<- hw(tdol, seasonal="additive",h=10)
hw2<- hw(tdol, seasonal="multiplicative",h=10)

x11()
plot(tdol, main= "Pronostico Dolar",  type="o", pch=17)
lines(hw1$mean, col="red", type="o", pch=8)
lines(hw2$mean, col="blue", type="o", pch=9)
legend("topleft", lty=1, col=c(1,"red","orange"), pch=c(17,8,9),
       legend=c( "Reales", "Aditivo", "Multiplicativo")) 
