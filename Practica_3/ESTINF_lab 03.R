#==================================================================
# ESTADÍSTICA - GRADO DE INGENIERÍA INFORMÁTICA - ULL
# PRÁCTICA DE LABORATORIO 03: Estadística descriptiva de una variable
# Profesores de laboratorio: Carlos González Martín; Sergio Alonso
#==================================================================

#Cargamos el data frame HIPERTEN200
setwd("actualícese el directorio de trabajo")
load("HIPER200.RData")
attach(HIPER200)
str(HIPER200)

# Usaremos una librería básica, DESCTOOLS - herramientas de descriptiva - para acceder a funciones 
# que construyen los estadísticos descriptivos de una variable
library(DescTools)

# Tabla de frecuencias para variables cualitativas nominales y ordenadas
Freq(act_fisi)
Freq(estudios)
Freq(cafe)
Freq(genero)

Mode(genero)
Mode(cafe)

# Tabla de frecuencias para variables continuas
Freq(edad)
edad
min(edad);max(edad)
c_<-c(15,20,30,40,50,60,70,90)
Freq(edad, breaks=c_,right=FALSE)
Mode(edad)

# Estadística descriptiva de una variable cuantitativa
Desc(edad, plotit=FALSE)

# Cálculo de cuantiles
quantile(edad,0.35)

quantile(act_fisi,0.35, type=1)  #cuantil para una variable cualitativa ordinal

# Estadística descriptiva de todo el el conjunto de datos (data frame)
Desc(HIPER200, plotit=FALSE)  

#===========================================================================================
# Estadística descriptiva de una variable numérica, dentro del programa básico STATS

summary(estudios)
summary(edad)
min(edad)
max(edad)
range(edad)
sum(edad)
cumsum(edad)
mean(edad)
median(edad)
length(edad)
# desviación típica
sd(edad)

# desviación poblacional
sd1<-sqrt((length(edad)-1)/length(edad))*sd(edad)
sd1

quantile(edad,0.35)
IQR(edad)
table(edad)
unique(edad)

summary(HIPER200)     # Estadística descriptiva de todo el data frame con la libreria STATS
#===========================================================================================

# Momentos de una variable
library(moments)
moment(edad)
all.moments(TAsist1, order.max = 4, central = FALSE, absolute = FALSE, na.rm = FALSE)
all.moments(TAsist1, order.max = 4, central = TRUE, absolute = FALSE, na.rm = FALSE)

# GRÁFICOS UNIVARIANTES  1D

# Diagrama de barras con R básico (graphics)
#table(estudios)
barplot(table(estudios), main="Diagrama de barras",sub="Nivel de estudios", 
        space=2, beside=TRUE)

plot(table(estudios),type="l") #polígono de frecuencias
#table(act_fisi)
barplot(table(act_fisi), width=1.5, space=1, border="blue",col=5, ylim=c(0,100),
      main="",xlab="Actividad física", ylab="frecuencias", beside=FALSE, horiz=FALSE)
title(main = list("Estudio de Hipertensión", cex = 1.5, col = "blue", font = 3))

# Diagrama de sectores con R básico (graphics)
#table(profesio)
pie(table(profesio), main="Gráfico de sectores\nProfesión")

# Histograma con R básico (graphics)
hist(peso)
hist(peso, breaks=5) 
hist(peso, freq=TRUE, breaks=7, col="darkgray", xlab="Peso", ylab="frec",
     main="Histograma")

# ggplot2 un paquete(libreria) MUY COMPLETO de gráficos
library(ggplot2) 

# Diagrama de barras
qplot(act_fisi, data = HIPER200, geom = "bar", ylab="frecuencias")
ggplot(HIPER200, aes(x=profesio)) + geom_bar(fill="white", colour="black") + theme_bw()

# Histograma
qplot(peso, data = HIPER200, geom = "histogram", ylab="frecuencias")
ggplot(HIPER200, aes(x=peso)) + geom_histogram(binwidth=10, fill="white", colour="black") + theme_bw() 

# Polígono de frecuencias
ggplot(HIPER200, aes(x=peso)) +  geom_freqpoly(binwidth=10)+ theme_bw() 

# Histograma más polígono de frecuencias
ggplot(HIPER200, aes(x=edad)) +
  geom_histogram(binwidth=10, fill="white", colour="black") +
  geom_freqpoly(binwidth=10)+ theme_bw()

# Curva acumulativa o de distribución
plot.ecdf(x=edad, verticals=FALSE , do.points=FALSE ,
          main="Curva de distribución de la Edad", lwd=2, ylab="valores de F",xlab="Edad", 
          panel.first=grid(col="gray",lty="dotted"))

# Gráfico de cajas de Box
boxplot(TAsist0)
boxplot(TAsist0, range=1.5,ylab="mmHg", main="Estudio de Hipertensión", sub="TA Sistólica inicial")

ggplot(HIPER200, aes(x=1, y=peso)) +  geom_boxplot() + theme_bw() 

# gráfico de tallo y hojas
stem(edad)

# Cómo crear una función en R
sd_ <- function (x) {sqrt((length(x)-1)/length(x))*sd(x)}
sd_(edad)

   

#=====================
# FINAL DE LA PRÁCTICA
#=====================

