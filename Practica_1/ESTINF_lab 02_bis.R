#==================================================================
# ESTAD�STICA - GRADO DE INGENIER�A INFORM�TICA - ULL
# PR�CTICA DE LABORATORIO 02: Operaciones con data frame (datasets)
# Profesores de laboratorio: Carlos Gonz�lez Mart�n; Sergio Alonso
#==================================================================

# Importaci�n del fichero HIPERTEN200 mediante el RStudio
# usando la opci�n de menu de File/Import Dataset
HIPERTEN200->HIPER200
# comprobando ahora que la importaci�n ha resultado ser un data frame de n�meros, tanto para cualitativas como cuantitativas
str(HIPER200)
attach(HIPER200)   #simplificamos el acceso a las variables del data.frame, evitando el $

# declaraci�n de variables cualitativas como factores, seg�n cada columna

HIPER200$sexo<-factor(sexo,levels=c(1,2),labels=c("masculino","femenino"),ordered=FALSE)
HIPER200$profesio<-factor(profesio,labels=c("campo","pescador","construccion","oficina","liberal","hogar","estudiante","otras"),ordered=FALSE)
HIPER200$sit_labo<-factor(sit_labo,labels=c("cuenta ajena","autnomo","parado","jubilado","otras"),ordered=FALSE)
HIPER200$cultura<-factor(cultura,labels=c("sin estudios","estudios primarios","estudios secundarios","estudios superiores"),ordered=TRUE) 
HIPER200$t_tabaco<-factor(t_tabaco, labels=c("no fumador","< 5 a�os","> 5 a�os"),ordered=TRUE)
HIPER200$cl_tabac<-factor(cl_tabac,labels=c("no fumador","poco","moderado","muy fumador"),ordered=TRUE)       
HIPER200$cafe<-factor(cafe,labels=c("no toma","poco","moderado","mucho"),ordered=TRUE)
HIPER200$act_fisi<-factor(act_fisi,labels=c("escasa","moderada","intensa"),ordered=TRUE)
HIPER200$sal<-factor(sal,labels=c("poca","normal","mucha"),ordered=TRUE)
HIPER200$es_hiper<-factor(es_hiper,labels=c("si","no","no lo sabe"))
HIPER200$cl_peso<-factor(cl_peso,labels=c("normal","obesidad discreta", "obesidad moderada", "obesidad grave","obesidad morbida"), ordered=TRUE)
HIPER200$conc_hta<-factor(conc_hta,labels=c("normotenso","bordeline","hipertenso"),ordered=TRUE)

# declaracion de variables cuantitativas como variables num�ricas, segun cada columna

HIPER200$edad<-as.numeric(HIPER200$edad)
HIPER200$peso<-as.numeric(HIPER200$peso)
HIPER200$talla<-as.numeric(HIPER200$talla)
HIPER200$sist_ini<-as.numeric(HIPER200$sist_ini)
HIPER200$dias_ini<-as.numeric(HIPER200$dias_ini)
HIPER200$sist_fin<-as.numeric(HIPER200$sist_fin)
HIPER200$dias_fin<-as.numeric(HIPER200$dias_fin)
attach(HIPER200)
str(HIPER200)

save(HIPER200,file="HIPER200.RData")

getwd()                             # muestra el directorio de trabajo actual
setwd("X:/Google Drive/ULL/Laboratorio grado estad�stica/P2")         # establece el directorio de trabajo actual

remove(list=ls())                # vacia el contenido del workspace
load("HIPER200.RData")

#====================================================
# PEQUE�AS COSAS QUE PODEMOS CAMBIAR EN UN DATA FRAME
#====================================================
# Cambiar el nombre de una variable
names(HIPER200)[names(HIPER200)=="cultura"]<-c("estudios") #por su nombre
names(HIPER200)[names(HIPER200)=="sist_ini"]<-c("TAsist0") #por su nombre
names(HIPER200)[names(HIPER200)=="dias_ini"]<-c("TAdias0") #por su nombre
names(HIPER200)[names(HIPER200)=="sist_fin"]<-c("TAsist1") #por su nombre
names(HIPER200)[names(HIPER200)=="dias_fin"]<-c("TAdias1") #por su nombre
names(HIPER200)[2]<-"genero"                 #por su posici�n en el fichero

# Cambiar las etiquetas de los niveles de un factor
library(plyr)
attach(HIPER200)
HIPER200$genero1<-revalue(genero,c("masculino"="M","femenino"="F"))

# Recodificar las etiquetas de los niveles de un factor
library(DescTools)
HIPER200$cl_tabac1<-Recode(HIPER200$cl_tabac, 
                         "no fumador" = c("no fumador"),
                         "fumador" = c("poco","moderado","muy fumador"))

# Crear un factor desde una variable num�rica
HIPER200$edad1<-cut(HIPER200$edad, breaks=c(15,30,60,90), labels=c("joven", "maduro", "jubilado"))

# Crea una variable con el n�mero secuencial de cada caso
HIPER200$id<-seq(dim(HIPER200)[1])   

# Reordenacion de columnas en el date frame
HIPER200<-HIPER200[c(23,1,12,13,15:18,2:10,14,11,19:22)]   #tener cuidado

# Seleccionar subconjuntos de datos y de variables
mydata02<-subset(HIPER200,select=c(-genero1,-cl_tabac1,-edad1))  #sin las variable derivadas que hemos creado
mydata03<-subset(HIPER200, genero=="femenino")           #todas las variable pero solo para femenino
mydata04<-subset(HIPER200, edad>=30 & genero=="masculino" & t_tabaco=="no fumador")


# Reordenacion FINAL de columnas en el date frame
HIPER200<-HIPER200[c(1:12,14,13,15:20)]   #estructura definitiva del fichero HIPER200

save(HIPER200,file="HIPER200.RData")


#Ejercicios
# El �ndice de masa corporal, IMC, sirve para catalogar el nivel de obesidad en relaci�n con la altura. Su f�rmula
# es IMC = peso (kilos)/estatura(metros)^2. A�ade a HIPER200 la variable calculada IMC
# 
# 
# La clasificaci�n del nivel de obesidad se realiza seg�n la siguiente tabla:
# Bajo peso, IMC < 18,50
# Normal, 18,50 - 24,99
# Sobrepeso, >= 25 (y si es >=30, Obesidad)
# Cataloga el nivel de obesidad conocido el IMC






















HIPER200$IMC <- peso/(talla/100)^2
attach(HIPER200)
str(HIPER200)


HIPER200$IMC_nivel <- cut(IMC, breaks=c(0,18.50,25, 30, 100), labels = c("Bajo peso", "Normal", "Sobrepeso", "Obesidad"))
  


#=====================
# FINAL DE LA PR�CTICA
#=====================
