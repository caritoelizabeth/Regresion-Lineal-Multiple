#########ESCUELA POLITECNICA NACIONAL#######
############FACULTAD DE CIENCIAS###########
########MODELOS LINEALES Y EXPERIEMENTOS#######

#Nombre: Carolina Yépez Castillo 
#Trabajo en R 02

#1.- Realice un Fork del repositorio Regresion-Lineal-Multiple.

#2.-  Genere un archivo rlm.R, de tal forma que le permita responder las siguientes preguntas:
#2.1.- Leer los archivos poblacion1.xlsx y poblacion2.xlsx , y analizar sus dimensiones.
library(readxl)
poblacion1<-read_excel("poblacion1.xlsx", sheet=1, na=" ")

poblacion2<-read_excel("poblacion2.xlsx", sheet=1, na=" ")

View(poblacion1)
str(poblacion1)

View(poblacion2)
str(poblacion2)

#Analizando las dimensiones de cada archivo

#Se puede observar que poblacion1 tiene:
nrow(poblacion1) #observaciones
ncol(poblacion1) #variables

#Asì, mismo poblacion2 tiene:
nrow(poblacion2) #observaciones
ncol(poblacion2) #variables

#2.2.- Una los archivos leídos en un mismo objeto llamado población.
#Uso la funcion merge para unir dos data frames
poblacion <- merge(poblacion1,poblacion2,by="identificador",suffixes=c("",""))
View(poblacion)

#2.3.- Cree un código que identiﬁque la clase de cada variable y genere diagramas
#de cajas para variables continuas y diagramas de barras para variables discretas.

#Para variables continuas
for(j in 2:dim(poblacion)[2]){
  if(is.numeric(poblacion[,j])==TRUE){
    boxplot(poblacion[,j])
    title(names(poblacion)[j])
  }
}

#Para variables discretas
for(j in 2:dim(poblacion)[2]){
  if(is.numeric(poblacion[,j])!=TRUE){
    barplot(table(poblacion[,j]))
    title(names(poblacion)[j])
  }
}

#2.3.- Cree un código que calcule automáticamente el mínimo, media,
#máximo, desviación estándar, primer cuartil de cada variable 
#numérica y la frecuencia en el caso de variables categóricas.

tipo <- 1:dim(poblacion)[2]
maxi <- 1:dim(poblacion)[2]
mini <- 1:dim(poblacion)[2]
media <- 1:dim(poblacion)[2]
desviacion <- 1:dim(poblacion)[2]
primer_cuartil <- 1:dim(poblacion)[2]



for(j in 2:dim(poblacion)[2]){
  if(is.numeric(poblacion[,j])==TRUE){
    tipo[j] <- class(poblacion[,j])
    maxi[j] <- max(poblacion[,j])
    mini[j] <- min(poblacion[,j])
    media[j] <- mean(poblacion[,j])
    desviacion[j] <- sd(poblacion[,j])
    primer_cuartil[j] <- quantile(poblacion[,j],probs=seq(0,1,0.25),na.rm = FALSE)[2]
  }else {
    
    tipo[j] <- class(poblacion[,j])
    maxi[j] <- NA
    mini[j] <- NA
    media[j] <- NA
    desviacion[j] <- NA
    primer_cuartil[j] <- NA 
    table(poblacion[,j])/dim(poblacion)[1]
  }
}

#####Resultados
tipo
#[1] "1"         "numeric"   "numeric"   "numeric"   "numeric"   "numeric"  
#[7] "numeric"   "numeric"   "character" "character"
maxi
# 1.0  18.7  32.2  41.5  88.5  50.5 704.1  27.2    NA    NA
mini
# 1.0   1.7  -3.3   7.5   5.7  11.7  32.0 -45.6    NA    NA
media
# [1]   1.0000   7.2325   6.6475  27.0725  53.1750  26.7875 111.0350 -15.9625       NA
#[10]       NA
desviacion
# [1]   1.000000   3.621403   8.148776   9.036337  25.420784   6.854165 108.889133
# [8]  15.758623         NA         NA
primer_cuartil
#   1.000   4.175   1.975  20.375  33.250  23.650  62.325 -32.050      NA      NA


resultados1 <- data.frame(names(poblacion),tipo,maxi,mini,media,desviacion,primer_cuartil)
View(resultados1)

#Ahora considerando las variables categoricas "region" y "serv.bas.compl", 
#hay que calcular las frecuencias:

frecuencia_region  <- table(poblacion[,9])/dim(poblacion)[1]
frecuencia_ser.bas.compl <- table(poblacion[,10])/dim(poblacion)[1]

frecuencia_region
frecuencia_ser.bas.compl

#2.3.- Calcule la correlación entre la variable dependiente poblacion
#  y cada una de las variables explicativas (numéricas).

correlacion <- 3:dim(poblacion)[2]
correlacion[1] <- NA


for(j in 2:dim(poblacion)[2]){
  if(is.numeric(poblacion[,j])==TRUE){
    correlacion[j] <- cor(poblacion[,2], poblacion[,j])
  } else {
    correlacion[j] <- NA
  }
}

#La correlación con cada variable será:

resultados2 <- data.frame(names(poblacion),correlacion) 
resultados2
View(resultados2)

#2.5.- Considere la variable categórica serv.bas.compl con una conﬁabilidad 
# del 90%, ¿Puede asumirse que la media de la variable poblacion en el 
# grupo serv.bas.compl: SI es distinta a la media del grupo 
#serv.bas.compl: NO ?


#Transformo a variable factor las columnas "region" y "serv.bas.compl" 
#para poder usar el test t(student)

serv.bas.compl_fac <- factor(poblacion[,"serv.bas.compl"],levels=c("SI","NO"),labels=c("si","no"))
region_fac <- factor(poblacion[,"region"],levels=c("A","B"),labels=c("a","b"))
poblacion_fac <- data.frame(poblacion[,1:7],region_fac,serv.bas.compl_fac)

#Diagrama de cajas
plot(poblacion ~ serv.bas.compl_fac , data = poblacion_fac) 

# Por último realizamos la prueba de hipotesis: 
pr_hipotesis <- t.test(poblacion ~ serv.bas.compl_fac , data=poblacion_fac, conf.level=0.9)
pr_hipotesis


# Por lo tanto se acepta que la diferencia de medias es igual a cero.

#2.6.- Considerando los cálculos anteriores genere el modelo de regresión 
# lineal múltiple que mejor se ajuste a los datos. 
# Interprete los coeﬁcientes obtenidos.



# Hacemos en modelo de regresión lineal multiple con la variable 
# independiente "poblacion", y como variables dependientes "var.pobl.mayor"
# "menores.18", "tasa.crimen" ya que fueron las que presentaron mayor correlacion 


reg1 <- lm(poblacion~var.pobl.mayor+menores.18+tasa.crimen, data=poblacion_fac)
summary(reg1)

qf(0.90,df1=3,df2=36)

#2.7.- Interprete el R2

#Analizando la regresión se tuvo un R^2: 
R2 <- summary(reg1)[["r.squared"]]
#R2= 0.1533239

#Esto quiere decir que la regresion explica el: 
porcentaje <- 100*summary(reg1)[["r.squared"]] 
#porcentaje=15.33%
#de la variabilidad.

#2.8.-  Analice la signiﬁcancia de la regresión y de cada uno de los 
# parámetros individuales. 

anova <- aov(reg1)
summary(anova)

#2.9.-  Realice un análisis detallado de los residuos.

residuos<-1:40  
for(i in 1:40){
  residuos[i]<-summary(reg1)[["residuals"]][i]
}

#poblacion vs residuos
plot(poblacion[,"poblacion"],residuos)

#histograma 
hist(residuos)

qqnorm(residuos)
qqline(residuos,col="red")
