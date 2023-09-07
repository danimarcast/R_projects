library(car)
library(nortest)
library(MASS)
library(readr)
library(latex2exp)
library(ggplot2)
library(tibble)
library(GGally)
library(csv)
library(MVN)
library(pracma)
library(dplyr)
library(ggpubr) # Para Realizar QQplots con función ggqqplot(model$resi)
library(QuantPsyc) # Para verificar multinormalidad
library(tidyverse)
library(devtools) # Instalar paquetes que no están en el CRAN
library(alr3)
y <- c(65, 156, 100, 134, 16, 108, 121, 
       4, 39, 143, 56, 26, 22, 1, 1, 5, 65)
x <- c(3.36, 2.88, 3.63, 3.41, 3.78, 4.02, 4, 
       4.23, 3.73, 3.85, 3.97, 4.51, 4.54, 5, 5, 4.72, 5)


plot(x,y,pch=19,cex=1.2,ylab =TeX(r'($y_i$ tiempo de supervivencia)'),xlab =TeX(r'($x_i$ conteo de globulos blancos)'))

modelo2 <- glm(y ~ x, family=Gamma(link="log"))
summary(modelo2, dispersion=1)


predicGLM<-function(modelo,x1){
  y<-exp(modelo2$coefficients[1]+modelo2$coefficients[2]*x1)
  return(y)
}
y1<-predicGLM(modelo2,x1)
x1<-seq(min(x),max(x),len=100)
lines(x1,y1,col="blue")



M<-read_csv("/Users/danimathud/Documents/GitHub/R_projects/ModelosEstadisticos1/Datas/Mujeres.csv")
H <- read_csv("/Users/danimathud/Documents/GitHub/R_projects/ModelosEstadisticos1/Datas/Hombres.csv")

años <- 1938:1947

medsh <- c(18,16,7,12,24,16,22,12,22,28)
medth <- c(22,23,17,25,50,21,32,14,34,37)

artsh <- c(16,13,11,12,8,11,4,4,NULL,13)
artth <- c(30,22,25,14,12,20,10,12,NULL,23)

ciensh <- c(9,9,12,1,20,16,25,32,4,25)
cienth <- c(14,12,19,15,28,21,31,38,5,31)

ingsh <- c(10,7,12,8,5,1,16,19,NULL,25)
ingth <- c(16,11,15,9,7,2,22,25,NULL,35)

artsm <- c(14,11,15,15,8,13,18,18,1,13)
arttm <- c(19,16,18,21,9,13,22,22,1,16)

ciensm <- c(1,4,6,3,4,8,5,16,1,10)
cientm <- c(1,4,7,3,4,9,5,17,1,10)

medproph <- medsh / medth
artproph <- artsh / artth
cienproph <- ciensh / cienth
ingproph <- ingsh / ingth
artpropm <- artsm / arttm
cienpropm <- ciensm / cientm

plot(años,medproph)

modelo_medh <- glm(medproph ~ años, family=quasibinomial(link = "logit"))
summary(modelo_medh)

lines(años,modelo_medh$fit)

modelo_medh <- glm( cbind(medsh, medth - medsh) ~ años, family=binomial )

df1 <- data.frame( cbind(años,medproph,"Medicina","Hombre") )
colnames(df1) <- c("Año", "Proporción", "Carrera", "Género")
df2 <- data.frame( cbind(años,artproph,"Arte","Hombre") )
df3 <- data.frame( cbind(años,cienproph,"Ciencia","Hombre") )
df4 <- data.frame( cbind(años,ingproph,"Ingeniería","Hombre") )
df5 <- data.frame( cbind(años,artpropm,"Arte","Mujer") )
df6 <- data.frame( cbind(años,cienpropm,"Ciencia","Mujer") )
colnames(df2) <- c("Año", "Proporción", "Carrera", "Género")
colnames(df3) <- c("Año", "Proporción", "Carrera", "Género")
colnames(df4) <- c("Año", "Proporción", "Carrera", "Género")
colnames(df5) <- c("Año", "Proporción", "Carrera", "Género")
colnames(df6) <- c("Año", "Proporción", "Carrera", "Género")

dfac <- filter(df, Carrera == "Ciencia" | Carrera == "Ingenería")
df <- rbind(df1,df2,df3,df4,df5,df6)
modelo <- glm(Proporción ~ Género + Carrera, data = dfac, family=quasibinomial(link="logit"))
