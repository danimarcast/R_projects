library(readr)
Dataset<- read_csv("Downloads/faithful.csv")

Media<-mean(Dataset$waiting)
z_a_2<-qnorm(0.95)

hist(Dataset$waiting,main = "Hist. Tiempo de espera erupciones")


Diferencias<-Dataset$waiting-Media
Cuadrados<-Diferencias^2
SumDif<-mean(Cuadrados)
Sd<-SumDif/length(Dataset$waiting)
error_estandar<-sqrt(Sd)
cola_izq<-Media-z_a_2*error_estandar
cola_der<-Media+z_a_2*error_estandar
Mediana<-median(Dataset$waiting)

cat("El tiempo medio para las erupciones es ",Media )
cat("El error estandar para la estimación de la media es ", Sd)
cat("El intervalo de confianza para la media es ","[", cola_izq, ",", cola_der,"]")
cat("La mediana para el tiempo de espera de las erupciones es", Mediana)

