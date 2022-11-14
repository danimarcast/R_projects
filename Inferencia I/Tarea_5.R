library(nptest)
set.seed(2022)
a<-1
b<-3
n<-10

#Creación de una muestra de tamaño n=10
#de una distribucion uniforme de parametros a=1 b=3

tau<-function(X){
  estimacion<-(min(X)+max(X))/2
  return(estimacion)
}

## Valor real 
tao<-(a+b)/2
tao

# funcion para simular multiples muestra de una uniforme
repetir<-function(R){
  repeticiones<-c(1:R)
  for(i in 1:R){
    muestra<-runif(n,min = 1,max=3)
    repeticiones[i]<-tau(muestra)
  }
  return(repeticiones)
}

muestras<-repetir(1000)
MSE_tau_hat<-var(muestras)-(mean(muestras)-tao)^2
MSE_tau_hat

MSE_tau_tilde<-((b-a)^2)/(12*n)
MSE_tau_tilde
