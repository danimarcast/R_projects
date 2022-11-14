#Inciso (b) ejercicio 12
set.seed(2022)
lambda0<-1
n<-20
alpha<-0.05
muestra<-rpois(n,lambda0)
hat_lambda<-mean(muestra)
#hat_lambda = 0.8
hat_se<-sqrt(mean(muestra))
#hat_se = 0.8944272
z_a_2<-qnorm(1-alpha/2)
R_a<-(hat_lambda - lambda0)^2 * n/hat_lambda > z_a_2^2

repeticiones<-10000
contador<-0 
for (i in 1:repeticiones){
  muestras<-rpois(n,lambda0)
  hat_lambdaa<-mean(muestras)
  R_a<-(hat_lambdaa - lambda0)^2 * n/hat_lambdaa > z_a_2^2
  if (R_a ==TRUE ){
    contador=contador+1
    i=i+1
  }
}
Proporcion_rechazos<-contador/repeticiones
#Proporción_rechazos =  0.0519


set.seed(2022)
n<-100
p0<-0.7 
z_a_2<-qnorm(1-0.05/2)
repeticiones<-10000
contador<-0
#Usando prueba de Wald
for (i in 1:repeticiones){
  muestra<-rbinom(1,size=n,prob=p0)
  hat_p<-muestra/n
  hat_se<-sqrt(hat_p*(1-hat_p)/n)
  rechazo<-((hat_p-p0)/hat_se)^2>z_a_2^2
  if (rechazo==TRUE)
  { contador<-contador+1
  i<-i+1}}
  proporcion_rechazos<-contador/repeticiones
  #proporcion_rechazos = 0.0498
  # Usando LRT
  repeticiones<-10000
  contador<-0
  for (i in 1:repeticiones){
    muestra<-rbinom(1,size=n,prob=p0)
    hat_p<-muestra/n
    rechazo<- 2*muestra*log(muestra/(n*p0))+
      2*(n-muestra)*log((n-muestra)/(n*(1-p0)))<qchisq(0.05,df=1)
    if (rechazo==TRUE){
      contador<-contador+1
      i<-i+1
    }
  }
proporcion_rechazos<-contador/repeticiones
#proporcion_rechazos = 0.0846


X<-c(55,33,70,49)
c<-sum((X-sum(X)*0.25)^2/(sum(X)*0.25))
1-pchisq(c,df=3)


