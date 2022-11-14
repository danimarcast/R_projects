# Ejercicio 1
#Consider the data in Example 8.6. Find the plug-in 
#estimate of the correlation coefficient. 
#Estimate the standard error using the bootstrap.
#Find a 95 percent confidence interval 
#using the Normal, pivotal, and percentile methods.
set.seed(2022)

LSAT<-c(576, 635, 558, 578, 666, 580, 555, 661, 651,
        605, 653, 575, 545, 572, 594)
GPA<-c(3.39, 3.30, 2.81, 3.03, 3.44, 3.07, 3.00, 3.43,
       3.36, 3.13, 3.12, 2.74, 2.76, 2.88, 3.96)
GPA<-data.frame(GPA)
LSAT<-data.frame(LSAT)
Data1<-cbind(LSAT,GPA)


#Calculando la correlación
Media_GPA<-mean(Data1$GPA)
Media_LSAT<-mean(Data1$LSAT)
Diferencias_GPA<-Data1$GPA-Media_GPA
Diferencias_LSAT<-Data1$LSAT-Media_LSAT
Correlacion<-(sum(Diferencias_GPA*Diferencias_LSAT))/(sqrt(sum(Diferencias_GPA^2)
*sum(Diferencias_LSAT^2)))

bootstrapBiva<-function(X,Y,size,fun){
  tboot<-c(1:size)
  for(i in 1:size){
    indices<-sample(1:length(X),size = length(X),replace = TRUE)
    x<-rep(0,times=length(X))
    y<-rep(0,times=length(X))
    for (j in 1:length(x)){
      x[j]<-X[indices[j]]
      y[j]<-Y[indices[j]]
    }
    tboot[i]<-fun(x,y)
  }
  return(tboot)
}
bootstrapUniva<-function(X,size,fun){
  tboot<-c(1:size)
  for(i in 1:size){
    indices<-sample(1:length(X),size = length(X),replace = TRUE)
    x<-rep(0,times=length(X))
    for (j in 1:length(x)){
      x[j]<-X[indices[j]]
    }
    tboot[i]<-fun(x)
  }
  return(tboot)
}

Boot1<-bootstrapBiva(Data1$LSAT,Data1$GPA,1000,cor)
se1<-sd(Boot1)

intNormal<-function(parametro,confianza,se){
  cola_izq<-parametro+qnorm((1-confianza)/2)*se
  cola_der<-parametro-qnorm((1-confianza)/2)*se
  return(c(cola_izq,cola_der))
}
intNormal(Correlacion,0.95,se1 )

intPivotal<-function(boots,confianza,parametro){
  cola_izq<-2*parametro-quantile(boots,1-(1-confianza)/2)
  cola_der<-2*parametro-quantile(boots,(1-confianza)/2)
  return(c(cola_izq,cola_der))
}
intPivotal(Boot1,0.95,Correlacion)

intPercentil<-function(confianza,boots){
  cola_izq<-quantile(boots,(1-confianza)/2)
  cola_der<-quantile(boots,1-(1-confianza)/2)
  return(c(cola_izq,cola_der))
}
intPercentil(0.95,Boot1)

#Ejercicio 2 
#Realice una simulación para comparar los distintos métodos de 
#intervalo de confianza bootstrap. Sea n = 50 y sea 
#T(F) = [∫ (x − μ)^3 dF(x)]/σ^3 la skewness. Seleccione Y1, . . . , Yn ∼ N(0, 1) 
#y el conjunto Xi = e^Yi , i = 1, . . . , n. Construir los tres tipos 
#de intervalos bootstrap del 95 % para T(F) a partir de los
#datos X_1, . . . , X_n. Repetir esto varias veces y 
#estimar la cobertura correcta de los tres intervalos.

#Creacion de la funcion de oblicuidad
skewness<-function(X){
  media<-mean(X)
  sd<-sd(X)
  skew = sum((X-media)^3)/(length(X)*sd^3)
  return(skew)
}
#Creación de la función que genera la muestra aleatoria de una normal
#N(0,1)
    set.seed(2500)
    aleatorios<-rnorm(50,mean=0,sd=1)
    Data2<-exp(aleatorios)
#Creacion de la muestra

intentos<-500
intNorm<-c(1:2*intentos)
intPivot<-c(1:2*intentos)
intPercen<-c(1:2*intentos)
Data2<-muestra(50)
for (i in 1:intentos){
  #Parámetro de la Skewness
  Skew<-skewness(Data2)
  Skew
  #Bootstrap para la Skewness
  boot2<-bootstrapUniva(Data2,10000,skewness)
  se2<-sd(boot2)
  
  a1 <- intNormal(Skew,0.95,se2)
  intNorm[2*i-1]<-a1[1]
  intNorm[2*i]<-a1[2]
  a2<-intPivotal(boot2,0.95,Skew)
  intPivot[2*i-1]<-a2[1]
  intPivot[2*i]<-a2[2]
  a3<-intPercentil(0.95,boot2)
  intPercen[2*i-1]<-a3[1]
  intPercen[2*i]<-a3[2]
  
}

IntNorm_izq<-mean(intNorm[seq(1,2*intentos,2)])
IntNorm_der<-mean(intNorm[seq(2,2*intentos,2)])
IntPivot_izq<-mean(intPivot[seq(1,2*intentos,2)])
IntPivot_der<-mean(intPivot[seq(2,2*intentos,2)])
IntPercen_izq<-mean(intPercen[seq(1,2*intentos,2)])
IntPercen_der<-mean(intPercen[seq(2,2*intentos,2)])

Promedios <- data.frame(IntNorm_izq, IntNorm_der, 
                        IntPivot_izq, IntPivot_der, IntPercen_izq, 
                        IntPercen_der)
skewness(Data2)
for(i in Promedios){
  print(mean(i))
}

#Ejercicio 3
#Experimento de computadora) Sean X1, . . . , Xn ∼ N(μ, 1), y sea θ = eμ y ˆθ = e  ̄X. Crea un conjunto
#de datos (usando μ = 5) que consista de n = 100 observaciones.
#(a) Usa el bootstrap para obtener el Se y un intervalo de confianza del 95 % para θ.
#(b) Gráfica un histograma de las replicaciones del bootstrap. Este es un estimado de la distribución
#de ˆθ. Compara esto con la verdadera distribución muestral de ˆθ

set.seed(2022)
muestra2<-rnorm(100,mean=5,sd=1)
estim_theta<-function(X){
  theta<-exp(mean(X))
  return(theta)
}

estim_theta(muestra2)
boot_estim_se<-bootstrapUniva(muestra2,1000,estim_theta)
se<-sd(boot_estim_se)
se
IntNormSe<-intNormal(estim_theta(muestra2),0.95,se)
IntNormSe

hist(boot_estim_se,main="Hist. Bootstrap Se",probability = TRUE,
     ylab="Probabilidad",xlab = "Estimacion Boots. Se"
     ,col="blue",add=TRUE,ylim = c(0,0.3))

x <- seq(100,220)
y <- dnorm(x, exp(5), sqrt((exp(1/100)-1)*(exp(10-1/100))) )
lines( x=x, y, col="red" )
