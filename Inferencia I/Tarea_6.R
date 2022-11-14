##Ejercicio 3 inciso (c)
set.seed(2022)
datos<-c(3.23, -2.50, 1.88, -0.68, 4.43, 0.17,
         1.03, -0.07, -0.01, 0.76, 1.76, 3.18,
         0.33, -0.31, 0.30, -0.61, 1.52, 5.43,
         1.54, 2.28, 0.42, 2.33, -1.03, 4.00,
         0.39)

hat_mu<-mean(datos)
hat_sigma<-sd(datos)
z_095<-qnorm(0.95,0,1)
hat_tau<-hat_mu+hat_sigma*z_095
hat_tau

hat_se_metodo_del<-hat_sigma*sqrt((1+(z_095^2)/2)/length(datos))
hat_se_metodo_del

repeticion<-1000
x<-c(1:repeticion)
for (i in 1:repeticion){
  muestras<-rnorm(length(datos),hat_mu,hat_sigma)
  x[i]<-mean(muestras)+sd(muestras)*qnorm(0.95,0,1)
}
se_boots_para<-sd(x)
se_boots_para


#Ejercicio 7 inciso (d)
#Suponga que n1 = n2 = 200, X1 = 160 y X2 = 148. Encuentra ˆψ. Encuentra un intervalo
#de confianza de aproximadamente 90 por ciento para ψ usando (i) el método delta y (ii) el
#bootstrap paramétrico
X1<-160
X2<-148
n1<-200
n2<-200
hat_p1<-X1/n1
hat_p2<-X2/n2

hat_psi<-hat_p1-hat_p2
hat_psi

#usando método delta
hat_se_psi<-sqrt(hat_p1*(1-hat_p1)/n1+hat_p2*(1-hat_p2)/n2)
hat_se_psi

C_izq<-hat_psi-qnorm(0.95,0,1)*hat_se_psi
C_izq
C_der<-hat_psi+qnorm(0.95,0,1)*hat_se_psi
C_der

cat("El intervalo de confianza para psi es: ", 
    "( ",C_izq,", ",C_der," )")
##usando bootstrap parametrico
repeticiones<-1000
x<-c(1:repeticiones)
for (i in 1:repeticiones){
  muestras1<-rbinom(1,n1,prob = hat_p1)
  muestras2<-rbinom(1,n2,prob = hat_p2)
  x[i]<-muestras1/n1-muestras2/n1
}
se<-sd(x)

C_izq<-hat_psi-qnorm(0.95,0,1)*se
C_izq
C_der<-hat_psi+qnorm(0.95,0,1)*se
C_der
cat("El intervalo de confianza para psi es: ", 
    "( ",C_izq,", ",C_der," )")


#Ejercicio 10 inciso (a)
set.seed(2022)
n<-50
theta<-1
datos2<-runif(n,0,max=theta)

y<-function(x){
  y<- (50*x^49)/(max(datos2))^n
  return (y)
}

xx<-seq(0,theta,0.001)
yy<-y(xx)
plot(xx,yy,add=TRUE,type = "l",col="green",xlim = c(0.75,1),ylab = "%")

repeticion<-10000
#Parametrico
x<-c(1:repeticion)
for (i in 1:repeticion){
  muestras<-runif(n,0,max = theta)
  x[i]<-max(muestras)
}
hist(x,freq  = FALSE,col=NULL,border ="red",add=TRUE)

#Noparametrico
repeticion<-10000
x<-c(1:repeticion)
muestras<-c(1:n)
for (i in 1:repeticion){
  x[i]<-max(datos2[sample(1:n,n,replace = TRUE)])
}

hist(x,freq  = FALSE,col=NULL,border ="blue",add=TRUE)
legend(0.75,47,legend = c(TeX("\\textbf{No Paramétrico}"),TeX("\\textbf{Paramétrico}"),TeX("\\textbf{Real}")),lty = 1,col = c("blue","red","green"),lwd=4,pch=3,cex=0.9,bty="n")
