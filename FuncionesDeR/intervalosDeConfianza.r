#función segments y aplicacion a intervalo de confianza
# a<-0
# b<-10
# f<-function(x){x^6*exp(-2*x)}
# curve(f,a,b,col=2,lwd=4)
# abline(h=0,v=0)
# grid(nx=10,col="blue",lty = "dotted")
# 
# delta1<-1
# delta2<-1/2
# w<-seq(a,b,delta1)
# z<-w+delta1
# segments(z,0,z,f(z),col=1,lwd = 1)
# for (i in 1:10){
#   segments(z-i*delta1/10,0,z-i*delta1/10,f(z),col=1,lwd=1)
# }
# segments(w,f(z),z,f(z),col=1,lwd = 1)
# 
# delta1<-1/2
# w<-seq(a,b,delta1)
# z<-w+delta1
# for (i in 1:10){
#   segments(z-i*delta1/10,0,z-i*delta1/10,f(z),col=2,lwd=1)
# }
# delta1<-1/3
# w<-seq(a,b,delta1)
# z<-w+delta1
# for (i in 1:10){
#   segments(z-i*delta1/10,0,z-i*delta1/10,f(z),col=3,lwd=1)
# }
# 
# delta1<-1/4
# w<-seq(a,b,delta1)
# z<-w+delta1
# for (i in 1:10){
#   segments(z-i*delta1/10,0,z-i*delta1/10,f(z),col=4,lwd=1)
# }
#INTERVALOS DE CONFIANZA USANDO SEGMENTS

m <- 100# Tamaño de la muestra
n <- 100 # Cantidad de intervalos de confianza
confianza <- 0.95
alpha <- 1-confianza
x <- matrix(0,m,n)
y1 <- 1
y2 <- 1
media <- 1
######### Calculo del intervalo######
for (i in 1:n){
  x[,i] <- rnorm(m)
  media[i] <- mean(x[,i])
  y1[i] <- media[i]-qnorm(alpha/2,lower.tail = FALSE)/sqrt(m)
  y2[i] <- media[i]+qnorm(alpha/2,lower.tail = FALSE)/sqrt(m)
}
###### Gafricar intervalo de confianza######
par(bg="gray",mar=c(2,3,4,2))
plot(media,pch=16,ylim=c(min(y1)-0.1,max(y1)+0.1),col="blue",
     cex=0.5,xlab = "intervalo")
segments(1:n,y1,1:n,y2,lwd=1)
abline(h=c(-qnorm(alpha/2,lower.tail = FALSE)/sqrt(m),qnorm(alpha/2,lower.tail = FALSE)/sqrt(m)),col="magenta",lwd=1)




