### Intervalos de confianza
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
  y1[i] <- media[i]-qnorm(1-alpha/2)/sqrt(m)
  y2[i] <- media[i]+qnorm(1-alpha/2)/sqrt(m)
}
###### Gafricar intervalo de confianza######
par(bg="gray",mar=c(2,3,4,2))
plot(media,pch=16,ylim=c(min(y1)-0.1,max(y1)+0.3),col="blue",
     cex=1,xlab = "intervalo")
segments(1:n,y1,1:n,y2,lwd=2)
#abline(h=c(-qnorm(alpha/2,lower.tail = FALSE)/sqrt(m),qnorm(alpha/2,lower.tail = FALSE)/sqrt(m)),col="magenta",lwd=1)
abline(h=mean(media),col="magenta",lwd=1.5)

fuera<-1

mal<-0
for (i in 1:n){
  p_poblacional<-mean(media)
  if (y1[i]>p_poblacional| p_poblacional>y2[i]){
    mal<-mal+1
    print("mal")
    print(c(y1[i],y2[i]))
    segments(i,y1[i],i,y2[i],col = "red",lwd=2)
  }
}
title(paste("Quedaron","fuera", mal,"intervalos" ,"de",n))

