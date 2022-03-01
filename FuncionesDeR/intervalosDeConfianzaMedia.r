
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




