### Intervalos de confianza
m <- 100# Tamaño de la muestra
n <- 100 # Cantidad de intervalos de confianza
df<- n-1
confianza <- 0.95
alpha <- 1-confianza
x <- matrix(0,m,n)
y1 <- 1
y2 <- 1
varianza<-0

for (i in 1:n){
  x[,i]<-rchisq(m,df)
  y2[i] <- (n-1)*var(x[,i])/qchisq(alpha/2,df)
  varianza[i] <- var(x[,i])
  var_pobla<-mean(varianza)
  y1[i]<- (n-1)*var(x[,i])/qchisq(1-alpha/2,df)
}
par(bg="gray",mar=c(2,3,4,2))
plot(varianza,pch=16,ylim=c(min(y1)-0.1,max(y2)+0.1),col="blue",
     cex=1,xlab = "intervalo")
segments(1:n,y1,1:n,y2,lwd=2)
abline(h=mean(varianza),col="magenta",lwd=1.5)

mal<-0
for (i in 1:n){
  if (y1[i]>var_pobla| var_pobla>y2[i]){
    mal<-mal+1
    print("mal")
    print(c(y1[i],y2[i]))
    segments(i,y1[i],i,y2[i],col = "red",lwd=2)
  }
}
title(paste("Quedaron","fuera", mal,"intervalos" ,"de",n))


