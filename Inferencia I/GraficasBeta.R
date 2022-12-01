p = seq(0,1, length=1000)

beta<-function(a,b,x){
  y<-gamma(a+b+10)/((gamma(a+2))*gamma(b+8))*x^(a+1)*(1-x)^(b+7)
return(y)
  }

plot(p,beta(0.5,0.5,p), type='l',col="red",ylim = c(0,10),lwd=2,ylab = "f(p)")
lines(p, beta(1,1,p), type='l',col="blue",ylim=c(0,10),lwd=2,ylab = "f(p)")
lines(p, beta(10,10,p), type='l',col="#0cad1c",lwd=2,ylab = "f(p)")
lines(p, beta(80,80,p), type='l',col="#c034eb",ylab = "Priori Beta(100,100)",ylim=c(0,10),lwd=2,ylab = "f(p)")
legend(x=0.7,y=9, # Coordenadas
       legend = c("Beta(1/2,1/2)","Beta(1,1)","Beta(10,10)","Beta(100,100)"),
       col = c("red", "blue","#0cad1c","#c034eb"),
       lwd = 2,cex=0.5)



