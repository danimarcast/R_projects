#Función segments : Para crear segmentos de recta
a<-0
b<-10
f<-function(x){x^6*exp(-2*x)}
curve(f,a,b,col=2,lwd=4)
abline(h=0,v=0)
grid(nx=10,col="blue",lty = "dotted")

delta1<-1
delta2<-1/2
w<-seq(a,b,delta1)
z<-w+delta1
segments(z,0,z,f(z),col=1,lwd = 1)
for (i in 1:10){
  segments(z-i*delta1/10,0,z-i*delta1/10,f(z),col=1,lwd=1)
}
segments(w,f(z),z,f(z),col=1,lwd = 1)

delta1<-1/2
w<-seq(a,b,delta1)
z<-w+delta1
for (i in 1:10){
  segments(z-i*delta1/10,0,z-i*delta1/10,f(z),col=2,lwd=1)
}
delta1<-1/3
w<-seq(a,b,delta1)
z<-w+delta1
for (i in 1:10){
  segments(z-i*delta1/10,0,z-i*delta1/10,f(z),col=3,lwd=1)
}

delta1<-1/4
w<-seq(a,b,delta1)
z<-w+delta1
for (i in 1:10){
  segments(z-i*delta1/10,0,z-i*delta1/10,f(z),col=4,lwd=1)
}