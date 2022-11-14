#define range
p = seq(0,1, length=1000)


beta<-function(a,b,x){
  y<-gamma(a+b+10)/((gamma(a+2))*gamma(b+8))*x^(a+1)*(1-x)^(b+7)
return(y)
  }

plot(p,beta(0.5,0.5,p), type='l',col="red")
plot(p, beta(1,1,p), type='l',col="blue")
plot(p, beta(10,10,p), type='l')
plot(p, beta(100,100,p), type='l',col="green")
