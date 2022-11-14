library(readr)
carmilage <- read_csv("Downloads/carmilage.csv")

#Ajuste de modelo de regresión lineal simple
model1<-lm(carmilage$MPG~carmilage$HP,data = carmilage)
carm_mpg1<-append(carmilage$MPG,model1$fitted.values)
carm_hp1<-append(carmilage$HP,carmilage$HP)

n<-length(carm_hp1)
colores1<-c()
for (i in 1:n){
  if(i<=floor(n/2)){
    colores1<-append(colores1,"red")
    i<-i+1
  }
  else{
    colores1<-append(colores1,"blue")
  }
}

summary(model1,parameters=TRUE)
plot(carm_hp1,carm_mpg1,pch=16,cex=0.9,xlab = "HP",ylab = "MPG",col=colores1)
abline(model1,col="black",lwd=2)
legend(x=290,y=60, # Coordenadas
       legend = c("MPG","Fitted"),
       col = c("red", "blue"),
       lwd = 2,cex=0.5)

#Ajuste del modelo usando el Log(MPG) en funcion de HP
model2<-lm(log(carmilage$MPG)~carmilage$HP,data = carmilage)
carm_mpg2<-append(log(carmilage$MPG),model2$fitted.values)
carm_hp2<-append(carmilage$HP,carmilage$HP)

n<-length(carm_hp2)
colores2<-c()
for (i in 1:n){
  if(i<=floor(n/2)){
    colores2<-append(colores,"red")
    i<-i+1
  }
  else{
    colores2<-append(colores,"blue")
  }
}
#Grafico de los datos reales y ajustados
plot(carm_hp2,carm_mpg2,pch=16,cex=0.9,col=colores2,ylab="Log(MPG)",xlab="HP")
#Recta de regresión
abline(model2,col="black",lwd=2)
legend(x=280,y=4, # Coordenadas
       legend = c("Log(MPG)","Fitted"),
       col = c("red", "blue"),
       lwd = 2,cex=0.5)
summary(model2,paremeters=TRUE)

