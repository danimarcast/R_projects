library(MASS)
library(ggplot2)
library(mclust)
set.seed(2022)

Sigma_1<-matrix(c(2,0.5,0.5,4),2,2)
#matrix(c(2,0.5,0.5,4),2,2)
Sigma_1
Sigma_2<-matrix(c(10,0.15,0.15,7),2,2)
Sigma_2
mu_1<-c(3,-5)
n<-500
mu_1
mu_2<-c(-8,1)
mu_2

comp_1<-as.data.frame(mvrnorm(n,mu=mu_1,Sigma = Sigma_1))
comp_1<-cbind(comp_1,componente=factor(rep(1,n)))
names(comp_1)<-c("x1","x2","Componente")
comp_2<-as.data.frame(mvrnorm(n,mu=mu_2,Sigma = Sigma_2))
comp_2<-cbind(comp_2,componente=factor(rep(2,n)))
names(comp_2)<-c("x1","x2","Componente")
muestrax<-rbind(comp_1,comp_2)
datos<-as.data.frame(muestrax)
names(datos)<-c("x1","x2","Componente")

color<-c("#E495A5","#7DB0DD")

confelli <- function(b, C, df, level = 0.95, xlab = "",
                     ylab = "", add=T, prec=51)
{
  d <- sqrt(diag(C))
  dfvec <- c(2, df)
  phase <- acos(C[1, 2]/(d[1] * d[2]))
  angles <- seq( - (pi), pi, len = prec)
  mult <- sqrt(dfvec[1] * qf(level, dfvec[1], dfvec[2]))
  xpts <- b[1] + d[1] * mult * cos(angles)
  ypts <- b[2] + d[2] * mult * cos(angles + phase)
  if(add) lines(xpts, ypts,col=color1)
  else plot(xpts, ypts, type = "l", xlab = xlab, ylab = ylab)
  a<-round(runif(1,1,51))
  #text(xpts[a], ypts[a],paste(level),adj=c(0.5,0.5),font=2,cex=0.7)
}
niveles<-c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.9)
plot(datos$x1,datos$x2,pch=16,col=color[datos$Componente],xlab = "x1",ylab="x2")
title("GMM con K=2")
for (i in 1:9){
  confelli(mu_1,Sigma_1,df=n-2,level = 0.95-niveles[i],color1 ="red" )
  confelli(mu_2,Sigma_2,df=n-2,level = 0.95-niveles[i],color1 = "blue")
}

#Implementacion del modelos de mezclas
indices<-sample(1:nrow(datos),size = floor(0.8*nrow(datos)))
mezcla<-Mclust(datos[indices,-3],G=2)

table(datos[indices,]$Componente,mezcla$classification)

prediccion<-predict.Mclust(mezcla,newdata = datos[-indices,-3])
table(datos[-indices,]$Componente,prediccion$classification)
  
Pesos<-mezcla$parameters$pro
Medias<-mezcla$parameters$mean
MatricesCov<-mezcla$parameters$variance$sigma

c1_mx<-Medias[1,1]
c1_my<-Medias[2,1]
c1_s2x<-MatricesCov[1,1,1]
c1_s2y<-MatricesCov[2,2,1]
c1_corxy<-MatricesCov[2,1,1]
c1_peso<-Pesos[1]


c2_mx<-Medias[1,2]
c2_my<-Medias[2,2]
c2_s2x<-MatricesCov[1,1,2]
c2_s2y<-MatricesCov[2,2,2]
c2_corxy<-MatricesCov[2,1,2]
c2_peso<-Pesos[2]

x<-seq(-5,5,length=0.01)
y<-x

MezclaGaussiana<-function(x,y){
  c1_peso*(1/(2*pi*sqrt(c1_s2x*c1_s2y*(1-c1_corxy))))*
    exp(-1/(2*(1-c1_corxy^2))*
          ((x-c1_mx)^2/c1_s2x+
             (y-c1_my)^2/c1_s2y -
             2*c1_corxy*((x-c1_mx)*(y-c1_my))/
             (sqrt(c1_s2x)*sqrt(c1_s2y))
          )
    )
  +
    c2_peso*(1/(2*pi*sqrt(c2_s2x*c2_s2y*(1-c2_corxy))))*
    exp(-1/(2*(1-c2_corxy^2))*
          ((x-c2_mx)^2/c2_s2x+
             (y-c2_my)^2/c2_s2y -
             2*c2_corxy*((x-c2_mx)*(y-c2_my))/
             (sqrt(c2_s2x)*sqrt(c2_s2y))
          )
    )
}
z<-MezclaGaussiana(x,y)

persp(x,y,z,theta=100,phi=20,
      expand=0.5,col=rainbow(1),
      ltheta=120,shad=0.75,
      ticktype="detailed",
      xlab="x",ylab="y",zlab="z")
title(sub="Función de densidad de probabilidad")
title(main="Distribución Normal Bivariada")