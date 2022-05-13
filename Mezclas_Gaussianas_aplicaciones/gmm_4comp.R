library(MASS)
library(mclust)
library(rgl)
library(latex2exp)
set.seed(2022)
n<-500
#Matrices de Varianzas y Covarianzas
Sigma_1<-matrix(c(2,0.5,0.5,4),2,2)
Sigma_2<-matrix(c(10,0.15,0.15,7),2,2)
Sigma_3<-matrix(c(1,0,0,1),2,2) 
Sigma_4<-matrix(c(3,0.7,0.7,3),2,2)     
#Vectores de medias
mu_1<-c(3,-5)
mu_2<-c(-8,1)
mu_3<-c(-16,-15)
mu_4<-c(-8,-10)


comp_1<-as.data.frame(mvrnorm(n,mu=mu_1,Sigma = Sigma_1))
comp_1<-cbind(comp_1,componente=factor(rep(1,n)))
names(comp_1)<-c("x1","x2","Componente")
comp_2<-as.data.frame(mvrnorm(n,mu=mu_2,Sigma = Sigma_2))
comp_2<-cbind(comp_2,componente=factor(rep(2,n)))
names(comp_2)<-c("x1","x2","Componente")
comp_3<-as.data.frame(mvrnorm(n,mu=mu_3,Sigma = Sigma_3))
comp_3<-cbind(comp_3,componente=factor(rep(3,n)))
names(comp_3)<-c("x1","x2","Componente")
comp_4<-as.data.frame(mvrnorm(n,mu=mu_4,Sigma = Sigma_4))
comp_4<-cbind(comp_4,componente=factor(rep(4,n)))
names(comp_4)<-c("x1","x2","Componente")
muestrax<-rbind(comp_1,comp_2,comp_3,comp_4)
datos<-as.data.frame(muestrax)
names(datos)<-c("x1","x2","Componente")

color<-c("#E495A5","#7DB0DD","#D099FF","#93FF9F")

confelli <- function(b, C, df, level = 0.95, xlab = "",
                     ylab = "", add=T, prec=51,color1="black")
{
  d <- sqrt(diag(C))
  dfvec <- c(2, df)
  phase <- acos(C[1, 2]/(d[1] * d[2]))
  angles <- seq( - (pi), pi, len = prec)
  mult <- sqrt(dfvec[1] * qf(level, dfvec[1], dfvec[2]))
  xpts <- b[1] + d[1] * mult * cos(angles)
  ypts <- b[2] + d[2] * mult * cos(angles + phase)
  if(add) lines(xpts, ypts,col=color1)
  else plot(xpts, ypts, type = "l", xlab = xlab, ylab = ylab,col=color1)
  a<-round(runif(1,1,51))
  #text(xpts[a], ypts[a],paste(level),adj=c(0.5,0.5),font=2,cex=0.7)
}
niveles<-c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.9)
par(mfrow=c(1,2),mar=c(4,4,1,1))
plot(datos$x1,datos$x2,pch=16,col=color[datos$Componente],xlab = TeX("$x_1$"),ylab=TeX("$x_2$"))
title(TeX("GMM con \\textit{$K=4$}"))
for (i in 1:9){
  confelli(mu_1,Sigma_1,df=n-2,level = 0.95-niveles[i],color1 ="red" )
  confelli(mu_2,Sigma_2,df=n-2,level = 0.95-niveles[i],color1 = "blue")
  confelli(mu_3,Sigma_3,df=n-2,level = 0.95-niveles[i],color1 = "#500192")
  confelli(mu_4,Sigma_4,df=n-2,level = 0.95-niveles[i],color1 = "#1C6A24")
}

indices<-sample(1:nrow(datos),size = floor(0.8*nrow(datos)))
mezcla<-Mclust(datos[indices,-3],G=4)

table(datos[indices,]$Componente,mezcla$classification)

prediccion<-predict.Mclust(mezcla,newdata = datos[-indices,-3])
table(datos[-indices,]$Componente,prediccion$classification)

densidad<-densityMclust(datos[,-3])

plot(densidad,what = "density",col="#DE2CC3",lwd=1.5,add=TRUE)
legend(-2,12,legend = c(TeX("\\textbf{COMP 1}"),TeX("\\textbf{COMP 2}")
                        ,TeX("\\textbf{COMP 3}"),TeX("\\textbf{COMP 4}"),
                        TeX("\\textbf{GMM}")),lty = 1,
       col = c("blue","red","#2D29A5","#1C6A24","#DE2CC3"),
       lwd=4,pch=3,cex=0.5,bty="n")

plot(densidad,what = "density",type="persp",col="#DE2CC3")
title(TeX("Distribución GMM con \\textit{$K=4$}"))
