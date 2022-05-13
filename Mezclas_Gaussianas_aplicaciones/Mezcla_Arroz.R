library(readxl)
library(mclust)
library(pROC)
Rice_MSC_Dataset <- read_excel("Rice_MSC_Dataset/Rice_MSC_Dataset.xlsx")

ArrozDatos<-Rice_MSC_Dataset[1:75000,- (5:106)]

set.seed(2022)
indices<-sample(1:nrow(ArrozDatos),size = floor(0.40*nrow(ArrozDatos)),replace = FALSE)
entreno<-ArrozDatos[indices,-c(4:16,17)]
table(ArrozDatos[indices,]$CLASS)
X<-ArrozDatos[indices,1:4]
y <-ArrozDatos[indices,5]


pairs(X,main="Diagrama de Dispersion",labels=c("Area","Perimetro","EjeMayor","EjeMenor"),lower.panel = NULL,col = as.factor(ArrozDatos[indices,]$CLASS))
par(xpd=TRUE)
legend(0,0.55,as.vector(unique(ArrozDatos[indices,]$CLASS)),bty='l')

GMM_Arroz<-Mclust(X,G=5)
summary(GMM_Arroz,parameters = TRUE)
table(ArrozDatos[indices,]$CLASS,GMM_Arroz$classification)

prediccion<-predict(GMM_Arroz,newdata = ArrozDatos[-indices,1:4],type='response')
table(ArrozDatos[-indices,]$CLASS,prediccion$classification)
table(ArrozDatos[-indices,]$CLASS)

x<-multiclass.roc(ArrozDatos[-indices,]$CLASS,prediccion$classification)
x
colores<-c("#FE12DA","#FF6202","#FE2312","#FFB202","#FFFF02","#C2FF02",
           "#28CF60","#28CDCF","#2860CF")
rs <- x[['rocs']]
plot.roc(rs[[1]],xlab = "Especificidad",ylab = "Sensitividad",legacy.axes = F,axes = T,xlim = c(1,0))
sapply(1:length(rs),function(i){ 
  if (i<=5){
    plot.roc(rs[[i]],col=colores[i],add=T,print.auc=T,print.auc.x=0.23,print.auc.y=0.5-0.09*i,axes=T,legacy.axes=T)
  }
    if(i>5){
      plot.roc(rs[[i]],col=colores[i],add=T,print.auc=T,print.auc.x=0,print.auc.y=0.091*i-0.5,axes=T,legacy.axes=T)}
})
sensitividades<-c()
especificidades<-c()
for (i in 1:10){
  sensitividades[i]<-mean(x$rocs[[i]]$sensitivities)
  especificidades[i]<-mean(x$rocs[[i]]$specificities)
}
sensitividad<-mean(sensitividades)
especificidad<-mean(especificidades)


