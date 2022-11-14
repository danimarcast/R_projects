#Carga de  libreria mclust para poder correr un modelo de mezclas gaussinas
#revisar Cran para leer la documentación detallada de todas lafunciones 
#de la funcion mclust
#descomentar y correr la linea siguiente en caso de que no se tenga instalado
## la paqueteria mclust
## install.packages("mclust")


##  Cargar la libreria pROC, para poder hacer evaluación multiclases
#usanddo curva y roc, ademas de calcular el área-
## #descomentar y correr la linea siguiente en caso de que no se tenga instalado
## la paqueteria pROC
## install.packages("pROC")
library(readxl)
library(mclust)
library(pROC)
library(xtable)
# Carga del Dataset "Rice_MSC_Dataset",cambiar la ruta de acuerdo al lugar 
#en donde se encuentre alojado el archivo
Rice_MSC_Dataset <- read_excel("Documents/GitHub/R_projects/Mezclas_Gaussianas_aplicaciones/Rice_MSC_Dataset/Rice_MSC_Dataset.xlsx")
View(Rice_MSC_Dataset)
#Filrar los datos, de manera que solo tomemos las primeras 4 columnas y la ultima
#columna

ArrozDatos<-Rice_MSC_Dataset[1:75000,- (5:106)]

#Fijamos una semilla para que obtengan los mismos resultados 
#obtenidos en el archivo
set.seed(2022)

#Creamos una muestra de indices, que nos ayudaran a dividir los datos 
#en datos  de prueba y datos de entrenamiento
indices<-sample(1:nrow(ArrozDatos),size = floor(0.40*nrow(ArrozDatos)),replace = FALSE)
#Corriendo este table puede observar como se estan tomando los datos 
#de entrenamiento
table(ArrozDatos[indices,]$CLASS)
table(ArrozDatos[-indices,]$CLASS)
print(xtable( table(ArrozDatos[indices,]$CLASS)),include.rownames=TRUE)
print(xtable( table(ArrozDatos[-indices,]$CLASS)),include.rownames=TRUE)
print(xtable(head(ArrozDatos),include.rownames=FALSE))
# Creamos el data frame X el cual solo contiene las caracteristicas 
#para los datos de entrenamiento
X<-ArrozDatos[indices,1:4]

# Esta parte corresponen con el grafico de dispersion para los datos 
#de entrenamiento, donde se puede observar una dispersion por pares
#lo que permite identificar en cuantos grupos se estan agrupando los datos

colores<-hcl.colors(5,"Plasma")
vectores<-as.vector(unique(ArrozDatos[indices,]$CLASS))

plot(X,lower.panel=NULL,col=colores[as.factor(ArrozDatos[indices,]$CLASS)],
     labels=c("AREA","PERIMETER","MAJOR_AXIS","MINOR_AXIS"),pch=19)
par(xpd=TRUE)



dev.new(width=15,heigth=7)

pairs(X, lower.panel=NULL,col=colores[as.factor(ArrozDatos[indices,]$CLASS)],
      labels=c("AREA","PERIMETER","MAJOR_AXIS","MINOR_AXIS"),pch=19,oma=c(4, 4, 8, 15))
par(xpd=TRUE)
legend(x=0.1,y=0.4,legend = c("Arborio","Basmati","Ipsala","Jasmine","Karacadag"),fill=colores,cex = 0.9)

quartz.save("rplotfinal",type="png",device=dev.cur(),dpi=100)


#Corremos la funcion Mclust, indicando "G", el numero de componentes

GMM_Arroz<-Mclust(X,G=5)
summary(GMM_Arroz,parameters=TRUE)
#Esta linea siguiente la puede descomentar en caso de que desee 
#observar los parametros de cada componente,
#indicando parameters=TRUE puede observar todas los parametros de cada componente,en caso de no
#hacerlos solo mostrara una información minima
#summary(GMM_Arroz,parameters = TRUE)

#Aqui puede ver la matriz de confusión para los datos de entrenamiento
table(ArrozDatos[indices,]$CLASS,GMM_Arroz$classification)

#con la función predict puede hacer las predicciones para los datos de prueba
#teniendo en cuenta los indices que creamos 
# puede probar inventando un dato con caracteristicas aleatoria
#y ver a que componente pertenece

prediccion<-predict(GMM_Arroz,newdata = ArrozDatos[-indices,1:4],type='response')
#Tabla para ver como se habian tomado los datos de prueba
table(ArrozDatos[-indices,]$CLASS)
#Matriz de confusión para los datos de prueba, cabe aclarar que no tiene
#la forma directa de una matriz de confusión por el orden en como la funcion esta creando las componentes
table(ArrozDatos[-indices,]$CLASS,prediccion$classification)

print(xtable(table(ArrozDatos[-indices,]$CLASS,prediccion$classification)))
#Usamos la funcion multiclass.roc para realizar evaluacion multiclase
#dentro de variable creada podra encontrar las curvas rocs realizadas por pares

x<-multiclass.roc(ArrozDatos[-indices,]$CLASS,prediccion$classification)
x
colores<-c("#FE12DA","#FF6202","#FE2312","#FFB202","#FFFF02","#C2FF02",
           "#28CF60","#28CDCF","#2860CF")
rs <- x[['rocs']]

#graficamos las curvas roc con sus respectivas AUC
plot.roc(rs[[1]],xlab = "1-Especificidad",ylab = "Sensibilidad",legacy.axes = T,axes = T,print.thres = F,
         grid = T,col=1)

sapply(1:length(rs),function(i){ 
  if (i<=5){
    plot.roc(rs[[i]],col=colores[i],add=T,print.auc=T,print.auc.x=0.23,print.auc.y=0.5-0.09*i,axes=T,legacy.axes=F,print.thres = F)
  }
    if(i>5){
      plot.roc(rs[[i]],col=colores[i],add=T,print.auc=T,print.auc.x=0,print.auc.y=0.091*i-0.5,axes=T,legacy.axes=T,print.thres = F)}
})
