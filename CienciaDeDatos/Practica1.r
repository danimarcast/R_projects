#Instlacion libreria Tidyverse
library("tidyverse")
Dataframe<-data.frame("Id"=c(1,2,3),#Dataframe=Tabla
                  "Nombre"=c("Hugo","Paco","Luis"),
                  "Apellido"=c("Lopez","Camargo","Martinez"))
length(Dataframe)
Dataframe[2,-3]
Vector<-c("Hugo","Paco","Luis")#Vector


Matriz<-matrix(Dataframe,nrow = 3,ncol=3)
Matriz[3]
Dataframe$Id
Dataframe$Nombre
Dataframe$Apellido
class(Dataframe$Id)
Dataframe$Id<-as.factor(Dataframe$Id)
Dataframe$Id<-as.numeric(Dataframe$Id)

for (i in 1:1536){
  if (c[i])
}
  
  
