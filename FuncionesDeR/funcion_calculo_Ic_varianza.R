#Funcion para calcular intervalo de confianza para la varianza
#considerando una unica muestra normal de tamaño n
# con grados de libertad igual a df=n-1


var1.test<-function(x,df,conf.level=0.95){
  intervalo<-1
  alfa1=1-conf.level
  alfa2=alfa1/2
  varianza<-var(x)
  liminf<-(df)*varianza/qchisq(alfa2,df,lower.tail = FALSE)
  limsup<-(df)*varianza/qchisq(1-alfa2,df,lower.tail = FALSE)
  intervalo<-c(liminf,limsup)
  paste("intervalo de confianza ",intervalo[1],intervalo[2],"con nivel de confianza del ",conf.level*100,"%")
}



