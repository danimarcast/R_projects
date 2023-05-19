library(dplyr)
library(ggplot2)
library(nortest)
Calif<-data.frame(PromediosMateViejo)
names(Calif)<-c("Semestre","Promedio","Historial")
Calif %>%  filter(Historial=="Sin_materias_reprobadas")-> SMR
Calif %>%  filter(Historial=="Reprobo_alguna_materia")-> RAM
gg_disp<-ggplot(Calif,
       aes(y = Promedio, x = 1:nrow(Calif) ))+
  geom_point(aes(col=Historial)) +
  labs(title = "Promedios ¿Demat?",  y = "Promedio", x="Estudiante")

media_RAM<-mean(RAM$Promedio)
var_RAM<-var(RAM$Promedio)

# a).
#Como podemos ver de la dispersion de los datos
#para los estudiantes que reprobaron alguna materia,
# vemos que aunque la media es bastante alta, hay bastante
# dispersion en la muestra en general pues 
# hay una parte de la subpoblacion que tuvieron notas por debajo 
# por debajo del promedio lo que indica una varianza
# notablemente considerable con la de los estudiantes que no
# que no reprobaron alguna materia.


media_SMR<-mean(SMR$Promedio)
var_SMR<-var(SMR$Promedio)

# De los datos dispersados de los estudiantes que no reprobaron
#alguna materia, podemos ver que tiene un promedio  general muy por
# encima del 9, con lo cual es rasonable que tengamos una varianza
#muy pequeña.

# b).
# Como queremos verificar si hay evidencia respecto a que 
# a que los dos grupos de estudiantes tuvo mejor promedio
# que la otra, consideremos una prueba de hipotesis que
# compare el comportamiento de las medias en ambas poblaciones
# para ello consideremos la prueba de hipotesis
#         H0: # µ1-µ2 = 0 vs H1:µ1 != µ2
#  Tomando como estadistico de prueba 
#    T0=\overline(X_m)-\overline(X_n)/sqrt(1/m+1/n)S^2_n,m
#
#
#Considerando un nivel del 1-a = 95%
VarMuesConj<-((length(RAM)-1)*var_RAM+(length(SMR)-1)*var_SMR)/(length(RAM)+length(SMR)-2)
t0<-(media_RAM-media_SMR)/sqrt((1/length(RAM)+1/length(SMR))*VarMuesConj)
valor_t<-pt(0.025,df=length(RAM)+length(SMR)-2)
t0>valor_t
# Se realizando la respectiva prueba obtenemos que nuestro
# estadistico de prueba cae fuera de la region de rechazo
# por lo cual no podemos rechazar la hipotesis nula
# Podemos concluir que no hay suficiente evidencia estadistica
# para poder afirmar que ambos grupos tienen promedios que
# difieran


# Veamos ahora si hay variabilidad en los promedios
# de ambos grupos, para ello comparemos las varianzas
# de ambos grupos 
# Considerando como estadistico de prueba
#       f0= (n-1)S_n^2/(m-1)S_m^2~ F_n-1,m-1
# veamos que \sigma^2_1/\sigma^2_2<=1
f0<-((length(SMR)-1)*var_SMR)/((length(RAM)-1)*var_RAM)
valor_f<-pf(0.05,df1=length(SMR)-1,df2=length(RAM)-1)
f0>valor_f
# Comparando las varianzas podemos observar que nuestro
# estadistico de prueba cae en ela región de rechaz, es decir
# que a partir de los datos que tenemos podemos afirmar
# que hay variabilidad entre los grupos de estudiantes
# sin materias reprobadas y los con alguna materia reprobada.

# c).
# Pruebas de Normalidad para el grupo que reprobo alguna 
# materia
shapiro.test(RAM$Promedio)
#	      Shapiro-Wilk normality test
# data:  RAM$Promedio
# W = 0.95841, p-value = 0.4074

ad.test(RAM$Promedio)
#       Anderson-Darling normality test
# data: RAM$Promedio
# A = 0.30479, p-value = 0.5425

# Para un alpha 0.01 y 0.05 podemos observar que el p-valor en ambas pruebas
# (Shapiro Wilk y Anderson Darling)
# es mayor que ambas cantidades, por tanto no rechazamos que la poblacion
# de estudiantes que reprobaron alguna materia
#  sigue una distribucion normal, ya que no hay evidencia suficiente
# que permita concluir que los datos no son normales.
#



# Pruebas de Normalidad para el grupo que no reprobo alguna 
# materia

shapiro.test(SMR$Promedio)
#	      Shapiro-Wilk normality test
# data:  RAM$Promedio
# W = 0.95841, p-value = 0.4074

ad.test(SMR$Promedio)
#       Anderson-Darling normality test
# data: RAM$Promedio
# A = 0.30479, p-value = 0.5425

# Para un alpha 0.01 y 0.05 podemos observar que el p-valor en ambas pruebas
# (Shapiro Wilk y Anderson Darling)
# es menor queque ambas cantidades, por lo tanto concluimos que los promedios 
# en los estudiantes que no reprobaron alguna materia
# no sigue una distribucion normal.

ggplot(RAM, aes(sample = Promedio))+stat_qq(col="red")+stat_qq_line()+
  labs(x="Teoricos", y = " Muestrales")

# Graficamente del QQplot podemos ver que los cuantiles
# teoricos vs los muestrales, tienen un buen comportamiento ya que se
# se aproximan de buena manera a la recta identidad, lo que nos permite
# concluit tambien que fue bastante rasonable haber obtenido el resultado
# de no rechazar la hipotesis de normalidad.
ggplot(SMR, aes(sample = Promedio))+stat_qq(col="blue")+stat_qq_line()+
  labs(x="Teoricos", y = " Muestrales")
# Del QQplot observamos un poco mas de poco relacion entre los cuantles
# teoricos y muestrales, ya que a diferencia de lo que obtimos 
# para el caso de estudiantes que reprobaron alguna materia,
# en este caso obtenemos una mayor cantidad de observaciones en la que 
# difieren sus cuantiles.

# d). Usando prueba U de Mann-Withney
##
wilcox.test(RAM$Promedio,SMR$Promedio)
# Usando la prueba U de Mann-Withney para contrastar si hay una diferencia
# en la distribucion de los promedios de ambos grupos, tomando la
# H0: la distribución es igual vs H1: difieren
# obtenemos:

#       Wilcoxon rank sum test with continuity
#          correction
# data:  RAM$Promedio and SMR$Promedio
# W = 67, p-value = 0.0001471
#  alternative hypothesis: true location shift is not equal to 0

# Podemos observar un p-valor de 0.0001471, el cual considerando un nivel de significacia
# de a=0.01, este p-valor esta muy por debajo, por lo cual,
# rechazamos la hipotesis nula de que los promedios en ambos grupos siguen una misma
# distribución. Es bastante razonable haber rechazado la hipotesis 
# nula puesto que como vimos de los incisos anteriores
# en el grupo que reprobaron alguna materia, no se rechazo el supuesto
# que los datos siguieran una distribucion normal, mientras que para el otro 
# grupo si habia evidencia estadistica para
# rechazar la normalidad, por lo cual era de esperarse que ambas distribuciones
# no fueran iguales.



