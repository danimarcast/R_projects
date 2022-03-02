par(bg="gray",mar=c(2,3,2,2))

chi_cua<-function(x){dchisq(x,df=10)}
curve(dchisq(x,10),0,30)
plot(chi_cua,0,30,xaxt="n",yaxt="n",xlab="",ylab=NA)
plot(c(0,1),c(1,1))
text(0.4,10,"uwu") 
#text(x=0.8,y=(cotasup-1),labels = as.character(10))

axis(1,at=c(qchisq (.025, 10),qchisq (.975, 10)),labels = c(TeX("$1-\\alpha/2$"),TeX("$\\alpha/2$")))

# encontrar valores superiores e inferiores para el 95% medio de la distribución
inferior_95 <- qchisq (.025, 10)
superior_95 <- qchisq (.975, 10)

#crear vector de valores x
x_vector <- seq (inferior_95, superior_95,length.out = 1000)

#crear vector de valores de densidad chi-cuadrado
p_vector <- dchisq (x_vector, df = 10)
polygon(c (x_vector, rev(x_vector)), c (p_vector, rep (0, length (p_vector))),
        col = "#87CEFA", border = NULL)

