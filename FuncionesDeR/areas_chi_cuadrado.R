#graficador para función chi cudrado

chicua_area<-function(df,cotainf,cotasup,color ="black",grosor=0.1){
  x<-seq(qchisq(0.00001,df),qchisq(0.9999,df),length=100000)
  if (missing(cotainf)){
    cotainf<-min(x)
  }
  if(missing(cotasup)){
    cotasup<-max(x)
  }
  par(bg="gray",mar=c(2,3,2,2))
  plot(x,dchisq(x,df)#,xaxt="n",yaxt="n",xlab="",ylab=NA
       ,col=color,cex=grosor)
  text(x=0.8,y=7,labels="GL=10",pos = 2)
  x_vector <- seq(cotainf,cotasup,length = 1000)
  #abline(h=0,v=0)
  #crear vector de valores de densidad chi-cuadrado
  p_vector <- dchisq (x_vector, df )
  polygon(c (x_vector, rev(x_vector)), c (p_vector, rep (0, length (p_vector))),
          col = "#87CEFA", border = NULL)
  
}