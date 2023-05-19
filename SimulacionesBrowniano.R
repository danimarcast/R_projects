library(ggplot2)

# Esta función simula una aproximación de un movimiento Brownianoç

# Función deriva (variar p_N)
p_N <- 0.58
N <- 10**4

browniano <- function(N,p){
  n <- rbinom(N,1,p)
  n <- 1*(n==1) - 1*(n==0)
  z <- c(0)
  for(i in 1:(N-1)){
    z[i+1] <- z[i] + n[i]
  }
  z <- z/sqrt(N)
  return(z)
}

deriva <- function(vec){
  sqrt(N)*(2*p_N - 1)*vec
}


axis <- seq(0,1,length=N)

derframe <- data.frame(axis)
for(i in 1:6){
  b <- browniano(N,p_N)
  derframe <- data.frame(derframe,b)
}
derplot <- ggplot( data=derframe, aes(x=axis))+
  geom_line(aes(y=b.2),col="salmon")+geom_line(aes(y=b.4),col="red")+geom_function(fun=deriva, aes(color="Media") ) + labs(x=quote(t), y=quote(X[t])) + geom_line(aes(y=b.3))+labs(x=quote(t), y=quote(X[t])) + scale_color_manual(name=NULL , breaks=c('Media'), values=c('Media'='#00B8E7')) +
  theme(legend.position = c(0.15, 0.9) )
print(derplot)

