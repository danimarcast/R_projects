n<-500
paso<-1:n
p<-0.5
c_inicial<-0
X<-rbinom(n,1,prob = p)
for (i in 1:length(X)){
  if (X[i]==0){
    X[i]=-1
  }
}

S<-c()

for (i in 1:length(X)){
  if (i==1){
    S[i]<-c_inicial
  }
  else{
    S[i]<-S[i-1]+X[i]
  }
}
#Numero de pasos hacia arriba
r<-0
for (i in 2:length(X)){
  if (X[i]==-1){
    r=r+1
  }
  else{
    j=j+1
  }
}
#Numero de pasos hacia abajo
l<- 0
for (i in 2:length(X)){
  if (X[i]==1){
    l=l+1
  }
  else{
    j=j+1
  }
}

r
l

plot(paso,S,type = "l",col="blue",pch=8)
maximo<-max(S)
maximo
minimo<-min(S)
minimo
