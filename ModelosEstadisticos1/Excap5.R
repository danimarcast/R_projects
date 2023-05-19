#Datos Seccion 5.1 transformacion y ponderación
# Ejemplo 5.1
library(MASS)
library(latex2exp)
x<-c(679,292,1012,493,582,1156,997,2189,1097,2078,1818,1700,747,2030,1643,414,354,1276,745,435,540,874,1543,1029,710,1434,
     837,1748,1381,1428,1255,1777,370,2316,1130,463,770,724,808,790,783,406,1242,658,1746,468,114,413,
     1787,3560,1495,2221,1526)
y<-c(0.79,0.44,0.56,0.79,2.70,3.64,4.73,9.50,5.34, 6.85, 5.84, 5.21, 3.25, 4.43,
     3.16, 0.50,0.17, 1.88, 0.77, 1.39, 0.56, 1.56, 5.28, 0.64, 4.00, 0.31,
     4.20,4.88,3.48,7.58,2.63,4.99,0.59,8.19,4.79,0.51,1.74,4.10,3.94,0.96,3.29,0.44,3.24,2.14,
     5.71,0.64,1.90,0.51,8.22, 14.94, 5.11, 3.85, 3.93)

plot(x,y,xlim=c(0,4000),ylab="Demanda",xlab="Consumo",pch=20,cex=2)

data<-cbind(x,y)
modelo1<-lm(y~x)
avar<-anova(modelo1)

studRes1<-studres(modelo1)
plot(modelo1$fitted.values,studRes1,pch=20,cex=2,ylab=TeX(r'($t_i$)'),xlab=TeX(r'($\hat{y}_i$)'),las=1,ylim = c(-3,3))

transformados<-sqrt(y)
modelo2<-lm(transformados~x)
studRes2<-studres(modelo2)
plot(modelo2$fitted.values,studRes2,pch=20,cex=2,ylab=TeX(r'($t_i$)'),xlab=TeX(r'($\hat{y}_i$)'),las=1,ylim = c(-3,3))


# Datos sección 5.3 Transformaciones para linealizar el modelo
# Ejemplo 5.2

y<-c(1.582, 1.822, 1.057, 0.500, 2.236, 2.386, 2.294, 0.558, 2.166, 1.866,
     0.653, 1.930, 1.562, 1.737, 2.088, 1.137, 2.179, 2.112, 1.800, 1.501, 2.303,
     2.310, 1.194, 1.144, 0.123)
x<- c(5.00, 6.00, 3.40, 2.70,
      10.00, 9.70, 9.55, 3.05, 8.15, 6.20, 2.90, 6.35, 4.60, 5.80, 7.40, 3.60,
      7.85, 8.80, 7.00, 5.45, 9.10, 10.20, 4.10, 3.95, 2.45)

plot(x,y,xlab=TeX(r'(Velocidad del Viento $x$ )'),ylab=TeX(r'(Salida DC $y$)'),pch=20,cex=2)

modelo3<-lm(y~x)
plot(modelo3$fitted.values,modelo3$residuals,pch=19,cex=1, ylab=TeX(r'($e_i$)'),xlab = TeX(r'($\hat{y}_i$)'))
plot(1/x,y,xlab=TeX(r'($x'=\frac{1}{x}$ )'),ylab=TeX(r'(Salida DC $y$)'),pch=20,cex=2)

y1<-y
x1<-1/x

modelo4<-lm(y1~x1)
studRes3<-studres(modelo4)
plot(modelo4$fitted.values,studRes3,pch=19,xlab = TeX(r'($\hat{y}_i$)'),ylab = TeX(r'($t_i$)'))


reg.conf.intervals <- function(x, y) {
  n <- length(y) # Find length of y to use as sample size
  lm.model <- lm(y ~ x) # Fit linear model
  
  # Extract fitted coefficients from model object
  b0 <- lm.model$coefficients[1]
  b1 <- lm.model$coefficients[2]
  
  # Find SSE and MSE
  sse <- sum((y - lm.model$fitted.values)^2)
  mse <- sse / (n - 2)
  
  t.val <- qt(0.975, n - 2) # Calculate critical t-value
  
  # Fit linear model with extracted coefficients
  x_new <- 1:max(x)
  y.fit <- b1 * x_new + b0
  
  # Find the standard error of the regression line
  se <- sqrt(sum((y - y.fit)^2) / (n - 2)) * sqrt(1 / n + (x - mean(x))^2 / sum((x - mean(x))^2))
  
  # Fit a new linear model that extends past the given data points (for plotting)
  x_new2 <- 1:max(x + 100)
  y.fit2 <- b1 * x_new2 + b0
  
  # Warnings of mismatched lengths are suppressed
  slope.upper <- suppressWarnings(y.fit2 + t.val * se)
  slope.lower <- suppressWarnings(y.fit2 - t.val * se)
  
  # Collect the computed confidence bands into a data.frame and name the colums
  bands <- data.frame(cbind(slope.lower, slope.upper))
  colnames(bands) <- c('Lower Confidence Band', 'Upper Confidence Band')
  
  # Plot the fitted linear regression line and the computed confidence bands
  plot(x, y, pch = 19)
  lines(y.fit2, col = 'black', lwd = 2)
  lines(bands[1], col = 'blue', lty = 2, lwd = 2)
  lines(bands[2], col = 'blue', lty = 2, lwd = 2)
  
  return(bands)
}



```{r}
modelo3<-lm(maria_dosis0$Altura.en.cm~maria_dosis0$Temperatura.en.C)
summary(modelo3)
plot(maria_dosis0$Temperatura.en.C,maria_dosis0$Altura.en.cm,xlab="Temperatura",ylab="Altura",pch=19)
abline(lm(maria_dosis0$Altura.en.cm~maria_dosis0$Temperatura.en.C),col="red")
```
```{r}
plot(modelo3$fitted.values,modelo3$residuals)
abline(h=0,lty=2,col="red")
qqnorm(y=modelo3$residuals)
qqline(modelo3$residuals)
shapiro.test(modelo3$residuals)

```


```{r}
modelo4<-lm(maria_dosis1$Altura.en.cm~maria_dosis1$Temperatura.en.C)
summary(modelo3)
plot(maria_dosis1$Temperatura.en.C,maria_dosis1$Altura.en.cm,xlab="Temperatura",ylab="Altura",pch=19)
abline(lm(maria_dosis1$Altura.en.cm~maria_dosis1$Temperatura.en.C),col="red")

```
Aplicando un transformación logaritmo a la variable respuesta tenemos
```{r}
ydosis1Trans<-log(maria_dosis1$Altura.en.cm)

modelo5<-lm(ydosis1Trans~maria_dosis1$Temperatura.en.C)
summary(modelo5)
plot(maria_dosis1$Temperatura.en.C,ydosis1Trans,xlab="Temperatura",ylab="Altura",pch=19)
abline(lm(ydosis1Trans~maria_dosis1$Temperatura.en.C),col="red")

```
validando supuestos para los datos transformados
```{r}
shapiro.test(modelo5$residuals)

plot(modelo5$fitted.values,rstudent(modelo5),ylab="studentizados",xlab="fitted",pch=19)
abline(h=0,lty=2,col="red")
#plot(modelo5)
```



