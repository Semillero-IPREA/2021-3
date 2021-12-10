##SIMULACION METODOS DE MUESTREO CON LA BASE IRIS
## MAS, ESTRATIFICADO, SISTEMATICO

#Cargar la base de datos iris en la variable iris.df
iris.df = data.frame(iris)

#Resumen estadistico de las variables de iris y boxplot para cada variable
summary(iris.df)
par(mfrow=c(1,1),mar=c(6.5,2.5,0.5,0.5),xpd=NA)
boxplot(iris.df[,1:4],ylim=c(0,8), las=2)

#Muestreo Aleatorio Simple (MAS) sobre la base de datos
iris.sample <- sample(1:nrow(iris.df), nrow(iris.df)*0.75, replace = FALSE)

par(mfrow=c(1,2),mar=c(6.5,2.5,1.5,0.5),xpd=NA)
boxplot(iris.df[,1:4],ylim=c(0,8), main="base completa", las=2)
boxplot(iris.df[iris.sample,1:4],ylim=c(0,8), main="muestra: aleatoria simple", las=2)



install.packages("splitstackshape")
library(splitstackshape)

#Muestreo estratificado sobre la base iris
#Estratos conformados con base en la variable "Sepal.Length"
iris.strat1 = stratified(iris.df, "Sepal.Length", 0.7)

par(mfrow=c(1,2),mar=c(6.5,2.5,1.5,0.5),xpd=NA)
boxplot(iris.df[,1:4],ylim=c(0,8), main="base completa", las=2)
boxplot(iris.strat1[,1:4],ylim=c(0,8), main="muestra: estratificado 1", las=2)

#Muestreo estratificado sobre la base iris
#Estratos conformados con base en la variable "Sepal.Width" y "Petal.Width" 
iris.strat2 = stratified(iris.df, c("Sepal.Width","Petal.Width"), 0.7)

par(mfrow=c(1,2),mar=c(6.5,2.5,1.5,0.5),xpd=NA)
boxplot(iris.df[,1:4],ylim=c(0,8), main="base completa", las=2)
boxplot(iris.strat2[,1:4],ylim=c(0,8), main="muestra: estratificado 2", las=2)

#Muestreo sistematicosobre la variable iris
sys.sample = function(N, n) {
  k = ceiling(N/n)
  r = sample(1:k, 1)
  sys.samp=seq(r,r+k*(n-1),k) }

iris.sys <- sys.sample(nrow(iris.df), nrow(iris)*0.75)

par(mfrow=c(1,2),mar=c(6.5,2.5,1.5,0.5),xpd=NA)
boxplot(iris.df[,1:4],ylim=c(0,8), main="base completa", las=2)
boxplot(iris.df[iris.sys,1:4],ylim=c(0,8), main="muestra: sistematica", las=2)