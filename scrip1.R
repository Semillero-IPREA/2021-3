
#Tamano de la muestra
n=1000
#Parametro p
p=0.1
#Numero de repeticiones del experimiento
rep = 10000
#Matriz para guardar las muestras 
muestras = matrix(data = NA, nrow = rep, ncol = n)
#Generacion de las muestras de tamaño n provenientes de una población
#Bernoulli con parametro p
for (i in 1:rep) {
  muestras[i,] = rbinom(n,1,p) 
}
#Convertir la matriz muestras en un data.frame
muestras = data.frame(muestras)
#Crear una variable columna al final del data.frame con la proporcion de exitos
#llamada P_gorro
muestras$P_gorro = apply(muestras, 1, mean)
#A medida que el tamaño de la muestra n se hace grande la distribución tiende
#a parecerse a una distribución normal
hist(muestras$P_gorro, breaks=50)
#Media del estimador P_gorro
mean(muestras$mean)
#Varianza del estimador P_gorro
var(muestras$mean)

