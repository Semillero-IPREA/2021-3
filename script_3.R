######################################
##DISTRIBUCION MUESTRAL DE LA MEDIA  #
######################################

#EJEMPLO 8.6 Walpole, et.al (2007)

#Una empresa de material eléctrico fabrica bombillas de luz que tienen 
#una duración que se distribuye aproximadamente en forma normal, 
#con media de 800 horas y desviación estándar de 40 horas. 
#Encuentre la probabilidad de que una muestra aleatoria de 16 bombillas 
#tenga una vida promedio de menos de 775 horas.

#repeticiones del experimento
rep = 10000
#tamano de la muestra
n = 16
#media de la población de bombillos
mu = 800
#desviacion estandar de la población de bombillos
sigma = 40
#matriz para guardar las muestras
muestras = matrix(data = NA, nrow = rep, ncol = n)
#Generacion de las muestras de tamaño n provenientes de una población
#Normal con media mu y desviación sigma
for (i in 1:rep) {
  muestras[i,] = rnorm(n, mean=mu, sd=sigma) 
}
#Convertir la matriz muestras en un data.frame
muestras = data.frame(muestras)
#Crear una variable columna al final del data.frame con la proporcion de exitos
#llamada Media
muestras$Media = apply(muestras, 1, mean)
#Histograma de frecuencias para la variable aleatorio o estimador Media
hist(muestras$Media, breaks=50)
#Media del estimador Media
mean(muestras$Media)
#Varianza del estimador Media
var(muestras$Media)
#Desviacion estandar del estimador Media
sd(muestras$Media)

#Histograma con frecuencias relativas
hist(muestras$Media, breaks=50, freq = FALSE)
#Curva normal teorica superpuesta
curve(dnorm(x, mean=mu, sd=sigma/sqrt(n)), col="red", lwd=2, add=TRUE, yaxt="n")

# Encuentre la probabilidad de que una muestra aleatoria 
# de 16 bombillas tenga una vida promedio de menos de 
# 775 horas.

#Indicadora de que Medias son menores que 775
muestras$ind = (muestras$Media < 775)
#Conteo del numero de Medias menores a 775
sum(muestras$ind)
#Proporcion del numero de Medias menores a 775
sum(muestras$ind)/rep
