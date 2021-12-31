#####################################
##DISTRIBUCION MUESTRAL DE LA MEDIA #
#####################################

#EJEMPLO 8.7 Walpole et.al (2007)
#Un importante proceso de fabricación produce partes de componentes 
#cilíndricos para la industria automotriz. Es importante que el 
#proceso produzca partes que tengan una media de 5 milímetros. 
#El ingeniero implicado hace la conjetura de que la media de la 
#población es de 5.0 milímetros. Se lleva a cabo un experimento 
#donde se seleccionan al azar 100 partes elaboradas por el proceso 
#y se mide el diámetro de cada una de ellas. Se sabe que la desviación 
#estándar de la población es sigma = 0.1. El experimento indica un diámetro 
#promedio de la muestra x_barra = 5.027 milímetros. ¿Esta información de la muestra 
#parece apoyar o refutar la conjetura del ingeniero?

  
#repeticiones del experimento
rep = 10000
#tamano de la muestra
n = 100
#media del diametro de las piezas
mu = 5
#desviacion estandar del diametro de las piezas
sigma = 0.1
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

# El ingeniero implicado hace la conjetura de que la media 
#de la población es de 5.0 milímetros. 
# Se lleva a cabo un experimento donde se seleccionan al 
#azar 100 partes elaboradas por el proceso y se mide el 
# diámetro de cada una de ellas.
# El experimento indica un diámetro promedio de la muestra
# x_barra = 5.027 milímetros. ¿Esta información de la 
#muestra parece apoyar o refutar la conjetura del ingeniero?

#Indicadora de que Medias mayores que 5.027
muestras$ind = (muestras$Media > 5.027)
#Conteo del numero de Medias menores a 775
sum(muestras$ind)
#Proporcion del numero de Medias menores a 775
sum(muestras$ind)/rep
