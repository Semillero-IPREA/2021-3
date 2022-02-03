########################################
##DISTRIBUCION MUESTRAL DE LA VARIANZA #
########################################

##-- Ejemplo 8.10 Walpole (2007)

#  Un fabricante de baterías para automóvil garantiza que 
# sus baterías durarán, en promedio, 3 años con una 
# desviación estándar de 1 año. Si cinco de estas baterías
# tienen duraciones de 1.9, 2.4, 3.0, 3.5 y 4.2 años, 
# ¿el fabricante aún está convencido de que sus baterías
# tienen una desviación estándar de 1 año? Suponga que 
# la duración de la batería sigue una distribución 
# normal.

#repeticiones del experimento
rep = 10000
#tamano de la muestra
n = 5
#media del diametro de las piezas
mu = 3
#desviacion estandar del diametro de las piezas
sigma = 1
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
muestras$S_2 = apply(muestras, 1, var)
#Crear una variable transformando S_2 que se llame Chi_Sq
muestras$Chi_Sq = (n-1)*muestras$S_2/sigma^2
#Histograma de frecuencias para la variable aleatoria o estimador Varianza
hist(muestras$S_2, breaks=50)
#Media del estimador Varianza
mean(muestras$S_2)
#Varianza del estimador Varianza
var(muestras$S_2)
#Histograma de frecuencias para la variable aleatorio o estimador Chi_Sq
hist(muestras$Chi_Sq, breaks=50)
#Media del estimador Chi_Sq
mean(muestras$Chi_Sq)
#Varianza del estimador Chi_Sq
var(muestras$Chi_Sq)

#Histograma con frecuencias relativas
hist(muestras$Chi_Sq, breaks=50, freq = FALSE)
#Curva Chi-cuadrado teorica superpuesta
curve(dchisq(x, df=4), col="red", lwd=2, add=TRUE, yaxt="n")

## -- Solución del ejercicio

#Creacion del vector de datos
datos = c(1.9, 2.4, 3.0, 3.5, 4.2)
#Calculo de su varianza
s2 = var(datos)
#Transformacion Chi-cuadrado
chis_quad = (n-1)*s2/sigma^2

#Que porcentaje de repeticiones del experimento son menores que 
#el valor de chis_quad
sum(muestras$Chi_Sq<chis_quad) 
#Que porcentaje de repeticiones del experimento son mayores que 
#el valor de chis_quad
sum(muestras$Chi_Sq>chis_quad)

#Podríamos preguntarnos entre que par de valores "centrales"
#se concentra el 95% de los valores de la variable Chi_Sq
quantile(muestras$Chi_Sq,c(0.025,0.975))

#Como el valor de chis_quad es un valor que cae entre estos
#dos valores, el valor de la muestra tiene alta probbilidad
#que ocurra