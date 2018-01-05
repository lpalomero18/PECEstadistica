
# Guardamos los datos en vectores
eritrocitos<-c(0,1,2,3,4)
frecuencias<-c(40000,8621,1259,99,21)

# Calculamos el parámetro lambda de la distribución de poisson, que no es otro que
# la media
lambda<-sum(eritrocitos*frecuencias)/sum(frecuencias)

#Calculamos las frecuencias que corresponderían a una poisson con esa lambda
frecuencias.teoricas<-dpois(eritrocitos,lambda)

# Añadimos las probabilidades que faltan, hace falta que el vector de probailidades
# sume 1, en este caso, es una probabilidad de 4.467641e-06, ridícula, y la asigno 
# al rarísimo caso de que una célula tenga 5 eritrocitos.

frecuencias.teoricas.b<-append(frecuencias.teoricas,1-sum(frecuencias.teoricas))
frecuencias.b<-append(frecuencias,5)
chisq.test(frecuencias.b,p=frecuencias.teoricas.b)

# El resultado es:
# 
# 
# >         Chi-squared test for given probabilities
# > 
# > data:  frecuencias
# > X-squared = 235.87, df = 5, p-value < 2.2e-16
# > 
# > Warning message:
# > In chisq.test(frecuencias, p = frecuencias.teoricas) :
# >   Chi-squared approximation may be incorrect
#
# Un p-valor tan pequeño que nos obliga a descartar la hipótesis nula de  
# que los datos se ajusten a una distribución de Poisson. 

# Si representamos los datos gráficamente, los datos reales en azul, los de la
# distribución de Poisson en rojo, :


plot(as.table( setNames(frecuencias/50000,eritrocitos)), type="h", col="blue", from=0, to=4, lwd=4)
points(as.table( setNames(frecuencias.teoricas,eritrocitos)), col="red", type="h", to=4)
