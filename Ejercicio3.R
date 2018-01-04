
# Metemos las cosas en vectores
eritrocitos<-c(0,1,2,3,4)
frecuencias<-c(40000,8621,1259,99,21)

# Calculamos el parámetro landa de la distribución de poisson, que no es otro que
# la media
landa<-sum(eritrocitos*frecuencias)/sum(frecuencias)

#Calculamos las frecuencias que corresponderían a una poisson con esa landa
frecuencias.teoricas<-dpois(eritrocitos,landa)

# Añadimos las probabilidades que faltan, hace falta que el vector de probailidades
# sume 1, en este caso, es una probabilidad de 4.467641e-06, ridícula, y la asigno 
# al rarísimo caso de que una célula tenga 5 ritrocitos.
frecuencias.teoricas<-append(frecuencias.teoricas,1-sum(frecuencias.teoricas))
frecuencias<-append(frecuencias,5)
chisq.test(frecuencias,p=frecuencias.teoricas)

# El resultado es:
# 
# 
#         Chi-squared test for given probabilities
# 
# data:  frecuencias
# X-squared = 235.87, df = 5, p-value < 2.2e-16
# 
# Warning message:
# In chisq.test(frecuencias, p = frecuencias.teoricas) :
#   Chi-squared approximation may be incorrect