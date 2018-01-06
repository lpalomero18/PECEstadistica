
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


# Si representamos los datos gráficamente, los datos reales en azul, los de la
# distribución de Poisson en rojo, :

png("histograma ej3.png")
plot(as.table( setNames(frecuencias/50000,eritrocitos)), type="h", col="blue",  lwd=4, ylab="Frecuencia", xlab="Eritrocitos por celula")
points(as.table( setNames(frecuencias.teoricas,eritrocitos)), col="red", type="h")
legend(2,0.6,c("Frecuencia teorica","Frecuencia real"),col=c(2,4), lty=c(1),lwd=3)
dev.off()