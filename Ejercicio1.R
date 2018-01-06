# Intentaré seguir los estándares de estilo de Google: https://google.github.io/styleguide/Rguide.xml

# Cargamos el fichero antebrazos.txt. He tenido que retocarlo un poco porque incluía
# saltos de línea y he puesto todos los datos en una única línea.

longitudes<-scan("antebrazos.txt") # cargamos el ficher, tiene 140 datos

# Con summary tenemos varios de los datos que nos piden. Tenemos que quitar cabeceras cada vez
long.resumen<-summary(longitudes)
long.media=long.resumen["Mean"]
names(long.media)<-NULL
long.mediana=long.resumen["Median"]
names(long.mediana)<-NULL
long.min=long.resumen["Min."]
names(long.min)<-NULL
long.max=long.resumen["Max."]
names(long.max)<-NULL
long.cuartil.1=long.resumen["1st Qu."]
names(long.cuartil.1)<-NULL
long.cuartil.3=long.resumen["3rd Qu."]
names(long.cuartil.3)<-NULL

#La desviación típica tiene su propia función
long.desv.tipica=sd(longitudes)

# Para la moda, E no tiene función específica (Olé tus naies para el mejor programa de estadística)
# pero una úsqueda en google nos cuenta como construirnos la función apropiada

 Moda<- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
long.moda<-Moda(longitudes)

#Coeficiente de asimetría de Pearson

long.asimetria.Pearson<-100*long.desv.tipica/long.media

long.recorrido<-long.max-long.min  # 5.3 de recorrido
# Distribución de frecuencias absolutas. Partiremos los datos en intervalos
# según el criterio de Sturges k = 1 + 3,322log10(n)
intervalos<-round(1+3.322*log10(length(longitudes))) # 8 intervalos

# Establecemos los puntos de rotura con el máximo y el mínimo redondeados y 
# el número de intervalos calculados
intervalo.min<- floor(long.min)
intervalo.max<-ceiling(long.max)
breaks<-seq(intervalo.min,intervalo.max,(intervalo.max-intervalo.min)/intervalos) # estos son los puntos de corte
longitudes.cut<-cut(longitudes,breaks,right=FALSE)
longitudes.freq.abs <- table(longitudes.cut) # y esta es la distribución de frecuencias absolutas
longitudes.freq.relativa<-longitudes.freq.abs/length(longitudes)


# El histograma normalero
jpeg("histo ej1 a.jpg")
hist(longitudes,breaks,freq=FALSE, right=FALSE)
dev.off()

# Ahora intentaré hacer algo más bonito con ggplot
jpeg("histo ej1 b.jpg")
library(ggplot2)
ggplot(data.frame(val=longitudes), aes(x=val)) + 
  geom_histogram(breaks=breaks, aes(fill=..count..))+ 
  labs(title="Frecuencia de longitud de antebrazos") +
  labs(x="longitud", y="frecuencia absoluta")
dev.off()


#imprimimos a fichero los datos
sink("resultados ej1.txt")
print(paste("Media: ", long.media))
print(paste("Mediana: ", long.mediana))
print(paste("Moda: ", long.moda))
print(paste("Primer cuartil: ", long.cuartil.1))
print(paste("Tercer cuartil: ", long.cuartil.3))
print(paste("Desviación típica: ", long.desv.tipica))
print(paste("Recorrido: ", long.recorrido))
print(paste("Coeficiente de asimetría de Pearson: ", long.asimetria.Pearson))
sink()