#Metemos los datos en "Vectores y componemos una tabla

pesos<-c(2,2.8,3.3,3.2,4.4,3.6,1.9,3.3,2.8,1.1,3.5,2.8,3.2,3.5,2.3,2.4,2,1.6,3.3,3.6,2.6,3.1,3.2,3.3,2.9,3.4,3.2,3.2,3.2,3.3,3.2,2.9,3.3,2.5,2.6,2.8,2.6,2.6,2.9,2,2,2.1,3.1,2.9,3.1,2.5,2.6,2.2,2.2,2.5,1.2,1.2,2.5,2.4,3,1.5)
camadas<-factor(rep(c("I","II","III","IV","V","VI","VII","VIII"),c(10,8,10,8,6,4,6,4)))
datos<-data.frame(camadas,pesos)

# HAremos una análisis de la varianza para contrastar la hipótesis nula de 
# de que los pesos medios de las camadas sean iguales, bajo las 
# suposiciones habituales de que los pesos de los lechones siguen una
# distribucion normal de probabilidad y que son poblaciones independientes:
# H0: Los pesos medios de las camadas son iguales.
# H1: Son diferencies
resultado<-aov(pesos~camadas)
summary(resultado)

# El resultado que arroja el análisis es:
# >             Df Sum Sq Mean Sq F value Pr(>F)
# > camadas      7  7.489  1.0698   2.992 0.0109 *
# > Residuals   48 17.165  0.3576
# > ---
# > Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
# Ep p-valor es muy bajo, lo que nos conduce a tener que rechazar la 
# hipótesis nula, por lo que concluimos que los pesos de los lechones en
# las distintas camadas no tienen la misma media, por lo que es posible que
# haya una causa subyaciente que explique esta discrepancia, como que las madres siguan dietas diferentes, enfermedades u otras explicaciones plausibles.
# En cualquier caso, vamos a analizar si las condiciones de homocedasticidad se cumplen en las poblaciones estudiadas o no, siguiendo las indicaciones del libro.
par(mfrow=c(2,4))
qqnorm(pesos[1:10])
qqnorm(pesos[11:18])
qqnorm(pesos[19:28])
qqnorm(pesos[29:36])
qqnorm(pesos[37:42])
qqnorm(pesos[43:46])
qqnorm(pesos[47:52])
qqnorm(pesos[53:56])

#Vemos que los datos están en la diagonal del gráfico más o menos, po rlo que damos por válidad la hipótesis de que los datos sigan una distribución normal.

#Por otro lado, analizando las varianzas de los pesos de las camadas usando gráficos de caja, obsevamos lo siguiente:
par(mfrow=c(1,1))
boxplot(pesos~camadas)
bartlett.test(pesos~camadas)

#Vemos que los gráficos de cajas no son muy parecidos unos a otros, lo que me lleva a pensar que las varianzas de las distintas poblaciones no son iguales. El test de Barlett confirma que las varianzas no son comparables:

# > 
# >         Bartlett test of homogeneity of variances
# > 
# > data:  pesos by camadas
# > Bartlett's K-squared = 18.921, df = 7, p-value = 0.008437

#Es posible que la causa de la diferencia de la media de las camadas también tenga una influencia en laa varianza de dichos pesos.  Por lo tanto, me veo obligado a ejecutar el test oneway para confirmar si los valores obtenidos en el primer test son válidos:

oneway.test(pesos~camadas)

# El resultado obtenido:
# 
# >         One-way analysis of means (not assuming equal variances)
# > 
# > data:  pesos and camadas
# > F = 4.5301, num df = 7.000, denom df = 16.603, p-value = 0.005431

# arroja un valor del pvalor también muy pequeño, por lo que refrenda el resultado del test anterior, las diferencias de pesos de las camadas son lo suficientemente significativas como para pensar que algún factor externo les ha afectado. 