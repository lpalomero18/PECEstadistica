# Esta correlación podría merecer un heco en la siguiente web: http://www.tylervigen.com/spurious-correlations
# no está muy claro a cual es la escala temporal de los datos, si son noticias publicadas diariamente vs accidentes diarios, o son acumulados mensuales, o si por contra no son datos cronológicos si no que son de un mismo periodo de tiempo en diferentes regiones, pero, en cualquier caso, es indiferente para el análisis.

# comenzaré como siempre por meter los datos en vectores.

noticias<-c(376,347,322,104,103,98,96,85,82,63,44,40,5,5,0,0,0)
accidentes<-c(8,5,8,4,6,4,8,6,4,2,7,4,3,2,4,3,2)

plot(accidentes,noticias,main="Ajuste por mínimos cuadrados", xlab="Accidentes de avioneta", ylab="Noticias sobre hechos luctuosos", pch=19,cex=0.5)

# Hacemos la regresión lineal.
# La hipótesis nula es la de ´NO existe relación lineal entre accidentes de avioneta y noticias. 
ajuste<-lm(noticias~accidentes)
summary(ajuste)
abline(ajuste, col="blue")
# > Call:
# > lm(formula = noticias ~ accidentes)
# > 
# > Residuals:
# >      Min       1Q   Median       3Q      Max
# > -144.808  -48.891   -4.227   25.941  232.025
# > 
# > Coefficients:
# >             Estimate Std. Error t value Pr(>|t|)
# > (Intercept)   -69.61      59.77  -1.165  0.26239
# > accidentes     36.92      11.64   3.171  0.00633 **
# > ---
# > Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# > 
# > Residual standard error: 98.47 on 15 degrees of freedom
# > Multiple R-squared:  0.4013,    Adjusted R-squared:  0.3614
# > F-statistic: 10.05 on 1 and 15 DF,  p-value: 0.006333

#El p-valor es muy pequeño, por lo que rechazamos la hipótesis nula y aceptamos que existe una covarianza entre ambos conjuntos de datos.
