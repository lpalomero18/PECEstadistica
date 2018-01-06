#Metemos los datos en "Vectores y componemos una tabla

pesos<-c(2,2.8,3.3,3.2,4.4,3.6,1.9,3.3,2.8,1.1,3.5,2.8,3.2,3.5,2.3,2.4,2,1.6,3.3,3.6,2.6,3.1,3.2,3.3,2.9,3.4,3.2,3.2,3.2,3.3,3.2,2.9,3.3,2.5,2.6,2.8,2.6,2.6,2.9,2,2,2.1,3.1,2.9,3.1,2.5,2.6,2.2,2.2,2.5,1.2,1.2,2.5,2.4,3,1.5)
camadas<-factor(rep(c("I","II","III","IV","V","VI","VII","VIII"),c(10,8,10,8,6,4,6,4)))
datos<-data.frame(camadas,pesos)

# Análisis de la varianza
resultado<-aov(pesos~camadas)
summary(resultado)


png("ajustes.png")
par(mfrow=c(2,4))
qqnorm(pesos[1:10] , main="camada I", ylab="Peso", xlab="")
qqnorm(pesos[11:18], main="camada II", ylab="Peso", xlab="")
qqnorm(pesos[19:28], main="camada III", ylab="Peso", xlab="")
qqnorm(pesos[29:36], main="camada IV", ylab="Peso", xlab="")
qqnorm(pesos[37:42], main="camada V", ylab="Peso", xlab="")
qqnorm(pesos[43:46], main="camada VI", ylab="Peso", xlab="")
qqnorm(pesos[47:52], main="camada VII", ylab="Peso", xlab="")
qqnorm(pesos[53:56], main="camada VIII", ylab="Peso", xlab="")
dev.off()
#Vemos que los datos están en la diagonal del gráfico más o menos, po rlo que damos por válidad la hipótesis de que los datos sigan una distribución normal.

png("cajas ej4.png")
#Por otro lado, analizando las varianzas de los pesos de las camadas usando gráficos de caja, obsevamos lo siguiente:
par(mfrow=c(1,1))
library(RColorBrewer)
cols  <- colorRampPalette(brewer.pal(12, "Set3"), alpha=TRUE)(8)

boxplot(pesos~camadas,ylab="Peso",xlab="Camada",col=cols )
  myjitter<-jitter(rep(1, length(indices)), amount=0.2)

  data=data.frame(camadas,pesos)
mylevels<-levels(data$camadas)
levelProportions<-summary(data$camadas)/nrow(data)

for(i in 1:length(mylevels)){

  thislevel<-mylevels[i]
  thisvalues<-data[data$camadas==thislevel, "pesos"]
   
  # take the x-axis indices and add a jitter, proportional to the N in each level
  myjitter<-jitter(rep(i, length(thisvalues)), amount=levelProportions[i]/2)
  points(myjitter, thisvalues, pch=19, col=adjustcolor(rep(cols[i],10),offset = c(0.05, 0.05, 0.05, 0)))
   
}

# plot gridlines
for (i in seq(0,100,by=5)) {
	lines( c(0,20),c(i,i), col=palette[4])
}

for (i in seq(1,17,by=1)) {
	lines(c(-5,105), c(i,i), col=palette[4])
}

dev.off()

#Test de barlett para comprobar la homocedasticidad

bartlett.test(pesos~camadas)

#Test oneway para confirmar si el primer análisis dio un dato coherente.
oneway.test(pesos~camadas)
