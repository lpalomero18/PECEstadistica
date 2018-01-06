
# Metemos los datos en un vector
indices <- c(63,80,69,70,69,72,75,60,71,64)

# hacemos el test

t.test(indices, y=NULL, alternative="less", mu=70, paired=FALSE, var.equal = FALSE, conf.level=0.95)


library(RColorBrewer)
cols  <- colorRampPalette(brewer.pal(12, "Set3"), alpha=TRUE)(ncol(indices))
colsm <-matrix(rep(cols, each=nrow(indices)), ncol=ncol(indices))
palette <- brewer.pal("Greys", n=9)
color.background = palette[2]
color.grid.major = palette[5]
# set graphical area
jpeg("cajas ej2.jpg")
par(bty="n", bg=palette[2], mar=c(5,8,3,1))


nombres = rep("A",10);
datos<-data.frame(nombres,indices)
boxplot(datos$indices~datos$nombres, main="Indice de colesterol LDL en sangre",ylim=c(50,90),col=cols, xaxt="n", yaxt="n", outline=FALSE, lty=1,  boxwex=0.8, boxlwd=1, medlwd=1)
# plot gridlines
for (i in seq(0,100,by=5)) {
	lines( c(0,20),c(i,i), col=palette[4])
}

for (i in seq(1,17,by=1)) {
	lines(c(-5,105), c(i,i), col=palette[4])
}
boxplot(datos$indices~datos$nombres, main="Indice de colesterol LDL en sangre",col=cols, xaxt="n", yaxt="n",add=TRUE, outline=FALSE, lty=1,  boxwex=0.8, boxlwd=1, medlwd=1)
  myjitter<-jitter(rep(1, length(indices)), amount=0.2)
  points(myjitter, indices, pch=19, col=rgb(0,0,0,.2)) 


axis(side=2, at=seq(0,100,by=10), col.axis=palette[7], cex.axis=0.8, lty=0, tick=NA, line=-1)
axis(side=2, at=70, labels="microgramos/ml", lty=1, tick=NA, col.axis=palette[7])
dev.off()
