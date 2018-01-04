# Lo que vamos a hacer es un contraste por hipótesis para ver si es cierta o no
# la hipótesis de que la media es significativamente menor que 70. Para ello, 
# estableceremos las siguientes hipótesis:

# H0: La media es igual o mayor que 70
# H1: La media es menor que 70

# Metemos los datos en un vector
indices <- c(63,80,69,70,69,72,75,60,71,64)

# hacemos el test

t.test(indices, y=NULL, alternative="less", mu=70, paired=FALSE, var.equal = FALSE, conf.level=0.95)

# la salida de este comando es:
#  > 
#  > One Sample t-test
#  > 
#  > data:  indices
#  > t = -0.3759, df = 9, p-value = 0.3578
#  > alternative hypothesis: true mean is less than 70
#  > 95 percent confidence interval:
#  > -Inf 72.71362
#  > sample estimates:
#  > mean of x
#  >      69.3
#
# Con un p-valor tan alto, no podemos descartar la hipótesis nula. 