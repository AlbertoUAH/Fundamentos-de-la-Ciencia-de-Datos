# Funcion que lee las muestras contenidas en un fichero .txt
# Para ello le pasamos la ruta del fichero, la separacion entre
# los datos, la separacion entre las muestras (salto) y si tiene
# cabecera o no.
muestra.leer <- function(ruta,sep=" ",salto="",header=FALSE){
  # Creamos la conexion y leemos las lineas del fichero.
	con <- file(ruta, "r", blocking = FALSE)
	datos <- readLines(con)
	close(con)
	
	# Establecemos las variables para controlar el programa
	# y la variable dataframes donde guardaremos cada una de
	# las muestras obtenidas.
	tabla = NULL
	contador = 1
	dataframes = list()

	# Leemos cada una de las lineas obtenidas anteriormente
	# y las escribimos en un archivo temporal. Cuando detectamos
	# el fin de la muestra, realizamos un read.table de ese
	# archivo temporal.
	for(linea in datos){
		if(linea==salto){
			write(tabla,file="data.temp")
			dataframes = append(dataframes,list(read.table("data.temp")))
			file.remove("data.temp")
			tabla = NULL
		} else{
			tabla = c(tabla,linea)
		}
	}
	if(!is.null(tabla)){
		write(tabla,file="data.temp")
		dataframes = append(dataframes,list(read.table("data.temp")))
		file.remove("data.temp")
	}

	dataframes
}

# Lectura del fichero muestra4.txt
muestras <- muestra.leer("muestra4.txt")
muestras

## Arboles de decision: Algoritmo de Hunt ##

# Funcion principal. Llama a las funciones que construyen el arbol y lo muestran.
arbol <- function(muestra){
  clasificacion <- arbol.clasificacion(muestra)
  arbol.mostrar(clasificacion)
}

# Realiza la construccion del arbol de decision mediante el comando rpart utilizando
# el Gini para calcular la ganancia de informacion.
# Además, como parametro le podemos pasar la columna que queramos considerar
# como suceso clasificador (por defecto, sera la ultima).
arbol.clasificacion <- function(muestra,posClasificador=length(muestra)){
  clasificacion <- rpart(paste(colnames(muestra)[posClasificador],"~."),data=muestra,
                         method="class",minsplit=1,parms=list(split="gini"),model=T)
}

# Muestra el arbol de decision mediante el comando rpart.plot.
arbol.mostrar <- function(clasificacion){
  rpart.plot(clasificacion,type=5)
}

## Ejecucion ##
arbol(vehiculos)

## Regresion ##

# Funcion encargada de realizar las 4 regresiones lineales y
# mostrarlas en una misma ventana (diagrama de dispersion y recta
# de ajuste).
mostrar.regresion <- function(ruta,sep=" ",salto="",header=FALSE){
  dataframe <- muestra.leer(ruta,sep,salto,header)
  par(mfrow=c(2,2))
  for (i in 1:length(dataframe)){
    data <- dataframe[[i]]
    regresion <- lm(Y~X,data = data)
    main <- paste("Muestra ",i)
    plot(data$X,data$Y,xlim=c(0,20),ylim=c(0,14),xlab='x', ylab='y', main = main)
    abline(regresion,col='red')
  }
}

# Varianza iterativa
varianza <- function(vector){
  media = mean(vector)
  suma=0
  for(i in 1:length(vector)){
    suma = suma + (as.numeric(vector[i])-media)^2
  }
  suma/length(vector)
}

# Desviacion tipica iterativa
desviacion.tipica <- function(vector){
  sqrt(varianza(vector))
}

# Covarianza
covarianza <- function(x,y){
  (sum(x*y)/length(x))-(mean(x)*mean(y))
}

# Correlacion
correlacion <- function(x,y){
  covarianza(x,y)/(desviacion.tipica(x)*desviacion.tipica(y))
}

# Funcion de regresion
# Encargada de obtener a y b
regresion <- function(x,y){
  b <- regresion.b(x,y)
  a <- regresion.a(x,y,b)
  data.frame(a,b)
}

# Obtiene b
regresion.b <- function(x,y){
  covarianza(x,y)/(desviacion.tipica(x)^2)
}

# Obtiene a
regresion.a <- function(x,y,b){
  mean(y)-(b*mean(x))
}

# Obtiene los valores de y para unos valores de x
# de la funcion de regresion
regresion.ycalculada <- function(x,regresion){
  regresion$a + x*regresion$b
}

## ANOVA ##

# Funcion que realiza el analisis de ANOVA
anova <- function(x,y,regresion){
  ssr <- anova.ss(regresion.ycalculada(x,regresion),mean(y))
  ssy <- anova.ss(y,mean(y))
  r2 <- ssr/ssy
  data.frame(ssr,ssy,r2)
}

# SSR y SSy dependiendo de la y (observada o calculada)
anova.ss <- function(y,media){
  sum((y-media)^2)
}

# Error estandar
error.estandar <- function(y,regresion){
  sqrt(sum((y-regresion.ycalculada(y,regresion))^2)/length(y))
}

# Funcion encargada de mostrar graficamente el error
# estandar junto con las rectas del 95% y 66%.
error.estandar.plot <- function(x,y,regresion){
  sr <- error.estandar(y,regresion)
  plot(x,y,xlim=c(0,ceiling(max(x))),ylim=c(floor(regresion$a),ceiling(max(y))),xlab='x', ylab='y', main = "Error Estándar")
  abline(regresion$a,regresion$b,col='red')
  
  abline(regresion$a+sr,regresion$b,col='green')
  abline(regresion$a-sr,regresion$b,col='green')
  
  abline(regresion$a+2*sr,regresion$b,col='blue')
  abline(regresion$a-2*sr,regresion$b,col='blue')
  
  legend(x="topleft",legend=c("Regresión","66%","95%"),fill=c("red","green","blue"))
}