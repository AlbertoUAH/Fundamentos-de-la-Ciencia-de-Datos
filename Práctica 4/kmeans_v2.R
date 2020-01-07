# # Pruebas # #
# muestra <- read.table("muestra.txt")
# centroides <- read.table("centroides.txt")
# plot2D.our.kmeans(m2,our.kmeans(m2,5))

### PRUEBAS ###
# Esto... es que me parece curioso que puedas meter toda esa mierda en una llave y R se lo trague
# rbind(a,{aux=c();for(i in 1:3){aux=c(aux,i)};aux})


our.kmeans <- function(muestra,centroides,aleatorio=TRUE){
  if(aleatorio) centroides = elegir.centroides(muestra,centroides)

  continuar = TRUE
  asignacion.old = matrix(0,nrow=nrow(centroides),ncol=nrow(muestra))

  while(continuar){
    distancias = crear.matriz.distancias(muestra,centroides)
    asignacion = crear.matriz.asignacion(distancias)
    centroides = crear.centroides(muestra,distancias,asignacion)

    if(comprobar.matriz.asignacion(asignacion,asignacion.old)) continuar = FALSE
    else asignacion.old = asignacion
  }

  vector.clusters = crear.vector.clusters(asignacion)
  list(m.distancias=distancias,m.asignacion=asignacion,centroides=centroides,v.clusteriz=vector.clusters)
}



## Distancia euclidea ##
# Calcula la distancia euclidea entre dos puntos.
distancia.euclidea <- function(p1,p2){
  sum = 0
  for(i in 1:length(p1)){
  	sum = sum + ((p1[1,i] - p2[1,i])^2)
  }
  sqrt(sum)
}


## Seleccion de centroides de forma aleatoria ##
# A partir de la muestra utilizada para el K-MEANS obtiene
# los centroides de forma aleatoria a partir de los puntos
# que conforman la muestra.
elegir.centroides <- function(muestra,num.centroides){
  if(num.centroides > nrow(muestra)){
  	num.centroides <- nrow(muestra)
  } else if(num.centroides < 1){
  	num.centroides <- 1
  }
  
  centroides = matrix(nrow=0,ncol=ncol(muestra))
  
  # Obtenemos cada uno de los centroides. Estos centroides seran,
  # de forma aleatoria, algunos o todos los puntos de la muestra.
  for(i in 1:num.centroides){
    centroide = floor(runif(1,min=1,max=nrow(muestra)))
    centroides = rbind(centroides,muestra[centroide,])
    muestra = muestra[-centroide,]
  }
  colnames(centroides) <- c(paste0("d",1:ncol(centroides)))
  rownames(centroides) <- c(paste0("C",1:nrow(centroides)))
  as.data.frame(centroides)
}


## Creacion de la matriz de distancias ##
# Obtenemos en forma de matriz la distancia de cada uno de
# los puntos de la muestra a cada uno de los centroides definidos.
crear.matriz.distancias <- function(muestra,centroides){
  vd = c()
  md = matrix(nrow=0,ncol=nrow(muestra))
  for(i in 1:nrow(centroides)){
    # Obtenemos las coordenadas del centroide correspondiente.
    c = centroides[i,]
    for(j in 1:nrow(muestra)){
      # Obtenemos las coordenadas del punto correspondiente.
      m = muestra[j,]
      # Calculamos la distancia entre ambos y la añadimos al vector.
      vd = c(vd,distancia.euclidea(m,c))
    }
    md = rbind(md,vd)
    vd = c()
  }
  colnames(md) = c(paste0("P",1:nrow(muestra)))
  rownames(md) = c(paste0("C",1:nrow(centroides)))
  as.data.frame(md)
}



## Creacion de la matriz de asignacion ##
# Obtenemos en forma de matriz la pertenencia de cada punto
# de la muestra a cada cluster de tal forma que se indicara
# con un 1 aquellos puntos que pertenezcan y un 0 a aquellos
# que no.
crear.matriz.asignacion <- function(distancias){
  #Creamos la matriz de asignacion con valores a 0
  ma = matrix(0,nrow=nrow(distancias),ncol=ncol(distancias))
  rownames(ma) = c(paste0("C",1:nrow(distancias)))
  colnames(ma) = c(paste0("P",1:ncol(distancias)))

  for(i in 1:ncol(distancias)){
    # Obtiene el indice del menor valor del punto correspondiente.
    # Este punto sera asignado al cluster mas cercano.
    indice.menor = which.min(distancias[,i])

    #Por tanto el valor de ese puto para ese cluster sera 1: pertenece
    ma[indice.menor,i] = 1
  }

  # Una vez tenemos el vector con las asignaciones lo convertimos a dataframe.
  as.data.frame(ma)
}


## Creacion de los nuevos centroides ##
# Obtenemos los nuevos centroides a partir de los nuevos
# puntos obtenidos en la matriz de asignacion. Para ello
# calculamos el promedio de cada coordenada obteniendo asi
# las coordenadas de cada uno de los centroides.
crear.centroides <- function(muestra,distancias,asignacion){
  puntos = matrix(nrow=0,ncol=ncol(muestra))
  centroides = matrix(nrow=0,ncol=ncol(muestra))

  for(i in 1:nrow(asignacion)){
    for(j in 1:ncol(asignacion)){
      # Para los puntos que pertenecen a cada cluster,
      # obtenemos la coordenada x e y de todos ellos.
      if(asignacion[i,j] == 1){
        puntos = rbind(puntos,muestra[j,])
      }
    }
    # Obtenemos el nuevo centroide mediante la media de las
    # coordenadas.
    centroides = rbind(centroides,calcular.centroide(puntos))
    puntos = matrix(nrow=0,ncol=ncol(muestra))
  }

  # Una vez tenemos el vector con los centroides lo convertimos a dataframe.
  rownames(centroides) = c(paste0("C",1:nrow(centroides)))
  colnames(centroides) = c(paste0("d",1:ncol(centroides)))
  as.data.frame(centroides)
}


## Calculo de las coordenadas del centroide ##
# Se calculan las coordenadas del centroide a partir de la 
# media de los puntos de su cluster
calcular.centroide <- function(puntos){
  centroide = c()
  sum = 0

  # Calculo de la media de cada coordenada o dimension de los puntos
  for(i in 1:ncol(puntos)){
    for(j in 1:nrow(puntos)){
      sum = sum + puntos[j,i]
    }
    centroide = c(centroide,sum/nrow(puntos))
    sum = 0
  }
  centroide
}


## Condicion de parada ##
# Indica al algoritmo si debe parar atendiendo a la comparacion de
# la matriz de asignacion anterior y la nueva. Si no cambia
# (iguales=TRUE) significa que los centroides no se han movido en
# esta iteracion y podemos parar
comprobar.matriz.asignacion <- function(asignacion.antigua, asignacion.nueva){
  iguales = asignacion.antigua == asignacion.nueva
  iguales = all(iguales)
}


## Creacion del vector de clusterizacion ##
crear.vector.clusters <- function(asignacion){
  mc = matrix(nrow=1,ncol=ncol(asignacion))
  rownames(mc) = "C"
  colnames(mc) = c(paste0("P",1:ncol(asignacion)))

  for(i in 1:ncol(asignacion)){
    for(j in 1:nrow(asignacion)){
      if(asignacion[j,i]) mc[1,i] = j
    }
  }

  as.data.frame(mc)
}

## Grafica de los resultados (2D)##
# Muestra los puntos sobre el plano X-Y, asi como los centroides
# y los clusters obtenidos
plot2D.our.kmeans <- function(muestra,kmeans){
  # Nos aseguramos de que el imput sea 2D
  if(ncol(muestra)!=2) stop("Dimensiones erroneas. Asegurate de que las estructuras son de 2D")
  
  # Estructura aux con vector de clusterizacion, muestra y matriz de distancias
  aux = cbind(C=t(kmeans$v.clusteriz),muestra=muestra,dist=t(kmeans$m.distancias))

  # Calculo del rango del plot
  rango.x = range(aux$muestra.d1); rango.y = range(aux$muestra.d2)
  centroide.min = centroide.minimo(kmeans$centroides,rango.x,rango.y) # Etiqueta del centroide min
  centroide.max = centroide.maximo(kmeans$centroides,rango.x,rango.y) # Etiqueta del centroide max
  clusters = crear.clusters(aux,nrow(kmeans$centroides)) # Datos clusterizados

  rango.x = c(min(kmeans$centroides[centroide.min,][1]-max(clusters[[centroide.min]][paste0("dist.",centroide.min)]),rango.x[1]),
    max(kmeans$centroides[centroide.max,][1]+max(clusters[[centroide.max]][paste0("dist.",centroide.max)]),rango.x[2]))
  rango.y = c(min(kmeans$centroides[centroide.min,][2]-max(clusters[[centroide.min]][paste0("dist.",centroide.min)]),rango.y[1]),
    max(kmeans$centroides[centroide.max,][2]+max(clusters[[centroide.max]][paste0("dist.",centroide.max)]),rango.y[2]))

  # Plot centroides
  plot(kmeans$centroides,pch=22,col='blue',xlim=rango.x,ylim=rango.y)
  etiquetas = c("Centroides"); puntos = c(22); linea = c(0); color = c(4)

  for(i in 1:length(clusters)){
    # Plot puntos
    points(clusters[[paste0("C",i)]]$muestra.d1,clusters[[paste0("C",i)]]$muestra.d2,pch=19,col=i)
    etiquetas = c(etiquetas,paste0("Cluster ",i)); puntos = c(puntos,19); linea = c(linea,0); color = c(color,i)

    # Plot contorno actual del cluster
    symbols(kmeans$centroides[paste0("C",i),'d1'],kmeans$centroides[paste0("C",i),'d2'],
      circle=0.1+max(clusters[[paste0("C",i)]][paste0("dist.C",i)]),add=TRUE,inches=FALSE)
    etiquetas = c(etiquetas,paste0("Frontera ",i)); puntos = c(puntos,46); linea = c(linea,1); color = c(color,1)
  }

  # Titulo
  title(sprintf("Clusterizacion K-means con %i clusters",length(clusters)))
  # Leyenda
  legend(rango.x[1], rango.y[2], legend=etiquetas, lty=linea, pch = puntos, col = color, cex=0.8)
}


## Calcular el centroide minimo ##
# Calcula cual es el centroide con la distancia mas cercana al limite inferior del rango del plot
centroide.minimo <- function(centroides,rango.x,rango.y){
  distancias = c()
  for(i in 1:nrow(centroides)){
    distancias = c(distancias,distancia.euclidea(centroides[i,],matrix(c(rango.x[1],rango.y[1]),nrow=1)))
  }
  rownames(centroides[(order(distancias)[1]),])
}

## Calcular el centroide maximo ##
# Calcula cual es el centroide con la distancia mas cercana al limite superior del rango del plot
centroide.maximo <- function(centroides,rango.x,rango.y){
  distancias = c()
  for(i in 1:nrow(centroides)){
    distancias = c(distancias,distancia.euclidea(centroides[i,],matrix(c(rango.x[2],rango.y[2]),nrow=1)))
  }
  rownames(centroides[(order(distancias)[1]),])
}

## Crear clusters ##
# Dado un conjunto de datos con un vector de clusterizacion en una de las
# columnas, agrupa los datos en grupos en funcion de a que cluster pertenece
# cada punto y devulve una lista de dichos puntos
crear.clusters <- function(datos,num.clusters){
  grupos = list()
  for(i in 1:num.clusters){
    grupos[[paste0("C",i)]] = subset(datos,datos$C==i)
  }
  grupos
}
