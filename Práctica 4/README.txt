El fichero .Rnw utiliza un paquete de GitHub:

--> Utilizando R: descargar el fichero nycmaps.zip adjunto, situarlo en una carpeta C:/Users/tmp

--> Mendiante RStudio: sustituir el comando (líneas 278-283 del Rnw)
# Instalamos el paquete
if(!require(nycmaps)){
  install.packages("C:/tmp/nycmaps.zip")
  # Lo importamos
  library(nycmaps)
}

Por el siguiente comando
if(!require(nycmaps)){
  devtools::install_github("zachcp/nycmaps", force = TRUE)
  # Lo importamos
  library(nycmaps)
}