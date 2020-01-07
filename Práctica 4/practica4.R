# Funcion que lee las muestras contenidas en un fichero .txt
# Para ello le pasamos la ruta del fichero, la separacion entre
# los datos, la separacion entre las muestras (salto) y si tiene
# cabecera o no.
#
# Lectura del fichero muestra4.txt
# muestras <- muestra.leer("muestra4.txt")
#
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

compilar <- function(nombre,quiet=T,keepTrash=F,adobe=T){
	if(adobe){
		proceso <- system(paste("tasklist /FI \"IMAGENAME eq AcroRd32.exe\" /FI \"WINDOWTITLE eq ",nombre,".pdf\" /V /FO \"CSV\"",sep=""),intern=T)
	}else{
		proceso <- system(paste("tasklist /FI \"IMAGENAME eq chrome.exe\" /FI \"WINDOWTITLE eq ",nombre,".pdf - Google Chrome\" /V /FO \"CSV\"",sep=""),intern=T)
	}
	
	write(proceso,file="proceso.txt")
	tryCatch(
		{
			proceso <- read.csv("proceso.txt")
			system(paste("taskkill /PID ",proceso$PID,sep=""),ignore.stdout=T,ignore.stderr=T)
		}
	)
	file.remove("proceso.txt")
	
	Sweave(paste(nombre,".Rnw",sep=""),quiet=quiet)
	tools::texi2pdf(paste(nombre,".tex",sep=""))
	if(!keepTrash){
		file.remove(paste(nombre,".aux",sep=""))
		file.remove(paste(nombre,".log",sep=""))
		file.remove(paste(nombre,".out",sep=""))
		file.remove(paste(nombre,".toc",sep=""))
		file.remove(paste(nombre,"-concordance.tex",sep=""))
            filenames = list.files()
		file.remove(filenames[grep(paste(nombre,"-",sep=""),filenames)]); 
	}

	if(adobe){
		system(paste("\"C:\\Program Files (x86)\\Adobe\\Acrobat Reader DC\\Reader\\AcroRd32.exe\" ",nombre,".pdf",sep=""),ignore.stdout=T,wait=F)
	} else{
		system(paste("\"C:\\Program Files (x86)\\Google\\Chrome\\Application\\chrome.exe\" \"",getwd(),"\\",nombre,".pdf\"",sep=""),ignore.stdout=T,wait=F,show.output.on.console=F)
	}
	print("Todo hecho, majete :)")
}