
# get_df.Datos_CESENS es un wrapper específico creado para las funciones de enfermedades
get_df.Datos_CESENS <- function(IDUbicacion, metrica, fechaInicio, fechaFin = NA, ajustarSegundos = TRUE, toUTC = TRUE)
{
	
	res <- get_df.Data_CESENS(token = tokenCESENS, locID = IDUbicacion, metricID = metrica, 
								startDate = as.POSIXct(fechaInicio), endDate = as.POSIXct(fechaFin),
								leapSeconds = ajustarSegundos, toUTC)
	names(res) <- c("fecha", "datos")
	return(res)
}

## buscaPrimer00 se usa en los cálculos de los datos horarios, para eliminar los registros que pueda haber al principio de la df.datosEnfermedades que no sean de las xx:00
buscaPrimer00 <- function(res){
	filasRes = nrow(res)
	
	repeat{
		if(nrow(res) == 0) break
		if(as.integer(strftime(res$fecha[1], format = "%M")) == 0) break
		res <- res[-1,]
	}
	res <- res[-1,] # Se elimina una fila más ya que este dato pertenece a la medida anterior
	print(paste("Se eliminan las", filasRes - nrow(res), "primeras filas."))
	return(res)
}

## buscaUltimor00 se usa en los cálculos de los datos horarios, para eliminar los registros que pueda haber al final de la df.datosEnfermedades que no sean de las xx:00
buscaUltimo00 <- function(res){
	filasRes = nrow(res)
	repeat{
		if(nrow(res) == 0) break
		if(as.integer(strftime(res$fecha[nrow(res)], format = "%M")) == 0) break
		res <- res[-nrow(res),]
	}
	print(paste("Se eliminan las", filasRes - nrow(res), "ultimas filas."))
	return(res)
}



# Proceso qeu para una ID y unas fechas genera una dt con los datos mínimos para ejecutar cálculos de enfermedades: temperaturas, humedadesRelativas, humectaciones, pluviometrías
get_CESENS_DatosEnfermedades <- function(IDUbicacion, fechaInicio, fechaFin = NA, horarios = TRUE, unidadesMetricaHumectacion = "porcentaje") #!!! Da por hecho que existe el objeto tokenCESENS en memoria
{
	metricaTemperatura = 1
	metricaHR = 6
	if(unidadesMetricaHumectacion == 'porcentaje')
	{
		metricaHumectacion = 2 # porcentaje
	}else
	{
		metricaHumectacion = 26 # tiempo de humectación
	}
	metricaLluvia = 11 # Lluvia

	#print(paste("Calculado desde: ", fechaInicio, "hasta fecha fin:", fechaFin))
	datosTemp <- get_df.Datos_CESENS(IDUbicacion, metricaTemperatura, fechaInicio, fechaFin)
	datosHR <- get_df.Datos_CESENS(IDUbicacion, metricaHR, fechaInicio, fechaFin)
	datosHumectacion <- get_df.Datos_CESENS(IDUbicacion, metricaHumectacion, fechaInicio, fechaFin)
	datosLluvia <- get_df.Datos_CESENS(IDUbicacion, metricaLluvia, fechaInicio, fechaFin)
	
	if(!is.na(datosTemp$fecha) && !is.na(datosHR$fecha))
	{
		res <- merge(datosTemp, datosHR, by = "fecha")
	# }else
	# {
		#!!! OJO QUE AQUÍ DEBERÍA HABER ALGO
	# }
		
		if(!is.na(datosHumectacion$fecha))
		{
			res <- merge(res, datosHumectacion, by = "fecha")
		}else
		{
			res$PHumectacion = NA
		}
		if(!is.na(datosLluvia$fecha))
		{
			res <- merge(res, datosLluvia, by = "fecha")
		}else
		{
			res$Lluvia = 0#NA
		}
	
		names(res) <- c("fecha", "TAirMd", "HRAirMd", "PHumectacion", "Lluvia")
		
		if(horarios != TRUE)
		{# Devolver los datos tal como vienen de CESENS
			res <- list(success = TRUE, datos = data.frame(fecha = fecha, TAirMd = temperatura, HRAirMd = HR, THumectacion = humectacion, PAcum = Lluvia))
		}else
		{# Devolver los datos en formato horario
			res <- buscaPrimer00(res)
			res <- buscaUltimo00(res)
		
			res$fecha <- res$fecha - 1
			ends <- endpoints(res$fecha, 'hours', 1)
			res$fecha <- res$fecha + 1
			fecha <- res$fecha[ends] 
			temperatura <- period.apply(res$TAirMd, INDEX = ends, FUN = mean)
			HR <- period.apply(res$HR, INDEX = ends, FUN = mean)
			Lluvia <- period.apply(res$Lluvia, INDEX = ends, FUN = sum)
			if(unidadesMetricaHumectacion == 'porcentaje')
			{
				calculaHumectacion <- function(fecha, ends, vectorHumectaciones, porcentajeHumectacionUmbral = 10)
				{					
					dataSamplingTime = difftime(last(fecha), first(fecha), units = "minutes")/length(fecha)
					res <- ifelse(vectorHumectaciones > porcentajeHumectacionUmbral, dataSamplingTime, 0)
                    return(res)
				}
			
				if(any(!is.na(res$PHumectacion)))
			    {
				    humectacion <- calculaHumectacion(res$fecha, ends, res$PHumectacion) #, porcentajeHumectacionUmbral = 10){
			    }else
			    {
				    humectacion <- 0
    			}
            humectacion <- calculaHumectacion(res$fecha, endpoints, res$PHumectacion)
			}else
			{
				humectacion <- period.apply(res$THumectacion, INDEX = ends, FUN = sum)
			}
				
			res <-  list(success = TRUE, 
                            data.frame(fecha = fecha, TAirMd = temperatura, HRAirMd = HR, THumectacion = humectacion, PAcum = Lluvia))
		}
	}else{
		print("Error, alguno de los datos devueltos es NA")
		res <- list(success = FALSE, 
                     data.frame(fecha = NA, TAirMd = NA, HRAirMd = NA, THumectacion= NA, PAcum = NA))
	}
	
	return(res)
}



