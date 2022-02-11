
get_data_CESENS <- function(token, locID, metricID, startDate, endDate = NA, DEBUG = FALSE)
{

	error = FALSE
	
	if(!(is.numeric(locID) && is.numeric(metricID) && (is.Date(startDate) || is.POSIXct(startDate)) && (is.na(endDate) || is.Date(endDate) || is.POSIXct(endDate)))){
		print("One or more of parameters are of wrong format")
		return(list(dat = NULL, net = NULL, error = TRUE))
	}
	
	if(is.na(endDate)){
		url = paste0(urlRoot_CESENS, "datos/", locID, "/", metricID, "/", strftime(startDate, format = "%Y%m%d"))
	}else{
		url = paste0(urlRoot_CESENS, "datos/", locID, "/", metricID, "/", strftime(startDate, format = "%Y%m%d"), "-", strftime(endDate, format = "%Y%m%d"))
	}
	
	res <- GET(url, add_headers(Authentication = paste("Token", token)))
	
	fullAPIResponse = if(DEBUG) res else NA
	
	if(res$status_code == 200){
		net = fromJSON(content(res, type="text"))
		if(length(net) == 0){
			print("Result for the provided lodID, start and end Dates contains no data")
			net = NULL
			error = TRUE
		}
	}else{
		print(paste0("problem when accessing: ", url, ", error occurs: ", res$status_code))
		net = NULL
		error = TRUE
		}
		
	return(list(fullAPIResponse = fullAPIResponse,
				net = net,
				error = error))
}



# La salida de CESENS de cada métrica da una objeto extraño, es una lista donde cada uno de los nombres es la dateStamp
# y cada uno de los elementos de la lista el valor del parámetro/métrica solicitado
# La función get_df.Data_CESENS organiza la información y la devuelve como data.frame
get_df.Data_CESENS <- function(token, locID, metricID, startDate, endDate = NA, leapSeconds = TRUE, toUTC = TRUE){
	#leapSeconds = TRUE, some API responses from CESENS happen to have dates such as: 2019-07-21 19:00:02, leapseconds Adjusts seconds value to 00
	# toUTC forces date to be expressed in UTC timezone
	
	res <- get_data_CESENS(token, locID, metricID, startDate, endDate)
	
	if(!res$error){
		dateStamp <- as.POSIXct(as.integer(names(res$net)), origin = "1970-01-01", tz = "CET")
		if(leapSeconds) dateStamp <- dateStamp - as.integer(strftime(dateStamp, "%S"))
		if(toUTC) attr(dateStamp, "tzone") <- "UTC"
	
		return(data.frame(dateStamp = dateStamp,
						values = unlist(res$net),
						row.names = 1:length(dateStamp)))
	}else{
		return(data.frame(dateStamp = NA,
						values = NA))
	}
}