# Documentacion
# https://rviews.rstudio.com/2018/07/23/rest-apis-and-plumber/
# https://www.tylerclavelle.com/code/2017/randapis/


# Environment Variables
urlRoot_CESENS = "https://app.cesens.com/api/"
DEBUG = FALSE
savelogs_APICESENS = FALSE
#userCESENS = "your@user.com"
#passwCESENS = "yourPassword"

varsToCheck = c("urlRoot_CESENS", # url de la API
						 "userCESENS", "passwCESENS", # usuario y contraseña
						 "DEBUG", "savelogs_APICESENS") # variables de control
						
if(!all(sapply(varsToCheck, exists)))
{
	variablesQueNoExisten = varsToCheck[which(!sapply(varsToCheck, exists))]
	stop(paste("Variable/s:", paste(variablesQueNoExisten, collapse = ","), "no definida/"))
}


# Load packages
library(httr)
require(rjson)
library(lubridate) # we use function is.Date()
library(xts)




# Functions
autenticate_CESENS <-function(user, passw)
{
	url <- paste0(urlRoot_CESENS, "usuarios/login") # url to reach as per API instruction manual

	res <- POST(url = url, body = list(nombre = user, clave = passw), encode = "json") # headings added as per API instruction manual

	return(res)
}

# #Test
# #Wrong password test
# user = "inventedEmail@domain.org" 
# passw = "unvalidPassword"
# a <- autenticate_CESENS(user, passw)
# str(a)
# #Correct password test
# user = userCESENS
# passw = passwCESENS
# a <- autenticate_CESENS(user, passw)
# str(a)


get_token_CESENS <- function(user, passw)
{
	res <- autenticate_CESENS(user, passw)
	
	if(res$status_code == 200){
		res <- content(res) # httr at work here
		return(res$auth)
	}else{
		return(NULL)
	}
}

# #Test
# #Wrong password test
# user = "inventedEmail@domain.org" 
# passw = "unvalidPassword"
# get_token_CESENS(user, passw)

# #Prueba contraseña correcta
# user = userCESENS
# passw = passwCESENS
# tokenCESENS <- get_token_CESENS(user, passw)


get_locations_CESENS <- function(token, userLocations = TRUE, DEBUG = FALSE)
{
	url = paste0(urlRoot_CESENS, "ubicaciones/") # url to reach as per API instruction manual
	
	if(userLocations){# user can access both his/her own stations or publicWeatherStations
		url <- paste0(url, "cliente") # url to reach as per API instruction manual
	}else{
		url <- paste0(url, "publicas") # url to reach as per API instruction manual
	}

	resJSON <- GET(url, add_headers(Authentication = paste("Token", token))) # headings added as per API instruction manual
	
	if(resJSON$status_code == 200)
	{
		content = fromJSON(content(resJSON, type="text"))
		
		#ID = sapply(1:length(content(res)), function(i) content(res)[i][[1]]$id) # straight using httr
		res <- list(ID = sapply(1:length(content), function(i) content[[i]]$id),
					name = sapply(1:length(content), function(i) content[[i]]$nombre),
					userName =  sapply(1:length(content), function(i) content[[i]]$cliente$nombre)  ,
					longitude = sapply(1:length(content), function(i) content[[i]]$longitud),
					latitude = sapply(1:length(content), function(i) content[[i]]$latitud),
					altitude = sapply(1:length(content), function(i) content[[i]]$altitud),
					firstSample = (sapply(1:length(content), function(i) content[[i]]$primerDato)), #as.POSIXct , format =
					lastSample = (sapply(1:length(content), function(i) content[[i]]$ultimoDato)), #as.POSIXct
					crop = sapply(1:length(content), function(i) content[[i]]$cultivo), #[i][[1]"" locations$net[[2]]$variedades$cultivo[[1]] 
					cultivar = sapply(1:length(content(resJSON)), function(i) if(length(content[[i]]$variedades) > 0) content[[i]]$variedades[[1]]$nombre else NA), # !!!OJO podría haber más de un variedad # !!!OJO podría haber más de una variedad de acuerdo a la estructura de la API
					nodeID = sapply(1:length(content), function(i) content[[i]]$id),
					nodeSerialNum = sapply(1:length(content), function(i) ifelse(is.null(content[[i]]$nodo$numeroSerie), NA, content[[i]]$nodo$numeroSerie)),
					nodeType =  sapply(1:length(content), function(i) ifelse(is.null(content[[i]]$nodo$tipo), NA,content[[i]]$nodo$tipo)),
					nodeObjectType = sapply(1:length(content), function(i) ifelse(is.null(content[[i]]$nodo$objectType), NA,content[[i]]$nodo$objectType)),
					nodeVersion = sapply(1:length(content), function(i) ifelse(is.null(content[[i]]$nodo$version), NA,content[[i]]$nodo$version)),
					nodeNumConnectors = sapply(1:length(content), function(i) ifelse(is.null(length(content[[i]]$nodo$conectores)), NA, length(content[[i]]$nodo$conectores))),
					samplingTime =  sapply(1:length(content), function(i) ifelse(is.null(content[[i]]$nodo$frecuenciaLectura), NA,content[[i]]$nodo$frecuenciaLectura)),
					dataUpdateTime = sapply(1:length(content), function(i) ifelse(is.null(content[[i]]$nodo$frecuenciaEnvio), NA,content[[i]]$nodo$frecuenciaEnvio))
					)
	}else
	{
		res = NULL
	}
		
	return(list(fullAPIResponse = if(DEBUG) resJSON else NULL,
				JSONcontent = if(DEBUG) fromJSON(content(resJSON, type="text")) else NULL,
				res = res)
			)
}

# #Test 
# if(exists("tokenCESENS")){ # needs a token in order to work
# 	userLocations <- get_locations_CESENS(token = tokenCESENS)
	# str(userLocations, 1)
	# publicLocations <- get_locations_CESENS(token = tokenCESENS, userLocations = FALSE)
	# str(publicLocations, 1)
#}


get_stationName_CESENS <- function(id)
{
	if(!exists("df.locInfo")) stop("get_stationName_CESENS requires df.locInfo variable loaded")
	locNames <- character()
	for(i in id)
	{
		locNames <- c(locNames, if(length(which(df.locInfo$ID == i)) > 0)
								{
									df.locInfo$name[which(df.locInfo$ID == i)]
								}else
								{
									NA
								})
	}
	return(locNames)
}
# #Test
# if(exists("tokenCESENS")){ # needs a token in order to work
	# userLocations <- get_locations_CESENS(token = tokenCESENS)
	# get_stationName_CESENS(userLocations$ID)
#}

ifelse(length(which(df.locInfo$ID == id)) > 0, df.locInfo$name, NA)
										

# Launcher Code

if(savelogs_APICESENS) sink("LogFile_ApiCesens.log")


print("Creating token using user/passw")
tokenCESENS <- get_token_CESENS(userCESENS, passwCESENS)


if(is.null(tokenCESENS)){
	mensajeLog = "Error when gettting token. Confirm correct user/passw, check with autenticate_CESENS(user, passw) or get_token_CESENS(user, passw)"
	print(mensajeLog)
	break
}

if(DEBUG)
{
}

#####

print("Loading locations on memory")
userLocations <- get_locations_CESENS(token = tokenCESENS)

IDs = userLocations$res$ID
locNames = userLocations$res$name

df.locInfo <- as.data.frame(userLocations$res, stringsAsFactors = FALSE) #data.frame(IDs = locations$ID, nombre = locations$name, firsSample = locations$firstSample, lastSample = locations$lastSample, user = locations$userName)

availableLocations <- get_stationName_CESENS(df.locInfo$ID)
print(paste("Data.frame \"df.locInfo\" created. Available locations (df.locInfo$name): ", paste(availableLocations, collapse = ", "), "\n use function getStationName_CESENS(df.locInfo$ID)"))

setwd("./APICesens")
	source("APICesens.DataCollection.r")
	source("APICeses.DatosEnfermedades.r")
#	source("APICesens.FuncionesLecturaMetricas.r")
#	source("APICeses.DatosOidioyHumecfol.r")
setwd("./..")

if(savelogs_APICESENS) sink()


##################################

