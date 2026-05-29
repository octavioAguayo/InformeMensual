library(dplyr)

# Funciones y parametros globales ####
options(scipen = 999) 
if (!exists("Trim_Actual") || is.null(Trim_Actual)) {
  Trim_Actual <- "202602"
}

# Verifica si existe el archivo o si se debe actualizar
archivo_bbdd <- file.path("BBDD/", Trim_Actual, "BBDD_Estadisticas.RData")

if (!file.exists(archivo_bbdd)) {
  source("CreaBBDD_Coyuntural.R")
}

# Lee Tablas globales desde la BBDD ####

load(archivo_bbdd)
