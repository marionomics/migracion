# Load ENIGH data
# En este script vamos a cargar los datos de la ENIGH de manera remota usando la

library(tidyverse)

# Tabla Población 
temp <- tempfile()
download.file('https://www.inegi.org.mx/contenidos/programas/enigh/nc/2018/microdatos/enigh2018_ns_poblacion_csv.zip', temp)
poblacion <- read.csv(unz(temp, 'poblacion.csv'))

# Gastos Hogar

url <- "https://www.inegi.org.mx/contenidos/programas/enigh/nc/2018/microdatos/enigh2018_ns_gastoshogar_csv.zip"
temp <- tempfile()
download.file(url =  url, temp)
gastoshogar <- read.csv(unz(temp, 'gastoshogar.csv'))

# Ingresos
url <- "https://www.inegi.org.mx/contenidos/programas/enigh/nc/2018/microdatos/enigh2018_ns_ingresos_csv.zip"
temp <- tempfile()
download.file(url =  url, temp)
ingresos <- read.csv(unz(temp, 'ingresos.csv'))

# Gastos a nivel persona
url <- "https://www.inegi.org.mx/contenidos/programas/enigh/nc/2018/microdatos/enigh2018_ns_gastospersona_csv.zip"
temp <- tempfile()
download.file(url =  url, temp)
gastospersona <- read.csv(unz(temp, 'gastospersona.csv'))

