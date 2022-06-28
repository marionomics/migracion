# Los municipios en el país en el que se recibieron más remesas en el país


# Libraries
library(ggplot2)
library(tidyverse)

# Load dataset
# Los datos estan en millones de dolares
remesas_mx = read.csv("data/remesas_mun_mx.csv", encoding="UTF-8")
head(remesas_mx)

unique(remesas_mx$Municipio)
