# Left join
library(dplyr)


#remesas_mx and
#narco_index
head(narco_index[,-5])
head(remesas_mx)

#df <- remesas_mx[,-1] %>%
#  left_join(narco_index[,-5], by = c("Estado" = "Entidad", "Municipio" = "municipio"))



df <- narco_index[,-5] %>%
  left_join(remesas_mx[,-1], by = c("Entidad" = "Estado", "municipio" = "Municipio"))

unique(df$AREAM)

head(df)

#######################
# Filtrar

df <- df %>%
  subset(!is.na(value)) %>%
  subset(value != 0) %>%
  subset(Preocupa_Narco != 0)

library(lubridate)

df$date <- as.POSIXlt(as.character(df$Fecha), format = "%d/%m/%Y")
df$month <- month(df$date)  
df$year <- year(df$date)


df$lockdown <- (df$date > "2020-04-01")
max(df$date)
df %>%
  ggplot()