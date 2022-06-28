# Libraries
library(ggplot2)
library(tidyverse)

# Load dataset
# Los datos estan en millones de dolares
remesas_dgo = read.csv("data/remesas_dgo.csv", encoding="UTF-8")
head(remesas_dgo)

unique(remesas_dgo$Fecha)
remesas_dgo %>%
  subset(Fecha %in% c("01/04/2020", "01/04/2021") &
           value > 1) %>%
  ggplot(aes(x=Municipio, y=log(value), fill=Fecha))+
  geom_bar(stat = "identity", position = "dodge")+
  coord_flip()

