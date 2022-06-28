# join databases

# Libraries
library(tidyverse)


narco_index

head(remesas_mx)
head(narco_index[-5])

# Veamos la heterogeneidad de preocupación por el narco por estado

df <- narco_index[-5] %>%
  group_by(CVE_ENT) %>%
  summarize(Narco = mean(Preocupa_Narco)) %>%
  mutate(ent = c("Aguascalientes", "Baja California",
                 "Baja California Sur", "Campeche", "Coahuila",
                 "Colima", "Chiapas", "Chihuahua", "Ciudad de México", "Durango", 
                 "Guanajuato", "Guerrero", "Hidalgo", "Jalisco",
                 "México", "Michoacán", "Morelos", "Nayarit",
                 "Nuevo León", "Oaxaca", "Puebla", "Querétaro",
                 "Quintana Roo", "San Luis Potosí", "Sinaloa", "Sonora",
                 "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz",
                 "Yucatán", "Zacatecas")) #%>%
  #write.csv("data/narco_ent.csv")

df %>%
  ggplot(aes(x = ent, y = Narco))+
  geom_bar(stat = "identity")+
  geom_vline(xintercept = mean(df$Narco))+
  coord_flip()+
  theme_light()


# Necesitamos unir las bases de datos
remesas_mx %>%
  left_join(narco_index,
            by = c("Estado" = ""))