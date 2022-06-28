# Explorando la ENVIPE

# Libraries
library(tidyverse)


# En este análisis sólo buscaremos el nivel de incidencia delictiva
# Del crimen organizado como un proxy de la presencia de grupos criminales en la ciudad

load("data/ENVIPE/bd_envipe_2021_RData/BD_ENVIPE_2021.RData")

# Tabla TVivienda: extraer Area Metropolitana de esta tabla
head(TVivienda)


# Tabla TMod_Vic: Características correspondientes a los delitos captados por el módulo
head(TMod_Vic)

# Podemos extraer AREAM CVE_ENT y CVE_MUN a la tabla TMod_Vic
rm(envipe)
envipe <- TPer_Vic1 %>%
  left_join(TPer_Vic2)
envipe <- envipe[!is.na(envipe$AREAM),]

envipe$cve_entmun <- gsub(" ", "",paste(envipe$CVE_ENT,envipe$CVE_MUN))

areasm <- c("CDMX", "Guadalajara", "Monterrey", "Puebla",
            "Leon", "La Laguna", "San Luis Potosi", "Merida","Chihuahua",
            "Tampico", "Veracruz", "Acapulco", "Aguascalientes", "Morelia", "Toluca",
            "Saltillo", "Villahermosa", "Tuxtla Gutiérrez", "Frontera Tijuana", "Culiacán",
            "Hermosillo", "Durango", "Tepic", "Campeche", "Cuernavaca", "Oaxaca",
            "Zacatecas", "Colima", "Querétaro", "Tlaxcala", "La Paz", "Cancún", "Pachuca")


envipe[envipe$AP4_3_3 != "9",] %>%
  group_by(AREAM) %>%
  summarize(Perc_seg_colonia = mean(2- as.numeric(AP4_3_3))) %>%
  ggplot(aes(x = AREAM, y = Perc_seg_colonia))+
  geom_bar(stat = "identity") +
  scale_x_discrete(labels = areasm)+
  labs(x = "Area Metropolitana", y = "Percepcion de Seguridad en su estado")+ 
  geom_hline(aes(yintercept = mean(Perc_seg_colonia)))+
  coord_flip()+
  theme_bw()





############
# ¿Sabe usted o ha escuchado si en los alrededores de su vivienda suceden o se dan las siguientes situaciones?
# 

envipe %>%
  group_by(AREAM) %>%
  summarize(Perc_seg_colonia = mean(as.numeric(AP4_5_10))) %>%
  ggplot(aes(x = AREAM, y = Perc_seg_colonia))+
  geom_bar(stat = "identity") +
  scale_x_discrete(labels = areasm)+
  labs(x = "Area Metropolitana", y = "Ha habido violencia policiaca contra ciudadanos")+ 
  geom_hline(aes(yintercept = mean(Perc_seg_colonia)))+
  coord_flip()+
  theme_bw()


# De los temas que le voy a mostrar, ¿cuáles son los tres que le preocupan más?

envipe %>%
  group_by(AREAM) %>%
  summarize(Preocupa_narco = mean(as.numeric(AP4_2_03))) %>%
  ggplot(aes(x = AREAM, y = Preocupa_narco))+
  geom_bar(stat = "identity") +
  scale_x_discrete(labels = areasm)+
  labs(x = "Area Metropolitana", y = "Les preocupa el Narcotráfico")+ 
  geom_hline(aes(yintercept = mean(Preocupa_narco)))+
  coord_flip()+
  theme_bw()


envipe$AREAM
municipios <- unique(envipe$CVE_MUN)
narco_index <- envipe %>%
  group_by(AREAM, CVE_ENT, CVE_MUN) %>%
  summarize(Preocupa_Narco = mean(as.numeric(AP4_2_03)))
narco_index$year <- 2021

unique(narco_index$CVE_ENT)
narco_index$ENT <- plyr::mapvalues(narco_index$CVE_ENT,
                from = c("05","09", "11", "13","14","15",  "19", "21",
                         "23","29",  
                         "10", "06", "20", "03", "22",
                         "02", "07", "17", "18", "08",
                         "24", "28", "04", "25", "26",
                         "31", "32", "01", "27", "16",
                         "12", "30"), 
                to= c("CDMX", "Guanajuato","Hidalgo", "Jalisco","Mexico", 
                      "Nuevo Leon", "Puebla", 
                      "Quintana Roo","Tlaxcala", 
                      "Coahuila", "Durango", "Colima",
                      "Oaxaca", "Baja California Sur", "Queretaro",
                      "Baja California", "Chiapas", "Morelos", "Nayarit",
                      "Chihuahua", "San Luis Potosi", "Tamaulipas",
                      "Campeche", "Sinaloa", "Sonora", "Yucatan",
                      "Zacatecas", "Aguascalientes", "Tabasco",
                      "Michoacan", "Guerrero", "Veracruz"
                      ))

source("scripts/R/load_claves.R")


narco_index[narco_index$CVE_ENT == unique(narco_index$CVE_ENT)[1],]


cves_ent %>%
  subset(clave_ent == narco_index$CVE_ENT[1]) %>%
  subset(cve_mun == narco_index$CVE_MUN[1]) %>%
  pull(mun)


head(narco_index)

# Insertar municipios
narco_index$municipio <- 0


for (municipio in 1:nrow(narco_index)) {
  m <- cves_ent %>%
    subset(clave_ent == narco_index$CVE_ENT[municipio]) %>%
    subset(cve_mun == narco_index$CVE_MUN[municipio]) %>%
    pull(mun)
  
  if(length(m) > 0){
    narco_index$municipio[municipio] <- m
  }else{
    print("Not today")
  }
}

########################### Hagamos lo mismo con las entidades para que aparezcan igual
narco_index$Entidad <- NA


for (estado in 1:nrow(narco_index)) {
  e <- cves_ent %>%
    subset(clave_ent == narco_index$CVE_ENT[estado]) %>%
    subset(cve_mun == narco_index$CVE_MUN[estado]) %>%
    pull(ent)
  
  if(length(e) > 0){
    narco_index$Entidad[estado] <- e
  }else{
    print("No encontré ningun registro para este estado")
  }
}

head(narco_index)


narco_index$Clave <- paste0(narco_index$CVE_ENT,narco_index$CVE_MUN)


