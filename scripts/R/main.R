narco_index
library(tidyverse)


pobreza <- read.csv("data/pobreza/indicadores_pobreza2020.csv")

pobreza <- pobreza[-1,c(1:4,7,10,13)]

names(pobreza) <- c("cve_ent", "Entidad", "cve_mun", "Municipio",
                    "Pob2020", "Por_pobreza", "Pers_Pobreza")

head(pobreza)


pobreza$cve_mun <- stringr::str_sub(paste0("0",pobreza$cve_mun),-5)

### Add poverty index to dataset
df <- narco_index %>%
  left_join(pobreza, by = c("Clave" = "cve_mun"))


df

### Add remittances
head(df)

remesas_mx$Fecha <- as.Date(remesas_mx$Fecha, 
        format = "%d/%m/%Y")
head(remesas_mx[,-1])

df2 <- remesas_mx[,-1] %>%
  subset(Fecha > "2019-01-01")
head(df2)

df2$Estado <- plyr::mapvalues(df2$Estado,
                c(as.character(unique(df2$Estado)[9]),
                  as.character(unique(df2$Estado)[11])),
                c(as.character(unique(df$Entidad)[1]),
                  as.character(unique(df$Entidad)[2])))
df3 <- df %>%
  left_join(df2, by = c("Entidad" = "Estado", "Municipio"))



head(df3)
#write.csv(df, "data/mix/df.csv")


df3

###################
# Filter

df4 <- df3 %>%
  filter(!is.na(value))


###############################################


df4$post_covid <- ifelse(df4$Fecha > "2020-03-01", 1, 0)
unique(df4$Fecha)

areasm <- c("CDMX", "Guadalajara", "Monterrey", "Puebla",
            "Leon", "La Laguna", "San Luis Potosi", "Merida","Chihuahua",
            "Tampico", "Veracruz", "Acapulco", "Aguascalientes", "Morelia", "Toluca",
            "Saltillo", "Villahermosa", "Tuxtla Gutiérrez", "Frontera Tijuana", "Culiacán",
            "Hermosillo", "Durango", "Tepic", "Campeche", "Cuernavaca", "Oaxaca",
            "Zacatecas", "Colima", "Querétaro", "Tlaxcala", "La Paz", "Cancún", "Pachuca")

df4$AREAMETRO <- plyr::mapvalues(df4$AREAM,
                from = stringr::str_sub(paste0("0",
                                               c(1:10,12:19,21,24:29,31:33,36,39:41,43)),
                                        -2),
                to = areasm)

df4$Pob2020 <- as.character(df4$Pob2020)
df4$Pob2020 <- as.numeric(gsub(",", "", df4$Pob2020))

df4 %>%
  subset(Fecha == "2019-04-01") %>%
  group_by(AREAM) %>%
  summarize(Remesas = sum(value),
            Poblacion = sum(Pob2020)) %>%
  ggplot(aes(x = AREAM, y = Remesas))+
  geom_bar(stat = "identity")+
  coord_flip()


length(unique(df4$CVE_MUN))
length(unique(df4$AREAM))
min(df4$Fecha)
max(df4$Fecha)
#### Regression??

head(df4)
df4$Por_pobreza <- as.numeric(as.character(df4$Por_pobreza))/100

model1 <- lm(value ~ Preocupa_Narco + Pob2020 +
               Por_pobreza  + factor(Fecha),
             data = df4)
summary(model1)

df4 %>%
  ggplot(aes(x = Preocupa_Narco,
             y = value,
             color = factor(post_covid)))+
  geom_point()

df4 %>%
  ggplot(aes(x = Por_pobreza,
             y = value, color = factor(post_covid)))+
  geom_point()

head(df4)


######### STARGAZER
model1 <- lm(value ~ Preocupa_Narco + post_covid,
             data = df4)
summary(model1)

model2 <- lm(value ~ Preocupa_Narco + Pob2020 +
               post_covid,
             data = df4)
summary(model2)

model3 <- lm(value ~ Preocupa_Narco + Pob2020 +
               Por_pobreza  + post_covid,
             data = df4)
summary(model3)

names(df4)

stargazer::stargazer(model1, model2, model3,
                     type = "text")


modelo_pobreza <- lm(Preocupa_Narco ~ Por_pobreza,
             data = df4)
summary(modelo_pobreza)

stargazer::stargazer(modelo_pobreza,
                     type = "text")


######### STARGAZER: Organize by dummy covid

df5 <- df4 %>%
  group_by(post_covid, AREAMETRO) %>%
  summarise(Narco = mean(Preocupa_Narco),
            Remesas = sum(value),
            Pobreza = mean(Por_pobreza),
            Poblacion = sum(Pob2020))


head(df5)


df5 %>%
  ggplot(aes(x = AREAMETRO, y = Remesas*1000000/Poblacion,
             fill = factor(post_covid)))+
  geom_bar(stat = "identity",
           position = "dodge")+
  labs(x = "Area Metropolitana",
       fill = "Periodo",
       y = "Remesas / Poblacion")+
  scale_fill_discrete(labels = c("Previo Marzo 2020",
                                 "Despues Marzo 2020"))+
  coord_flip()+
  theme_light()

head(df5)
df5 %>% reshape2::melt()

model1 <- lm(Remesas ~ Narco + post_covid,
             data = df5)

write.csv(df5, "tables/remesas.csv")
summary(model1)

model2 <- lm(value ~ Preocupa_Narco + Pob2020 +
               post_covid,
             data = df4)
summary(model2)

model3 <- lm(value ~ Preocupa_Narco + Pob2020 +
               Por_pobreza  + post_covid,
             data = df4)
summary(model3)


stargazer::stargazer(model1, model2, model3,
                     type = "latex")


###########################################
# Incluir datos de la ENIF
df6 <- df4
regiones <- c("Ciudad de México", "Centro Sur Y Oriente",
              "Occidente y Bajio", "Centro Sur Y Oriente",
              "Noroeste", "Noroeste",
              "Noreste", "Occidente y Bajio",
              "Sur", "Sur",
              "Noroeste", "Noroeste",
              "Occidente y Bajio", "Sur",
              "Centro Sur Y Oriente", "Sur",
              "Occidente y Bajio", "Occidente y Bajio",
              "Centro Sur Y Oriente", "Noroeste",
              "Sur", "Centro Sur Y Oriente")

df6$Region <- plyr::mapvalues(as.character(df4$Entidad),
                from = unique(as.character(df4$Entidad)),
                to = regiones)

# Porcentaje de población de 18 a 70 años con algún producto financiero por región
unique(df6$Region)
prod_fin <- c(.72, .6, .67, .82, .75, .68 )

df6$prod_fin <- plyr::mapvalues(df6$Region,
                              from = unique(df6$Region),
                              to = prod_fin)
df6$prod_fin <- as.numeric(df6$prod_fin)


df6 %>%
  group_by(Region) %>%
  summarise(`Tenencia de algun producto financiero` = mean(prod_fin))%>%
  ggplot(aes(y = `Tenencia de algun producto financiero`, x = Region))+
  geom_bar(stat = "identity")+
  theme_light()

### Regresiones

model1 <- lm(value ~ Preocupa_Narco + Pob2020,
             data = df6)
summary(model1)

model2 <- lm(value ~ Preocupa_Narco + Pob2020 +
               post_covid,
             data = df6)
summary(model2)

model3 <- lm(value ~ Preocupa_Narco + Pob2020 +
               Por_pobreza  + post_covid,
             data = df6)
summary(model3)

names(df6)

model4 <- lm(value ~ Preocupa_Narco + Pob2020 + post_covid+
               Por_pobreza   + Region,
             data = df6)
summary(model4)

stargazer::stargazer(model1, model2,
                     type = "latex")
stargazer::stargazer(model1, model2, model3, model4,
                     type = "text")

#### AIC

install.packages("AICcmodavg")

library(AICcmodavg)

models <- list(model1, model2, model3, model4)

aictab(cand.set = models)

#####################################
# Efecto de la pobreza unicamente

model1 <- lm(value ~  Preocupa_Narco  + post_covid,
             data = df4)
summary(model1)

model2 <- lm(value ~  Preocupa_Narco  + Pob2020 +
               post_covid , data = df4)
summary(model2)

model3 <- lm(value ~  Por_pobreza  + Pob2020 +
               post_covid , data = df4)
summary(model3)

stargazer::stargazer(model1, model2, model3, model4
                     type = "latex")
