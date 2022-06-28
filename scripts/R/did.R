library(tidyverse)
library(ggthemes)

# Start the main.R script up until creation of df4

head(df4)
summary(df4$Preocupa_Narco)

(mean(df4$Preocupa_Narco)+ sd(df4$Preocupa_Narco))

df4 %>%
  ggplot(aes(x = Preocupa_Narco))+
  geom_density()


df5 <- df4 %>%
  mutate(narco = ifelse(Preocupa_Narco > (mean(df4$Preocupa_Narco)+2*sd(df4$Preocupa_Narco)),1,0)) %>%
  group_by(Fecha, narco) %>%
  summarise(Remesas = mean(value))

  
head(df5)


df5 %>%
  ggplot(aes(x = Fecha, linetype = factor(narco)))+
  geom_line(aes(y = Remesas))+
  geom_vline(xintercept = as.Date("2020-04-01"), linetype="dotted")+
  #geom_vline(xintercept = as.Date("2020-07-01"), linetype="dotted")+
  #geom_vline(xintercept = as.Date("2021-01-01"), linetype="dotted")+
  scale_linetype_discrete(lab = c("Baja", "Alta"), name = "Presencia de \n narcotrafico")+
  theme_base()
  


######################################################
library("plm")


df6 <- df4 %>%
  mutate(post = ifelse(as.Date(Fecha) > "2020-03-01", 1, 0)) %>%
  mutate(narco = ifelse(Preocupa_Narco > 0.1989,1,0))


df_low <- df6 %>%
  subset(narco == 0)

df_high <- df6 %>%
  subset(narco == 1)

model_rd1 <- estimatr::lm_robust(value ~ Preocupa_Narco + post_covid + 
                                   as.numeric(Por_pobreza),
                    data = df_low,
                    weights = Pob2020, clusters = AREAMETRO)
summary(model_rd1)



model_rd2 <- estimatr::lm_robust(value ~ post_covid+ 
                                   as.numeric(Por_pobreza),
                                 data = df_low,
                                 weights = Pob2020, clusters = AREAMETRO)
model_rd3 <- estimatr::lm_robust(value ~ post_covid + 
                                   as.numeric(Por_pobreza),
                                 data = df_high,
                                 weights = Pob2020, clusters = AREAMETRO)
summary(model_rd2)
summary(model_rd3)

install.packages("estimatr")
?estimatr::lm_robust()
