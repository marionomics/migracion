# Factor de expansion

source("scripts/get_dataset.r")
library(survey)
library(srvyr)

install.packages('tidyverse')
library(tidyverse)

install.packages('srvyr')
library(srvyr)

head(usu)


design <- svydesign(ids = ~upm, 
                    strata =~est_dis ,
                    weights =~fac,
                    data = redes)

design <- redes %>%
  as_survey_design(ids=upm,
                   strata = est_dis,
                   weights=fac)

install.packages('dplyr')
library(dplyr)
design %>%
  filter(usainternet==1)%>%
  summarise(usainternet = survey_total(
    vartype = c('cv', 'ci'),
    level = 0.95
  ))