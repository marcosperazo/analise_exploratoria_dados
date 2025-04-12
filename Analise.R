install.packages("renv")
library(renv)
renv::init()

renv::snapshot()
renv::restore()

# Instalação de pacotes

install.packages('tidyverse')
install.packages('ggplot2')
install.packages('summarytools')
install.packages('dlookr')
install.packages('readxl')
install.packages('knitr')
install.packages('scales')
install.packages('dplyr')


library(tidyverse)
library(ggplot2)
library(summarytools)
library(dlookr)
library(readxl)
library(knitr)
library(scales)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(lubridate)

#
# Carregando a tabela bruta
dados_tratados <- read_csv("Dados_auxiliares/dados_apos_coluna_mes.csv", quote = "\"", locale = locale(encoding = "UTF-8"))

# Transformando peso, altura, ppg, rpg e apg em númerico
dados <- dados_tratados %>%
  mutate(
    PPG = as.numeric(PPG),
    RPG = as.numeric(RPG),
    APG = as.numeric(APG),
  )

dados <- dados %>% slice(-507)



dados <- dados %>%
  mutate(
    PPG = replace_na(PPG, 0),
    RPG = replace_na(RPG, 0),
    APG = replace_na(APG, 0)
  )



kable(head(dados))

dados %>% dplyr::select(Altura, PPG, APG, RPG) %>% summarytools::descr() %>% kable()


dados %>% dplyr::select(PPG, Altura) %>% ggplot(aes(x=PPG, y =Altura)) + geom_point() + geom_smooth(method = "lm", se = FALSE) + theme_classic()

pairs(dados)

dados_numericos <- dados[sapply(dados, is.numeric)]
pairs(dados_numericos)


dados %>% 
  ggplot(aes(x = Altura, y = RPG)) +
  geom_point() +
  geom_smooth(method = "lm")


dados %>% 
  ggplot(aes(x = Altura, y = PPG)) +
  geom_point() +
  geom_smooth(method = "lm")

dados %>% 
  ggplot(aes(x = APG, y = PPG)) +
  geom_point() +
  geom_smooth(method = "lm")

