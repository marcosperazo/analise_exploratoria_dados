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



fd <- function(x) {
  n <-length(x)
  return((2*IQR(x))/n^(1/3))
}

sr <- function(x) {
  n <-length(x)
  return((3.49*sd(x))/n^(1/3))
}

dados %>% dplyr::select(PPG) %>% ggplot(aes(x=PPG)) +
  geom_histogram(binwidth = fd) + 
  labs(title='Histograma de pontos por jogo',subtitle='Binarização pela Regra de Freedman-Diaconis' ,x='Pontos por jogo', y='Quantidade de jogadores')



dados %>% dplyr::select(PPG) %>% ggplot(aes(x=PPG)) +
  geom_histogram(bins = 20) + labs(title='Histograma de pontos por jogo', x='Pontos por jogo', y='Quantidade de jogadores')
  
dados %>% 
  ggplot(aes(x = Altura, y = APG)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title='Gráfico com a dispersão entre Altura e Assistência', x='Altura', y='Assistências')



