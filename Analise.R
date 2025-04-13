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
install.packages('naniar')
install.packages('mice')

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
library(naniar)
library(mice)
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


library(dplyr)

# Supondo que o tibble se chama "dados"
linhas_com_na <- dados %>% filter(is.na(Peso))

# Para visualizar as linhas filtradas
print(linhas_com_na)

library(dplyr)

# Atualizando o tibble removendo as linhas onde Peso é NA
dados <- dados %>% filter(!is.na(Peso))

# Instalando o pacote ggpubr, se ainda não estiver instalado
install.packages("ggpubr")

library(ggpubr)

# Supondo que o tibble 'dados' contenha a variável 'Altura'
ggqqplot(dados$Altura, 
         title = "Gráfico Q-Q para Altura",
         xlab = "Quantis teóricos",
         ylab = "Quantis observados")






shapiro.test(dados$Peso)



dados %>% 
  ggplot(aes(x = Altura, y = Peso)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title='Gráfico com a dispersão entre Altura e Rebotes por jogo', x='Altura', y='Peso') +
  theme_classic2()





dados_preenchidos <- complete(imp)


names(imp$imp$Peso) <- paste("imp", names(imp$imp$Peso), sep = "_")
imp_Peso <- imp$imp$Peso %>% tidyr::pivot_longer(cols = imp_1:ncol(imp$imp$Peso), values_to = "Peso", names_to   = "imp") %>% arrange(imp)






dados <- as.data.frame(lapply(dados, function(x) {
  if (is.character(x)) {
    Encoding(x) <- "UTF-8"
  }
  return(x)
}))
