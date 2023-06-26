# Análise dos dados do Censo 2020, divulgados em junho de 2023
# O Objetivo é ver como as populações das cidades evoluíram entre 2020 e 2020
# Apontar quantas cidades encolheram, em quais estados isso aconteceu mais e o top 5 destas cidades


# Importando os pacotes
library(readxl)
library(tidyverse)
library(stringr)

# Importando o banco de dados
censo_2010 <- read_excel("tabela136.xlsx")

# Informações básicas do banco de dados
dim(censo_2010)
glimpse(censo_2010)

# Eliminar linhas inúteis

censo_2010 <- censo_2010[5:5569,]

# Renomear colunas

censo_2010 <- censo_2010 %>%
  rename(cidade_uf = 1,
         pop_2010 = 2)

#Separar cidade do estado

censo_2010 <- censo_2010 %>%
  separate(cidade_uf,
           into = c("cidade", "uf"),
           sep = "\\s\\(") %>%
  mutate(uf = str_remove(uf, pattern = "\\)"))


