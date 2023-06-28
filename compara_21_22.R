# Análise dos dados do Censo 2022, divulgados em junho de 2023
# O Objetivo é ver as diferenças entre as estimativas das populações divulgadas em 2022 e os dados do Censo divulgados em 2023

# Importando os pacotes
library(readxl)
library(tidyverse)
library(stringr)

# Importando os bancos de dados
censo_22 <- read_excel("pop_muni_2022.xlsx")
estima_21 <- read_excel("POP2021_20221212.xls")

glimpse(estima_21)
glimpse(censo_22)

# Eliminar linhas inúteis

estima_21 <- estima_21[2:5573,]

# Selecionar e renomear colunas

estima_21 <- estima_21 %>%
  rename(uf = 1,
         cod_muni = 3,
         cidade_2021 = 4,
         pop_2021 = 5) %>%
  select(uf, cod_muni, cidade_2021, pop_2021)

# Limpeza e formatação na variável das populações

estima_21$pop_2021 <- gsub("\\([0-9]{1,2})", "", estima_21$pop_2021)
estima_21$pop_2021 <- gsub("\\.", "", estima_21$pop_2021)
estima_21$pop_2021 <- as.double(estima_21$pop_2021)

