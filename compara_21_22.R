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

estima_21 <- estima_21[2:5571,]

# Selecionar e renomear colunas

estima_21 <- estima_21 %>%
  rename(uf = 1,
         cod_uf = 2,
         cod_muni = 3,
         cidade_2021 = 4,
         pop_2021 = 5)

# Criar coluna com códigos da UF + município juntos

estima_21 <- estima_21 %>% 
  mutate(cod_muni_novo = paste0(estima_21$cod_uf, estima_21$cod_muni)) %>%
  select(uf, cod_muni_novo, cidade_2021, pop_2021)

estima_21 <- estima_21 %>%
  rename(cod_muni = 2)

# Limpar e formatar variável das populações

estima_21$pop_2021 <- gsub("\\([0-9]{1,2})", "", estima_21$pop_2021)
estima_21$pop_2021 <- gsub("\\.", "", estima_21$pop_2021)
estima_21$pop_2021 <- as.double(estima_21$pop_2021)
estima_21$cod_muni <- as.double(estima_21$cod_muni)

# Fazer o merge das duas tabelas

censo_22_var <- left_join(censo_22, estima_21,
                          by = "cod_muni")

# Criar coluna com a variação entre as populações

censo_22_var <- censo_22_var %>%
  mutate(var_pop = pop_2022 - pop_2021) %>%
  mutate(var_pop_per = var_pop * 100 / pop_2021)

# Ver resumo estatístico da base de dados

censo_22_var %>%
  summarise(média = mean(var_pop_per),
            mediana = median(var_pop_per),
            desv_pad = sd(var_pop_per),
            mínimo = min(var_pop_per),
            máximo = max(var_pop_per))

# Calcular defasagem entre as bases

pop21 <- sum(censo_22_var$pop_2021)
pop22 <- sum(censo_22_var$pop_2022)

dif <- pop21 - pop22 #diferença absoluta entre estimativa e censo
(dif*100)/pop21 #diferença proporcional entre estimativa e censo

# Ver cidades em que a variação é maior que - 100 mil pessoas

grandes_difs <- censo_22_var %>% 
  filter(var_pop <= -100000)
view(grandes_difs)

var_gran <- sum(grandes_difs$var_pop) #soma das variações destas cidades

(var_gran*100)/dif #proporção destas variações diante da defasagem total

# Quantificar cidades que tiveram queda e tiveram alta

censo_22_var %>% count(var_pop < 0)
censo_22_var %>% count(var_pop > 0)

#Analisar grupos de cidades que estavam infladas ou não

muni_menos <- censo_22_var %>% filter(var_pop < 0) #cidades em que a estimativa estava maior que o censo
muni_mais <- censo_22_var %>% filter(var_pop > 0) #cidades em que a estimativa estava menor que o censo

muni_menos %>% summarise(média = mean(pop_2022),
                         mediana = median(pop_2022),
                         desv_pad = sd(pop_2022),
                         mínimo = min(pop_2022),
                         máximo = max(pop_2022))

muni_mais %>% summarise(média = mean(pop_2022),
                         mediana = median(pop_2022),
                         desv_pad = sd(pop_2022),
                         mínimo = min(pop_2022),
                         máximo = max(pop_2022))

# Ver cidades por UFs

muni_uf <- censo_22_var %>% count(uf) # quantidade de cidade por uf nacional

muni_menos_uf <- muni_menos %>% 
  count(uf)

muni_menos_uf <- left_join(muni_menos_uf, muni_uf,
                          by = "uf")

muni_menos_uf <- muni_menos_uf %>%
  rename(cidades_infladas = 2,
         cidades_total = 3) %>%
  mutate(cidades_infladas_perc = cidades_infladas * 100 / cidades_total)

muni_mais_uf <- muni_mais %>% 
  count(uf)
