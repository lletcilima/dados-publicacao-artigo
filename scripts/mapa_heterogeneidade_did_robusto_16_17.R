####
# Objetivo: Mapa com efeitos heterogeneos para regioes e estados do Brasil
# # Autor: Fca. Letícia F. Lima
# Data de criação: 05/05/23
# Última modificação: 07/06/2025
###

#### Pacotes ####
#install.packages("maps")
#install.packages("rgdal")
#install.packages("haven")
install.packages("sf")

library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(foreign)
library(haven)
library(maps)
library(rgdal)
library(rstatix)

gc(reset = TRUE)

####dados regiao ####

#regioes <- readxl::read_excel("G:/Meu Drive/dissertacao_leticia/graficos_e_tabelas/result_did/result_final_dissertacao/efeitos_heterogeneos.xlsx",
 #                            sheet = "regiao") %>% select(regiao:etapa, significancia)
regioes <- read.csv("dados/efeitos_heterogeneos - regiao.csv") %>%
  select(regiao:etapa, significancia)

regioes$regiao <- factor(regioes$regiao)
#regioes$significancia <- factor(regioes$significancia)
# Criar uma lista com as UFs de cada região
ufs <- list(
  c("AC", "AP", "AM", "PA", "RO", "RR", "TO"), # Norte
  c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE"), # Nordeste
  c("PR", "RS", "SC"), # Sul
  c("ES", "MG", "RJ", "SP"), # Sudeste
  c("DF", "GO", "MT", "MS") # Centro Oeste
)


#### ai_aprov_regiao####
regioes_ai_aprov <- regioes %>% filter(tx_rendimento== "aprov", etapa == "ai") %>%
  mutate(UF = ufs)
# Duplicar as linhas para cada UF
regioes_ai_aprov_duplicado <- regioes_ai_aprov %>%
  tidyr::unnest(UF)
# mapa por etapa - regioes ###
# Ler o arquivo shapefile com os limites geográficos dos municípios
limites_regioes <- st_read("dados/BR_UF_2022.shp")
# Juntar as informações de código do município e se o teste foi significativo ou não com os dados geográficos dos municípios
limites_regioes <- merge(limites_regioes, regioes_ai_aprov_duplicado, by.x = "SIGLA_UF", by.y = "UF", all.x = TRUE)

# Criar o mapa dos municípios
ggplot(data = limites_regioes) +
  geom_sf(aes(fill = factor(significancia)), color = "black")+
  scale_fill_manual(values = c("gray80", "#1170AA"),
                    name = "Significant",
                    labels = c("No", "Yes")) +
  labs(title = "Approval - Early Years", y = "", x = "") +
  theme_bw() +
  theme(legend.position = "right")

ggsave("resultados/figuras/mapa1_heterogeneidade.png", width = 4, height = 3, dpi = 300)

#### ai_reprov_regiao####
regioes_ai_reprov <- regioes %>% filter(tx_rendimento== "reprov", etapa == "ai")%>%
  mutate(UF = ufs)
# Duplicar as linhas para cada UF
regioes_ai_reprov_duplicado <- regioes_ai_reprov %>%
  tidyr::unnest(UF)
# mapa por etapa - regioes ###
# Ler o arquivo shapefile com os limites geográficos dos municípios
limites_regioes <- st_read("dados/BR_UF_2022.shp")
# Juntar as informações de código do município e se o teste foi significativo ou não com os dados geográficos dos municípios
limites_regioes <- merge(limites_regioes, regioes_ai_reprov_duplicado, by.x = "SIGLA_UF", by.y = "UF", all.x = TRUE)

# Criar o mapa dos municípios
ggplot(data = limites_regioes) +
  geom_sf(aes(fill = factor(significancia)), color = "black")+
  scale_fill_manual(values = c("gray80", "#1170AA"),
                    name = "Significant",
                    labels = c("No", "Yes")) +
  labs(title = "Failure - Early Years", y = "", x = "") +
  theme_bw() +
  theme(legend.position = "right")

ggsave("resultado/figuras/mapa2_heterogeneidade.png", width = 4, height = 3, dpi = 300)

#### ai_aband_regiao####
regioes_ai_aband <- regioes %>% filter(tx_rendimento== "aband", etapa == "ai")%>%
  mutate(UF = ufs)
# Duplicar as linhas para cada UF
regioes_ai_aband_duplicado <- regioes_ai_aband %>%
  tidyr::unnest(UF)
# mapa por etapa - regioes ###
# Ler o arquivo shapefile com os limites geográficos dos municípios
limites_regioes <- st_read("dados/BR_UF_2022.shp")
# Juntar as informações de código do município e se o teste foi significativo ou não com os dados geográficos dos municípios
limites_regioes <- merge(limites_regioes, regioes_ai_aband_duplicado, by.x = "SIGLA_UF", by.y = "UF", all.x = TRUE)

# Criar o mapa dos municípios
ggplot(data = limites_regioes) +
  geom_sf(aes(fill = factor(significancia)), color = "black")+
  scale_fill_manual(values = c("gray80", "#1170AA"),
                    name = "Significant",
                    labels = c("No", "Yes")) +
  labs(title = "Dropout - Early Years", y = "", x = "") +
  theme_bw() +
  theme(legend.position = "right")

ggsave("resultados/figuras/mapa3_heterogeneidade.png", width = 4, height = 3, dpi = 300)

#### af_aprov_regiao####
regioes_af_aprov <- regioes %>% filter(tx_rendimento== "aprov", etapa == "af")%>%
  mutate(UF = ufs)
# Duplicar as linhas para cada UF
regioes_af_aprov_duplicado <- regioes_af_aprov %>%
  tidyr::unnest(UF)
# mapa por etapa - regioes ###
# Ler o arquivo shapefile com os limites geográficos dos municípios
limites_regioes <- st_read("dados/BR_UF_2022.shp")
# Juntar as informações de código do município e se o teste foi significativo ou não com os dados geográficos dos municípios
limites_regioes <- merge(limites_regioes, regioes_af_aprov_duplicado, by.x = "SIGLA_UF", by.y = "UF", all.x = TRUE)

# Criar o mapa dos municípios
ggplot(data = limites_regioes) +
  geom_sf(aes(fill = factor(significancia)), color = "black")+
  scale_fill_manual(values = c("gray80", "#1170AA"),
                    name = "Significant",
                    labels = c("No", "Yes")) +
  labs(title = "Approval - Final Years", y = "", x = "") +
  theme_bw() +
  theme(legend.position = "right")

ggsave("resultado/figuras/mapa4_heterogeneidade.png", width = 4, height = 3, dpi = 300)

#### af_reprov_regiao####
regioes_af_reprov <- regioes %>% filter(tx_rendimento== "reprov", etapa == "af")%>%
  mutate(UF = ufs)
# Duplicar as linhas para cada UF
regioes_af_reprov_duplicado <- regioes_af_reprov %>%
  tidyr::unnest(UF)
# mapa por etapa - regioes ###
# Ler o arquivo shapefile com os limites geográficos dos municípios
limites_regioes <- st_read("dados/BR_UF_2022.shp")
# Juntar as informações de código do município e se o teste foi significativo ou não com os dados geográficos dos municípios
limites_regioes <- merge(limites_regioes, regioes_af_reprov_duplicado, by.x = "SIGLA_UF", by.y = "UF", all.x = TRUE)

# Criar o mapa dos municípios
ggplot(data = limites_regioes) +
  geom_sf(aes(fill = factor(significancia)), color = "black")+
  scale_fill_manual(values = c("gray80", "#1170AA"),
                    name = "Significant",
                    labels = c("No", "Yes")) +
  labs(title = "Failure - Final Years", y = "", x = "") +
  theme_bw() +
  theme(legend.position = "right")

ggsave("resultado/figuras/mapa5_heterogeneidade.png", width = 4, height = 3, dpi = 300)

#### af_aband_regiao####
regioes_af_aband <- regioes %>% filter(tx_rendimento== "aband", etapa == "af")%>%
  mutate(UF = ufs)
# Duplicar as linhas para cada UF
regioes_af_aband_duplicado <- regioes_ai_aband %>%
  tidyr::unnest(UF)
# mapa por etapa - regioes ###
# Ler o arquivo shapefile com os limites geográficos dos municípios
limites_regioes <- st_read("dados/BR_UF_2022.shp")
# Juntar as informações de código do município e se o teste foi significativo ou não com os dados geográficos dos municípios
limites_regioes <- merge(limites_regioes, regioes_af_aband_duplicado, by.x = "SIGLA_UF", by.y = "UF", all.x = TRUE)

# Criar o mapa dos municípios
ggplot(data = limites_regioes) +
  geom_sf(aes(fill = factor(significancia)), color = "black")+
  scale_fill_manual(values = c("gray80", "#1170AA"),
                    name = "Significant",
                    labels = c("No", "Yes")) +
  labs(title = "Dropout - Final Years", y = "", x = "") +
  theme_bw() +
  theme(legend.position = "right")

ggsave("resultado/figuras/mapa6_heterogeneidade.png", width = 4, height = 3, dpi = 300)

### em_aprov_regiao####
regioes_em_aprov <- regioes %>% filter(tx_rendimento== "aprov", etapa == "em")%>%
  mutate(UF = ufs)
# Duplicar as linhas para cada UF
regioes_em_aprov_duplicado <- regioes_em_aprov %>%
  tidyr::unnest(UF)
# mapa por etapa - regioes ###
# Ler o arquivo shapefile com os limites geográficos dos municípios
limites_regioes <- st_read("dados/BR_UF_2022.shp")
# Juntar as informações de código do município e se o teste foi significativo ou não com os dados geográficos dos municípios
limites_regioes <- merge(limites_regioes, regioes_em_aprov_duplicado, by.x = "SIGLA_UF", by.y = "UF", all.x = TRUE)

# Criar o mapa dos municípios
ggplot(data = limites_regioes) +
  geom_sf(aes(fill = factor(significancia)), color = "black")+
  scale_fill_manual(values = c("gray80", "#1170AA"),
                    name = "Significant",
                    labels = c("No", "Yes")) +
  labs(title = "Approval - High School", y = "", x = "") +
  theme_bw() +
  theme(legend.position = "right")

ggsave("resultado_figuras/mapa7_heterogeneidade.png", width = 4, height = 3, dpi = 300)

### em_reprov_regiao####
regioes_em_reprov <- regioes %>% filter(tx_rendimento== "reprov", etapa == "em")%>%
  mutate(UF = ufs)
# Duplicar as linhas para cada UF
regioes_em_reprov_duplicado <- regioes_em_reprov %>%
  tidyr::unnest(UF)
# mapa por etapa - regioes ###
# Ler o arquivo shapefile com os limites geográficos dos municípios
limites_regioes <- st_read("dados/BR_UF_2022.shp")
# Juntar as informações de código do município e se o teste foi significativo ou não com os dados geográficos dos municípios
limites_regioes <- merge(limites_regioes, regioes_em_reprov_duplicado, by.x = "SIGLA_UF", by.y = "UF", all.x = TRUE)

# Criar o mapa dos municípios
ggplot(data = limites_regioes) +
  geom_sf(aes(fill = factor(significancia)), color = "black")+
  scale_fill_manual(values = c("gray80", "#1170AA"),
                    name = "Significant",
                    labels = c("No", "Yes")) +
  labs(title = "Failure - High School", y = "", x = "") +
  theme_bw() +
  theme(legend.position = "right")

ggsave("resultado/figura/mapa8_heterogeneidade.png", width = 4, height = 3, dpi = 300)

### em_aband_regiao####
regioes_em_aband <- regioes %>% filter(tx_rendimento== "aband", etapa == "em")%>%
  mutate(UF = ufs)
# Duplicar as linhas para cada UF
regioes_em_aband_duplicado <- regioes_em_aband %>%
  tidyr::unnest(UF)
# mapa por etapa - regioes ###
# Ler o arquivo shapefile com os limites geográficos dos municípios
limites_regioes <- st_read("dados/BR_UF_2022.shp")
# Juntar as informações de código do município e se o teste foi significativo ou não com os dados geográficos dos municípios
limites_regioes <- merge(limites_regioes, regioes_em_aband_duplicado, by.x = "SIGLA_UF", by.y = "UF", all.x = TRUE)

# Criar o mapa dos municípios
ggplot(data = limites_regioes) +
  geom_sf(aes(fill = factor(significancia)), color = "black")+
  scale_fill_manual(values = c("gray80", "#1170AA"),
                    name = "Significant",
                    labels = c("No", "Yes")) +
  labs(title = "Dropout - High School", y = "", x = "") +
  theme_bw() +
  theme(legend.position = "right")

ggsave("resultado/figuras/mapa9_heterogeneidade.png", width = 4, height = 3, dpi = 300)



