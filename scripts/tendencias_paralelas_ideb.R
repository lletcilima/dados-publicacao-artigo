#####
# Objetivo: Tendencias Paralelas
# Variaveis: aprovação, abandono, reprovação, taxa de participação no saeb
# Autor: Fca. Letícia F. Lima
# Data de criação: 30/01/24
# Última modificação: 07/06/2025
####
rm(list = ls())
gc(reset = TRUE)

####pacotes####
library(plm)
library(ggplot2)
library(dplyr)
library(clusterSEs)
library(readxl)
library(haven)
library(tidyverse)
library(tidyr)
library(broom)
library(estimatr)
library(texreg)
library(stargazer)


####dados####
#grupo tratado e controle
#control_ai_ <- read.csv("dados/base_controle_tratado_ai_17.csv")
#control_ai <- read.csv("dados/base_controle_tratado_ai_17.csv") %>%
#select(cod_munic, cod_esc, trat)

#control_af_ <- read.csv("dados/base_controle_tratado_af_17.csv")
#control_af <- read.csv("dados/base_controle_tratado_af_17.csv") %>%
#select(cod_munic, cod_esc, trat)

#control_em_ <- read.csv("dados/base_controle_tratado_em_17.csv")
#control_em <- read.csv("dados/base_controle_tratado_em_17.csv") %>%
#select(cod_munic, cod_esc, trat)

####modelo considerando apenas os anos pares (que não possui IDEB), ####
#a fim de verificar se existe alguma diferença estatisticamente significante entre as escolas tratadas e controle
#2012####
#anos_iniciais_12 <- read_dta("dados/tx_rend_painel_2.dta") %>%
 # filter(localizacao == "Urbana" & rede != "Privada") %>% select(ano:nm_escola,tx_aprov_ai, tx_reprov_ai, tx_aband_ai, impar) %>%
  #filter(ano == 2012)

#anos_finais_12 <- read_dta("dados/tx_rend_painel_2.dta") %>%
 # filter(localizacao == "Urbana" & rede != "Privada") %>% select(ano:nm_escola,tx_aprov_af, tx_reprov_af, tx_aband_af, impar) %>%
  #filter(ano == 2012)

#ensino_medio_12 <- read_dta("dados/tx_rend_painel_2.dta") %>%
 # filter(localizacao == "Urbana" & rede != "Privada") %>% select(ano:nm_escola,tx_aprov_em, tx_reprov_em, tx_aband_em, impar) %>%
  #filter(ano == 2012)
####Definindo tratados e controle###
#ai_12_ <- left_join(control_ai, anos_iniciais_12, by = c("cod_munic", "cod_esc"))
#ai_12_$ano <- c(2012)

#af_12_ <- left_join(control_af, anos_finais_12, by = c("cod_munic", "cod_esc"))
#af_12_$ano <- c(2012)

#em_12_ <- left_join(control_em, ensino_medio_12, by = c("cod_munic", "cod_esc"))
#em_12_$ano <- c(2012)

#2014####
#anos_iniciais_14 <- read_dta("dados/tx_rend_painel_2.dta") %>%
 # filter(localizacao == "Urbana" & rede != "Privada") %>% select(ano:nm_escola,tx_aprov_ai, tx_reprov_ai, tx_aband_ai, impar) %>%
  #filter(ano == 2014)

#anos_finais_14 <- read_dta("dados/tx_rend_painel_2.dta") %>%
 # filter(localizacao == "Urbana" & rede != "Privada") %>% select(ano:nm_escola,tx_aprov_af, tx_reprov_af, tx_aband_af, impar) %>%
  #filter(ano == 2014)

#ensino_medio_14 <- read_dta("dados/tx_rend_painel_2.dta") %>%
 # filter(localizacao == "Urbana" & rede != "Privada") %>% select(ano:nm_escola,tx_aprov_em, tx_reprov_em, tx_aband_em, impar) %>%
  #filter(ano == 2014)
####Definindo tratados e controle###
#ai_14_ <- left_join(control_ai, anos_iniciais_14, by = c("cod_munic", "cod_esc"))
#ai_14_$ano <- c(2014)

#af_14_ <- left_join(control_af, anos_finais_14, by = c("cod_munic", "cod_esc"))
#af_14_$ano <- c(2014)

#em_14_ <- left_join(control_em, ensino_medio_14, by = c("cod_munic", "cod_esc"))
#em_14_$ano <- c(2014)

#2016####
#base por etapa
#anos_iniciais_16 <- read_dta("dados/tx_rend_painel_2.dta") %>%
 # filter(localizacao == "Urbana" & rede != "Privada") %>% select(ano:nm_escola,tx_aprov_ai, tx_reprov_ai, tx_aband_ai, impar) %>%
  #filter(ano == 2016)

#anos_finais_16 <- read_dta("dados/tx_rend_painel_2.dta") %>%
 # filter(localizacao == "Urbana" & rede != "Privada") %>% select(ano:nm_escola,tx_aprov_af, tx_reprov_af, tx_aband_af, impar) %>%
  #filter(ano == 2016)

#ensino_medio_16 <- read_dta("dados/tx_rend_painel_2.dta") %>%
 # filter(localizacao == "Urbana" & rede != "Privada") %>% select(ano:nm_escola,tx_aprov_em, tx_reprov_em, tx_aband_em, impar) %>%
  #filter(ano == 2016)
####Definindo tratados e controle###
#ai_16_ <- left_join(control_ai, anos_iniciais_16, by = c("cod_munic", "cod_esc"))
#ai_16_$ano <- c(2016)

#af_16_ <- left_join(control_af, anos_finais_16, by = c("cod_munic", "cod_esc"))
#af_16_$ano <- c(2016)

#em_16_ <- left_join(control_em, ensino_medio_16, by = c("cod_munic", "cod_esc"))
#em_16_$ano <- c(2016)

#anos pares empilhados - base final####
#ai <- rbind(ai_12_, ai_14_, ai_16_)
#names(ai)[14] <- "time"
#ai <- ai %>% select(cod_munic, cod_esc, ano:uf, rede, tx_aprov_ai:tx_aband_ai, trat, time)
#ai_17 <- control_ai_ %>% select(cod_munic, cod_esc, ano:tx_aband_ai, trat, time)
#ai <- rbind(ai, ai_17)

#af <- rbind(af_12_, af_14_, af_16_)
#names(af)[14] <- "time"
#af <- af %>% select(cod_munic, cod_esc, ano:uf, rede, tx_aprov_af:tx_aband_af, trat, time)
#af_17 <- control_af_ %>% select(cod_munic, cod_esc, ano:tx_aband_af, trat, time)
#af <- rbind(af, af_17)

#em <- rbind(em_12_, em_14_, em_16_)
#names(em)[14] <- "time"
#em <- em %>% select(cod_munic, cod_esc, ano:uf, rede, tx_aprov_em:tx_aband_em, trat, time)
#em_17 <- control_em_ %>% select(cod_munic, cod_esc, ano:tx_aband_em, trat, time)
#em <- rbind(em, em_17)

#write.csv2(ai, "dados/base_ai_tend_paralelas.csv")
#write.csv2(af, "dados/base_af_tend_paralelas.csv")
#write.csv2(em, "dados/base_em_tend_parelelas.csv")

#### dados finalizados#####
ai <- read.csv2("dados/base_ai_tend_paralelas.csv")
af <- read.csv2("dados/base_af_tend_paralelas.csv")
em <- read.csv2("dados/base_em_tend_paralelas.csv")

#anos iniciais####
#### teste t das tx de rendimento por regiao para cada etapa de ensino ###
anos <- unique(ai$ano)

# Criar uma lista para armazenar os resultados dos testes t
resultados_t <- list()

# Loop sobre os valores de anos que serão testados para aprov
for (i in anos) {

  # Filtrar os dados para as condições especificadas
  dados_filtrados <- subset(ai, ano == i)

  # Verificar se há valores NA nos dados filtrados
  if (any(is.na(dados_filtrados$tx_aprov_ai))) {
    # Se houver valores NA, podemos optar por remover essas observações
    dados_filtrados <- dados_filtrados[complete.cases(dados_filtrados$tx_aprov_ai), ]
  }

  # Realizar o teste t por grupo definido por trat
  t_teste <- t.test(dados_filtrados$tx_aprov_ai ~ dados_filtrados$trat)

  # Calcular o desvio padrão apenas se não houver valores NA
  if (!any(is.na(dados_filtrados$tx_aprov_ai))) {
    dp_tratados <- sd(dados_filtrados$tx_aprov_ai[dados_filtrados$trat == 1])
    dp_controle <- sd(dados_filtrados$tx_aprov_ai[dados_filtrados$trat == 0])
  } else {
    # Se houver valores NA, definimos o desvio padrão como NA também
    dp_tratados <- NA
    dp_controle <- NA
  }

  # Adicionar os resultados do teste à lista
  resultados_t[[as.character(i)]] <- data.frame("media_grupo_tratados" = t_teste$estimate[2],
                                                "media_grupo_controle" = t_teste$estimate[1],
                                                "dp_grupo_tratados" = dp_tratados,
                                                "dp_grupo_controle" = dp_controle,
                                                "diferenca_medias" = t_teste$estimate[2] - t_teste$estimate[1],
                                                "p_valor" = t_teste$p.value)
}

# Converter a lista em um data frame
resultados_t_aprov <- as.data.frame(do.call(rbind, resultados_t))
resultados_t_aprov$ano <- row.names(resultados_t_aprov)
resultados_t_aprov$significativo <- ifelse(resultados_t_aprov$p_valor <= 0.05, 1,0)

#base de dados pronta
#resultados_t_aprov <- read.csv2("G:/Meu Drive/dissertacao_leticia/graficos_e_tabelas/result_did/result_test_t_por_grupos/teste_t_aprov_ai_anos_pares.csv") %>%
 # filter(ano >= 2012)

# Vamos primeiro reformatar os dados para o formato longo
dados_longos <- pivot_longer(resultados_t_aprov, cols = c(media_grupo_tratados, media_grupo_controle), names_to = "grupo", values_to = "media")

# Crie o gráfico de linhas
ggplot(dados_longos, aes(x = ano, y = media, color = grupo, linetype = grupo, group = grupo)) +
  geom_line() +
  geom_point(size = 1) +
  labs(title = "Approval – Early Years",
       x = "", y = "",
       color = "Group",
       linetype = "Group") +
  scale_color_manual(values = c("media_grupo_controle" = "#E69F00",  # Laranja
                                "media_grupo_tratados" = "#0072B2"), # Azul escuro
                     labels = c("Control", "Treated")) +
  scale_linetype_manual(values = c("media_grupo_controle" = "dashed",
                                   "media_grupo_tratados" = "solid"),
                        labels = c("Control", "Treated")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid = element_blank())

ggsave("G:/Meu Drive/dissertacao_leticia/artigo_ideb_submissao_traduzida/tendencias_paralelas/grafico1.png", width = 4, height = 3, dpi = 300)

# Loop sobre os valores de anos que serão testados para reprov
for (i in anos) {

  # Filtrar os dados para as condições especificadas
  dados_filtrados <- subset(ai, ano == i)

  # Verificar se há valores NA nos dados filtrados
  if (any(is.na(dados_filtrados$tx_reprov_ai))) {
    # Se houver valores NA, podemos optar por remover essas observações
    dados_filtrados <- dados_filtrados[complete.cases(dados_filtrados$tx_reprov_ai), ]
  }

  # Realizar o teste t por grupo definido por trat
  t_teste <- t.test(dados_filtrados$tx_reprov_ai ~ dados_filtrados$trat)

  # Calcular o desvio padrão apenas se não houver valores NA
  if (!any(is.na(dados_filtrados$tx_reprov_ai))) {
    dp_tratados <- sd(dados_filtrados$tx_reprov_ai[dados_filtrados$trat == 1])
    dp_controle <- sd(dados_filtrados$tx_reprov_ai[dados_filtrados$trat == 0])
  } else {
    # Se houver valores NA, definimos o desvio padrão como NA também
    dp_tratados <- NA
    dp_controle <- NA
  }

  # Adicionar os resultados do teste à lista
  resultados_t[[as.character(i)]] <- data.frame("media_grupo_tratados" = t_teste$estimate[2],
                                                "media_grupo_controle" = t_teste$estimate[1],
                                                "dp_grupo_tratados" = dp_tratados,
                                                "dp_grupo_controle" = dp_controle,
                                                "diferenca_medias" = t_teste$estimate[2] - t_teste$estimate[1],
                                                "p_valor" = t_teste$p.value)
}

# Converter a lista em um data frame
resultados_t_reprov <- as.data.frame(do.call(rbind, resultados_t))
resultados_t_reprov$ano <- row.names(resultados_t_reprov)
resultados_t_reprov$significativo <- ifelse(resultados_t_reprov$p_valor <= 0.05, 1,0)

#base de dados pronta
#resultados_t_reprov <- read.csv2("G:/Meu Drive/dissertacao_leticia/graficos_e_tabelas/result_did/result_test_t_por_grupos/teste_t_reprov_ai_anos_pares.csv") %>%
 # filter(ano >= 2012)

# Vamos primeiro reformatar os dados para o formato longo
dados_longos <- pivot_longer(resultados_t_reprov, cols = c(media_grupo_tratados, media_grupo_controle), names_to = "grupo", values_to = "media")

# Crie o gráfico de linhas
ggplot(dados_longos, aes(x = ano, y = media, color = grupo, linetype = grupo, group = grupo)) +
  geom_line() +
  geom_point(size = 1) +
  labs(title = "Failure – Early Years",
       x = "", y = "",
       color = "Group",
       linetype = "Group") +
  scale_color_manual(values = c("media_grupo_controle" = "#E69F00",  # Laranja
                                "media_grupo_tratados" = "#0072B2"), # Azul escuro
                     labels = c("Control", "Treated")) +
  scale_linetype_manual(values = c("media_grupo_controle" = "dashed",
                                   "media_grupo_tratados" = "solid"),
                        labels = c("Control", "Treated")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid = element_blank())

ggsave("G:/Meu Drive/dissertacao_leticia/artigo_ideb_submissao_traduzida/tendencias_paralelas/grafico2.png", width = 4, height = 3, dpi = 300)


# Loop sobre os valores de anos que serão testados para aband
for (i in anos) {

  # Filtrar os dados para as condições especificadas
  dados_filtrados <- subset(ai, ano == i)

  # Verificar se há valores NA nos dados filtrados
  if (any(is.na(dados_filtrados$tx_aband_ai))) {
    # Se houver valores NA, podemos optar por remover essas observações
    dados_filtrados <- dados_filtrados[complete.cases(dados_filtrados$tx_aband_ai), ]
  }

  # Realizar o teste t por grupo definido por trat
  t_teste <- t.test(dados_filtrados$tx_aband_ai ~ dados_filtrados$trat)

  # Calcular o desvio padrão apenas se não houver valores NA
  if (!any(is.na(dados_filtrados$tx_aband_ai))) {
    dp_tratados <- sd(dados_filtrados$tx_aband_ai[dados_filtrados$trat == 1])
    dp_controle <- sd(dados_filtrados$tx_aband_ai[dados_filtrados$trat == 0])
  } else {
    # Se houver valores NA, definimos o desvio padrão como NA também
    dp_tratados <- NA
    dp_controle <- NA
  }

  # Adicionar os resultados do teste à lista
  resultados_t[[as.character(i)]] <- data.frame("media_grupo_tratados" = t_teste$estimate[2],
                                                "media_grupo_controle" = t_teste$estimate[1],
                                                "dp_grupo_tratados" = dp_tratados,
                                                "dp_grupo_controle" = dp_controle,
                                                "diferenca_medias" = t_teste$estimate[2] - t_teste$estimate[1],
                                                "p_valor" = t_teste$p.value)
}

# Converter a lista em um data frame
resultados_t_aband <- as.data.frame(do.call(rbind, resultados_t))
resultados_t_aband$ano <- row.names(resultados_t_aband)
resultados_t_aband$significativo <- ifelse(resultados_t_aband$p_valor <= 0.05, 1,0)

#base de dados pronta
#resultados_t_aband <- read.csv2("G:/Meu Drive/dissertacao_leticia/graficos_e_tabelas/result_did/result_test_t_por_grupos/teste_t_aband_ai_anos_pares.csv") %>%
 # filter(ano >= 2012)

# Vamos primeiro reformatar os dados para o formato longo
dados_longos <- pivot_longer(resultados_t_aband, cols = c(media_grupo_tratados, media_grupo_controle), names_to = "grupo", values_to = "media")

# Crie o gráfico de linhas
ggplot(dados_longos, aes(x = ano, y = media, color = grupo, linetype = grupo, group = grupo)) +
  geom_line() +
  geom_point(size = 1) +
  labs(title = "Dropout – Early Years",
       x = "", y = "",
       color = "Group",
       linetype = "Group") +
  scale_color_manual(values = c("media_grupo_controle" = "#E69F00",  # Laranja
                                "media_grupo_tratados" = "#0072B2"), # Azul escuro
                     labels = c("Control", "Treated")) +
  scale_linetype_manual(values = c("media_grupo_controle" = "dashed",
                                   "media_grupo_tratados" = "solid"),
                        labels = c("Control", "Treated")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid = element_blank())

ggsave("G:/Meu Drive/dissertacao_leticia/artigo_ideb_submissao_traduzida/tendencias_paralelas/grafico3.png", width = 4, height = 3, dpi = 300)


write.csv2(resultados_t_aprov, "G:/Meu Drive/dissertacao_leticia/graficos_e_tabelas/result_did/teste_t_aprov_ai_anos_pares.csv")
write.csv2(resultados_t_reprov, "G:/Meu Drive/dissertacao_leticia/graficos_e_tabelas/result_did/teste_t_reprov_ai_anos_pares.csv")
write.csv2(resultados_t_aband, "G:/Meu Drive/dissertacao_leticia/graficos_e_tabelas/result_did/teste_t_aband_ai_anos_pares.csv")

#anos finais####
#### teste t das tx de rendimento por regiao para cada etapa de ensino ###
anos <- unique(af$ano)

# Criar uma lista para armazenar os resultados dos testes t
resultados_t <- list()

# Loop sobre os valores de anos que serão testados para aprov
for (i in anos) {

  # Filtrar os dados para as condições especificadas
  dados_filtrados <- subset(af, ano == i)

  # Verificar se há valores NA nos dados filtrados
  if (any(is.na(dados_filtrados$tx_aprov_af))) {
    # Se houver valores NA, podemos optar por remover essas observações
    dados_filtrados <- dados_filtrados[complete.cases(dados_filtrados$tx_aprov_af), ]
  }

  # Realizar o teste t por grupo definido por trat
  t_teste <- t.test(dados_filtrados$tx_aprov_af ~ dados_filtrados$trat)

  # Calcular o desvio padrão apenas se não houver valores NA
  if (!any(is.na(dados_filtrados$tx_aprov_af))) {
    dp_tratados <- sd(dados_filtrados$tx_aprov_af[dados_filtrados$trat == 1])
    dp_controle <- sd(dados_filtrados$tx_aprov_af[dados_filtrados$trat == 0])
  } else {
    # Se houver valores NA, definimos o desvio padrão como NA também
    dp_tratados <- NA
    dp_controle <- NA
  }

  # Adicionar os resultados do teste à lista
  resultados_t[[as.character(i)]] <- data.frame("media_grupo_tratados" = t_teste$estimate[2],
                                                "media_grupo_controle" = t_teste$estimate[1],
                                                "dp_grupo_tratados" = dp_tratados,
                                                "dp_grupo_controle" = dp_controle,
                                                "diferenca_medias" = t_teste$estimate[2] - t_teste$estimate[1],
                                                "p_valor" = t_teste$p.value)
}

# Converter a lista em um data frame
resultados_t_aprov <- as.data.frame(do.call(rbind, resultados_t))
resultados_t_aprov$ano <- row.names(resultados_t_aprov)
resultados_t_aprov$significativo <- ifelse(resultados_t_aprov$p_valor <= 0.05, 1,0)

#base de dados pronta
#resultados_t_aprov <- read.csv2("G:/Meu Drive/dissertacao_leticia/graficos_e_tabelas/result_did/result_test_t_por_grupos/teste_t_aprov_af_anos_pares.csv") %>%
 # filter(ano >= 2012)


# Vamos primeiro reformatar os dados para o formato longo
dados_longos <- pivot_longer(resultados_t_aprov, cols = c(media_grupo_tratados, media_grupo_controle), names_to = "grupo", values_to = "media")

# Crie o gráfico de linhas
ggplot(dados_longos, aes(x = ano, y = media, color = grupo, linetype = grupo, group = grupo)) +
  geom_line() +
  geom_point(size = 1) +
  labs(title = "Approval – Final Years",
       x = "", y = "",
       color = "Group",
       linetype = "Group") +
  scale_color_manual(values = c("media_grupo_controle" = "#E69F00",  # Laranja
                                "media_grupo_tratados" = "#0072B2"), # Azul escuro
                     labels = c("Control", "Treated")) +
  scale_linetype_manual(values = c("media_grupo_controle" = "dashed",
                                   "media_grupo_tratados" = "solid"),
                        labels = c("Control", "Treated")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid = element_blank())

ggsave("G:/Meu Drive/dissertacao_leticia/artigo_ideb_submissao_traduzida/tendencias_paralelas/grafico4.png", width = 4, height = 3, dpi = 300)


# Loop sobre os valores de anos que serão testados para reprov
for (i in anos) {

  # Filtrar os dados para as condições especificadas
  dados_filtrados <- subset(af, ano == i)

  # Verificar se há valores NA nos dados filtrados
  if (any(is.na(dados_filtrados$tx_reprov_af))) {
    # Se houver valores NA, podemos optar por remover essas observações
    dados_filtrados <- dados_filtrados[complete.cases(dados_filtrados$tx_reprov_af), ]
  }

  # Realizar o teste t por grupo definido por trat
  t_teste <- t.test(dados_filtrados$tx_reprov_af ~ dados_filtrados$trat)

  # Calcular o desvio padrão apenas se não houver valores NA
  if (!any(is.na(dados_filtrados$tx_reprov_af))) {
    dp_tratados <- sd(dados_filtrados$tx_reprov_af[dados_filtrados$trat == 1])
    dp_controle <- sd(dados_filtrados$tx_reprov_af[dados_filtrados$trat == 0])
  } else {
    # Se houver valores NA, definimos o desvio padrão como NA também
    dp_tratados <- NA
    dp_controle <- NA
  }

  # Adicionar os resultados do teste à lista
  resultados_t[[as.character(i)]] <- data.frame("media_grupo_tratados" = t_teste$estimate[2],
                                                "media_grupo_controle" = t_teste$estimate[1],
                                                "dp_grupo_tratados" = dp_tratados,
                                                "dp_grupo_controle" = dp_controle,
                                                "diferenca_medias" = t_teste$estimate[2] - t_teste$estimate[1],
                                                "p_valor" = t_teste$p.value)
}

# Converter a lista em um data frame
resultados_t_reprov <- as.data.frame(do.call(rbind, resultados_t))
resultados_t_reprov$ano <- row.names(resultados_t_reprov)
resultados_t_reprov$significativo <- ifelse(resultados_t_reprov$p_valor <= 0.05, 1,0)

#base de dados pronta
#resultados_t_reprov <- read.csv2("G:/Meu Drive/dissertacao_leticia/graficos_e_tabelas/result_did/result_test_t_por_grupos/teste_t_reprov_af_anos_pares.csv") %>%
 # filter(ano >= 2012)

# Vamos primeiro reformatar os dados para o formato longo
dados_longos <- pivot_longer(resultados_t_reprov, cols = c(media_grupo_tratados, media_grupo_controle), names_to = "grupo", values_to = "media")

# Crie o gráfico de linhas
ggplot(dados_longos, aes(x = ano, y = media, color = grupo, linetype = grupo, group = grupo)) +
  geom_line() +
  geom_point(size = 1) +
  labs(title = "Failure – Final Years",
       x = "", y = "",
       color = "Group",
       linetype = "Group") +
  scale_color_manual(values = c("media_grupo_controle" = "#E69F00",  # Laranja
                                "media_grupo_tratados" = "#0072B2"), # Azul escuro
                     labels = c("Control", "Treated")) +
  scale_linetype_manual(values = c("media_grupo_controle" = "dashed",
                                   "media_grupo_tratados" = "solid"),
                        labels = c("Control", "Treated")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid = element_blank())

ggsave("G:/Meu Drive/dissertacao_leticia/artigo_ideb_submissao_traduzida/tendencias_paralelas/grafico5.png", width = 4, height = 3, dpi = 300)


# Loop sobre os valores de anos que serão testados para aband
for (i in anos) {

  # Filtrar os dados para as condições especificadas
  dados_filtrados <- subset(af, ano == i)

  # Verificar se há valores NA nos dados filtrados
  if (any(is.na(dados_filtrados$tx_aband_af))) {
    # Se houver valores NA, podemos optar por remover essas observações
    dados_filtrados <- dados_filtrados[complete.cases(dados_filtrados$tx_aband_af), ]
  }

  # Realizar o teste t por grupo definido por trat
  t_teste <- t.test(dados_filtrados$tx_aband_af ~ dados_filtrados$trat)

  # Calcular o desvio padrão apenas se não houver valores NA
  if (!any(is.na(dados_filtrados$tx_aband_af))) {
    dp_tratados <- sd(dados_filtrados$tx_aband_af[dados_filtrados$trat == 1])
    dp_controle <- sd(dados_filtrados$tx_aband_af[dados_filtrados$trat == 0])
  } else {
    # Se houver valores NA, definimos o desvio padrão como NA também
    dp_tratados <- NA
    dp_controle <- NA
  }

  # Adicionar os resultados do teste à lista
  resultados_t[[as.character(i)]] <- data.frame("media_grupo_tratados" = t_teste$estimate[2],
                                                "media_grupo_controle" = t_teste$estimate[1],
                                                "dp_grupo_tratados" = dp_tratados,
                                                "dp_grupo_controle" = dp_controle,
                                                "diferenca_medias" = t_teste$estimate[2] - t_teste$estimate[1],
                                                "p_valor" = t_teste$p.value)
}

# Converter a lista em um data frame
resultados_t_aband <- as.data.frame(do.call(rbind, resultados_t))
resultados_t_aband$ano <- row.names(resultados_t_aband)
resultados_t_aband$significativo <- ifelse(resultados_t_aband$p_valor <= 0.05, 1,0)

#base de dados pronta
#resultados_t_aband <- read.csv2("G:/Meu Drive/dissertacao_leticia/graficos_e_tabelas/result_did/result_test_t_por_grupos/teste_t_aband_af_anos_pares.csv") %>%
 # filter(ano >= 2012)

# Vamos primeiro reformatar os dados para o formato longo
dados_longos <- pivot_longer(resultados_t_aband, cols = c(media_grupo_tratados, media_grupo_controle), names_to = "grupo", values_to = "media")

# Crie o gráfico de linhas
ggplot(dados_longos, aes(x = ano, y = media, color = grupo, linetype = grupo, group = grupo)) +
  geom_line() +
  geom_point(size = 1) +
  labs(title = "Dropout – Final Years",
       x = "", y = "",
       color = "Group",
       linetype = "Group") +
  scale_color_manual(values = c("media_grupo_controle" = "#E69F00",  # Laranja
                                "media_grupo_tratados" = "#0072B2"), # Azul escuro
                     labels = c("Control", "Treated")) +
  scale_linetype_manual(values = c("media_grupo_controle" = "dashed",
                                   "media_grupo_tratados" = "solid"),
                        labels = c("Control", "Treated")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid = element_blank())

ggsave("G:/Meu Drive/dissertacao_leticia/artigo_ideb_submissao_traduzida/tendencias_paralelas/grafico6.png", width = 4, height = 3, dpi = 300)




write.csv2(resultados_t_aprov, "G:/Meu Drive/dissertacao_leticia/graficos_e_tabelas/result_did/teste_t_aprov_af_anos_pares.csv")
write.csv2(resultados_t_reprov, "G:/Meu Drive/dissertacao_leticia/graficos_e_tabelas/result_did/teste_t_reprov_af_anos_pares.csv")
write.csv2(resultados_t_aband, "G:/Meu Drive/dissertacao_leticia/graficos_e_tabelas/result_did/teste_t_aband_af_anos_pares.csv")

#ensino medio####
#### teste t das tx de rendimento por ano para cada etapa de ensino ###
anos <- unique(em$ano)

# Criar uma lista para armazenar os resultados dos testes t
resultados_t <- list()

# Loop sobre os valores de anos que serão testados para aprov
for (i in anos) {

  # Filtrar os dados para as condições especificadas
  dados_filtrados <- subset(em, ano == i)

  # Verificar se há valores NA nos dados filtrados
  if (any(is.na(dados_filtrados$tx_aprov_em))) {
    # Se houver valores NA, podemos optar por remover essas observações
    dados_filtrados <- dados_filtrados[complete.cases(dados_filtrados$tx_aprov_em), ]
  }

  # Realizar o teste t por grupo definido por trat
  t_teste <- t.test(dados_filtrados$tx_aprov_em ~ dados_filtrados$trat)

  # Calcular o desvio padrão apenas se não houver valores NA
  if (!any(is.na(dados_filtrados$tx_aprov_em))) {
    dp_tratados <- sd(dados_filtrados$tx_aprov_em[dados_filtrados$trat == 1])
    dp_controle <- sd(dados_filtrados$tx_aprov_em[dados_filtrados$trat == 0])
  } else {
    # Se houver valores NA, definimos o desvio padrão como NA também
    dp_tratados <- NA
    dp_controle <- NA
  }

  # Adicionar os resultados do teste à lista
  resultados_t[[as.character(i)]] <- data.frame("media_grupo_tratados" = t_teste$estimate[2],
                                                "media_grupo_controle" = t_teste$estimate[1],
                                                "dp_grupo_tratados" = dp_tratados,
                                                "dp_grupo_controle" = dp_controle,
                                                "diferenca_medias" = t_teste$estimate[2] - t_teste$estimate[1],
                                                "p_valor" = t_teste$p.value)
}

# Converter a lista em um data frame
resultados_t_aprov <- as.data.frame(do.call(rbind, resultados_t))
resultados_t_aprov$ano <- row.names(resultados_t_aprov)
resultados_t_aprov$significativo <- ifelse(resultados_t_aprov$p_valor <= 0.05, 1,0)

#base de dados pronta
#resultados_t_aprov <- read.csv2("G:/Meu Drive/dissertacao_leticia/graficos_e_tabelas/result_did/result_test_t_por_grupos/teste_t_aprov_em_anos_pares.csv") %>%
 # filter(ano >= 2012)

# Vamos primeiro reformatar os dados para o formato longo
dados_longos <- pivot_longer(resultados_t_aprov, cols = c(media_grupo_tratados, media_grupo_controle), names_to = "grupo", values_to = "media")

# Crie o gráfico de linhas
ggplot(dados_longos, aes(x = ano, y = media, color = grupo, linetype = grupo, group = grupo)) +
  geom_line() +
  geom_point(size = 1) +
  labs(title = "Approval – High School",
       x = "", y = "",
       color = "Group",
       linetype = "Group") +
  scale_color_manual(values = c("media_grupo_controle" = "#E69F00",  # Laranja
                                "media_grupo_tratados" = "#0072B2"), # Azul escuro
                     labels = c("Control", "Treated")) +
  scale_linetype_manual(values = c("media_grupo_controle" = "dashed",
                                   "media_grupo_tratados" = "solid"),
                        labels = c("Control", "Treated")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid = element_blank())

ggsave("G:/Meu Drive/dissertacao_leticia/artigo_ideb_submissao_traduzida/tendencias_paralelas/grafico7.png", width = 4, height = 3, dpi = 300)




# Loop sobre os valores de anos que serão testados para reprov
for (i in anos) {

  # Filtrar os dados para as condições especificadas
  dados_filtrados <- subset(em, ano == i)

  # Verificar se há valores NA nos dados filtrados
  if (any(is.na(dados_filtrados$tx_reprov_em))) {
    # Se houver valores NA, podemos optar por remover essas observações
    dados_filtrados <- dados_filtrados[complete.cases(dados_filtrados$tx_reprov_em), ]
  }

  # Realizar o teste t por grupo definido por trat
  t_teste <- t.test(dados_filtrados$tx_reprov_em ~ dados_filtrados$trat)

  # Calcular o desvio padrão apenas se não houver valores NA
  if (!any(is.na(dados_filtrados$tx_reprov_em))) {
    dp_tratados <- sd(dados_filtrados$tx_reprov_em[dados_filtrados$trat == 1])
    dp_controle <- sd(dados_filtrados$tx_reprov_em[dados_filtrados$trat == 0])
  } else {
    # Se houver valores NA, definimos o desvio padrão como NA também
    dp_tratados <- NA
    dp_controle <- NA
  }

  # Adicionar os resultados do teste à lista
  resultados_t[[as.character(i)]] <- data.frame("media_grupo_tratados" = t_teste$estimate[2],
                                                "media_grupo_controle" = t_teste$estimate[1],
                                                "dp_grupo_tratados" = dp_tratados,
                                                "dp_grupo_controle" = dp_controle,
                                                "diferenca_medias" = t_teste$estimate[2] - t_teste$estimate[1],
                                                "p_valor" = t_teste$p.value)
}

# Converter a lista em um data frame
resultados_t_reprov <- as.data.frame(do.call(rbind, resultados_t))
resultados_t_reprov$ano <- row.names(resultados_t_reprov)
resultados_t_reprov$significativo <- ifelse(resultados_t_reprov$p_valor <= 0.05, 1,0)

#base de dados pronta
#resultados_t_reprov <- read.csv2("G:/Meu Drive/dissertacao_leticia/graficos_e_tabelas/result_did/result_test_t_por_grupos/teste_t_reprov_em_anos_pares.csv") %>%
 # filter(ano >= 2012)

# Vamos primeiro reformatar os dados para o formato longo
dados_longos <- pivot_longer(resultados_t_reprov, cols = c(media_grupo_tratados, media_grupo_controle), names_to = "grupo", values_to = "media")

# Crie o gráfico de linhas
ggplot(dados_longos, aes(x = ano, y = media, color = grupo, linetype = grupo, group = grupo)) +
  geom_line() +
  geom_point(size = 1) +
  labs(title = "Failure – High School",
       x = "", y = "",
       color = "Group",
       linetype = "Group") +
  scale_color_manual(values = c("media_grupo_controle" = "#E69F00",  # Laranja
                                "media_grupo_tratados" = "#0072B2"), # Azul escuro
                     labels = c("Control", "Treated")) +
  scale_linetype_manual(values = c("media_grupo_controle" = "dashed",
                                   "media_grupo_tratados" = "solid"),
                        labels = c("Control", "Treated")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid = element_blank())

ggsave("G:/Meu Drive/dissertacao_leticia/artigo_ideb_submissao_traduzida/tendencias_paralelas/grafico8.png", width = 4, height = 3, dpi = 300)




# Loop sobre os valores de anos que serão testados para aband
for (i in anos) {

  # Filtrar os dados para as condições especificadas
  dados_filtrados <- subset(em, ano == i)

  # Verificar se há valores NA nos dados filtrados
  if (any(is.na(dados_filtrados$tx_aband_em))) {
    # Se houver valores NA, podemos optar por remover essas observações
    dados_filtrados <- dados_filtrados[complete.cases(dados_filtrados$tx_aband_em), ]
  }

  # Realizar o teste t por grupo definido por trat
  t_teste <- t.test(dados_filtrados$tx_aband_em ~ dados_filtrados$trat)

  # Calcular o desvio padrão apenas se não houver valores NA
  if (!any(is.na(dados_filtrados$tx_aband_em))) {
    dp_tratados <- sd(dados_filtrados$tx_aband_em[dados_filtrados$trat == 1])
    dp_controle <- sd(dados_filtrados$tx_aband_em[dados_filtrados$trat == 0])
  } else {
    # Se houver valores NA, definimos o desvio padrão como NA também
    dp_tratados <- NA
    dp_controle <- NA
  }

  # Adicionar os resultados do teste à lista
  resultados_t[[as.character(i)]] <- data.frame("media_grupo_tratados" = t_teste$estimate[2],
                                                "media_grupo_controle" = t_teste$estimate[1],
                                                "dp_grupo_tratados" = dp_tratados,
                                                "dp_grupo_controle" = dp_controle,
                                                "diferenca_medias" = t_teste$estimate[2] - t_teste$estimate[1],
                                                "p_valor" = t_teste$p.value)
}

# Converter a lista em um data frame
resultados_t_aband <- as.data.frame(do.call(rbind, resultados_t))
resultados_t_aband$ano <- row.names(resultados_t_aband)
resultados_t_aband$significativo <- ifelse(resultados_t_aband$p_valor <= 0.05, 1,0)

#base de dados pronta
#resultados_t_aband <- read.csv2("G:/Meu Drive/dissertacao_leticia/graficos_e_tabelas/result_did/result_test_t_por_grupos/teste_t_aband_em_anos_pares.csv") %>%
 # filter(ano >= 2012)

# Vamos primeiro reformatar os dados para o formato longo
dados_longos <- pivot_longer(resultados_t_aband, cols = c(media_grupo_tratados, media_grupo_controle), names_to = "grupo", values_to = "media")

# Crie o gráfico de linhas
ggplot(dados_longos, aes(x = ano, y = media, color = grupo, linetype = grupo, group = grupo)) +
  geom_line() +
  geom_point(size = 1) +
  labs(title = "Dropout – High School",
       x = "", y = "",
       color = "Group",
       linetype = "Group") +
  scale_color_manual(values = c("media_grupo_controle" = "#E69F00",  # Laranja
                                "media_grupo_tratados" = "#0072B2"), # Azul escuro
                     labels = c("Control", "Treated")) +
  scale_linetype_manual(values = c("media_grupo_controle" = "dashed",
                                   "media_grupo_tratados" = "solid"),
                        labels = c("Control", "Treated")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        panel.grid = element_blank())

ggsave("G:/Meu Drive/dissertacao_leticia/artigo_ideb_submissao_traduzida/tendencias_paralelas/grafico9.png", width = 4, height = 3, dpi = 300)



write.csv2(resultados_t_aprov, "G:/Meu Drive/dissertacao_leticia/graficos_e_tabelas/result_did/teste_t_aprov_em_anos_pares.csv")
write.csv2(resultados_t_reprov, "G:/Meu Drive/dissertacao_leticia/graficos_e_tabelas/result_did/teste_t_reprov_em_anos_pares.csv")
write.csv2(resultados_t_aband, "G:/Meu Drive/dissertacao_leticia/graficos_e_tabelas/result_did/teste_t_aband_em_anos_pares.csv")

