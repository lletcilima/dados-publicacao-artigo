####
# Objetivo: teste placebo para as estimações drdid 16 e 18
# hipotese: tratados e controles terão quantidades semelhantes a 2017
# hipotese: o tratamento será aleatório para as escolas
# Variaveis: aprovação, abandono, reprovação, taxa de participação no saeb
# Autor: Fca. Letícia F. Lima
# Data de criação: 20/06/24
# Última modificação: 07/06/2025
####
rm(list = ls())
gc(reset = TRUE)

####pacotes####
# install.packages("remotes")
#remotes::install_github("pedrohcgs/DRDID")
library(DRDID)
library(readxl)
library(haven)
library(tidyverse)
library(broom)
library(estimatr)
library(texreg)
library(stargazer)

####dados####
#ai_16 <- read.csv("D:/dissertacao_leticia/dados/dados_finais/base_ai_16_final.csv") %>%
 # select(-(X), -(tx_part_5ef))
#names(ai_16)[1] <- "ano"
#af_16 <- read.csv("D:/dissertacao_leticia/dados/dados_finais/base_af_16_final.csv") %>%
 # select(-(X), -(tx_part_9ef), -(trat))
#names(af_16)[1] <- "ano"
#em_16 <- read.csv("D:/dissertacao_leticia/dados/dados_finais/base_em_16_final.csv") %>%
 # select(-(X), -(tx_part_3em), -(trat))
#names(em_16)[1] <- "ano"


#ai_18 <- read.csv("D:/dissertacao_leticia/dados/dados_finais/base_ai_18_final.csv") %>%
 # select(-(X))
#names(ai_18)[1] <- "ano"
#af_18 <- read.csv("D:/dissertacao_leticia/dados/dados_finais/base_af_18_final.csv") %>%
 # select(-(X))
#names(af_18)[1] <- "ano"
#em_18 <- read.csv("D:/dissertacao_leticia/dados/dados_finais/base_em_18_final.csv") %>%
 # select(-(X))
#names(em_18)[1] <- "ano"


#controles
#ird_18 <- read_xlsx("D:/dissertacao_leticia/dados/inep_dados/IRD_2018_ESCOLAS/IRD_ESCOLAS_2018.xlsx",
 #                   sheet = "ESCOLAS", skip = 10)

#ird_16 <- read_xlsx("D:/dissertacao_leticia/dados/inep_dados/IRD_2016_ESCOLAS/IRD_ESCOLAS_2016.xlsx",
 #                   sheet = "Ind. reg. doc.", skip = 10)
#ird <-rbind(ird_16, ird_18)
#names(ird)[c(4,6, 10)] <- c("cod_munic", "cod_esc", "ird")
#ird_ <- ird %>% select("ano", "cod_munic", "cod_esc", "ird")
#str(ird_)

#dsu_18 <- read_xlsx("D:/dissertacao_leticia/dados/inep_dados/DSU_2018_ESCOLAS/DSU_ESCOLAS_2018_ATUALIZADO.xlsx",
      #              sheet = "ESCOLA", skip = 9) %>%
 # select(NU_ANO_CENSO, CO_MUNICIPIO, CO_ENTIDADE, FUN_AI_CAT0:MED_CAT0)
#names(dsu_18) <- c("ano", "cod_munic", "cod_esc", "dsu_ai", "dsu_af", "dsu_em")
#str(dsu_18)


#dsu_16 <- read_xlsx("D:/dissertacao_leticia/dados/inep_dados/DSU_2016_ESCOLAS/DSU_ESCOLAS_2016.xlsx",
 #                   sheet = "ESCOLA", skip = 9)
#names(dsu_16)[c(1,4,6, 14,15,16)] <- c("ano", "cod_munic", "cod_esc", "dsu_ai", "dsu_af", "dsu_em")
#dsu_16 <- dsu_16 %>% select("ano", "cod_munic", "cod_esc", "dsu_ai", "dsu_af", "dsu_em")
#transformar <- c("dsu_ai", "dsu_af", "dsu_em")
#dsu_16[, transformar] <- lapply(dsu_16[, transformar], as.numeric)

#dsu_ <- rbind(dsu_16, dsu_18)
#str(dsu_)

#censo_18 <- read.csv2("D:/dissertacao_leticia/dados/dados_censo/microdados_censo_escolar_2018/microdados_ed_basica_2018/dados/microdados_ed_basica_2018.csv") %>%
 # filter(TP_DEPENDENCIA != 4, TP_LOCALIZACAO == 1 ) %>%
  #select("NU_ANO_CENSO", "CO_MUNICIPIO", "CO_ENTIDADE", "IN_BIBLIOTECA", "IN_BIBLIOTECA_SALA_LEITURA",
   #      "IN_LABORATORIO_CIENCIAS", "IN_LABORATORIO_INFORMATICA", "IN_PISCINA", "IN_QUADRA_ESPORTES",
    #     "IN_SALA_LEITURA", "QT_SALAS_UTILIZADAS", "IN_DESKTOP_ALUNO", "IN_COMP_PORTATIL_ALUNO",
     #    "IN_TABLET_ALUNO", "IN_INTERNET_ALUNOS", "IN_INTERNET_APRENDIZAGEM")

#censo_16 <- read.csv2("D:/dissertacao_leticia/dados/dados_censo/microdados_censo_escolar_2016/dados/microdados_ed_basica_2016.csv") %>%
 # filter(TP_DEPENDENCIA != 4, TP_LOCALIZACAO == 1 ) %>%
  #select("NU_ANO_CENSO", "CO_MUNICIPIO", "CO_ENTIDADE", "IN_BIBLIOTECA", "IN_BIBLIOTECA_SALA_LEITURA",
   #      "IN_LABORATORIO_CIENCIAS", "IN_LABORATORIO_INFORMATICA", "IN_PISCINA", "IN_QUADRA_ESPORTES",
    #     "IN_SALA_LEITURA", "QT_SALAS_UTILIZADAS", "IN_DESKTOP_ALUNO", "IN_COMP_PORTATIL_ALUNO",
     #    "IN_TABLET_ALUNO", "IN_INTERNET_ALUNOS", "IN_INTERNET_APRENDIZAGEM"
  #)

#censo <- rbind(censo_16, censo_18)
#str(censo)
#censo <- mutate_all(censo, as.numeric)
#names(censo)[1:3] <- c("ano", "cod_munic", "cod_esc")


### estimacoes ####
####anos iniciais####
#merge_18_ai <- ai_18 %>% select("cod_munic", "cod_esc", "ideb")
#ai_16 <- ai_16 %>%
 # select(-ideb, -trat)
#ai_16_ <- inner_join(merge_18_ai, ai_16, by = c("cod_munic", "cod_esc"))
#ai_16_ <- ai_16_ %>% select("cod_munic", "cod_esc", "ano", "regiao", "uf", "rede",
 #                           "tx_aprov_ai", "tx_reprov_ai", "tx_aband_ai", "ideb")
#merge_16_ai <- ai_16_ %>% select("cod_munic", "cod_esc")
#ai_18_ <- inner_join(merge_16_ai, ai_18, by = c("cod_munic", "cod_esc"))

##anos iniciais (38670)###
#> sum(ai$trat == 1)
#[1] 60820 (30410)
#> sum(ai$trat == 0)
#[1] 16520 (8260)

# Verificando as primeiras linhas da base de dados
#head(ai_18_)

# Definindo as quantidades de tratados e controles desejadas
#num_controles <- 8260
#num_tratados <- 30410

# Verificando se o número total de tratamentos não excede o número de linhas do dataframe
#if (num_controles + num_tratados > nrow(ai_18_)) {
 # stop("O número total de tratamentos excede o número de linhas do dataframe.")
#}

# Criar a coluna de tratamento com a quantidade desejada
#tratamentos <- c(rep("controle", num_controles), rep("tratado", num_tratados))

# Adicionar valores "NA" para o restante das observações, se houver
#if (length(tratamentos) < nrow(ai_18_)) {
 # tratamentos <- c(tratamentos, rep(NA, nrow(ai_18_) - length(tratamentos)))
#}

# Embaralhar a coluna de tratamento
#set.seed(321) # Defina uma semente para reprodutibilidade
#tratamentos <- sample(tratamentos)

# Substituir "tratado" por 1 e "controle" por 0
#tratamentos_numeric <- ifelse(tratamentos == "tratado", 1,
 #                             ifelse(tratamentos == "controle", 0, NA))

# Adicionar a coluna de tratamento ao dataframe
#ai_18_$tratamento <- tratamentos_numeric

# Visualizar as primeiras linhas do dataframe atualizado
#head(ai_18_)

#merge_controle <- ai_18_ %>% select(cod_munic, cod_esc, tratamento)
#ai_16_ <- left_join(merge_controle, ai_16_, by = c("cod_munic", "cod_esc"))
#ai_16_ <- ai_16_ %>% select(cod_munic, cod_esc, ano:ideb, tratamento)

#ai <- rbind(ai_16_, ai_18_)
#ai$time <- if_else(ai$ano == 2018, 1,0)
#ai <- ai %>% filter(tratamento >= 0)

#ai_ <- left_join(ai, ird_, by = c("ano", "cod_munic", "cod_esc"))
#ai_ <- left_join(ai_, dsu_, by = c("ano", "cod_munic", "cod_esc"))
#ai_ <- left_join(ai_, censo, by = c("ano", "cod_munic", "cod_esc"))

#write.csv2(ai_,"D:/dissertacao_leticia/dados/dados_finais/base_ai_16_18_placebo.csv")
ai_ <- read.csv2("dados/base_ai_16_18_placebo.csv")

#Implement "improved" DR locally efficient DiD with repeated cross-section data
ai_aprov_certify2 <- drdid(yname="tx_aprov_ai", tname = "time", idname = "cod_esc", dname = "tratamento",
                           xformla= ~ ird + dsu_ai + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                             IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                             IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                           data = ai_, panel = FALSE, estMethod = "imp")
summary(ai_aprov_certify2)

#aprov - did tradicional
ai_aprov_ <- lm(tx_aprov_ai ~ trat + time + did + ird + dsu_ai + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                  IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                  IN_SALA_LEITURA + QT_SALAS_UTILIZADAS, data = ai_)
summary(ai_aprov_)


#Implement "improved" DR locally efficient DiD with repeated cross-section data
ai_reprov_certify2 <- drdid(yname="tx_reprov_ai", tname = "time", idname = "cod_esc", dname = "tratamento",
                            xformla= ~ ird + dsu_ai + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                              IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                              IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                            data = ai_, panel = FALSE, estMethod = "imp")
summary(ai_reprov_certify2)

#reprov - did tradicional
ai_reprov_ <- lm(tx_reprov_ai ~ trat + time + did + ird + dsu_ai + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                   IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                   IN_SALA_LEITURA + QT_SALAS_UTILIZADAS, data = ai_)
summary(ai_reprov_)


#Implement "improved" DR locally efficient DiD with repeated cross-section data
ai_aband_certify2 <- drdid(yname="tx_aband_ai", tname = "time", idname = "cod_esc", dname = "tratamento",
                           xformla= ~ ird + dsu_ai + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                             IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                             IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                           data = ai_, panel = FALSE, estMethod = "imp")
summary(ai_aband_certify2)

#aband - did tradicional
ai_aband_ <- lm(tx_aband_ai ~ trat + time + did + ird + dsu_ai + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                  IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                  IN_SALA_LEITURA + QT_SALAS_UTILIZADAS, data = ai_)
summary(ai_aband_)

####anos finais####
#merge_18_af <- af_18 %>% select("cod_munic", "cod_esc","ideb")
#af_16 <- af_16 %>%
 # select(-ideb)
#af_16_ <- inner_join(merge_18_af, af_16, by = c("cod_munic", "cod_esc"))
#af_16_ <- af_16_ %>% select("cod_munic", "cod_esc", "ano", "regiao", "uf", "rede",
#                            "tx_aprov_af", "tx_reprov_af", "tx_aband_af", "ideb")
#merge_16_af <- af_16_ %>% select("cod_munic", "cod_esc")
#af_18_ <- inner_join(merge_16_af, af_18, by = c("cod_munic", "cod_esc"))

## anos finais (57477)###
#> sum(af$trat == 1)
#[1] 37024 (18512)
#> sum(af$trat == 0)
#[1] 77930 (38965)

# Verificando as primeiras linhas da base de dados
#head(af_18_)

# Definindo as quantidades de tratados e controles desejadas
#num_controles <- 38965
#num_tratados <- 18512

# Verificando se o número total de tratamentos não excede o número de linhas do dataframe
#if (num_controles + num_tratados > nrow(af_18_)) {
 # stop("O número total de tratamentos excede o número de linhas do dataframe.")
#}

# Criar a coluna de tratamento com a quantidade desejada
#tratamentos <- c(rep("controle", num_controles), rep("tratado", num_tratados))

# Adicionar valores "NA" para o restante das observações, se houver
#if (length(tratamentos) < nrow(af_18_)) {
 # tratamentos <- c(tratamentos, rep(NA, nrow(af_18_) - length(tratamentos)))
#}

# Garantir que a coluna de tratamentos tem exatamente o mesmo comprimento do dataframe
#tratamentos <- tratamentos[1:nrow(af_18_)]

# Embaralhar a coluna de tratamento
#set.seed(321) # Defina uma semente para reprodutibilidade
#tratamentos <- sample(tratamentos)

# Substituir "tratado" por 1 e "controle" por 0
#tratamentos_numeric <- ifelse(tratamentos == "tratado", 1,
 #                             ifelse(tratamentos == "controle", 0, NA))

# Adicionar a coluna de tratamento ao dataframe
#af_18_$tratamento <- tratamentos_numeric

# Visualizar as primeiras linhas do dataframe atualizado
#head(af_18_)

#merge_controle <- af_18_ %>% select(cod_munic, cod_esc, tratamento)
#af_16_ <- left_join(merge_controle, af_16_, by = c("cod_munic", "cod_esc"))
#af_16_ <- af_16_ %>% select(cod_munic, cod_esc, ano:ideb, tratamento)


#af <- rbind(af_16_, af_18_)
#af$time <- if_else(af$ano == 2018, 1,0)
#af <- af %>% filter(tratamento >= 0)

#af_ <- left_join(af, ird_, by = c("ano", "cod_munic", "cod_esc"))
#af_ <- left_join(af_, dsu_, by = c("ano", "cod_munic", "cod_esc"))
#af_ <- left_join(af_, censo, by = c("ano", "cod_munic", "cod_esc"))

#write.csv2(af_,"D:/dissertacao_leticia/dados/dados_finais/base_af_16_18_placebo.csv")
af_ <- read.csv2("dados/base_af_16_18_placebo.csv")

#Implement "improved" DR locally efficient DiD with repeated cross-section data
af_aprov_certify2 <- drdid(yname="tx_aprov_af", tname = "time", idname = "cod_esc", dname = "tratamento",
                           xformla= ~ ird + dsu_af + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                             IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                             IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                           data = af_, panel = FALSE, estMethod = "imp")
summary(af_aprov_certify2)


#Implement "improved" DR locally efficient DiD with repeated cross-section data
af_reprov_certify2 <- drdid(yname="tx_reprov_af", tname = "time", idname = "cod_esc", dname = "tratamento",
                            xformla= ~ ird + dsu_af + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                              IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                              IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                            data = af_, panel = FALSE, estMethod = "imp")
summary(af_reprov_certify2)


#Implement "improved" DR locally efficient DiD with repeated cross-section data
af_aband_certify2 <- drdid(yname="tx_aband_af", tname = "time", idname = "cod_esc", dname = "tratamento",
                           xformla= ~ ird + dsu_af + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                             IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                             IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                           data = af_, panel = FALSE, estMethod = "imp")
summary(af_aband_certify2)



####ensino medio####
#merge_18_em <- em_18 %>% select("cod_munic", "cod_esc","ideb")
#em_16 <- em_16 %>%
 # select(-ideb)
#em_16_ <- inner_join(merge_18_em, em_16, by = c("cod_munic", "cod_esc"))
#em_16_ <- em_16_ %>% select("cod_munic", "cod_esc", "ano", "regiao", "uf", "rede",
 #                           "tx_aprov_em", "tx_reprov_em", "tx_aband_em", "ideb")
#merge_16_em <- em_16_ %>% select("cod_munic", "cod_esc")
#em_18_ <- inner_join(merge_16_em, em_18, by = c("cod_munic", "cod_esc"))

## ensino medio (57477)###
#> sum(em$trat == 1)
#[1] 15146 (7573)
#> sum(em$trat == 0)
#[1] 99808 (49904)

# Verificando as primeiras linhas da base de dados
#head(em_18_)

# Definindo as quantidades de tratados e controles desejadas
#num_controles <- 49904
#num_tratados <- 7573

# Verificando se o número total de tratamentos não excede o número de linhas do dataframe
#if (num_controles + num_tratados > nrow(em_18_)) {
 # stop("O número total de tratamentos excede o número de linhas do dataframe.")
#}

# Criar a coluna de tratamento com a quantidade desejada
#tratamentos <- c(rep("controle", num_controles), rep("tratado", num_tratados))

# Adicionar valores "NA" para o restante das observações, se houver
#if (length(tratamentos) < nrow(em_18_)) {
 # tratamentos <- c(tratamentos, rep(NA, nrow(em_18_) - length(tratamentos)))
#}

# Garantir que a coluna de tratamentos tem exatamente o mesmo comprimento do dataframe
#tratamentos <- tratamentos[1:nrow(em_18_)]

# Embaralhar a coluna de tratamento
#set.seed(321) # Defina uma semente para reprodutibilidade
#tratamentos <- sample(tratamentos)

# Substituir "tratado" por 1 e "controle" por 0
#tratamentos_numeric <- ifelse(tratamentos == "tratado", 1,
 #                             ifelse(tratamentos == "controle", 0, NA))

# Adicionar a coluna de tratamento ao dataframe
#em_18_$tratamento <- tratamentos_numeric

# Visualizar as primeiras linhas do dataframe atualizado
#head(em_18_)

#merge_controle <- em_18_ %>% select(cod_munic, cod_esc, tratamento)
#em_16_ <- left_join(merge_controle, em_16_, by = c("cod_munic", "cod_esc"))
#em_16_ <- em_16_ %>% select(cod_munic, cod_esc, ano:ideb, tratamento)


#em <- rbind(em_16_, em_18_)
#em$time <- if_else(em$ano == 2018, 1,0)
#em <- em %>% filter(tratamento >= 0)

#em_ <- left_join(em, ird_, by = c("ano", "cod_munic", "cod_esc"))
#em_ <- left_join(em_, dsu_, by = c("ano", "cod_munic", "cod_esc"))
#em_ <- left_join(em_, censo, by = c("ano", "cod_munic", "cod_esc"))

#write.csv2(em_,"D:/dissertacao_leticia/dados/dados_finais/base_em_16_18_placebo.csv")
em_ <- read.csv2("dados/base_em_16_18_placebo.csv")

#Implement "improved" DR locally efficient DiD with repeated cross-section data
em_aprov_certify2 <- drdid(yname="tx_aprov_em", tname = "time", idname = "cod_esc", dname = "tratamento",
                           xformla= ~ ird + dsu_em + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                             IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                             IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                           data = em_, panel = FALSE, estMethod = "imp")
summary(em_aprov_certify2)


#Implement "improved" DR locally efficient DiD with repeated cross-section data
em_reprov_certify2 <- drdid(yname="tx_reprov_em", tname = "time", idname = "cod_esc", dname = "tratamento",
                            xformla= ~ ird + dsu_em + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                              IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                              IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                            data = em_, panel = FALSE, estMethod = "imp")
summary(em_reprov_certify2)


#Implement "improved" DR locally efficient DiD with repeated cross-section data
em_aband_certify2 <- drdid(yname="tx_aband_em", tname = "time", idname = "cod_esc", dname = "tratamento",
                           xformla= ~ ird + dsu_em + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                             IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                             IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                           data = em_, panel = FALSE, estMethod = "imp")
summary(em_aband_certify2)


