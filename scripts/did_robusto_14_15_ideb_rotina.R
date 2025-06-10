#####
# Objetivo: Estimacoes DID robusto santana and Zhao (2014 e 2015)
# Variaveis: aprovação, abandono, reprovação, taxa de participação no saeb
# Autor: Fca. Letícia F. Lima
# Data de criação: 12/03/24
# Última modificação: 08/06/2025
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
#ai_14 <- read.csv("D:/dissertacao_leticia/dados/dados_finais/base_ai_14_final.csv") %>%
 # select(-(X))
#names(ai_14)[1] <- "ano"
#af_14 <- read.csv("D:/dissertacao_leticia/dados/dados_finais/base_af_14_final.csv") %>%
 # select(-(X))
#names(af_14)[1] <- "ano"
#ai_15 <- read.csv("D:/dissertacao_leticia/dados/dados_finais/base_ai_15_final.csv") %>%
 # select(-(X))
#af_15 <- read.csv("D:/dissertacao_leticia/dados/dados_finais/base_af_15_final.csv")%>%
 # select(-(X))
#ird_14 <- read_xlsx("D:/dissertacao_leticia/dados/inep_dados/IRD_2014_ESCOLAS/IRD_ESCOLAS_2014.xlsx",
 #                   sheet = "Ind. reg. doc.", skip = 10)
#ird_15 <- read_xlsx("D:/dissertacao_leticia/dados/inep_dados/IRD_2015_ESCOLAS/IRD_ESCOLAS_2015.xlsx",
 #                   sheet = "Ind. reg. doc.", skip = 10)
#ird <-rbind(ird_14, ird_15)
#names(ird)[c(4,6, 10)] <- c("cod_munic", "cod_esc", "ird")
#ird_ <- ird %>% select("ano", "cod_munic", "cod_esc", "ird")
#str(ird_)

#dsu_14 <- read_xlsx("D:/dissertacao_leticia/dados/inep_dados/DSU_2014_ESCOLAS/DSU_ESCOLAS_2014.xlsx",
 #                   sheet = "ESCOLAS", skip = 9) %>%
#  select(Ano, PK_COD_MUNICIPIO, PK_COD_ENTIDADE, DSU_F14:DSU_MED)
#names(dsu_14) <- c("ano", "cod_munic", "cod_esc", "dsu_ai", "dsu_af", "dsu_em")
#dsu_15 <- read_xlsx("D:/dissertacao_leticia/dados/inep_dados/DSU_2015_ESCOLAS/DSU_ESCOLAS_2015.xlsx",
 #                   sheet = "ESCOLA", skip = 9) %>%
  #select(NU_ANO_CENSO, CO_MUNICIPIO, CO_ENTIDADE, DSU_F14:DSU_MED)
#names(dsu_15) <- c("ano", "cod_munic", "cod_esc", "dsu_ai", "dsu_af", "dsu_em")

#dsu_ <- rbind(dsu_14, dsu_15)
#str(dsu_)
#transformar <- c("dsu_ai", "dsu_af", "dsu_em")
#dsu_[, transformar] <- lapply(dsu_[, transformar], as.numeric)

#censo_14 <- read.csv2("D:/dissertacao_leticia/dados/dados_censo/microdados_censo_escolar_2014/microdados_ed_basica_2014/dados/microdados_ed_basica_2014.csv") %>%
 # filter(TP_DEPENDENCIA != 4, TP_LOCALIZACAO == 1 ) %>%
#  select("NU_ANO_CENSO", "CO_MUNICIPIO", "CO_ENTIDADE", "IN_BIBLIOTECA", "IN_BIBLIOTECA_SALA_LEITURA",
 #        "IN_LABORATORIO_CIENCIAS", "IN_LABORATORIO_INFORMATICA", "IN_PISCINA", "IN_QUADRA_ESPORTES",
  #       "IN_SALA_LEITURA", "QT_SALAS_UTILIZADAS", "IN_DESKTOP_ALUNO", "IN_COMP_PORTATIL_ALUNO",
   #      "IN_TABLET_ALUNO", "IN_INTERNET_ALUNOS", "IN_INTERNET_APRENDIZAGEM")
#censo_15 <- read.csv2("D:/dissertacao_leticia/dados/dados_censo/microdados_censo_escolar_2015/microdados_ed_basica_2015/dados/microdados_ed_basica_2015.csv") %>%
 # filter(TP_DEPENDENCIA != 4, TP_LOCALIZACAO == 1 ) %>%
  #select("NU_ANO_CENSO", "CO_MUNICIPIO", "CO_ENTIDADE", "IN_BIBLIOTECA", "IN_BIBLIOTECA_SALA_LEITURA",
   #      "IN_LABORATORIO_CIENCIAS", "IN_LABORATORIO_INFORMATICA", "IN_PISCINA", "IN_QUADRA_ESPORTES",
    #     "IN_SALA_LEITURA", "QT_SALAS_UTILIZADAS", "IN_DESKTOP_ALUNO", "IN_COMP_PORTATIL_ALUNO",
     #    "IN_TABLET_ALUNO", "IN_INTERNET_ALUNOS", "IN_INTERNET_APRENDIZAGEM")

#censo <- rbind(censo_14, censo_15)
#str(censo)
#censo <- mutate_all(censo, as.numeric)
#names(censo)[1:3] <- c("ano", "cod_munic", "cod_esc")



#### estimacoes ####
####anos iniciais####
#merge_15_ai <- ai_15 %>% select("cod_munic", "cod_esc")
#ai_14_ <- inner_join(merge_15_ai, ai_14, by = c("cod_munic", "cod_esc"))
#merge_14_ai <- ai_14_ %>% select("cod_munic", "cod_esc")
#ai_15_ <- inner_join(merge_14_ai, ai_15, by = c("cod_munic", "cod_esc"))

#ai <- rbind(ai_14_, ai_15_)
#ai$time <- if_else(ai$ano == 2015, 1,0)
#ai$trat <- if_else(ai$tx_part_5ef>= 0.5 & ai$ideb>=1, 1,0, missing = 0)
#ai$did <- ai$time * ai$trat

#ai_ <- left_join(ai, ird_, by = c("ano", "cod_munic", "cod_esc"))
#ai_ <- left_join(ai_, dsu_, by = c("ano", "cod_munic", "cod_esc"))
#ai_ <- left_join(ai_, censo, by = c("ano", "cod_munic", "cod_esc"))

#write.csv2(ai_, "D:/dissertacao_leticia/dados/dados_finais/base_ai_14_15.csv")
ai_ <- read.csv2("dados/base_ai_14_15.csv")

#aprov - sem controles
ai_aprov_certify1 <- drdid(yname="tx_aprov_ai", tname = "time", idname = "cod_esc", dname = "trat",
                           xformla= ~ 1,
                           data = ai_, panel = FALSE, estMethod = "imp")
summary(ai_aprov_certify1)


#Implement "improved" DR locally efficient DiD with repeated cross-section data
ai_aprov_certify2 <- drdid(yname="tx_aprov_ai", tname = "time", idname = "cod_esc", dname = "trat",
                           xformla= ~ ird + dsu_ai + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                             IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                             IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                           data = ai_, panel = FALSE, estMethod = "imp")
summary(ai_aprov_certify2)


#reprov - sem controles
ai_reprov_certify1 <- drdid(yname="tx_reprov_ai", tname = "time", idname = "cod_esc", dname = "trat",
                            xformla= ~ 1,
                            data = ai_, panel = FALSE, estMethod = "imp")
summary(ai_reprov_certify1)

#Implement "improved" DR locally efficient DiD with repeated cross-section data
ai_reprov_certify2 <- drdid(yname="tx_reprov_ai", tname = "time", idname = "cod_esc", dname = "trat",
                            xformla= ~ ird + dsu_ai + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                              IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                              IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                            data = ai_, panel = FALSE, estMethod = "imp")
summary(ai_reprov_certify2)


#aband - sem controle
ai_aband_certify1 <- drdid(yname="tx_aband_ai", tname = "time", idname = "cod_esc", dname = "trat",
                           xformla= ~ 1,
                           data = ai_, panel = FALSE, estMethod = "imp")
summary(ai_aband_certify1)

#Implement "improved" DR locally efficient DiD with repeated cross-section data
ai_aband_certify2 <- drdid(yname="tx_aband_ai", tname = "time", idname = "cod_esc", dname = "trat",
                           xformla= ~ ird + dsu_ai + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                             IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                             IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                           data = ai_, panel = FALSE, estMethod = "imp")
summary(ai_aband_certify2)

#anos finais####
#merge_15_af <- af_15 %>% select("cod_munic", "cod_esc")
#af_14_ <- inner_join(merge_15_af, af_14, by = c("cod_munic", "cod_esc"))
#merge_14_af <- af_14_ %>% select("cod_munic", "cod_esc")
#af_15_ <- inner_join(merge_14_af, af_15, by = c("cod_munic", "cod_esc"))

#af <- rbind(af_14_, af_15_)
#af$time <- if_else(af$ano == 2015, 1,0)
#af$trat <- if_else(af$tx_part_9ef>= 0.5 & af$ideb>=1, 1,0, missing = 0)
#af$did <- af$time * af$trat

#af_ <- left_join(af, ird_, by = c("ano", "cod_munic", "cod_esc"))
#af_ <- left_join(af_, dsu_, by = c("ano", "cod_munic", "cod_esc"))
#af_ <- left_join(af_, censo, by = c("ano", "cod_munic", "cod_esc"))

#write.csv2(af_, "D:/dissertacao_leticia/dados/dados_finais/base_af_14_15.csv")
af_ <- read.csv2("dados/base_af_14_15.csv")

#aprov - sem controles
af_aprov_certify1 <- drdid(yname="tx_aprov_af", tname = "time", idname = "cod_esc", dname = "trat",
                           xformla= ~ 1,
                           data = af_, panel = FALSE, estMethod = "imp")
summary(af_aprov_certify1)

#Implement "improved" DR locally efficient DiD with repeated cross-section data
af_aprov_certify2 <- drdid(yname="tx_aprov_af", tname = "time", idname = "cod_esc", dname = "trat",
                           xformla= ~ ird + dsu_af + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                             IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                             IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                           data = af_, panel = FALSE, estMethod = "imp")
summary(af_aprov_certify2)

#reprov - sem controle
af_reprov_certify1 <- drdid(yname="tx_reprov_af", tname = "time", idname = "cod_esc", dname = "trat",
                            xformla= ~ 1,
                            data = af_, panel = FALSE, estMethod = "imp")
summary(af_reprov_certify1)

#Implement "improved" DR locally efficient DiD with repeated cross-section data
af_reprov_certify2 <- drdid(yname="tx_reprov_af", tname = "time", idname = "cod_esc", dname = "trat",
                            xformla= ~ ird + dsu_af + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                              IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                              IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                            data = af_, panel = FALSE, estMethod = "imp")
summary(af_reprov_certify2)

#aband - sem controle
af_aband_certify1 <- drdid(yname="tx_aband_af", tname = "time", idname = "cod_esc", dname = "trat",
                           xformla= ~ 1,
                           data = af_, panel = FALSE, estMethod = "imp")
summary(af_aband_certify1)

#Implement "improved" DR locally efficient DiD with repeated cross-section data
af_aband_certify2 <- drdid(yname="tx_aband_af", tname = "time", idname = "cod_esc", dname = "trat",
                           xformla= ~ ird + dsu_af + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                             IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                             IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                           data = af_, panel = FALSE, estMethod = "imp")
summary(af_aband_certify2)

####ensino medio####
#não tem taxa de participação do saeb para EM em 2015, assim não podemos calcular a variavel de tratamento
