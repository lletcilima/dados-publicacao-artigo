#####
# Objetivo: Estimacoes efeitos heterogeneos DID robusto Santan'A e Zhao (2016 e 2017)
# Variaveis: regiao; estados
# Autor: Fca. Letícia F. Lima
# Data de criação: 05/05/24
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
ai_ <- read.csv2("data/base_ai_16_17_final_atualizado.csv")
af_ <- read.csv2("data/base_af_16_17_final_atualizado.csv")
em_ <- read.csv2("data/base_em_16_17_final_atualizado.csv")

#### estimacoes ####
####Norte####
#anos iniciais - com covariadas
ai_norte <- ai_ %>% filter(regiao == "Norte")

ai_aprov_no <- drdid(yname="tx_aprov_ai", tname = "time", idname = "cod_esc", dname = "trat",
                           xformla= ~ ird + dsu_ai + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                             IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                             IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                           data = ai_norte, panel = FALSE, estMethod = "imp")
summary(ai_aprov_no)

ai_reprov_no <- drdid(yname="tx_reprov_ai", tname = "time", idname = "cod_esc", dname = "trat",
                     xformla= ~ ird + dsu_ai + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                       IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                       IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                     data = ai_norte, panel = FALSE, estMethod = "imp")
summary(ai_reprov_no)

ai_aband_no <- drdid(yname="tx_aband_ai", tname = "time", idname = "cod_esc", dname = "trat",
                      xformla= ~ ird + dsu_ai + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                        IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                        IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                      data = ai_norte, panel = FALSE, estMethod = "imp")
summary(ai_aband_no)

#anos finais - com covariadas
af_norte <- af_ %>% filter(regiao == "Norte")

af_aprov_no <- drdid(yname="tx_aprov_af", tname = "time", idname = "cod_esc", dname = "trat",
                     xformla= ~ ird + dsu_af + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                       IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                       IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                     data = af_norte, panel = FALSE, estMethod = "imp")
summary(af_aprov_no)

af_reprov_no <- drdid(yname="tx_reprov_af", tname = "time", idname = "cod_esc", dname = "trat",
                      xformla= ~ ird + dsu_af + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                        IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                        IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                      data = af_norte, panel = FALSE, estMethod = "imp")
summary(af_reprov_no)

af_aband_no <- drdid(yname="tx_aband_af", tname = "time", idname = "cod_esc", dname = "trat",
                     xformla= ~ ird + dsu_af + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                       IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                       IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                     data = af_norte, panel = FALSE, estMethod = "imp")
summary(af_aband_no)

#ensino medio - com covariadas
em_norte <- em_ %>% filter(regiao == "Norte")

em_aprov_no <- drdid(yname="tx_aprov_em", tname = "time", idname = "cod_esc", dname = "trat",
                     xformla= ~ ird + dsu_em + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                       IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                       IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                     data = em_norte, panel = FALSE, estMethod = "imp")
summary(em_aprov_no)

em_reprov_no <- drdid(yname="tx_reprov_em", tname = "time", idname = "cod_esc", dname = "trat",
                      xformla= ~ ird + dsu_em + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                        IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                        IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                      data = em_norte, panel = FALSE, estMethod = "imp")
summary(em_reprov_no)

em_aband_no <- drdid(yname="tx_aband_em", tname = "time", idname = "cod_esc", dname = "trat",
                     xformla= ~ ird + dsu_em + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                       IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                       IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                     data = em_norte, panel = FALSE, estMethod = "imp")
summary(em_aband_no)

####Nordeste####
#anos iniciais - com covariadas
ai_nordeste <- ai_ %>% filter(regiao == "Nordeste")

ai_aprov_ne <- drdid(yname="tx_aprov_ai", tname = "time", idname = "cod_esc", dname = "trat",
                     xformla= ~ ird + dsu_ai + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                       IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                       IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                     data = ai_nordeste, panel = FALSE, estMethod = "imp")
summary(ai_aprov_ne)

ai_reprov_ne <- drdid(yname="tx_reprov_ai", tname = "time", idname = "cod_esc", dname = "trat",
                      xformla= ~ ird + dsu_ai + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                        IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                        IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                      data = ai_nordeste, panel = FALSE, estMethod = "imp")
summary(ai_reprov_ne)

ai_aband_ne <- drdid(yname="tx_aband_ai", tname = "time", idname = "cod_esc", dname = "trat",
                     xformla= ~ ird + dsu_ai + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                       IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                       IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                     data = ai_nordeste, panel = FALSE, estMethod = "imp")
summary(ai_aband_ne)

#anos finais - com covariadas
af_nordeste <- af_ %>% filter(regiao == "Nordeste")

af_aprov_ne <- drdid(yname="tx_aprov_af", tname = "time", idname = "cod_esc", dname = "trat",
                     xformla= ~ ird + dsu_af + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                       IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                       IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                     data = af_nordeste, panel = FALSE, estMethod = "imp")
summary(af_aprov_ne)

af_reprov_ne <- drdid(yname="tx_reprov_af", tname = "time", idname = "cod_esc", dname = "trat",
                      xformla= ~ ird + dsu_af + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                        IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                        IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                      data = af_nordeste, panel = FALSE, estMethod = "imp")
summary(af_reprov_ne)

af_aband_ne <- drdid(yname="tx_aband_af", tname = "time", idname = "cod_esc", dname = "trat",
                     xformla= ~ ird + dsu_af + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                       IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                       IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                     data = af_nordeste, panel = FALSE, estMethod = "imp")
summary(af_aband_ne)

#ensino medio - com covariadas
em_nordeste <- em_ %>% filter(regiao == "Nordeste")

em_aprov_ne <- drdid(yname="tx_aprov_em", tname = "time", idname = "cod_esc", dname = "trat",
                     xformla= ~ ird + dsu_em + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                       IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                       IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                     data = em_nordeste, panel = FALSE, estMethod = "imp")
summary(em_aprov_ne)

em_reprov_ne <- drdid(yname="tx_reprov_em", tname = "time", idname = "cod_esc", dname = "trat",
                      xformla= ~ ird + dsu_em + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                        IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                        IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                      data = em_nordeste, panel = FALSE, estMethod = "imp")
summary(em_reprov_ne)

em_aband_ne <- drdid(yname="tx_aband_em", tname = "time", idname = "cod_esc", dname = "trat",
                     xformla= ~ ird + dsu_em + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                       IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                       IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                     data = em_nordeste, panel = FALSE, estMethod = "imp")
summary(em_aband_ne)

####Sul####
#anos iniciais - com covariadas
ai_sul <- ai_ %>% filter(regiao == "Sul")

ai_aprov_sul <- drdid(yname="tx_aprov_ai", tname = "time", idname = "cod_esc", dname = "trat",
                     xformla= ~ ird + dsu_ai + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                       IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                       IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                     data = ai_sul, panel = FALSE, estMethod = "imp")
summary(ai_aprov_sul)

ai_reprov_sul <- drdid(yname="tx_reprov_ai", tname = "time", idname = "cod_esc", dname = "trat",
                      xformla= ~ ird + dsu_ai + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                        IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                        IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                      data = ai_sul, panel = FALSE, estMethod = "imp")
summary(ai_reprov_sul)

ai_aband_sul <- drdid(yname="tx_aband_ai", tname = "time", idname = "cod_esc", dname = "trat",
                     xformla= ~ ird + dsu_ai + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                       IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                       IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                     data = ai_sul, panel = FALSE, estMethod = "imp")
summary(ai_aband_sul)

#anos finais - com covariadas
af_sul <- af_ %>% filter(regiao == "Sul")

af_aprov_sul <- drdid(yname="tx_aprov_af", tname = "time", idname = "cod_esc", dname = "trat",
                     xformla= ~ ird + dsu_af + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                       IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                       IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                     data = af_sul, panel = FALSE, estMethod = "imp")
summary(af_aprov_sul)

af_reprov_sul <- drdid(yname="tx_reprov_af", tname = "time", idname = "cod_esc", dname = "trat",
                      xformla= ~ ird + dsu_af + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                        IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                        IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                      data = af_sul, panel = FALSE, estMethod = "imp")
summary(af_reprov_sul)

af_aband_sul <- drdid(yname="tx_aband_af", tname = "time", idname = "cod_esc", dname = "trat",
                     xformla= ~ ird + dsu_af + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                       IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                       IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                     data = af_sul, panel = FALSE, estMethod = "imp")
summary(af_aband_sul)

#ensino medio - com covariadas
em_sul <- em_ %>% filter(regiao == "Sul")

em_aprov_sul <- drdid(yname="tx_aprov_em", tname = "time", idname = "cod_esc", dname = "trat",
                     xformla= ~ ird + dsu_em + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                       IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                       IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                     data = em_sul, panel = FALSE, estMethod = "imp")
summary(em_aprov_sul)

em_reprov_sul <- drdid(yname="tx_reprov_em", tname = "time", idname = "cod_esc", dname = "trat",
                      xformla= ~ ird + dsu_em + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                        IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                        IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                      data = em_sul, panel = FALSE, estMethod = "imp")
summary(em_reprov_sul)

em_aband_sul <- drdid(yname="tx_aband_em", tname = "time", idname = "cod_esc", dname = "trat",
                     xformla= ~ ird + dsu_em + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                       IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                       IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                     data = em_sul, panel = FALSE, estMethod = "imp")
summary(em_aband_sul)

####Sudeste####
#anos iniciais - com covariadas
ai_se <- ai_ %>% filter(regiao == "Sudeste")

ai_aprov_se <- drdid(yname="tx_aprov_ai", tname = "time", idname = "cod_esc", dname = "trat",
                      xformla= ~ ird + dsu_ai + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                        IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                        IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                      data = ai_se, panel = FALSE, estMethod = "imp")
summary(ai_aprov_se)

ai_reprov_se <- drdid(yname="tx_reprov_ai", tname = "time", idname = "cod_esc", dname = "trat",
                       xformla= ~ ird + dsu_ai + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                         IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                         IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                       data = ai_se, panel = FALSE, estMethod = "imp")
summary(ai_reprov_se)

ai_aband_se <- drdid(yname="tx_aband_ai", tname = "time", idname = "cod_esc", dname = "trat",
                      xformla= ~ ird + dsu_ai + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                        IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                        IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                      data = ai_se, panel = FALSE, estMethod = "imp")
summary(ai_aband_se)

#anos finais - com covariadas
af_se <- af_ %>% filter(regiao == "Sudeste")

af_aprov_se <- drdid(yname="tx_aprov_af", tname = "time", idname = "cod_esc", dname = "trat",
                      xformla= ~ ird + dsu_af + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                        IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                        IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                      data = af_se, panel = FALSE, estMethod = "imp")
summary(af_aprov_se)

af_reprov_se <- drdid(yname="tx_reprov_af", tname = "time", idname = "cod_esc", dname = "trat",
                       xformla= ~ ird + dsu_af + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                         IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                         IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                       data = af_se, panel = FALSE, estMethod = "imp")
summary(af_reprov_se)

af_aband_se <- drdid(yname="tx_aband_af", tname = "time", idname = "cod_esc", dname = "trat",
                      xformla= ~ ird + dsu_af + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                        IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                        IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                      data = af_se, panel = FALSE, estMethod = "imp")
summary(af_aband_se)

#ensino medio - com covariadas
em_se <- em_ %>% filter(regiao == "Sudeste")

em_aprov_se <- drdid(yname="tx_aprov_em", tname = "time", idname = "cod_esc", dname = "trat",
                      xformla= ~ ird + dsu_em + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                        IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                        IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                      data = em_se, panel = FALSE, estMethod = "imp")
summary(em_aprov_se)

em_reprov_se <- drdid(yname="tx_reprov_em", tname = "time", idname = "cod_esc", dname = "trat",
                       xformla= ~ ird + dsu_em + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                         IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                         IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                       data = em_se, panel = FALSE, estMethod = "imp")
summary(em_reprov_se)

em_aband_se <- drdid(yname="tx_aband_em", tname = "time", idname = "cod_esc", dname = "trat",
                      xformla= ~ ird + dsu_em + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                        IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                        IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                      data = em_se, panel = FALSE, estMethod = "imp")
summary(em_aband_se)

####Centro Oeste####
#anos iniciais - com covariadas
ai_co <- ai_ %>% filter(regiao == "Centro-Oeste")

ai_aprov_co <- drdid(yname="tx_aprov_ai", tname = "time", idname = "cod_esc", dname = "trat",
                     xformla= ~ ird + dsu_ai + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                       IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                       IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                     data = ai_co, panel = FALSE, estMethod = "imp")
summary(ai_aprov_co)

ai_reprov_co <- drdid(yname="tx_reprov_ai", tname = "time", idname = "cod_esc", dname = "trat",
                      xformla= ~ ird + dsu_ai + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                        IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                        IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                      data = ai_co, panel = FALSE, estMethod = "imp")
summary(ai_reprov_co)

ai_aband_co <- drdid(yname="tx_aband_ai", tname = "time", idname = "cod_esc", dname = "trat",
                     xformla= ~ ird + dsu_ai + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                       IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                       IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                     data = ai_co, panel = FALSE, estMethod = "imp")
summary(ai_aband_co)

#anos finais - com covariadas
af_co <- af_ %>% filter(regiao == "Centro-Oeste")

af_aprov_co <- drdid(yname="tx_aprov_af", tname = "time", idname = "cod_esc", dname = "trat",
                     xformla= ~ ird + dsu_af + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                       IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                       IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                     data = af_co, panel = FALSE, estMethod = "imp")
summary(af_aprov_co)

af_reprov_co <- drdid(yname="tx_reprov_af", tname = "time", idname = "cod_esc", dname = "trat",
                      xformla= ~ ird + dsu_af + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                        IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                        IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                      data = af_co, panel = FALSE, estMethod = "imp")
summary(af_reprov_co)

af_aband_co <- drdid(yname="tx_aband_af", tname = "time", idname = "cod_esc", dname = "trat",
                     xformla= ~ ird + dsu_af + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                       IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                       IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                     data = af_co, panel = FALSE, estMethod = "imp")
summary(af_aband_co)

#ensino medio - com covariadas
em_co <- em_ %>% filter(regiao == "Centro-Oeste")

em_aprov_co <- drdid(yname="tx_aprov_em", tname = "time", idname = "cod_esc", dname = "trat",
                     xformla= ~ ird + dsu_em + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                       IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                       IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                     data = em_co, panel = FALSE, estMethod = "imp")
summary(em_aprov_co)

em_reprov_co <- drdid(yname="tx_reprov_em", tname = "time", idname = "cod_esc", dname = "trat",
                      xformla= ~ ird + dsu_em + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                        IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                        IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                      data = em_co, panel = FALSE, estMethod = "imp")
summary(em_reprov_co)

em_aband_co <- drdid(yname="tx_aband_em", tname = "time", idname = "cod_esc", dname = "trat",
                     xformla= ~ ird + dsu_em + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                       IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                       IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                     data = em_co, panel = FALSE, estMethod = "imp")
summary(em_aband_co)
