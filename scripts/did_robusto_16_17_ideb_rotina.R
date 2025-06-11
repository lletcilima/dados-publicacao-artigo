#####
# Objetivo: Estimacoes DID robusto Santan'A e Zhao (2016 e 2017)
# Variaveis: aprovação, abandono, reprovação, taxa de participação no saeb
# Autor: Fca. Letícia F. Lima
# Data de criação: 23/02/24
# Última modificação: 05/05/24
####
gc(reset = TRUE)

####pacotes####
# install.packages("remotes")
remotes::install_github("pedrohcgs/DRDID")
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

#### estimacoes anos iniciais####

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

#aprov - did tradicional
ai_aprov_ <- lm(tx_aprov_ai ~ trat + time + did + ird + dsu_ai + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                  IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                  IN_SALA_LEITURA + QT_SALAS_UTILIZADAS, data = ai_)
summary(ai_aprov_)



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

#reprov - did tradicional
ai_reprov_ <- lm(tx_reprov_ai ~ trat + time + did + ird + dsu_ai + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                  IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                  IN_SALA_LEITURA + QT_SALAS_UTILIZADAS, data = ai_)
summary(ai_reprov_)


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

#aband - did tradicional
ai_aband_ <- lm(tx_aband_ai ~ trat + time + did + ird + dsu_ai + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                  IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                  IN_SALA_LEITURA + QT_SALAS_UTILIZADAS, data = ai_)
summary(ai_aband_)


#### estimacoes anos finais####

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

#aprov - did tradicional
af_aprov_ <- lm(tx_aprov_af ~ trat + time + did + ird + dsu_af + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                  IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                  IN_SALA_LEITURA + QT_SALAS_UTILIZADAS, data = af_)
summary(af_aprov_)


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

#reprov - did tradicional
af_reprov_ <- lm(tx_reprov_af ~ trat + time + did + ird + dsu_af + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                  IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                  IN_SALA_LEITURA + QT_SALAS_UTILIZADAS, data = af_)
summary(af_reprov_)

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

#aband - did tradicional
af_aband_ <- lm(tx_aband_af ~ trat + time + did + ird + dsu_af + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                  IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                  IN_SALA_LEITURA + QT_SALAS_UTILIZADAS, data = af_)
summary(af_aband_)

#### estimacoes ensino medio####

#aprov - sem controles
em_aprov_certify1 <- drdid(yname="tx_aprov_em", tname = "time", idname = "cod_esc", dname = "trat",
                           xformla= ~ 1,
                           data = em_, panel = FALSE, estMethod = "imp")
summary(em_aprov_certify1)

#Implement "improved" DR locally efficient DiD with repeated cross-section data
em_aprov_certify2 <- drdid(yname="tx_aprov_em", tname = "time", idname = "cod_esc", dname = "trat",
                           xformla= ~ ird + dsu_em + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                             IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                             IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                           data = em_, panel = FALSE, estMethod = "imp")
summary(em_aprov_certify2)

#aprov - did tradicional
em_aprov_ <- lm(tx_aprov_em ~ trat + time + did + ird + dsu_em + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                  IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                  IN_SALA_LEITURA + QT_SALAS_UTILIZADAS, data = em_)
summary(em_aprov_)

#reprov - sem controle
em_reprov_certify1 <- drdid(yname="tx_reprov_em", tname = "time", idname = "cod_esc", dname = "trat",
                            xformla= ~ 1,
                            data = em_, panel = FALSE, estMethod = "imp")
summary(em_reprov_certify1)

#Implement "improved" DR locally efficient DiD with repeated cross-section data
em_reprov_certify2 <- drdid(yname="tx_reprov_em", tname = "time", idname = "cod_esc", dname = "trat",
                            xformla= ~ ird + dsu_em + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                              IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                              IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                            data = em_, panel = FALSE, estMethod = "imp")
summary(em_reprov_certify2)

#reprov - did tradicional
em_reprov_ <- lm(tx_reprov_em ~ trat + time + did + ird + dsu_em + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                  IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                  IN_SALA_LEITURA + QT_SALAS_UTILIZADAS, data = em_)
summary(em_reprov_)

#aband - doubly robust sem controle
em_aband_certify1 <- drdid(yname="tx_aband_em", tname = "time", idname = "cod_esc", dname = "trat",
                           xformla= ~ 1,
                           data = em_, panel = FALSE, estMethod = "imp")
summary(em_aband_certify1)


#aband - doubly robust com controle
em_aband_certify2 <- drdid(yname="tx_aband_em", tname = "time", idname = "cod_esc", dname = "trat",
                           xformla= ~ ird + dsu_em + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                             IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                             IN_SALA_LEITURA + QT_SALAS_UTILIZADAS,
                           data = em_, panel = FALSE, estMethod = "imp")
summary(em_aband_certify2)

#aband - did tradicional
em_aband_ <- lm(tx_aband_em ~ trat + time + did + ird + dsu_em + IN_BIBLIOTECA + IN_BIBLIOTECA_SALA_LEITURA +
                  IN_LABORATORIO_CIENCIAS + IN_LABORATORIO_INFORMATICA + IN_QUADRA_ESPORTES +
                  IN_SALA_LEITURA + QT_SALAS_UTILIZADAS, data = em_)
summary(em_aband_)

### resultados ai, af e em para did tradicional
stargazer(ai_aprov_, ai_reprov_, ai_aband_,
          #af_aprov_, af_reprov_, af_aband_,
          #em_aprov_, em_reprov_, em_aband_,
          type = "latex", title = "Resultados - did tradicional ensino medio",
                   out = "results/tables/did_tradicional_anos_iniciais.tex")

