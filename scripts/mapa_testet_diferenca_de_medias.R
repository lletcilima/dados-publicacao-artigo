####
# Objetivo: Automatizar o teste t da taxa de rendimento para todos os municipios e estados do Brasil
# Criar um data frame que salve todos os resultados
# Criar um mapa municipal e estadual para visualizar os municípios que os testes t deram significativos
# Autor: Fca. Letícia F. Lima
# Data de criação: 26/04/23
# Última modificação: 07/06/2025
###

#### Pacotes ####
#install.packages("sf")
#install.packages("maps")
#install.packages("rgdal")
#install.packages("haven")

library(dplyr)
library(tidyr)
library(ggplot2)
#library(tidyverse)
library(foreign)
library(haven)
library(maps)
library(rgdal)
library(rstatix)
library(sf)
library(stringr)

#### Base de dados - taxa de rendimento escolar do inep ####

gc(reset = TRUE)

tx_rend_painel <- read_dta("dados/tx_rend_painel_2.dta")
tx_rend_filtrado <- tx_rend_painel %>% filter(localizacao == "Urbana", rede != "Privada")



# identificar quantas observações em cada município
cont_munic <- tx_rend_filtrado %>% count(cod_munic)

#### teste t das tx de rendimento a nivel brasil para cada etapa de ensino ####
# Vetor com os nomes das variáveis de interesse
variaveis <- c("tx_aprov_ai", "tx_aprov_af", "tx_aprov_em",
               "tx_reprov_ai", "tx_reprov_af", "tx_reprov_em",
               "tx_aband_ai", "tx_aband_af", "tx_aband_em")

# Lista para armazenar os resultados
resultados_geral <- list()

# Loop pelas variáveis
for (variavel in variaveis) {

  # Monta a fórmula do teste t
  formula_t <- as.formula(paste(variavel, "~ impar"))

  # Executa o teste t no banco completo (sem filtro por região)
  t_teste <- t.test(formula_t, data = tx_rend_filtrado)

  # Armazena os resultados
  resultados_geral[[variavel]] <- data.frame(
    media_grupo_par = t_teste$estimate[1],
    media_grupo_impar = t_teste$estimate[2],
    diferenca_medias = t_teste$estimate[1] - t_teste$estimate[2],
    p_valor = t_teste$p.value,
    variavel = variavel,
    significativo = ifelse(t_teste$p.value <= 0.05, 1, 0)
  )
}

# Consolida os resultados em um único data frame
resultados_geral_df <- do.call(rbind, resultados_geral)

# Salva o resultado geral em um CSV
write.csv2(resultados_geral_df, "resultados/tabelas/teste_t_geral_tx_rendimento.csv", row.names = FALSE)

descritiva_geral <- tx_rend_filtrado %>%
  group_by(impar) %>%
  get_summary_stats(variaveis, type = "mean_sd")

# Salva as estatísticas descritivas
write.csv2(descritiva_geral, "resultados/tabelas/estatisticas_descritivas_geral.csv", row.names = FALSE)

#### teste t das tx de rendimento por regiao para cada etapa de ensino ####

resultados_t_df_aprov_ai <- read.csv2("dados/teste_t_aprov_ai.csv")

resultados_t_df_reprov_ai <- read.csv2("dados/teste_t_reprov_ai.csv")

resultados_t_df_aband_ai <- read.csv2("dados/teste_t_aband_ai.csv")

resultados_t_df_aprov_af <- read.csv2("dados/teste_t_aprov_af.csv")

resultados_t_df_reprov_af <- read.csv2("dados/teste_t_reprov_af.csv")

resultados_t_df_aband_af <- read.csv2("dados/teste_t_aband_af.csv")

resultados_t_df_aprov_em <- read.csv2("dados/teste_t_aprov_em.csv")

resultados_t_df_reprov_em <- read.csv2("dados/teste_t_reprov_em.csv")

resultados_t_df_aband_em <- read.csv2("dados/teste_t_aband_em.csv")

#### mapa por etapa - regioes ####

# Ler o arquivo shapefile com os limites geográficos dos municípios
limites_regioes <- readOGR("D:/dissertacao_leticia/dados/malha_territorial_brasil/BR_UF_2022/BR_UF_2022.shx")
# Converter o objeto Spatial para sf
limites_regioes <- st_as_sf(limites_regioes)
limites_regioes$NM_REGIAO <- limites_regioes$NM_REGIAO %>%
  stringr::str_replace_all("\\n", "") %>%
  stringr::str_trim() %>%
  stringr::str_to_title()  # Coloca a primeira letra maiúscula

# Juntar as informações de código do município e se o teste foi significativo ou não com os dados geográficos dos municípios
limites_regioes <- merge(limites_regioes, resultados_t_df_reprov_em, by.x = "NM_REGIAO", by.y = "uf", all.x = TRUE)

# Criar o mapa
ggplot(limites_regioes) +
  geom_sf(aes(fill = factor(significativo)), color = "black", size = 0.2) +
  scale_fill_manual(values = c("0" = "gray80", "1" = "#1170AA"),
                    name = "Significant",
                    labels = c("No", "Yes"),
                    na.value = "white") +
  theme_minimal() +
  labs(title = "T-Test: Failure – High School", x = "", y = "") +
  theme(legend.position = "right")

#salvar resultados em mapas test-t por regiao
ggsave("resultados/figuras/mapaA1_aprov_ai_regiao.png", width = 4, height = 3, dpi = 300)
ggsave("resultados/figuras/mapaA1_reprov_ai_regiao.png", width = 4, height = 3, dpi = 300)
ggsave("resultados/figuras/mapaA1_aband_ai_regiao.png", width = 4, height = 3, dpi = 300)
ggsave("resultados/figuras/mapaA1_aprov_af_regiao.png", width = 4, height = 3, dpi = 300)
ggsave("resultados/figuras/mapaA1_reprov_af_regiao.png", width = 4, height = 3, dpi = 300)
ggsave("resultados/figuras/mapaA1_aband_af_regiao.png", width = 4, height = 3, dpi = 300)
ggsave("resultados/figuras/mapaA1_aprov_em_regiao.png", width = 4, height = 3, dpi = 300)
ggsave("resultados/figuras/mapaA1_reprov_em_regiao.png", width = 4, height = 3, dpi = 300)
ggsave("resultados/figuras/mapaA1_aband_em_regiao.png", width = 4, height = 3, dpi = 300)


#### teste t das tx de rendimento por estado para cada etapa de ensino ####

resultados_t_df_aprov_ai <- read.csv2("dados/teste_t_aprov_ai_estado.csv")

resultados_t_df_reprov_ai <- read.csv2("dados/teste_t_reprov_ai_estado.csv")

resultados_t_df_aband_ai <- read.csv2("dados/teste_t_aband_ai_estado.csv")

resultados_t_df_aprov_af <- read.csv2("dados/teste_t_aprov_af_estado.csv")

resultados_t_df_reprov_af <- read.csv2("dados/teste_t_reprov_af_estado.csv")

resultados_t_df_aband_af <- read.csv2("dados/teste_t_aband_af_estado.csv")

resultados_t_df_aprov_em <- read.csv2("dados/teste_t_aprov_em_estado.csv")

resultados_t_df_reprov_em <- read.csv2("dados/teste_t_reprov_em_estado.csv")

resultados_t_df_aband_em <- read.csv2("dados/teste_t_aband_em_estado.csv")

#### mapa por etapa - estados ####

# Ler o arquivo shapefile com os limites geográficos dos municípios
limites_estados <- readOGR("D:/dissertacao_leticia/dados/malha_territorial_brasil/BR_UF_2022/BR_UF_2022.shp")
# Converter o objeto Spatial para sf
limites_estados <- st_as_sf(limites_estados)

# Juntar as informações de código do município e se o teste foi significativo ou não com os dados geográficos dos municípios
limites_estados <- merge(limites_estados, resultados_t_df_aband_em, by.x = "SIGLA_UF", by.y = "uf", all.x = TRUE)

# Criar o mapa dos estados
ggplot(limites_estados) +
  geom_sf(aes(fill = factor(significativo)), color = "black", size = 0.2) +
  scale_fill_manual(values = c("0" = "gray80", "1" = "#1170AA"),
                    name = "Significant",
                    labels = c("No", "Yes"),
                    na.value = "white") +
  theme_minimal() +
  labs(title = "T-Test: Dropout – High School", x = "", y = "") +
  theme(legend.position = "right")

#salvar resultados em mapas test-t por regiao
ggsave("resultados/figuras/mapaA2_aprov_ai_estado.png", width = 4, height = 3, dpi = 300)
ggsave("resultados/figuras/mapaA2_reprov_ai_estado.png", width = 4, height = 3, dpi = 300)
ggsave("resultados/figuras/mapaA2_aband_ai_estado.png", width = 4, height = 3, dpi = 300)
ggsave("resultados/figuras/mapaA2_aprov_af_estado.png", width = 4, height = 3, dpi = 300)
ggsave("resultados/figuras/mapaA2_reprov_af_estado.png", width = 4, height = 3, dpi = 300)
ggsave("resultados/figuras/mapaA2_aband_af_estado.png", width = 4, height = 3, dpi = 300)
ggsave("resultados/figuras/mapaA2_aprov_em_estado.png", width = 4, height = 3, dpi = 300)
ggsave("resultados/figuras/mapaA2_reprov_em_estado.png", width = 4, height = 3, dpi = 300)
ggsave("resultados/figuras/mapaA2_aband_em_estado.png", width = 4, height = 3, dpi = 300)
