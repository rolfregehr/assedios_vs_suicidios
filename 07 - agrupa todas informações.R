rm(list = ls())
library(pacman)
p_load(lubridate,
       jsonlite,
       httr,
       tidyverse,
       dplyr, 
       read.dbc)

load("mortalidade_ae.rda")
load("tabela_assedios.rda")
load("dados_trts_agrupados.rda")


grupos <- dados_trts_agrupados |> distinct(grupo)


grupo_mun <- dados_trts_agrupados |>
  mutate(cod_municipio = str_sub(cod_municipio, 1, 6)) |> 
  distinct(grupo, cod_municipio)

mun_ae <- mortalidade_ae |> 
  mutate(CODMUNRES = as.character(CODMUNRES))


grupo_ae <- grupo_mun |> 
  left_join(mun_ae,
            by = c('cod_municipio' = 'CODMUNRES')) |> 
  mutate(n= replace_na(n, 0)) |> 
  group_by(grupo) |> 
  summarise(`N autoextermínio` = sum(n)) |> 
  ungroup()
    

dados <- grupo_ae |> left_join(tabela_assedios |> group_by(grupo, assunto) |> 
                                 summarise(num_processos=n()) |> 
                                 pivot_wider(names_from = assunto, values_from = num_processos, values_fill = 0),
                               by = 'grupo')


dados[is.na(dados)] <- 0

dados <- dados |> remove_rownames() |> column_to_rownames(var = 'grupo')

save(dados, file = 'dados.rda')
