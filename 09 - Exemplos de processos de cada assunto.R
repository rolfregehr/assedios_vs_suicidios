rm(list = ls())
library(pacman)
p_load(lubridate,
       jsonlite,
       httr,
       tidyverse,
       dplyr)

load("tabela_assedios.rda")

assuntos <- unique(tabela_assedios$assunto)


processos_exemplo <- tabela_assedios |>
  group_by(processo) |> 
  summarise(n=n()) |> 
  filter(n==1) |> 
  pull(processo)

# Acidente de Trabalho

tabela_assedios |> 
  filter(assunto == assuntos[1],
         processo %in% processos_exemplo) |> 
  sample_n(10)


tabela_assedios |> 
  filter(assunto == assuntos[2],
         processo %in% processos_exemplo) |> 
  sample_n(10)


tabela_assedios |> 
  filter(assunto == assuntos[3],
         processo %in% processos_exemplo) |> 
  sample_n(10) |> 
  arrange(trt)

tabela_assedios |> 
  filter(assunto == assuntos[4],
         processo %in% processos_exemplo) |> 
  sample_n(10) |> 
  arrange(trt)

tabela_assedios |> 
  filter(assunto == assuntos[5],
         processo %in% processos_exemplo) |> 
  sample_n(10) |> 
  arrange(trt)


tabela_assedios |> 
  filter(assunto == assuntos[6],
         processo %in% processos_exemplo) |> 
  sample_n(10) |> 
  arrange(trt)



tabela_assedios |> 
  filter(assunto == assuntos[7],
         processo %in% processos_exemplo) |> 
  sample_n(10) |> 
  arrange(trt) |> 
  mutate(link = paste0('https://pje.',
                       'trt',
                       str_remove(trt, 'trt0|trt'),
                       '.',
                       'jus.br/consultaprocessual/detalhe-processo/',
                       str_sub(processo, 1, 7),
                       '-',
                       str_sub(processo, 8, 9),
                       '.',
                       str_sub(processo, 10, 13),
                       '.',
                       str_sub(processo, 14, 14),
                       '.',
                       str_sub(processo, 15, 16),
                       '.',
                       str_sub(processo, 17, 20)
                       
  ))


tabela_assedios |> 
  filter(assunto == assuntos[8],
         processo %in% processos_exemplo) |> 
  sample_n(10) |> 
  arrange(trt) |> 
  mutate(link = paste0('https://pje.',
                       'trt',
                       str_remove(trt, 'trt0|trt'),
                       '.',
                       'jus.br/consultaprocessual/detalhe-processo/',
                       str_sub(processo, 1, 7),
                       '-',
                       str_sub(processo, 8, 9),
                       '.',
                       str_sub(processo, 10, 13),
                       '.',
                       str_sub(processo, 14, 14),
                       '.',
                       str_sub(processo, 15, 16),
                       '.',
                       str_sub(processo, 17, 20)
                       
  )) |> 
  pull(link)


tabela_assedios |> 
  filter(assunto == assuntos[9],
         processo %in% processos_exemplo) |> 
  sample_n(10) |> 
  arrange(trt) |> 
  mutate(link = paste0('https://pje.',
                       'trt',
                       str_remove(trt, 'trt0|trt'),
                       '.',
                       'jus.br/consultaprocessual/detalhe-processo/',
                       str_sub(processo, 1, 7),
                       '-',
                       str_sub(processo, 8, 9),
                       '.',
                       str_sub(processo, 10, 13),
                       '.',
                       str_sub(processo, 14, 14),
                       '.',
                       str_sub(processo, 15, 16),
                       '.',
                       str_sub(processo, 17, 20)
                       
  )) |> 
  distinct(assunto,link)


tabela_assedios |> 
  filter(assunto == assuntos[10],
         processo %in% processos_exemplo) |> 
  sample_n(10) |> 
  arrange(trt) |> 
  mutate(link = paste0('https://pje.',
                       'trt',
                       str_remove(trt, 'trt0|trt'),
                       '.',
                       'jus.br/consultaprocessual/detalhe-processo/',
                       str_sub(processo, 1, 7),
                       '-',
                       str_sub(processo, 8, 9),
                       '.',
                       str_sub(processo, 10, 13),
                       '.',
                       str_sub(processo, 14, 14),
                       '.',
                       str_sub(processo, 15, 16),
                       '.',
                       str_sub(processo, 17, 20)
                       
  )) |> 
  distinct(assunto,link)


tabela_assedios |> 
  filter(assunto == assuntos[11],
         processo %in% processos_exemplo) |> 
  sample_n(10) |> 
  arrange(trt) |> 
  mutate(link = paste0('https://pje.',
                       'trt',
                       str_remove(trt, 'trt0|trt'),
                       '.',
                       'jus.br/consultaprocessual/detalhe-processo/',
                       str_sub(processo, 1, 7),
                       '-',
                       str_sub(processo, 8, 9),
                       '.',
                       str_sub(processo, 10, 13),
                       '.',
                       str_sub(processo, 14, 14),
                       '.',
                       str_sub(processo, 15, 16),
                       '.',
                       str_sub(processo, 17, 20)
                       
  )) |> 
  distinct(assunto,link)


tabela_assedios |> 
  filter(assunto == assuntos[12],
         processo %in% processos_exemplo) |> 
  sample_n(10) |> 
  arrange(trt) |> 
  mutate(link = paste0('https://pje.',
                       'trt',
                       str_remove(trt, 'trt0|trt'),
                       '.',
                       'jus.br/consultaprocessual/detalhe-processo/',
                       str_sub(processo, 1, 7),
                       '-',
                       str_sub(processo, 8, 9),
                       '.',
                       str_sub(processo, 10, 13),
                       '.',
                       str_sub(processo, 14, 14),
                       '.',
                       str_sub(processo, 15, 16),
                       '.',
                       str_sub(processo, 17, 20)
                       
  )) |> 
  distinct(assunto,link)


tabela_assedios |> 
  filter(assunto == assuntos[13],
         processo %in% processos_exemplo) |> 
  arrange(trt) |> 
  mutate(link = paste0('https://pje.',
                       'trt',
                       str_remove(trt, 'trt0|trt'),
                       '.',
                       'jus.br/consultaprocessual/detalhe-processo/',
                       str_sub(processo, 1, 7),
                       '-',
                       str_sub(processo, 8, 9),
                       '.',
                       str_sub(processo, 10, 13),
                       '.',
                       str_sub(processo, 14, 14),
                       '.',
                       str_sub(processo, 15, 16),
                       '.',
                       str_sub(processo, 17, 20)
                       
  )) |> 
  distinct(assunto,link)




tabela_assedios |> 
  filter(assunto == assuntos[14],
         processo %in% processos_exemplo) |> 
  arrange(trt) |> 
  mutate(link = paste0('https://pje.',
                       'trt',
                       str_remove(trt, 'trt0|trt'),
                       '.',
                       'jus.br/consultaprocessual/detalhe-processo/',
                       str_sub(processo, 1, 7),
                       '-',
                       str_sub(processo, 8, 9),
                       '.',
                       str_sub(processo, 10, 13),
                       '.',
                       str_sub(processo, 14, 14),
                       '.',
                       str_sub(processo, 15, 16),
                       '.',
                       str_sub(processo, 17, 20)
                       
  )) |> 
  distinct(assunto,link)

tabela_assedios |> 
  filter(assunto == assuntos[15],
         processo %in% processos_exemplo) |> 
  sample_n(10) |> 
  arrange(trt) |> 
  mutate(link = paste0('https://pje.',
                       'trt',
                       str_remove(trt, 'trt0|trt'),
                       '.',
                       'jus.br/consultaprocessual/detalhe-processo/',
                       str_sub(processo, 1, 7),
                       '-',
                       str_sub(processo, 8, 9),
                       '.',
                       str_sub(processo, 10, 13),
                       '.',
                       str_sub(processo, 14, 14),
                       '.',
                       str_sub(processo, 15, 16),
                       '.',
                       str_sub(processo, 17, 20)
                       
  )) |> 
  distinct(assunto,link)

