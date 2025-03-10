rm(list = ls())
library(pacman)
p_load(lubridate,
       jsonlite,
       httr,
       tidyverse,
       dplyr)

load('tabela_final.rda')


tabela_final |> 
  distinct(codigo, nome) |> 
  arrange(nome) |> 
  print(n=Inf)

tabela_final |> 
  distinct(nome) |> 
  arrange(nome) |> 
  print(n=Inf)

tabela_final$nome = str_replace(tabela_final$nome, 'Indenizaçao por Dano Moral', 'Indenização por Dano Moral')





# exlui o assunto GERAL ('Indenização por Dano Moral') caso haja assunto mais específico, segundo tabela CNJ

tabela <- tabela_final |> 
  distinct(processo, nome) |> 
  mutate(n=1) |> 
  pivot_wider(names_from = nome, values_from = n, values_fill = 0) 

tabela$total <- rowSums(tabela[, 2:16])


tabela$`Indenização por Dano Moral` <- if_else(tabela$total > 1 & tabela$`Indenização por Dano Moral` == 1, 0, 1)



tabela <- tabela |> 
  select(-total) |> 
  pivot_longer(cols = 2:16, names_to = 'assunto', values_to = 'n') |> 
  filter(n!=0) |> 
  mutate(trt = paste0('trt',str_sub(processo, 15, 16)),
         vara =  str_sub(processo, 17, 20)) |>
  arrange(trt)
save(tabela, file = 'tabela.rda')
