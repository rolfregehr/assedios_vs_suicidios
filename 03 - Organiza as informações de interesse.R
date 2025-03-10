rm(list = ls())
library(pacman)
p_load(lubridate,
       jsonlite,
       httr,
       tidyverse,
       dplyr)


tabela_final <- tibble(processo = character(),
                       codigo = integer(),
                       nome = character())

assuntos <- c("1855", "2569", "13344", "1723", "1724", "13355", "13380", "13387", "9051", "13411", "13436", "13474", "13475", "13517", "13529", "13560",
              "13195", "13196", "14010", "14016", "14017", "14018", "14019", "14020", "14021", "14022", "14023", "14024", "14025", "14026", "14027", "14028",
              "14029", "14030" )

arquivos_trt <- list.files('dados', full.names = T, pattern = '_2021')

cont = 1
for(arq in arquivos_trt){
load(arq)

numero_de_processos <- length(resultados$hits$hits$`_source`$numeroProcesso)

for (i in 1:numero_de_processos) {
  
  
  if(class(resultados$hits$hits$`_source`$assuntos[i][[1]]) != 'data.frame'){
    if('erros_tabela_assuntos' %in% ls()){
      erros_tabela_assuntos <- bind_rows(erros_tabela_assuntos,
                                         tibble(arquivo = arq,
                                                i_do_processo = i))
    } else {
      erros_tabela_assuntos <- tibble(arquivo = character(),
                                      i_do_processo = integer())
    }
    next}
  
  processo_i <- resultados$hits$hits$`_source`$numeroProcesso[i]
  
  assuntos_i <- resultados$hits$hits$`_source`$assuntos[i][[1]] |> 
    filter(codigo %in% assuntos) |> 
    mutate(processo = processo_i) |> 
    select(processo, everything())
  
  tabela_final = bind_rows(tabela_final, assuntos_i)
  

  
}
print(paste(str_pad(cont, width = 3, pad='0', side = 'left'), '/ 288'))
cont = cont+1  
}





#########################
# Organiza processos com erros



######################################################################
# 1

load(erros_tabela_assuntos$arquivo[1])

resultados$hits$hits$`_source`$numeroProcesso[erros_tabela_assuntos$i_do_processo[1]]
assuntos_temp <- resultados$hits$hits$`_source`$assuntos[erros_tabela_assuntos$i_do_processo[1]] 


tabela_final <- tabela_final |> bind_rows(
  tibble(codigo = integer(unlist(assuntos_temp)[which(unlist(assuntos_temp) %in% assuntos)])) |> 
    mutate(nome = tabela_final |> filter(codigo == '14010') |> distinct(nome) |> pull(),
           processo = resultados$hits$hits$`_source`$numeroProcesso[erros_tabela_assuntos$i_do_processo[1]]) |> 
    select(processo, everything()))


######################################################################
i = 2

load(erros_tabela_assuntos$arquivo[i])

resultados$hits$hits$`_source`$numeroProcesso[erros_tabela_assuntos$i_do_processo[i]]
assuntos_temp <- resultados$hits$hits$`_source`$assuntos[erros_tabela_assuntos$i_do_processo[i]] 

assuntos_temp <- unlist(assuntos_temp)

tabela_final <- tabela_final |> bind_rows(
  tibble(codigo = as.integer(assuntos_temp[which(assuntos_temp %in% assuntos)])) |> 
    mutate(nome = tabela_final |> filter(codigo == '14010') |> distinct(nome) |> pull(),
           processo = resultados$hits$hits$`_source`$numeroProcesso[erros_tabela_assuntos$i_do_processo[i]]) |> 
    select(processo, everything()))


######################################################################
i = 3

load(erros_tabela_assuntos$arquivo[i])

resultados$hits$hits$`_source`$numeroProcesso[erros_tabela_assuntos$i_do_processo[i]]
assuntos_temp <- resultados$hits$hits$`_source`$assuntos[erros_tabela_assuntos$i_do_processo[i]] 

assuntos_temp <- unlist(assuntos_temp)

tabela_final <- tabela_final |> bind_rows(
  tibble(codigo = as.integer(assuntos_temp[which(assuntos_temp %in% assuntos)])) |> 
    mutate(nome = tabela_final |> filter(codigo == '14010') |> distinct(nome) |> pull(),
           processo = resultados$hits$hits$`_source`$numeroProcesso[erros_tabela_assuntos$i_do_processo[i]]) |> 
    select(processo, everything()))


######################################################################
i = 4

load(erros_tabela_assuntos$arquivo[i])

resultados$hits$hits$`_source`$numeroProcesso[erros_tabela_assuntos$i_do_processo[i]]
assuntos_temp <- resultados$hits$hits$`_source`$assuntos[erros_tabela_assuntos$i_do_processo[i]] 

assuntos_temp <- unlist(assuntos_temp)

tabela_final <- tabela_final |> bind_rows(
  tibble(codigo = as.integer(assuntos_temp[which(assuntos_temp %in% assuntos)])) |> 
    mutate(nome = tabela_final |> filter(codigo == '14010') |> distinct(nome) |> pull(),
           processo = resultados$hits$hits$`_source`$numeroProcesso[erros_tabela_assuntos$i_do_processo[i]]) |> 
    select(processo, everything()))


######################################################################
i = 5

load(erros_tabela_assuntos$arquivo[i])

resultados$hits$hits$`_source`$numeroProcesso[erros_tabela_assuntos$i_do_processo[i]]
assuntos_temp <- resultados$hits$hits$`_source`$assuntos[erros_tabela_assuntos$i_do_processo[i]] 

assuntos_temp <- unlist(assuntos_temp)

tabela_final <- tabela_final |> bind_rows(
  tibble(codigo = as.integer(assuntos_temp[which(assuntos_temp %in% assuntos)])) |> 
    mutate(nome = tabela_final |> filter(codigo == '14010') |> distinct(nome) |> pull(),
           processo = resultados$hits$hits$`_source`$numeroProcesso[erros_tabela_assuntos$i_do_processo[i]]) |> 
    select(processo, everything()))


######################################################################
i = 6

load(erros_tabela_assuntos$arquivo[i])

resultados$hits$hits$`_source`$numeroProcesso[erros_tabela_assuntos$i_do_processo[i]]
assuntos_temp <- resultados$hits$hits$`_source`$assuntos[erros_tabela_assuntos$i_do_processo[i]] 

assuntos_temp <- unlist(assuntos_temp)

tabela_final <- tabela_final |> bind_rows(
  tibble(codigo = as.integer(assuntos_temp[which(assuntos_temp %in% assuntos)])) |> 
    mutate(nome = tabela_final |> filter(codigo == '14010') |> distinct(nome) |> pull(),
           processo = resultados$hits$hits$`_source`$numeroProcesso[erros_tabela_assuntos$i_do_processo[i]]) |> 
    select(processo, everything()))

######################################################################
i = 7

load(erros_tabela_assuntos$arquivo[i])

resultados$hits$hits$`_source`$numeroProcesso[erros_tabela_assuntos$i_do_processo[i]]
assuntos_temp <- resultados$hits$hits$`_source`$assuntos[erros_tabela_assuntos$i_do_processo[i]] 

assuntos_temp <- unlist(assuntos_temp)

tabela_final <- tabela_final |> bind_rows(
  tibble(codigo = as.integer(assuntos_temp[which(assuntos_temp %in% assuntos)])) |> 
    mutate(nome = tabela_final |> filter(codigo == '14010') |> distinct(nome) |> pull(),
           processo = resultados$hits$hits$`_source`$numeroProcesso[erros_tabela_assuntos$i_do_processo[i]]) |> 
    select(processo, everything()))

######################################################################
i = 8

load(erros_tabela_assuntos$arquivo[i])

resultados$hits$hits$`_source`$numeroProcesso[erros_tabela_assuntos$i_do_processo[i]]
assuntos_temp <- resultados$hits$hits$`_source`$assuntos[erros_tabela_assuntos$i_do_processo[i]] 

assuntos_temp <- unlist(assuntos_temp)

tabela_final <- tabela_final |> bind_rows(
  tibble(codigo = as.integer(assuntos_temp[which(assuntos_temp %in% assuntos)])) |> 
    mutate(nome = tabela_final |> filter(codigo == '14010') |> distinct(nome) |> pull(),
           processo = resultados$hits$hits$`_source`$numeroProcesso[erros_tabela_assuntos$i_do_processo[i]]) |> 
    select(processo, everything()))



save(tabela_final, file = 'tabela_final.rda')
