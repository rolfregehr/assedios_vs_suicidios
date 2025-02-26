rm(list = ls())
load('controle.rda')

library(pacman)
p_load(lubridate,
       jsonlite,
       httr,
       tidyverse,
       dplyr)

# Gera info para nova coleta ####
info_10000 <- controle |> filter(num_processos == 10000) |> 
  mutate(data_fim = as.Date(cut((data+32), 'month'))-1)

# Exclusão pelo limite máximo de 10000 itens da API
# Possível perda de informações


# NOVA COLETA ####


## Cria acesso à API da Justiça do Trabalho ####

### Cabeçalho ####
headers = c(
  'Authorization' = 'ApiKey cDZHYzlZa0JadVREZDJCendQbXY6SkJlTzNjLV9TRENyQk1RdnFKZGRQdw==',
  'Content-Type' = 'application/json'
)


### Processos por data de ajuizamento ####






## Loop para tribunais e datas ####
url_tribunais <- info_10000$tribunal

for(i in 1:nrow(info_10000)){
  
  
  
  datas <- seq.Date(info_10000$data[i], info_10000$data_fim[i], by='days')
  

  for(dia_i in datas){
    
    dia_i = as.Date(dia_i)
    dia_f = as.Date(dia_i)
    
    
    body = '{
  "size": 10000,
  "query": {
        "bool": {
            "must": [
    {"range": {"dataAjuizamento": {"gte": "AAAI-MI-DI",
                                   "lte": "AAAF-MF-DF"}}},
                {"terms": {"assuntos.codigo": [ "1855", "2569", "13344", "1723", "1724", "13355", "13380", "13387", "9051", "13411", "13436", "13474", "13475", "13517", "13529", "13560", "13195", "13196", "14010", "14016", "14017", "14018", "14019", "14020", "14021", "14022", "14023", "14024", "14025", "14026", "14027", "14028", "14029", "14030" ]}}
            ]
        }
   }
}';
    
    ## Substitui o dia ####
    body = str_replace(body, "AAAI", as.character(year(dia_i)))
    body = str_replace(body, "MI", str_pad(as.character(month(dia_i)), width = 2, pad = '0'))
    body = str_replace(body, "DI", str_pad(as.character(day(dia_i)), width = 2, pad = '0'))
    
    body = str_replace(body, "AAAF", as.character(year(dia_f)))
    body = str_replace(body, "MF", str_pad(as.character(month(dia_f)), width = 2, pad = '0'))
    body = str_replace(body, "DF", str_pad(as.character(day(dia_f)), width = 2, pad = '0'))
    
    ## Consulta API ####
    res <- VERB("POST",
                url = url_tribunais[i],
                body = body,
                add_headers(headers))
    
    resultados <- fromJSON(content(res, 'text', encoding = 'UTF-8'))    
    
    if(length(resultados$hits$hits$`_source`$numeroProcesso) == 0){next}
    
    ## Salva o rda ####  
    save(resultados,
         file = paste0('./dados/',str_extract(url_tribunais[i], 'trt[0-9]{1,2}'),
                       '_',
                       year(dia_i),
                       str_pad(month(dia_i), pad = '0', width = 2),
                       str_pad(day(dia_i), pad = '0', width = 2),
                       '.rda')
    )
    
    print(paste0(str_extract(url_tribunais[i], 'trt[0-9]{1,2}'),
                 '_',
                 year(dia_i),
                 str_pad(month(dia_i), pad = '0', width = 2),
                 str_pad(day(dia_i), pad = '0', width = 2),
                 '_',
                 length(resultados$hits$hits$`_source`$numeroProcesso),
                 '.rda'))
    
    controle <- bind_rows(controle, 
                          tibble(tribunal = url_tribunais[i],
                                 data = dia_i,
                                 num_processos = length(resultados$hits$hits$`_source`$numeroProcesso)))
    save(controle, file = 'controle.rda')
    
    
  }
}


