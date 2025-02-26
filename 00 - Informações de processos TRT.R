rm(list = ls())
library(pacman)
p_load(lubridate,
       jsonlite,
       httr,
       tidyverse,
       dplyr)

# Cria acesso à API da Justiça do Trabalho ####

## Cabeçalho ####
headers = c(
  'Authorization' = 'ApiKey cDZHYzlZa0JadVREZDJCendQbXY6SkJlTzNjLV9TRENyQk1RdnFKZGRQdw==',
  'Content-Type' = 'application/json'
)


## Processos por data de ajuizamento ####



## Define endpoints dos TRTs ####
url_tribunais <- c('https://api-publica.datajud.cnj.jus.br/api_publica_trt2/_search',
                   'https://api-publica.datajud.cnj.jus.br/api_publica_trt15/_search',
                   'https://api-publica.datajud.cnj.jus.br/api_publica_trt1/_search',
                   'https://api-publica.datajud.cnj.jus.br/api_publica_trt3/_search',
                   'https://api-publica.datajud.cnj.jus.br/api_publica_trt4/_search',
                   'https://api-publica.datajud.cnj.jus.br/api_publica_trt5/_search',
                   'https://api-publica.datajud.cnj.jus.br/api_publica_trt6/_search',
                   'https://api-publica.datajud.cnj.jus.br/api_publica_trt7/_search',
                   'https://api-publica.datajud.cnj.jus.br/api_publica_trt8/_search',
                   'https://api-publica.datajud.cnj.jus.br/api_publica_trt9/_search',
                   'https://api-publica.datajud.cnj.jus.br/api_publica_trt10/_search',
                   'https://api-publica.datajud.cnj.jus.br/api_publica_trt11/_search',
                   'https://api-publica.datajud.cnj.jus.br/api_publica_trt12/_search',
                   'https://api-publica.datajud.cnj.jus.br/api_publica_trt13/_search',
                   'https://api-publica.datajud.cnj.jus.br/api_publica_trt14/_search',
                   'https://api-publica.datajud.cnj.jus.br/api_publica_trt16/_search',
                   'https://api-publica.datajud.cnj.jus.br/api_publica_trt17/_search',
                   'https://api-publica.datajud.cnj.jus.br/api_publica_trt18/_search',
                   'https://api-publica.datajud.cnj.jus.br/api_publica_trt19/_search',
                   'https://api-publica.datajud.cnj.jus.br/api_publica_trt20/_search',
                   'https://api-publica.datajud.cnj.jus.br/api_publica_trt21/_search',
                   'https://api-publica.datajud.cnj.jus.br/api_publica_trt22/_search',
                   'https://api-publica.datajud.cnj.jus.br/api_publica_trt23/_search',
                   'https://api-publica.datajud.cnj.jus.br/api_publica_trt24/_search')

load('controle.rda')

# Loop para tribunais e datas ####


for(i in 2:length(url_tribunais)){
  
  datas <- seq.Date(as.Date('2005-01-01'), as.Date('2024-12-31'), by = 'months')
  
  if(i==2){datas <- seq.Date(as.Date('2023-06-01'), as.Date('2024-12-31'), by = 'months')}
    
  for(dia_i in datas){
    
    
    if(length(which(controle$tribunal == url_tribunais[i] & controle$data == dia_i)) > 0){next}
    
    dia_i = as.Date(dia_i)
    dia_f = (seq(dia_i,length=2,by="months")-1)[2]
    
    
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



# Recoleta para n == 10000 ####