rm(list = ls())
library(pacman)
p_load(lubridate,
       jsonlite,
       httr,
       tidyverse,
       dplyr)
load('tabela.rda')

load("dados_trts_agrupados.rda")

tabela$grupo <- NA



for (i in 1:nrow(dados_trts_agrupados)) {
  

trt_i <- dados_trts_agrupados$trt[i]
vara_i <- dados_trts_agrupados$vara[i]
grupo <- dados_trts_agrupados$grupo[i]




tabela$grupo[which(tabela$trt == trt_i & tabela$vara == vara_i)] <- grupo

print(i)

}

  
tabela$grupo[which(tabela$trt == 'trt01' & tabela$vara == '0581')] <- 'G_014'

tabela |> 
  filter(vara != '0000',
         is.na(grupo)) 


tabela$grupo[which(tabela$trt == 'trt02' & tabela$vara == '0251')] <- 'G_033'



tabela |> 
  filter(vara != '0000',
         is.na(grupo)) 


tabela$grupo[which(tabela$trt == 'trt03' & tabela$vara == '0158')] <- 'G_095'



tabela |> 
  filter(vara != '0000',
         is.na(grupo),
         trt != 'trt02' & vara != '2100') 


tabela$grupo[which(tabela$trt == 'trt05' & tabela$vara == '0311')] <- 'G_179'



tabela |> 
  filter(vara != '0000',
         is.na(grupo),
         trt != 'trt02' & vara != '2100') 


tabela$grupo[which(tabela$trt == 'trt05' & tabela$vara == '0521')] <- 'G_179'



tabela |> 
  filter(vara != '0000',
         is.na(grupo),
         trt != 'trt02' & vara != '2100') 


tabela$grupo[which(tabela$trt == 'trt06' & tabela$vara == '0292')] <- 'G_226'



tabela |> 
  filter(vara != '0000',
         is.na(grupo),
         trt != 'trt02' & vara != '2100') 


tabela$grupo[which(tabela$trt == 'trt06' & tabela$vara == '0193')] <- 'G_216'



tabela |> 
  filter(vara != '0000',
         is.na(grupo),
         trt != 'trt02' & vara != '2100') 


tabela$grupo[which(tabela$trt == 'trt06' & tabela$vara == '0262')] <- 'G_223'


tabela |> 
  filter(vara != '0000',
         is.na(grupo),
         trt != 'trt02' & vara != '2100') 


tabela$grupo[which(tabela$trt == 'trt07' & tabela$vara == '0037')] <- 'G_243'


tabela |> 
  filter(vara != '0000',
         is.na(grupo),
         trt != 'trt02' & vara != '2100') 


tabela$grupo[which(tabela$trt == 'trt08' & tabela$vara == '0106')] <- 'G_259'


tabela |> 
  filter(vara != '0000',
         is.na(grupo),
         trt != 'trt02' & vara != '2100') 


tabela$grupo[which(tabela$trt == 'trt08' & tabela$vara == '0108')] <- 'G_262'


tabela |> 
  filter(vara != '0000',
         is.na(grupo),
         trt != 'trt02' & vara != '2100') 


tabela$grupo[which(tabela$trt == 'trt08' & tabela$vara == '0104')] <- 'G_254'


tabela |> 
  filter(vara != '0000',
         is.na(grupo),
         trt != 'trt02' & vara != '2100') 


tabela$grupo[which(tabela$trt == 'trt09' & tabela$vara == '0594')] <- 'G_299'


tabela |> 
  filter(vara != '0000',
         is.na(grupo),
         trt != 'trt02' & vara != '2100') 


tabela$grupo[which(tabela$trt == 'trt09' & tabela$vara == '3668')] <- 'G_312'


tabela |> 
  filter(vara != '0000',
         is.na(grupo),
         trt != 'trt02' & vara != '2100') 


tabela$grupo[which(tabela$trt == 'trt09' & tabela$vara == '1980')] <- 'G_312'


tabela |> 
  filter(vara != '0000',
         is.na(grupo),
         trt != 'trt02' & vara != '2100') 


tabela$grupo[which(tabela$trt == 'trt13' & tabela$vara == '0017')] <- 'G_365'


tabela |> 
  filter(vara != '0000',
         is.na(grupo),
         trt != 'trt02' & vara != '2100') 


tabela$grupo[which(tabela$trt == 'trt13' & tabela$vara == '0018')] <- 'G_363'


tabela |> 
  filter(vara != '0000',
         is.na(grupo),
         trt != 'trt02' & vara != '2100') 


tabela$grupo[which(tabela$trt == 'trt13' & tabela$vara == '0028')] <- 'G_368'


tabela |> 
  filter(vara != '0000',
         is.na(grupo),
         trt != 'trt02' & vara != '2100') 


tabela$grupo[which(tabela$trt == 'trt13' & tabela$vara == '0020')] <- 'G_368'


tabela |> 
  filter(vara != '0000',
         is.na(grupo),
         trt != 'trt02' & vara != '2100') 


tabela$grupo[which(tabela$trt == 'trt13' & tabela$vara == '0015')] <- 'G_368'


tabela |> 
  filter(vara != '0000',
         is.na(grupo),
         trt != 'trt02' & vara != '2100') 


tabela$grupo[which(tabela$trt == 'trt14' & tabela$vara == '0007')] <- 'G_369'


tabela |> 
  filter(vara != '0000',
         is.na(grupo),
         trt != 'trt02' & vara != '2100') 


tabela$grupo[which(tabela$trt == 'trt14' & tabela$vara == '0006')] <- 'G_369'


tabela |> 
  filter(vara != '0000',
         is.na(grupo),
         trt != 'trt02' & vara != '2100') 


tabela$grupo[which(tabela$trt == 'trt14' & tabela$vara == '0092')] <- 'G_376'


tabela |> 
  filter(vara != '0000',
         is.na(grupo),
         trt != 'trt02' & vara != '2100') 


tabela$grupo[which(tabela$trt == 'trt14' & tabela$vara == '0008')] <- 'G_369'


tabela |> 
  filter(vara != '0000',
         is.na(grupo),
         trt != 'trt02' & vara != '2100') 


tabela$grupo[which(tabela$trt == 'trt15' & tabela$vara == '0114')] <- 'G_389'


tabela |> 
  filter(vara != '0000',
         is.na(grupo),
         trt != 'trt02' & vara != '2100') 


tabela$grupo[which(tabela$trt == 'trt15' & tabela$vara == '0044')] <- 'G_404'


tabela |> 
  filter(vara != '0000',
         is.na(grupo),
         trt != 'trt02' & vara != '2100') 


tabela$grupo[which(tabela$trt == 'trt15' & tabela$vara == '0067')] <- 'G_392'



tabela |> 
  filter(vara != '0000',
         is.na(grupo),
         trt != 'trt02' & vara != '2100') 


tabela$grupo[which(tabela$trt == 'trt15' & tabela$vara == '0161')] <- 'G_390'




tabela |> 
  filter(vara != '0000',
         is.na(grupo),
         trt != 'trt02' & vara != '2100') 


tabela$grupo[which(tabela$trt == 'trt21' & tabela$vara == '0041')] <- 'G_555'





tabela |> 
  filter(vara != '0000',
         is.na(grupo),
         trt != 'trt02' & vara != '2100') 


tabela$grupo[which(tabela$trt == 'trt21' & tabela$vara == '0042')] <- 'G_555'





tabela |> 
  filter(vara != '0000',
         is.na(grupo),
         trt != 'trt02' & vara != '2100') 


tabela$grupo[which(tabela$trt == 'trt21' & tabela$vara == '0043')] <- 'G_555'



tabela |> 
  filter(vara != '0000',
         is.na(grupo),
         trt != 'trt02' & vara != '2100') 


tabela$grupo[which(tabela$trt == 'trt21' & tabela$vara == '0021')] <- 'G_562'



tabela |> 
  filter(vara != '0000',
         is.na(grupo),
         trt != 'trt02' & vara != '2100') 


tabela$grupo[which(tabela$trt == 'trt22' & tabela$vara == '0105')] <- 'G_564'



tabela |> 
  filter(vara != '0000',
         is.na(grupo),
         trt != 'trt02' & vara != '2100') 


tabela$grupo[which(tabela$trt == 'trt22' & tabela$vara == '0110')] <- 'G_567'



tabela |> 
  filter(vara != '0000',
         is.na(grupo),
         trt != 'trt02' & vara != '2100') 


tabela$grupo[which(tabela$trt == 'trt23' & tabela$vara == '0108')] <- 'G_568'



tabela |> 
  filter(vara != '0000',
         is.na(grupo),
         trt != 'trt02' & vara != '2100') 


tabela$grupo[which(tabela$trt == 'trt23' & tabela$vara == '0116')] <- 'G_583'



tabela |> 
  filter(vara != '0000',
         is.na(grupo),
         trt != 'trt02' & vara != '2100') 


tabela$grupo[which(tabela$trt == 'trt23' & tabela$vara == '0146')] <- 'G_589'



tabela |> 
  filter(vara != '0000',
         is.na(grupo),
         trt != 'trt02' & vara != '2100') 


tabela$grupo[which(tabela$trt == 'trt23' & tabela$vara == '0136')] <- 'G_583'


tabela_assedios <- tabela

save(tabela_assedios, file = 'tabela_assedios.rda')
