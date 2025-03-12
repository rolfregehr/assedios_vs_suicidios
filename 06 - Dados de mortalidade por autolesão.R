rm(list = ls())
library(pacman)
p_load(lubridate,
       jsonlite,
       httr,
       tidyverse,
       dplyr, 
       read.dbc)

cid_ae <- c(paste0('X', 60:84), 'Y87')

mortalidade <- read.dbc('DOBR2021.dbc') # ftp://ftp.datasus.gov.br/dissemin/publicos/SIM/CID10/DORES/

str(mortalidade)

mortalidade_ae <- mortalidade |> 
  select(CODMUNRES,
         CAUSABAS,
         IDADE) |> 
  filter(str_detect(IDADE, '^4')) |> 
  mutate(IDADE = as.integer(str_sub(IDADE, 2, 3))) |> 
  filter(IDADE >= 18,
         CAUSABAS %in% cid_ae | str_sub(CAUSABAS, 1, 3) %in% cid_ae) |> 
  group_by(CODMUNRES) |> 
  summarise(n=n()) |> 
  arrange(-n)
  

save(mortalidade_ae, file = 'mortalidade_ae.rda')
