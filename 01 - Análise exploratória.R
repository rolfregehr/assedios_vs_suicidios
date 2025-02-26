library(dplyr)
library(tidyverse)
library(forecast)
library(zoo)

load('controle.rda')

controle |> 
  group_by(data, tribunal) |> 
  summarise(n=sum(num_processos)) |> 
  filter(n==10000) |> 
  print(n=Inf)

controle |> 
  group_by(data) |> 
  summarise(n=sum(num_processos)) |> 
  mutate(acum_1_ano = rollapply(n, width = 12, FUN = sum, fill = 0, align = 'right')) |>
  ggplot(aes(x = data, y = acum_1_ano))+
  geom_line()



controle |> 
  group_by(data) |> 
  summarise(n=sum(num_processos)) |> 
  ggplot(aes(x = data, y = n))+
  geom_line()
