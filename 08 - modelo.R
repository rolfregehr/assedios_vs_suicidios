rm(list = ls())
library(pacman)
p_load(lubridate,
       jsonlite,
       httr,
       tidyverse,
       dplyr, 
       read.dbc,
       broom,
       nlme,
       lme4 ,
       PerformanceAnalytics,
       car)


# https://www.r-bloggers.com/2017/12/linear-mixed-effect-models-in-r/

load('dados.rda')

modelo <- glm(`N autoextermínio` ~ ., data = dados, family = poisson())

summary(modelo)


summary(dados)

hist(modelo$residuals)

chart.Correlation(dados, histogram = TRUE, method = "pearson")

dados$total = rowSums(dados[, 2:16])
modelo <-  glm(`N autoextermínio` ~ total, data = dados, family = poisson())
summary(modelo)




# Centralização de dados 

dados$`Acidente de Trabalho` <- scale(dados$`Acidente de Trabalho`)
dados$`Anotação/Retenção da CTPS`<- scale(dados$`Anotação/Retenção da CTPS`)
dados$`Assédio Moral`<- scale(dados$`Assédio Moral`)
dados$`Assédio Sexual`<- scale(dados$`Assédio Sexual`)
dados$`Atos Discriminatórios`<- scale(dados$`Atos Discriminatórios`)
dados$`Condições Degradantes`<- scale(dados$`Condições Degradantes`)
dados$`Controle de Correspondência Eletrônica`<- scale(dados$`Controle de Correspondência Eletrônica`)
dados$`Desconfiguração de Justa Causa`<- scale(dados$`Desconfiguração de Justa Causa`)
dados$`Doença Ocupacional`<- scale(dados$`Doença Ocupacional`)
dados$`Indenização por Dano Moral`<- scale(dados$`Indenização por Dano Moral`)
dados$`Limitação de Uso do Banheiro`<- scale(dados$`Limitação de Uso do Banheiro`)
dados$`Lista Suja`<- scale(dados$`Lista Suja`)
dados$`Quebra de sigilo Bancário`<- scale(dados$`Quebra de sigilo Bancário`)
dados$`Revistas Íntimas/Pertences`<- scale(dados$`Revistas Íntimas/Pertences`)
dados$`Valor Arbitrado`<- scale(dados$`Valor Arbitrado`)



modelo <- glm(`N autoextermínio` ~ ., data = dados, family = poisson())

summary(modelo)


summary(dados)

hist(modelo$residuals)

chart.Correlation(dados, histogram = TRUE, method = "pearson")

dados$total = rowSums(dados[, 2:16])
modelo <-  glm(`N autoextermínio` ~ total, data = dados, family = poisson())
summary(modelo)

