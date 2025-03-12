 

library(httr)

#Substituir <API Key> pela Chave Pública
headers = c(
  'Authorization' = 'ApiKey cDZHYzlZa0JadVREZDJCendQbXY6SkJlTzNjLV9TRENyQk1RdnFKZGRQdw==',
  'Content-Type' = 'application/json'
)

body = '{
  "query": {
    "match": {
      "numeroProcesso": "00105997620215030026"
    }
  }
}';

res <- VERB("POST",
            url = 'https://api-publica.datajud.cnj.jus.br/api_publica_trt3/_search',
            body = body,
            add_headers(headers))

resultados <- fromJSON(content(res, 'text', encoding = 'UTF-8')) 

resultados$hits$hits$`_source`$movimentos[[1]]
