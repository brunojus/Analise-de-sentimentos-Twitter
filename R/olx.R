# funcoes


# pacotes
library(magrittr) # não vivo sem esse pacote
library(rvest) # principal pacote para web-scraping
library(readr) # usado para extrair numeros de texto
library(stringr) # usado para o data cleaning
library(curl) # usado como suporte para o rvest
library(tidyr) # data cleaningry
library(dplyr) # data cleaning
library(ggmap)
library(DT); d <- datatable
library(lubridate)


# definir função para limpar strings coletadas
limparString <- function(x) {
  # x = string coletado do olx
  x %<>% str_replace_all("[\t]", "")
  x %<>% str_replace_all("[\n]", "")
  x %<>% str_replace_all("Apartamentos", "")
  x %<>% str_replace_all("Anúncio Profissional", "")
  x %<>% str_replace("-", "")
  x %<>% str_replace_all("[R$]", "")
  x %<>% str_replace_all("[.]", "")
  x %<>% str_trim()
  return(x)
}


extrairCEP <- function(url) {
  # url = url de um quarto
  mycurl <- curl(url, handle = curl::new_handle("useragent" = "Mozilla/5.0"))
  url <- read_html(mycurl, encoding = "ISO8859-1")
  #url <- read_html(url, encoding = "ISO8859-1")
  #url <- html_nodes(url, ".OLXad-location-map") deprecated
  
  # if clause para pegar casos em que o node id é diferente
  if (length(html_nodes(url, ".OLXad-location-map")) > 0) {
    url %<>% html_nodes(".OLXad-location-map")
  } else {
    url %<>% html_nodes(".OLXad-location")
  }
  
  url <- html_nodes(url, "p")
  url <- url[2]
  url <- html_text(url)
  cep <- limparString(url)
  cep <- readr::parse_number(cep)
  return(cep)
}


extrairAnuncios <- function(url_pagina, info_adicional) {
  ### INPUTS:
  # url_pagina: url de uma pagina do olx com uma lista de links de anúncios.
  # info_adicional: variavel booleana. se verdadeiro, faz o scraping de dados adicionais do anuncio
  # ... deve ser usado apenas para apartamentos, pois a sintaxe do html para quartos é diferente
  mycurl <- curl(url_pagina, handle = curl::new_handle("useragent" = "Mozilla/5.0"))
  mycurl <- read_html(mycurl)
  
  x <- mycurl %>% html_nodes(".OLXad-list-link")
  
  # extrair link do anuncio
  col_links <- mycurl %>% html_nodes(".OLXad-list-link") %>% html_attr("href")
  # extrair titulo do anuncio
  col_titles <- mycurl %>% html_nodes(".OLXad-list-link") %>% html_attr("title")
  # extrair preço
  precos <- lapply(x, . %>% html_nodes(".col-3"))
  precos %<>% lapply(html_text)
  precos %<>% unlist()
  precos %<>% limparString()
  precos %<>% as.numeric()
  col_precos <- precos
  # extrair bairros
  bairros <- mycurl %>% html_nodes(".OLXad-list-line-2") %>% html_text()
  bairros %<>% str_replace_all("[\t]", "")
  bairros %<>% str_replace_all("[\n]", "")
  bairros %<>% str_replace_all("Apartamentos", "")
  bairros %<>% str_replace_all("Aluguel de quartos", "")
  bairros %<>% str_replace_all("Profissional", "")
  bairros %<>% str_replace_all("Casas", "")
  bairros %<>% str_replace_all("Anúncio", "")
  bairros %<>% str_replace("-", "")
  bairros %<>% str_trim()
  col_bairros <- bairros
  # extrair data do anuncio
  html_nodes_lapply <- function(nodes_list, sub_node) {lapply(nodes_list, . %>% html_nodes(sub_node))}
  
  data <- x %>%
    html_nodes_lapply(".col-4") %>%
    lapply(html_text) %>%
    unlist %>%
    # limpar strings. Output:Hojehh:mm
    limparString() %>%
    substr(1, nchar(.) - 5)
  # extrair informações adicionais de apartamento
  if (info_adicional) {
    adicional <- mycurl %>% html_nodes(".OLXad-list-line-1") %>% html_nodes("p") %>% html_text()
    adicional %<>% str_replace_all("[\t]", "")
    adicional %<>% str_replace_all("[\n]", "")
    col_adicionais <- adicional
    
  }
  
  ## novo: extrair data e horario do anuncio
  
  x <- data.frame(
    link = col_links,
    titulo = col_titles,
    aluguel = col_precos,
    bairro = col_bairros,
    data_anuncio = data,
    stringsAsFactors = FALSE
  )
  
  if (info_adicional) {x[["adicional"]] <- col_adicionais}
  
  return(x)
  # return(data.frame(link = col_links,
  #                 titulo = col_titles,
  #                 preco = col_precos,
  #                 bairro = col_bairros,
  #                 adicional = col_adicionais,
  #                 stringsAsFactors = FALSE))
}

postal<-function(cep){
  # converter cep em endereço
  library(httr)
  l<-list()
  for(i in seq_along(cep)){
    cep <- stringr::str_replace(cep,"\\D","")
    cep <- stringr::str_pad(cep,8,side="left",pad="0")
    cep <- as.character(cep)
    url <- paste0("http://correiosapi.apphb.com/cep/",cep)
    a <- GET(url[i])
    b <- content(a,as="parsed")
    l[[i]] <- b
  }
  x <- as.data.frame(do.call("rbind",l))
  for (col in 1:ncol(x)) {x[, col] <- as.character(x[, col])}
  return(x)
}


extrairAdicionais <- function(vetor_adicional) {
  # COLUNA DE QUANTIDADE DE QUARTOS
  # Quarto: pegar posicao inicial e final do string quarto
  # Localizar trecho dentro do string referente a quartos
  matriz_posicao <- str_locate(vetor_adicional, "quarto")
  # Voltar 2 posições no string para pegar o número (ex: 2 quarto)
  matriz_posicao[,1] <- matriz_posicao[,1] - 2
  # extrair string com posições iniciais e finais
  vetor_quartos <- str_sub(vetor_adicional, matriz_posicao[,1], matriz_posicao[,2])
  # extrair apenas número (primeiro caractere do string) e converter para numeric
  vetor_quartos <- str_sub(vetor_quartos, 1, 1)
  vetor_quartos %<>% as.numeric()
  
  
  # Condominio
  # retirar cifrao pra ficar mais facil
  vetor_adicional %<>% str_replace_all("\\$", "S")
  matriz_posicao <- str_locate(vetor_adicional, "Condomínio: RS ")
  # mover cinco posicoes para pegar algarismos após o RS
  vetor_taxa <- str_sub(vetor_adicional, matriz_posicao[, 2], matriz_posicao[, 2] + 4)
  # extrair apenas numeros
  vetor_taxa %<>% parse_number()
  
  # Área
  matriz_posicao <- str_locate(vetor_adicional, " m²")
  # voltar quatro posições
  vetor_area <- str_sub(vetor_adicional, matriz_posicao[,1] - 4, matriz_posicao[, 1])
  # converter para numerico
  vetor_area %<>% parse_number()
  
  # Garagem
  matriz_posicao <- str_locate(vetor_adicional, " vaga")
  # voltar quatro posições
  vetor_garagem <- str_sub(vetor_adicional, matriz_posicao[,1] - 2, matriz_posicao[, 1])
  # converter para numerico
  vetor_garagem %<>% readr::parse_number()
  
  return(data.frame(
    quartos = vetor_quartos,
    condominio = vetor_taxa,
    area = vetor_area,
    garagem = vetor_garagem,
    stringsAsFactors = FALSE
  ))
  
}


#define a function that will process googles server responses for us.
getGeoDetails <- function(address){   
  # source: http://www.shanelynn.ie/massive-geocoding-with-r-and-google-maps/
  
  
  #use the gecode function to query google servers
  geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
  #now extract the bits that we need from the returned list
  answer <- data.frame(lat=NA, long=NA, accuracy=NA, formatted_address=NA, address_type=NA, status=NA)
  answer$status <- geo_reply$status
  
  #if we are over the query limit - want to pause for an hour
  while(geo_reply$status == "OVER_QUERY_LIMIT"){
    print("OVER QUERY LIMIT - Pausing for 1 hour at:") 
    time <- Sys.time()
    print(as.character(time))
    Sys.sleep(60*60)
    geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
    answer$status <- geo_reply$status
  }
  
  #return Na's if we didn't get a match:
  if (geo_reply$status != "OK"){
    return(answer)
  }   
  #else, extract what we need from the Google server reply into a dataframe:
  answer$lat <- geo_reply$results[[1]]$geometry$location$lat
  answer$long <- geo_reply$results[[1]]$geometry$location$lng   
  if (length(geo_reply$results[[1]]$types) > 0){
    answer$accuracy <- geo_reply$results[[1]]$types[[1]]
  }
  answer$address_type <- paste(geo_reply$results[[1]]$types, collapse=',')
  answer$formatted_address <- geo_reply$results[[1]]$formatted_address
  
  return(answer)
}

converterMes <- function(x) {
  meses_ptbr <- c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun",
                  "Jul", "Ago", "Set", "Out", "Nov", "Dez")
  
  x %<>% str_replace_all(meses_ptbr[1], month.abb[1])
  x %<>% str_replace_all(meses_ptbr[2], month.abb[2])
  x %<>% str_replace_all(meses_ptbr[3], month.abb[3])
  x %<>% str_replace_all(meses_ptbr[4], month.abb[4])
  x %<>% str_replace_all(meses_ptbr[5], month.abb[5])
  x %<>% str_replace_all(meses_ptbr[6], month.abb[6])
  x %<>% str_replace_all(meses_ptbr[7], month.abb[7])
  x %<>% str_replace_all(meses_ptbr[8], month.abb[8])
  x %<>% str_replace_all(meses_ptbr[9], month.abb[9])
  x %<>% str_replace_all(meses_ptbr[10], month.abb[10])
  x %<>% str_replace_all(meses_ptbr[11], month.abb[11])
  x %<>% str_replace_all(meses_ptbr[12], month.abb[12])
  # Hoje e ontem
  hj <- today()
  x %<>% str_replace("Hoje", paste0(day(hj), " ", month(hj, label = TRUE)))
  x %<>% str_replace("Ontem", paste0(day(hj - 1), " ", month(hj - 1, label = TRUE)))
  # Converter para data
  x <- strptime(x, "%d %b")
  x <- as.Date(x)
  return(x)
}

x <- c(1, 2, 30, 40)
x[x > 4] <- NA


big_data %<>% filter(str_detect(bairro, "Brasília"))
big_data %<>% separate(bairro, c("cidade", "bairro"), sep = ",")
head(big_data) %>% knitr::kable()

# substituir quartos por quarto
big_data$adicional %<>% str_replace_all("quartos", "quarto")
big_data %<>% mutate(
  tem_quarto = str_detect(adicional, "quarto"),
  tem_area = str_detect(adicional, "m²"),
  tem_taxa = str_detect(adicional, "Condomínio"),
  tem_garagem = str_detect(adicional, "vaga")
)

x <- round(apply(big_data[, 7:10], 2, mean), 3) * 100
print(x)

matriz_posicao <- str_locate(big_data$adicional, "quarto")
# Voltar 2 posições no string para pegar o número (ex: 2 quarto)
matriz_posicao[,1] <- matriz_posicao[,1] - 2
# extrair string com posições iniciais e finais
vetor_quartos <- str_sub(big_data$adicional, matriz_posicao[,1], matriz_posicao[,2])
# extrair apenas número (primeiro caractere do string) e converter para numeric
vetor_quartos <- str_sub(vetor_quartos, 1, 1)
vetor_quartos %<>% as.numeric()
# adicionar ao data frame
big_data$qtd_quarto <- vetor_quartos


# Condominio
# retirar cifrao pra ficar mais facil
big_data$adicional %<>% str_replace_all("\\$", "S")
matriz_posicao <- str_locate(big_data$adicional, "Condomínio: RS ")
# mover cinco posicoes para pegar algarismos após o RS
vetor_taxa <- str_sub(big_data$adicional, matriz_posicao[, 2], matriz_posicao[, 2] + 4)
# extrair apenas numeros
vetor_taxa %<>% parse_number()
# vendo se funcionou
data.frame(big_data$adicional, vetor_taxa) %>% head(20)

# Funcionou! Incorporar vetor ao data frame
big_data$taxa_condominio <- vetor_taxa


# Área
matriz_posicao <- str_locate(big_data$adicional, " m²")
# voltar quatro posições
vetor_area <- str_sub(big_data$adicional, matriz_posicao[,1] - 4, matriz_posicao[, 1])
# converter para numerico
vetor_area %<>% parse_number()
# vendo se funcionou
data.frame(big_data$adicional, vetor_area) %>% head(20)

big_data$area_condominio <- vetor_area


big_data$area_condominio <- vetor_area


# Garagem
matriz_posicao <- str_locate(big_data$adicional, " vaga")
# voltar quatro posições
vetor_garagem <- str_sub(big_data$adicional, matriz_posicao[,1] - 2, matriz_posicao[, 1])
# converter para numerico
vetor_garagem %<>% readr::parse_number()
# vendo se funcionou
data.frame(big_data$adicional, vetor_garagem) %>% head(20)

# Funcionou! Incorporar ao data frame
big_data$garagem <- vetor_garagem

# Remover objetos desnecessários
rm(matriz_posicao, vetor_adicional, vetor_area, vetor_garagem, vetor_quartos, vetor_taxa)