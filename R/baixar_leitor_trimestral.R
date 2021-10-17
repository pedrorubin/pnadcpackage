library(tidyverse)
library(glue)
library(readxl)

baixar_leitor_trimestral <- function(){
  
  url <- "https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Trimestral/Microdados/Documentacao/Dicionario_e_input_20210617.zip"
  
  tempFile <- tempfile()
  
  download.file(url,tempFile,quiet=TRUE,mode="wb")
  
  datax <- tibble(unzip(tempFile)) %>% filter(str_detect(unzip(tempFile),".xls"))
  
  arquivo <- pluck(datax$`unzip(tempFile)`) %>% str_remove("./")
  
  dic <- read_excel(unzip(tempFile, files = arquivo))
  
  nomes_pnad <- c("inicio", "tamanho", "variavel","quesito", "desc", "categoria", "desc2", "periodo")
  
  names(dic) <- nomes_pnad
  
  leitores <- dic %>% select(inicio, tamanho, variavel) %>% 
    filter(complete.cases(inicio, tamanho, variavel)) %>% 
    tail(-1) %>% 
    mutate(across(.cols = c(inicio, tamanho),
                  .fns = as.numeric)) %>% 
    mutate(fim = inicio + tamanho -1) %>% 
    select(inicio, fim, tamanho, variavel)
  
}
