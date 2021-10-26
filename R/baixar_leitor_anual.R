#' Baixa e cria o arquivo de leitor para a PNADC anual, em rds e csv
#'
#' Baixa e cria o arquivo de leitor para a PNADC anual, em rds e csv (é preciso acesso a internet)
#' @param ano Ano da PNADC Anual
#' @param caminho_pasta A pasta (o caminho para a pasta) na qual os arquivos serão guardados (se não existir, a pasta será criada)
#' @return Os arquivos rds e csv na pasta designada
#' @examples baixar_leitor_anual(ano = 2019, caminho_pasta = "./leitores");
#' @export

baixar_leitor_anual <- function(ano, caminho_pasta){

  if(ano %in% 2012:2014){
    url <- "https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Visita/Visita_1/Documentacao/dicionario_PNADC_microdados_2012_a_2014_visita1_20210617.xls"
  }
  else if (ano == 2015){
    url <- "https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Visita/Visita_1/Documentacao/dicionario_PNADC_microdados_2015_visita1_20210617.xls"
  }
  else if (ano == 2016){
    url <- "https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Visita/Visita_1/Documentacao/dicionario_PNADC_microdados_2016_visita1_20210617.xls"
  }
  else if (ano == 2017){
    url <- "https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Visita/Visita_1/Documentacao/dicionario_PNADC_microdados_2017_visita1_20210617.xls"
  }
  else if (ano == 2018){
    url <- "https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Visita/Visita_1/Documentacao/dicionario_PNADC_microdados_2018_visita1_20210617.xls"
  }
  else if (ano == 2019){
    url <- "https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Visita/Visita_1/Documentacao/dicionario_PNADC_microdados_2019_visita1_20210617.xls"
  }

  tempFile <- tempfile()

  download.file(url,tempFile,quiet=TRUE,mode="wb")

  datax <- read_excel(tempFile)

  names(datax) <- c("inicio", "tamanho", "variavel", "quesito", "desc", "categoria", "desc2", "periodo")

  leitores <- datax %>% select(inicio, tamanho, variavel) %>%
    filter(complete.cases(inicio, tamanho, variavel)) %>%
    tail(-1) %>%
    mutate(across(.cols = c(inicio, tamanho),
                  .fns = as.numeric)) %>%
    mutate(fim = inicio + tamanho -1) %>%
    select(inicio, fim, tamanho, variavel)


  write_rds(leitores, file = glue("{caminho_pasta}/leitores_{ano}.rds"))
  write_csv(leitores, file = glue("{caminho_pasta}/leitores_{ano}.csv"))

}

