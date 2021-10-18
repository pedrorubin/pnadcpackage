#' Download and unzip Annual PNADC microdata
#'
#' Download and create the reader file and save as both rds and csv
#' @param ano Year of of the Annual PNADC
#' @param destination_path The folder in which the files are to be stored (if folder does not exist, it will be created)
#' @return The microdata files (txt) in the designated path
#' @examples baixar_pnadc_anual(ano = 2019, destination_path = "./microdata");
#' @export

baixar_pnadc_anual <- function(ano, destination_path){

  if(ano == 2012){
    url <- "https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Visita/Visita_1/Dados/PNADC_2012_visita1_20191016.zip"
  }
  else if(ano == 2013){
    url <- "https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Visita/Visita_1/Dados/PNADC_2013_visita1_20191016.zip"
  }
  else if(ano == 2014){
    url <- "https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Visita/Visita_1/Dados/PNADC_2014_visita1_20191016.zip"
  }
  else if(ano == 2015){
    url <- "https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Visita/Visita_1/Dados/PNADC_2015_visita1_20191016.zip"
  }
  else if(ano == 2016){
    url <- "https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Visita/Visita_1/Dados/PNADC_2016_visita1_20191016.zip"
  }
  else if(ano == 2017){
    url <- "https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Visita/Visita_1/Dados/PNADC_2017_visita1_20191016.zip"
  }
  else if(ano == 2018){
    url <- "https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Visita/Visita_1/Dados/PNADC_2018_visita1_20191218.zip"
  }
  else if(ano == 2019){
    url <- "https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Visita/Visita_1/Dados/PNADC_2019_visita1_20200826.zip"
  }

  tempFile <- tempfile()

  download.file(url,tempFile,quiet=TRUE,mode="wb")

  unzip(file.path(tempFile), exdir = destination_path)

}
