#' Baixa e cria o arquivo de deflator para a PNADC anual, em rds e em csv
#'
#' Baixa e cria o arquivo de deflator para a PNADC anual, em rds e em csv (é preciso acesso a internet)
#' @param caminho_pasta A pasta (o caminho para a pasta) na qual os arquivos serão guardados (se não existir, a pasta será criada)
#' @return Os arquivos rds e csv na pasta designada
#' @examples baixar_deflator_anual(caminho_pasta = "./deflator");
#' @export

baixar_deflator_anual <- function(caminho_pasta){

  url <- "https://ftp.ibge.gov.br/Trabalho_e_Rendimento/Pesquisa_Nacional_por_Amostra_de_Domicilios_continua/Anual/Microdados/Visita/Documentacao_Geral/deflator_PNADC_2019.xls"

  tempFile <- tempfile()

  download.file(url,tempFile,quiet=TRUE,mode="wb")

  deflator <- readxl::read_excel(tempFile) %>%
    mutate(Trimestre = trim,
           Ano = ano,
           UF = uf,
           ID_deflator = str_c(Ano,Trimestre,UF)) %>%
    mutate(across(.fns = as.numeric)) %>%
    select(Ano, Trimestre, UF, ID_deflator, CO1, CO1e, CO2, CO2e, CO3)


  write_rds(deflator, file = glue("{caminho_pasta}/deflator.rds"))
  write_csv(deflator, file = glue("{caminho_pasta}/deflator.csv"))

}
