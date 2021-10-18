#' Download and create the deflator file for the Annual PNADC as both rds and csv
#'
#' Download and create the deflator file for the Annual PNADC as both rds and csv
#' @param ano Year of of the Annual PNADC
#' @param destination_path The folder in which the files are to be stored (if folder does not exist, it will be created)
#' @return The files in the designated path (rds and csv)
#' @examples baixar_deflator_anual(destination_path = "./deflator");
#' @export

baixar_deflator_anual <- function(destination_path){

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


  write_rds(deflator, file = glue("{destination_path}/deflator.rds"))
  write_csv(deflator, file = glue("{destination_path}/deflator.csv"))

}
