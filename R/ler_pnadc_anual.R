#' Load annual PNADC microdata
#'
#' Load annual PNADC microdata
#' @param ano Year
#' @param path_pnadc The folder in which the microdata txt files are stores
#' @return A dataframe with all the microdata information
#' @examples ler_pnadc_anual(2018, "./microdata_folder");
#' @export


ler_pnadc_anual <- function(ano, path_pnadc) {
  arquivo <- glue("{path_pnadc}/PNADC_{ano}_visita1.txt")

  leitor_nome <- glue("leitores_{ano}")

  leitores <- get(leitor_nome)

  colpos <- fwf_widths(leitores$tamanho,
                       col_names = leitores$variavel)
  pnad <- read_fwf(file = as.character(arquivo),
                   col_positions = colpos,
                   col_types = cols(.default = col_character()))

  pnadx <- pnad %>%
    mutate(ID_dom = str_c(UPA,V1008,V1014),
           ID_pes = str_c(UPA,V1008,V1014,V2003),
           ID_deflator = str_c(Ano,Trimestre,UF)) %>%
    mutate(across(.fns = as.numeric),
           regiao = floor(UF/10))

  deflator <- deflator %>%
    filter(Ano == unique(pnad$Ano))

  pnady <- pnadx %>%
    left_join(deflator, by = "ID_deflator") %>%
    mutate(VD4019_real = VD4019*CO2,
           VD4020_real = VD4020*CO2e,
           VD4048_real = VD4048*CO2)
}
