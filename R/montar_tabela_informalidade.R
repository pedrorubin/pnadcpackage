#' Adicionar informalidade como uma coluna
#'
#' Adicionar informalidade como uma coluna (dummy)
#' @param df_pnadc O dataframe com dados da PNADC anual (ver ler_pnadc_anual)
#' @return Um dataframe com 1 coluna adicional: "informalidade" com valor 1 se a pessoa é informal, 0 caso contrário, e NA se não for aplicável (pessoas não ocupadas)
#' @examples calcular_informalidade(pnadc_2018);
#' @references https://biblioteca.ibge.gov.br/visualizacao/livros/liv101743_informativo.pdf nota 7
#' @references Roubaud et al, 2020. Disponível em https://www.ie.ufrj.br/images/IE/TDS/2020/TD_IE_031_2020_ROUBAUD_et%20al.pdf
#' @seealso ler_pnadc_anual, calcular_informalidade
#' @export

montar_tabela_informalidade <- function(df_pnadc){

  df_pnadc %>%
    mutate(informal1 = case_when(VD4009 %in% c(2,4,10) ~ 1,
                                VD4009 %in% c(8,9) & V4019 == 2 ~ 1,
                                TRUE ~ 0)) %>%
    mutate(informal = case_when(informal1 == 1 ~ 1,
                                informal1 == 0 & VD4002 == 1 ~ 0)) %>%
    select(-informal1)

}

