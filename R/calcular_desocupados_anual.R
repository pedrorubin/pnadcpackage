#' Calculate the unoccupied population
#'
#' Calculate the unoccupied population
#' @param df_pnadc The dataframe with the Annual PNADC data (see ler_pnadc_anual)
#' @return A dataframe with 3 cols: year, total unoccupied and % of the workforce that are unoccupied
#' @examples calcular_desocupados_anual(pnadc_2019);
#' @export


calcular_desocupados_anual <- function(df_pnadc){

  df_pnadc %>%
    filter(VD4001 == 1) %>%
    mutate(ft = sum(V1032)) %>%
    filter(VD4002 == 2) %>%
    summarise(Ano = unique(Ano),
              desocupados = sum(V1032),
              tx_desocupacao = round(100*desocupados / unique(ft),2))


}
