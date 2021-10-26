#' Calcular desocupados
#'
#' Calcular desocupados (número absoluto e taxa de desocupação)
#' @param df_pnadc O dataframe com dados da PNADC anual (ver ler_pnadc_anual)
#' @return Um dataframe com 3 colunas: ano, total desocupados e taxa de desocupação (% da força de trabalho)
#' @examples calcular_desocupados_anual(pnadc_2019);
#' @seealso ler_pnadc_anual
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
