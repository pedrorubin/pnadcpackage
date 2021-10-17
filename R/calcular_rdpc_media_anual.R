#' Calculate average household per capita monthly income
#'
#' Calculate average household per capita monthly income
#' @param df_pnadc The dataframe with the Annual PNADC data (see ler_pnadc_anual)
#' @return A dataframe with 2 cols: year and the average household per capita monthly income
#' @examples calcular_rdpc_media_anual(pnadc_2019);
#' @export

calcular_rdpc_media_anual <- function(df_pnadc){

  df_pnadc %>%
    mutate(id_deflator = str_c(Ano,Trimestre,UF),
           id_dom = str_c(UPA,V1008,V1014)) %>%
    rowwise() %>%
    mutate(tudo_real = sum(VD4019_real,VD4048_real,na.rm=T)) %>%
    ungroup() %>%
    filter(VD2002 %in% 1:14) %>%
    group_by(id_dom) %>%
    mutate(n = n(),
           renda = sum(tudo_real,na.rm = T),
           rdpc = renda/n) %>%
    ungroup() %>%
    # distinct(id_dom, .keep_all = T) %>%
    summarise(Ano = unique(Ano),
              rdpc = sum(rdpc * V1032)/sum(V1032))

}
