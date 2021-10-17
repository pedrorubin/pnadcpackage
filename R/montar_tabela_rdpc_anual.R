#' Add household per capita monthly income as a column
#'
#' Add household per capita monthly income as a column
#' @param df_pnadc The dataframe with the Annual PNADC data (see ler_pnadc_anual)
#' @return A dataframe without rows in which VD2002 is 15, 16, 17; and +1 col (rdpc) with household per capita monthly income
#' @examples montar_tabela_rdpc_anual(pnadc_2019);
#' @export

montar_tabela_rdpc_anual <- function(df_pnadc){

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
    ungroup()

}
