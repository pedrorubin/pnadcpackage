#' Adicionar renda domiciliar per capita mensal real como uma coluna
#'
#' Adicionar renda domiciliar per capita mensal real como uma coluna
#' @param df_pnadc O dataframe com dados da PNADC anual (ver ler_pnadc_anual)
#' @return Um dataframe sem as linhas nas quais VD2002 igual a 15, 16 ou 17; e com 1 coluna adicional: "rdpc" com a renda domiciliar per capital mensal real
#' @examples montar_tabela_rdpc_anual(pnadc_2019);
#' @seealso calcular_rdpc_media_anual, calcular_rdpc_percentil_anual
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
