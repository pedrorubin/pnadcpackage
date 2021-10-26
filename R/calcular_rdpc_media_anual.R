#' Calcular a renda domiciliar per capital mensal média
#'
#' Calcular a renda domiciliar per capital mensal média
#' @param df_pnadc O dataframe com dados da PNADC anual (ver ler_pnadc_anual)
#' @return Um dataframe com 6 colunas: ano e a renda domiciliar per capita média mensal
#' @examples calcular_rdpc_media_anual(pnadc_2019);
#' @seealso calcular_rdpc_percentil_anual
#' @seealso calcular_renda_trabalho_media_anual
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
