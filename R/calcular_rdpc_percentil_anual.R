#' Calcular o valor da renda domiciliar per capital mensal associado a determinado percentil
#'
#' Calcular o valor da renda domiciliar per capital mensal associado a determinado percentil (é o ponto de corte, não o valor médio)
#' @param df_pnadc O dataframe com dados da PNADC anual (ver ler_pnadc_anual)
#' @param percentil O percentil (default = 0.5 : isto é, o padrão é retornar a renda mediana). Pode ser mais de um valor na mesma chamada (ver exemplos)
#' @return Um dataframe com 3 colunas: ano, percentil e a respectiva renda domiciliar per capita mensal máxima
#' @examples calcular_rdpc_percentil_anual(pnadc_2019);
#' @examples calcular_rdpc_percentil_anual(pnadc_2015, percentil = c(0.1, 0.4, 0.9, 0.95))
#' @seealso calcular_rdpc_media_anual
#' @export

calcular_rdpc_percentil_anual <- function(df_pnadc, percentil = 0.5){

  df_medio <- df_pnadc %>%
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

  Ano <- df_medio %>% distinct(Ano) %>% chuck()


  plan <- svydesign(ids = ~0,
                    strata = ~Estrato,
                    weights = ~V1032,
                    data = df_medio)

  quantis_rend_dom <- svyquantile(~rdpc, plan, quantiles = percentil, na.rm = F, ci = F)
  x <- quantis_rend_dom$rdpc
  y <- as_tibble(x %>% t() %>% as.data.frame() %>%  rownames_to_column() %>% set_names(c("percentil", "renda"))) %>%
    mutate(Ano = Ano$Ano)
  y

}
