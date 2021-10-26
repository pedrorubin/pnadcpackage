#' Adicionar condição de pobreza monetária como uma coluna
#'
#' Adicionar condição de pobreza monetária como uma coluna (dummy)
#' @param df_pnadc O dataframe com dados da PNADC anual (ver ler_pnadc_anual)
#' @param linha_pob A linha monetária de pobreza, em termos da renda domiciliar per capita mensal
#' @return Um dataframe sem as linhas nas quais VD2002 igual a 15, 16 ou 17; e com 2 colunas adicionais: "rdpc" com a renda domiciliar per capital mensal real e "pobreza" com valor igual a 1 se a pessoa for pobre (rdpc < linha de pobreza)e 0 caso contrário
#' @examples montar_tabela_pobreza_anual(pnadc_2019, linha_pob = 400);
#' @seealso calcular_pobreza_anual, montar_tabela_rdpc_anual, calcular_rdpc_media_anual
#' @export

montar_tabela_pobreza_anual <- function(df_pnadc, linha_pob){

  df_pnadc %>%
    montar_tabela_rdpc_anual() %>%
    mutate(pobreza = case_when(rdpc < linha_pob ~ 1,
                               TRUE ~ 0))

}
