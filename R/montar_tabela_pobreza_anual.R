#' Add monetary poverty condition as a column
#'
#' Add household per capita monthly income as a column
#' @param df_pnadc The dataframe with the Annual PNADC data (see ler_pnadc_anual)
#' @param linha_pob The monetary poverty threshold (household per capita monthly income)
#' @return A dataframe without rows in which VD2002 is 15, 16, 17; and +2 cols: rdpc with household per capita monthly income and pobreza that equals 1 if person is poor (household per capita monthly income lower than the poverty threshold)
#' @examples montar_tabela_pobreza_anual(pnadc_2019, 400);
#' @export

montar_tabela_pobreza_anual <- function(df_pnadc, linha_pob){

  df_pnadc %>%
    montar_tabela_rdpc_anual() %>%
    mutate(pobreza = case_when(rdpc < linha_pob ~ 1,
                               TRUE ~ 0))

}
