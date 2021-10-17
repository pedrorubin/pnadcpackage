#' Calculate poverty
#'
#' From a poverty threshold, calculate the number of poor and the 3 indexes from the FGT family
#' @param df_pnadc The dataframe with the Annual PNADC data (see ler_pnadc_anual)
#' @param linha_pob The monetary poverty threshold (household per capita monthly income)
#' @return A dataframe with 6 cols: year, povery threshold, number of poor, P0 (proportion of poor - %), P1 (income-gap measure) and P2 (square income-gap)
#' @examples calcular_pobreza_anual(pnadc_2019, 400);
#' @references Foster, J.;  Greer, J.; Thorbecke, E. (1984) A class of decomposable poverty measures. Econometrica, v. 52, n. 3, pp. 761-766
#' @references See also https://guilhermejacob.github.io/context/2-poverty.html#poverty
#' @export

calcular_pobreza_anual <- function(df_pnadc, linha_pob){

  df_pnadc %>%
    montar_tabela_pobreza_anual(linha_pob) %>%
    mutate(pop = sum(V1032),
           renda_pobreza = rdpc*pobreza,
           hiato_pobreza = case_when(linha_pob >= rdpc ~ (linha_pob - rdpc) / linha_pob,
                                     TRUE ~ 0)) %>%
    summarise(Ano = unique(Ano),
              linha_pobreza = linha_pob,
              n_pobres = sum(pobreza*V1032),
              p0_pobreza = 100*sum(pobreza*V1032)/sum(V1032),
              p1_pobreza = sum(hiato_pobreza*pobreza*V1032)/sum(V1032),
              p2_pobreza = sum(((hiato_pobreza*pobreza)^2)*V1032)/sum(V1032))
}
