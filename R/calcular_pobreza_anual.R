#' Calcular pobreza
#'
#' A partir de uma linha de pobreza, calcular o número de pobres e os 3 índices FGT
#' @param df_pnadc O dataframe com dados da PNADC anual (ver ler_pnadc_anual)
#' @param linha_pob A linha monetária de pobreza, em termos da renda domiciliar per capita mensal
#' @return Um dataframe com 6 colunas: ano, linha de pobreza, número de pobres, P0 (proporção de pobres - %), P1 (medida hiato de renda) and P2 (hiato da renda ao quadrado)
#' @examples calcular_pobreza_anual(pnadc_2019, 400);
#' @references Foster, J.;  Greer, J.; Thorbecke, E. (1984) A class of decomposable poverty measures. Econometrica, v. 52, n. 3, pp. 761-766
#' @references https://guilhermejacob.github.io/context/2-poverty.html#poverty
#' @seealso ler_pnadc_anual, montar_tabela_pobreza_anual, montar_tabela_rdpc_anual, calcular_rdpc_media_anual
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
