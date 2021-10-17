#' Calculate the "nem-nem" youth population
#'
#' Calculate the "nem-nem" youth population: people between "idade_min" and "idade_max" that neither study nor work
#' @param df_pnadc The dataframe with the Annual PNADC data (see ler_pnadc_anual)
#' @param idade_min The (inclusive) lowerbound age to define youth - default = 15
#' @param idade_max The (inclusive) upperbound age to define youth - deafult = 29
#' @return A dataframe with 3 cols: year, total youth "nem-nem" and % of the youth that are "nem-nem"
#' @examples calcular_desocupados_anual(pnadc_2019);
#' @references Neri, 2021 - https://www.cps.fgv.br/cps/NemNem/
#' @export


calcular_nem_nem_anual <- function(df_pnad, idade_min = 15, idade_max = 29){

  df_pnad %>%
    filter(V2009 %>% between(idade_min, idade_max)) %>%
    mutate(pop = sum(V1032)) %>%
    filter(V3002 == 2) %>%
    filter(is.na(VD4002) == T | VD4002 == 2) %>%
    summarise(Ano = unique(Ano),
              nem_nem = sum(V1032),
              nem_nem_pct_jovens = round(100*sum(V1032)/unique(pop),3))

}
