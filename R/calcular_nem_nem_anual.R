#' Calcular população jovem nem-nem
#'
#' Calcular população jovem nem-nem: pessoas com idade entre "idade_min" e "idade_max" que não estudam nem trabalham
#' @param df_pnadc O dataframe com dados da PNADC anual (ver ler_pnadc_anual)
#' @param idade_min O patamar inferior de idade que define "jovem" (default = 15; mínimo = 14). A idade escolhida faz parte da população jovem
#' @param idade_max O patamar inferior de idade que define "jovem" (default = 15). A idade escolhida faz parte da população jovem
#' @return Um dataframe com 3 colunas: ano, total nem-nem e % de nem-nem dentro da população jovem
#' @examples calcular_nem_nem_anual(pnadc_2013, idade_min = 15, idade_max = 29);
#' @examples calcular_nem_nem_anual(pnadc_2014, idade_min = 14, idade_max = 18);
#' @references Neri, 2021 - https://www.cps.fgv.br/cps/NemNem/
#' @seealso ler_pnadc_anual
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
