#' Calculate average work income
#'
#' Calculate average work income
#' @param df_pnadc The dataframe with the Annual PNADC data (see ler_pnadc_anual)
#' @param tipo "habitual" or "efetivo" The kind of income: "habitual" is the customary monthly income; "efetivo" is the income actually received in that month
#' @return A dataframe with 3 cols: year, real average monthly work income and nominal average monthly work income
#' @examples calcular_renda_trabalho_media_anual(pnadc_2019, tipo = "habitual");
#' @examples calcular_renda_trabalho_media_anual(pnadc_2013, tipo = "efetivo");
#' @export


calcular_renda_trabalho_media_anual <- function(df_pnadc, tipo){

  if(tipo == "habitual"){

    df_pnadc %>%
      mutate(across(.fns = as.numeric)) %>%
      filter(VD4002 == 1,
             is.na(VD4019_real) == F) %>%
      summarise(Ano = unique(Ano),
                renda_trab_habitual_real = sum(VD4019_real * V1032)/sum(V1032),
                renda_trab_habitual_nominal = sum(VD4019 * V1032)/sum(V1032))

  }
  else if (tipo == "efetivo"){

    df_pnadc %>%
      mutate(across(.fns = as.numeric)) %>%
      filter(VD4002 == 1,
             is.na(VD4020_real) == F) %>%
      summarise(Ano = unique(Ano),
                renda_trab_efetivo_real = sum(VD4020_real * V1032)/sum(V1032),
                renda_trab_efetivo_nominal = sum(VD4020 * V1032)/sum(V1032))
  }

  else{
    cat("tipo tem que ser habitual ou efetivo")
    stop()
  }

}
