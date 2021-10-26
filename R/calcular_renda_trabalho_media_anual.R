#' Calcular a renda média mensal do trabalho
#'
#' Calcular a renda média mensal do trabalho
#' @param df_pnadc O dataframe com dados da PNADC anual (ver ler_pnadc_anual)
#' @param tipo "habitual" ou "efetivo". O primeiro é a renda que costuma receber, o segundo é a renda efetivamente recebida
#' @return Um dataframe com 3 colunas: ano, renda média mensal do trabalho real e nominal
#' @examples calcular_renda_trabalho_media_anual(pnadc_2019, tipo = "habitual");
#' @examples calcular_renda_trabalho_media_anual(pnadc_2013, tipo = "efetivo");
#' @seealso calcular_rdpc_media_anual,  calcular_renda_trabalho_percentil_anual
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
             is.na(VD4020_real) == F,
             VD4020_real > 0) %>%
      summarise(Ano = unique(Ano),
                renda_trab_efetivo_real = sum(VD4020_real * V1032)/sum(V1032),
                renda_trab_efetivo_nominal = sum(VD4020 * V1032)/sum(V1032))
  }

  else{
    cat("tipo tem que ser habitual ou efetivo")
    stop()
  }

}
