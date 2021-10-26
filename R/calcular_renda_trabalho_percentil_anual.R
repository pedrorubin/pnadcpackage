#' Calcular o valor da renda do trabalho mensal associado a determinado percentil
#'
#' Calcular o valor da renda do trabalho mensal associado a determinado percentil (é o ponto de corte, não o valor médio)
#' @param df_pnadc O dataframe com dados da PNADC anual (ver ler_pnadc_anual)
#' @param percentil O percentil (default = 0.5 : isto é, o padrão é retornar a renda mediana). Pode ser mais de um valor na mesma chamada (ver exemplos)
#' @param tipo "habitual" ou "efetivo". O primeiro é a renda que costuma receber, o segundo é a renda efetivamente recebida
#' @return Um dataframe com 3 colunas: ano, percentil e a respectiva renda do trabalho mensal máxima
#' @examples calcular_renda_trabalho_percentil_anual(pnadc_2019);
#' @examples calcular_renda_trabalho_percentil_anual(pnadc_2015, percentil = c(0.1, 0.4, 0.9, 0.95))
#' @export



calcular_renda_trabalho_percentil_anual <- function(df_pnadc, percentil = 0.5, tipo){


  if(tipo == "habitual"){

    df_medio <- df_pnadc %>%
      mutate(across(.fns = as.numeric)) %>%
      filter(VD4002 == 1,
             is.na(VD4019_real) == F)

    Ano <- df_medio %>% distinct(Ano) %>% chuck()

    plan <- svydesign(ids = ~0,
                      strata = ~Estrato,
                      weights = ~V1032,
                      data = df_medio)

    quantis_rend_dom <- svyquantile(~VD4019_real, plan, quantiles = percentil, na.rm = F, ci = F)
    x <- quantis_rend_dom$VD4019_real

    y <- as_tibble(x %>% t() %>% as.data.frame() %>%  rownames_to_column() %>% set_names(c("percentil", "renda"))) %>%
      mutate(Ano = Ano$Ano)
    y

  }
  else if (tipo == "efetivo"){

    df_medio <- df_pnadc %>%
      mutate(across(.fns = as.numeric)) %>%
      filter(VD4002 == 1,
             is.na(VD4020_real) == F)

    Ano <- df_medio %>% distinct(Ano) %>% chuck()

    plan <- svydesign(ids = ~0,
                      strata = ~Estrato,
                      weights = ~V1032,
                      data = df_medio)

    quantis_rend_dom <- svyquantile(~VD4020_real, plan, quantiles = percentil, na.rm = F, ci = F)
    x <- quantis_rend_dom$VD4020_real

    y <- as_tibble(x %>% t() %>% as.data.frame() %>%  rownames_to_column() %>% set_names(c("percentil", "renda"))) %>%
      mutate(Ano = Ano$Ano)
    y
  }

  else{
    cat("tipo tem que ser habitual ou efetivo")
    stop()
  }



}
