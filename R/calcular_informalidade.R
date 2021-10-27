#' Calcular informalidade
#'
#' Calcular a informalidade: soma de trabalhadores do setor privado e domésticos sem carteira, empregados e por conta própria sem CNPJ e trabalhadores familiares auxiliares
#' @param df_pnadc O dataframe com dados da PNADC anual (ver ler_pnadc_anual)
#' @return Um dataframe com 3 colunas: ano, total de informais, taxa de informalidade (% da população ocupada)
#' @examples calcular_informalidade(pnadc_2018);
#' @references https://biblioteca.ibge.gov.br/visualizacao/livros/liv101743_informativo.pdf nota 7
#' @references Roubaud et al, 2020. Disponível em https://www.ie.ufrj.br/images/IE/TDS/2020/TD_IE_031_2020_ROUBAUD_et%20al.pdf
#' @seealso ler_pnadc_anual, montar_tabela_informalidade
#' @export

calcular_informalidade <- function(df_pnadc){

  df_pnadc %>%
    filter(VD4002 == 1) %>%
    mutate(ocup = sum(V1032)) %>%
    mutate(informal = case_when(VD4009 %in% c(2,4,10) ~ 1,
                                VD4009 %in% c(8,9) & V4019 == 2 ~ 1,
                                TRUE ~ 0)) %>%
    summarise(ano = unique(Ano),
              total_informal = sum(V1032*informal),
              taxa_informalidade = 100*sum(V1032*informal)/unique(ocup))
}

