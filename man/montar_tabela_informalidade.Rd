% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/montar_tabela_informalidade.R
\name{montar_tabela_informalidade}
\alias{montar_tabela_informalidade}
\title{Adicionar informalidade como uma coluna}
\usage{
montar_tabela_informalidade(df_pnadc)
}
\arguments{
\item{df_pnadc}{O dataframe com dados da PNADC anual (ver ler_pnadc_anual)}
}
\value{
Um dataframe com 1 coluna adicional: "informalidade" com valor 1 se a pessoa é informal, 0 caso contrário, e NA se não for aplicável (pessoas não ocupadas)
}
\description{
Adicionar informalidade como uma coluna (dummy)
}
\examples{
calcular_informalidade(pnadc_2018);
}
\references{
https://biblioteca.ibge.gov.br/visualizacao/livros/liv101743_informativo.pdf nota 7

Roubaud et al, 2020. Disponível em https://www.ie.ufrj.br/images/IE/TDS/2020/TD_IE_031_2020_ROUBAUD_et%20al.pdf
}
\seealso{
ler_pnadc_anual, calcular_informalidade
}
