library(tidyverse)
library(glue)
library(readxl)

ler_pnadc_anual <- function(ano, path_pnadc, path_leitores, path_deflator) {
  arquivo = glue("{path_pnadc}/PNADC_{ano}_visita1.txt")
  # salvamento = glue("./dados/anual/PNADC_{ano}.rds")
  dicionario = glue("{path_leitores}/leitores_{ano}.rds")
  
  leitores <- read_rds(dicionario)
  
  colpos <- fwf_widths(leitores$tamanho,
                       col_names = leitores$variavel)
  pnad <- read_fwf(file = as.character(arquivo),
                   col_positions = colpos,
                   col_types = cols(.default = col_character()))
  
  pnadx <- pnad %>% 
    mutate(ID_dom = str_c(UPA,V1008,V1014),
           ID_pes = str_c(UPA,V1008,V1014,V2003),
           ID_deflator = str_c(Ano,Trimestre,UF)) %>% 
    mutate(across(.fns = as.numeric),
           regiao = floor(UF/10))
  
  deflator <- read_xls(glue("{path_deflator}/deflator_PNADC_2019.xls")) %>% 
    mutate(Trimestre = trim,
           Ano = ano,
           UF = uf,
           Habitual = CO2,
           Efetivo = CO2e,
           ID_deflator = str_c(Ano,Trimestre,UF)) %>% 
    filter(Ano == unique(pnad$Ano)) %>% 
    mutate(across(.fns = as.numeric)) %>% 
    select(ID_deflator, Habitual, Efetivo)
  
  pnady <- pnadx %>% 
    left_join(deflator, by = "ID_deflator") %>% 
    mutate(VD4019_real = VD4019*Habitual,
           VD4020_real = VD4020*Efetivo,
           VD4048_real = VD4048*Efetivo)
}