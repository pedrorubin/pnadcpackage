taxa_informalidadex <- function(data_PNAD, sex = c(1,2)){
  data_PNAD %>% mutate(pesocompos=as.numeric(pesocompos),
                       idade=as.numeric(idade),
                       cond_ocup=as.numeric(cond_ocup)) %>%
    filter(between(idade, 14, 64)) %>%
    mutate(total_pia = sum(pesocompos)) %>%
    filter(ft == 1) %>%
    mutate(total_ft = sum(pesocompos)) %>%
    filter(ocup == 1) %>%
    mutate(total_ocup = sum(pesocompos)) %>%
    filter(cond_ocup %in% c(2,3,4,6,9)) %>%
    filter(sexo %in% sex) %>%
    mutate(total_informal = sum(pesocompos)) %>%
    summarise(inf_pia  = round(100*unique(total_informal)/unique(total_pia), digits = 1),
              inf_ft   = round(100*unique(total_informal)/unique(total_ft), digits = 1),
              inf_ocup = round(100*unique(total_informal)/unique(total_ocup), digits = 1),
              periodo = str_c(unique(ano),unique(trimestre), sep="_"))
}

A <- pnad_16 %>% select(V2007,VD4002, VD4009, VD4012, V1032) %>%
  mutate(across(.fns = as.numeric)) %>%
  filter(VD4002 == 1) %>%
  mutate(inf = case_when(
    VD4009 %in% c(2,4,6,10) ~ 1,
    VD4009 %in% c(8,9) & VD4012 == 2 ~ 2,
    TRUE ~ 0
  ),
  V1032 = V1032) %>%
  filter(inf != 0) %>%
  group_by(V2007) %>%
  summarise(x = round(sum(V1032)/1000,0))
