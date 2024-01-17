# Gini.em.R
Estimando um índice de Gini por Unidade da Federação brasileira usando R

Segue o código e logo em seguida o gráfico gerado

# Bibliotecas
library(PNADcIBGE)
library(survey)
library(srvyr)
library(tidyverse)
library(convey)
library(magrittr)
library(forcats)
library(rmarkdown)
library(tinytex)

# Testando o uso da get_pnadc
pnad_df <- get_pnadc(year = 2020, 
                     quarter = 1,
                     design = FALSE)

# Selecionando as variáveis para o 1º trimestre de 2016
variaveis_selecionadas <- c("UF","V6002","V6007","V9029","V9032","V9046","V9073","V9891","V9117","V4704","V4805", "V4706", "V4719", "V4745", "VD4020", "V2007")

# Baixando dados da PNAD
dadosPNADC = get_pnadc(year = 2016, quarter = 4, vars = variaveis_selecionadas)

# Estimando a renda total
total_renda <- svytotal(x=~VD4020, design=dadosPNADC,
                        na.rm=TRUE)

# Intervalo de confiança a 99%
confint(object = total_renda,
        level=0.99)

# Obter número total de homens e mulheres
svytotal(x = ~V2007, design = dadosPNADC,
         na.rm = TRUE)

# Estimar o índice de Gini
dadosPNADC  %>%
  convey_prep() %>%
  svygini(formula = ~VD4020, na.rm = TRUE)

# Gráfico do índice de GINI estimado
pnadc_0416 <- PNADcIBGE::get_pnadc(year = 2016, quarter = 4, 
                                   vars = c("UF", "VD4020")) %>%
  convey::convey_prep()

gini_uf <- survey::svyby(~VD4020,
                         by = ~UF,
                         design = pnadc_0416,
                         FUN = convey::svygini,
                         na.rm = TRUE)

gini_uf %>%
  dplyr::as_tibble() %>%
  dplyr::mutate(UF = forcats::fct_reorder(UF, VD4020)) %>%
  ggplot2::ggplot(ggplot2::aes(x = VD4020, y = UF)) +
  ggplot2::geom_col(fill = "blue") +
  ggplot2::theme_classic() +
  ggplot2::labs(
    title = "Índice de Gini por Estado",
    subtitle = "Dados do 4º trimestre de 2016",
    x = NULL,
    y = NULL,
    caption = "Fonte: Microdados PNADC-T/IBGE"
  )

  ![000014 (1)](https://github.com/Luiz-Fernando-Oliveira/Gini.em.R/assets/156798656/e3c196b9-3083-4c6e-adcc-37b6dd9223ca)

