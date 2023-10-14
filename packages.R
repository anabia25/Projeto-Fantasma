if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse, data.table,
  readxl, readr, ggcorrplot, cowplot,
  RColorBrewer, scales, nortest, xlsx,
  skimr,xtable
)

windowsFonts(Arial=windowsFont("sans"))

options(scipen=999)

# Definindo paleta de cores da Estat
cores_estat <- c(
  "#A11D21", "#003366", "#CC9900", "#663333", "#FF6600",
  "#CC9966", "#999966", "#006606", "#008091", "#041835",
  "#666666")

theme_estat <- function(...) {
  theme <- ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.y = ggplot2::element_text(colour = "black", size = 10),
      axis.title.x = ggplot2::element_text(colour = "black", size = 10),
      axis.text = ggplot2::element_text(colour = "black", size = 10),
      panel.border = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      text = element_text(family = "sans", size = 12),
      legend.position = "top",
      ...
    )
  return(
    list(
      theme,
      scale_fill_manual(values = cores_estat),
      scale_colour_manual(values = cores_estat),
      scale_y_continuous(
        labels = scales::number_format(decimal.mark = ',',
                                       #accuracy = 0.01,
                                       big.mark = "."))
    )
  )
}

# Definindo função que retorna frequências relativas de um vetor
percent <- function(absolute, digits = 2) {
  return(round(100 * absolute / sum(absolute), digits))
}

# Definindo função que retorna banco de dados com frequências
# relativas e absolutas de uma variável categórica
vector_frequencies <- function(vector) {
  frequency <- vector %>%
    table() %>%
    as_tibble() %>%
    mutate(
      rel = n %>%
        percent() %>%
        paste("%", sep = "")
    )
  colnames(frequency) <- c("groups", "absolute", "relative")
  return(frequency)
}

quadro <- function(dados,t=T){
  rn=F
  dados <- dados %>%
    select_if(is.numeric)
  tabela <- skim(dados)
  tabela <- as.data.frame(tabela)
  tabela <- tabela %>% 
    select(!c(1,3:4,12)) %>%
    mutate(
      numeric.mean = round(numeric.mean,2),
      numeric.sd = round(numeric.sd,2),
      numeric.p0 = round(numeric.p0,2),
      numeric.p25 = round(numeric.p25,2),
      numeric.p50 = round(numeric.p50,2),
      numeric.p75 = round(numeric.p75,2),
      numeric.p100 = round(numeric.p100,2),
      di = numeric.p75-numeric.p25,
      di = round(di,2)
    )
  colnames(tabela) <- c("Categoria",
                        "Média","Desvio padrão",
                        "Mínimo","q25","Mediana","q75","Máximo","D I")
  if (t == T){
    tabela <- as.data.frame(t(tabela))
    colnames(tabela) <- tabela[1,]
    tabela <- tabela[-1,]
    rn=T
  }
  tabela <- tabela %>%
    mutate_if(is.character, function(x) {
      as.numeric(x)
    })
  
  tabela <- tabela %>%
    mutate_if(is.numeric, function(x) {
      format(x, decimal.mark = ",")
    })

  print(xtable(tabela, type = "latex"), include.rownames=rn)
}

