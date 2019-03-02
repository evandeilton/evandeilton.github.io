#########################################################################################
## title:        Códigos do curso R NA PRÁTICA: Estatística Descomplicada com R
## Módulo:       Estatística I - Descritiva
## author:       Lopes
## data:         26/01/2019
#########################################################################################

# ---------------------------------------------------------------------------------------
# Renderização do livro

#setwd("C:/Users/Evandeilton/Dropbox/02_Produtos_Digitais/02_Udemy/01_RNP/STATS/eBookv5")
#setwd("C:/Users/home/Dropbox/02_Produtos_Digitais/02_Udemy/01_RNP/STATS/eBookv5")
setwd("~/Dropbox/02_Produtos_Digitais/02_Udemy/01_RNP/STATS/eBookv5")

bookdown::render_book("index.Rmd", "bookdown::gitbook",  encoding = "UTF-8",
                      clean = TRUE, clean_envir = TRUE)

bookdown::render_book("index.Rmd", "bookdown::pdf_book", encoding = "UTF-8",
                      clean = TRUE, clean_envir = TRUE)


# ---------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------------
# Pacotes, funções e dados necessários para a sessão
pacotes <- c("tidyverse","lubridate", "magrittr","broom","stringr", "plotly","ggplot2",
             "data.table","citr","DT","formatR", "svglite","tufte")

lapply(pacotes, function(pkg) {
  if (!require(pkg, character.only = TRUE)) install.packages(pkg)
})

# Funções extras
source("Codes/02_FUNCOES.R")

setwd("~/Dropbox/02_Produtos_Digitais/02_Udemy/01_RNP/STATS/eBookv5")

# Base de IES
base_ies <- rnp_read("Dados/CSV/DM_IES.CSV")
base_cur <- rnp_read("Dados/CSV/DM_CURSO.CSV")
base_doc <- rnp_read("Dados/CSV/DM_DOCENTE.CSV")
base_loc <- rnp_read("Dados/CSV/DM_LOCAL_OFERTA.CSV")
base_ies_prep <- rnp_read("Dados/CSV/DM_IES_PREP.CSV", sep = ";", dec = ".", encoding = "UTF-8")

# ---------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------
# Trecho 01: Criando uma amostra tamanho 10 sem reposição da base de IES
sample_n(tbl = base_ies, size = 10, replace = FALSE) %>%
  transmute(Nome = NO_IES, Sigla = SGL_IES, TotalTecnicos = QT_TEC_TOTAL,
            ReceitaPropria = VL_RECEITA_PROPRIA, DepesaPesquisa = VL_DES_PESQUISA) %>%
  knitr::kable(caption = "Exemplos de conjunto de dados.", align = "llrrr", booktabs = TRUE)
## -------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------
# Trecho 02: Frequências simples para escolaridade do docente
rnp_freq(x = base_docentes$escolaridade, sortd = TRUE, digits = 3) %>%
  knitr::kable(booktabs = TRUE, format = tb_formata,
               caption = "Frequências simples para escolaridade do docente")
## -------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------
# Trecho 03: Frequências cruzadas da escolaridade por sexo do docent
rnp_2freq(x = base_docentes$escolaridade, y = base_docentes$sexo,
          digits = 3, percents = TRUE) %>%
  knitr::kable(booktabs = TRUE, format = tb_formata,
               caption = "Frequências cruzadas da escolaridade por sexo do docente")
## -------------------------------------------------------------------------------------


# ---------------------------------------------------------------------------------------
# Trecho 04: Gráfico de setores, barras e mosaicplot
tb <- rnp_freq(base_docentes$escolaridade)

# Setores
tb <- rnp_freq(base_docentes$faixaIdade)

if(tipo_grafico == "ggplot") {
  p <- ggplot(tb, aes("", fr, fill = classe))
  p <- p + geom_bar(width = 1, size = 1, color = "white", stat = "identity") +
    coord_polar("y") +
    geom_text(aes(label = paste0(round(100*fr, 1), "%")),
              position = position_stack(vjust = 0.5), size=3) +
    labs(x = NULL, y = NULL, fill = NULL, title = "") +
    guides(fill = guide_legend(reverse = TRUE)) +
    theme_classic() +
    theme(axis.line = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position="right", legend.text = element_text("Classe"))
} else {
  p <- plot_ly(tb, labels = ~classe, values = ~fr, colors = ~classe, type = 'pie') %>%
    layout(title = '',
           xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
           yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
}
p

# Barras
if(tipo_grafico == "ggplot") {
  p <- ggplot(tb, aes(classe, fr, fill = classe))
  p <- p + geom_bar(width = 1, size = 1, color = "white",
                    stat = "identity", show.legend = FALSE) +
    geom_text(aes(label = paste0(round(100*fr, 1), "%")),
              position = position_stack(vjust = 1), size=3) +
    labs(x = NULL, y = NULL, fill = NULL, title = "") +
    theme_classic() + coord_flip() +
    theme(axis.line = element_blank())
} else {
  p <- plot_ly(tb, x = ~classe, y = ~fr, type = "bar") %>%
    layout(title = '',
           xaxis = list(title = "", showgrid = FALSE),
           yaxis = list(title = "", showgrid = FALSE))
}
p

p <- ggplot2::ggplot(base_docentes)
p <- p + theme_classic() +
  ggmosaic::geom_mosaic(aes(x = product(sexo), fill = faixaIdade)) +
  labs(x = NULL, y = NULL, fill = NULL, title = "") +
  theme(axis.line = element_blank())
if(tipo_grafico == "ggplot") p else plotly::ggplotly(p)
p
## -------------------------------------------------------------------------------------

aggregate(idade ~ escolaridade,
          data = base_docentes,
          FUN = function(i) rnp::rnp_summary(i))




rnp_summary_by(base_docentes, variavel = "idade", grupos = c("sexo", "faixaIdade"))


plyr::ddply(base_docentes, .(escolaridade), summarise, rnp_summary(idade))


tapply(base_docentes$idade,
       base_docentes$escolaridade,
       function(i) {
         t(rnp::rnp_summary(i, digits = 3))
       }) %>%
  dplyr::bind_rows()


