#Carregamento de pacotes
if (!require(pacman)) install.packages("pacman")

pacman::p_load(  
shiny, # Permite criar aplicativos web interativos com R
shinyWidgets, # Adiciona mais widgets e opções ao Shiny
shinycssloaders, # Adiciona animações de carregamento a saídas Shiny
shinydashboard,
shinydashboardPlus,
shinyjs, # Permite usar JavaScript em aplicativos Shiny
waiter, # Adiciona telas de carregamento a aplicativos Shiny
rintrojs, # Oferece funcionalidades para adicionar tutoriais interativos (introduções) em aplicativos Shiny usando o intro.js
bs4Dash, # Fornece um conjunto de ferramentas e temas Bootstrap 4 para criar dashboards interativos com Shiny
# Manipulação e visualização de dados
tidyverse, # Uma coleção de pacotes R para manipulação e visualização de dados
janitor, # Fornece funções para limpeza de dados
# reactablefmtr, # Fornece funções de formatação para o pacote reactable
reactable, # Cria tabelas interativas com R
# viridis, # Fornece paletas de cores para visualizações de dados
# Interação com HTML e JavaScript
htmlwidgets, # Permite criar widgets HTML para uso em documentos R Markdown e aplicativos Shiny
htmltools, # Fornece ferramentas para trabalhar com HTML em R
# Leitura e escrita de arquivos
openxlsx, # Lê e escreve arquivos do Microsoft Excel
readxl, # Lê arquivos do Excel em R
# Gráficos interativos
echarts4r, # Cria gráficos interativos com a biblioteca JavaScript Echarts
billboarder, # Cria gráficos interativos com a biblioteca JavaScript billboard.js
# Dados geoespaciais
#geobr, # Lê dados geoespaciais do Brasil em diferentes níveis administrativos
leaflet, # Cria mapas interativos com R
sf, # Fornece classes e funções para trabalhar com dados geoespaciais em R
sp, # Fornece classes e métodos para análise de dados espaciais em R
# Outros
fontawesome, # Fornece acesso aos ícones do Font Awesome em documentos R Markdown e aplicativos Shiny
BAMMtools, # Fornece ferramentas para análise filogenética de taxas de diversificação usando o método BAMM (Bayesian Analysis of Macroevolutionary Mixtures,
readr, # Facilita a leitura de dados retangulares (como CSV ou TSV) em R, parte do conjunto de pacotes tidyverse
classInt # Seleciona intervalos de classe para dados numéricos
)

#Carregar base de dados
load("data2025-03-29.RData")

#Função de formatação mapa
labelFormat_decimal <- function (prefix = "", suffix = "", between = " &ndash; ", digits = 3,
                                 big.mark = ",", transform = identity, decimal.mark = "."){
  formatNum <- function(x) {
    format(round(transform(x), digits), trim = TRUE, scientific = FALSE,
           big.mark = big.mark, decimal.mark = decimal.mark)
  }
  function(type, ...) {
    switch(type, numeric = (function(cuts) {
      paste0(prefix, formatNum(cuts), suffix)
    })(...), bin = (function(cuts) {
      n <- length(cuts)
      paste0(prefix, formatNum(cuts[-n]), between, formatNum(cuts[-1]),
             suffix)
    })(...), quantile = (function(cuts, p) {
      n <- length(cuts)
      p <- paste0(round(p * 100), "%")
      cuts <- paste0(formatNum(cuts[-n]), between, formatNum(cuts[-1]))
      paste0("<span title=\"", cuts, "\">", prefix, p[-n],
             between, p[-1], suffix, "</span>")
    })(...), factor = (function(cuts) {
      paste0(prefix, as.character(transform(cuts)), suffix)
    })(...))
  }
}
#Carregar Módulos
source("modulos/01_geral.R")
source("modulos/02_1_carac_PA_RI.R")
source("modulos/02_2_carac_muni.R")
source("modulos/02_3_carac_tipo.R")
source("modulos/03_mod_downset.R")
source("modulos/frota_pa.R")
source("modulos/frota_muni.R")