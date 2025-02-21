# Funções de módulo de Demografia Estadual
# Função de UI
tipo_ui <- function(id) {
  fluidPage(
    
    #Controle----
    panel(
      fluidRow(
        column(2,
               selectInput(
                 inputId = NS(id, "tipo"),
                 label = "TIPO DE VEÍCULO",
                 choices = frota %>% filter(tipo_veiculo != "Total de Veículos") %>% pull(tipo_veiculo) %>% unique(),
                 width = "200px"
               )
        ),
        column(2,
        popover(
          title = "Filtro Dinâmico",
               selectInput(
                 inputId = NS(id, "ano"),
                 label = "ANO",
                 choices = NULL,
                 width = "200px"
               ),
               placement = "right",
               content = "A lista de anos está condicionada à ocorrência do licenciamento do tipo de veículo, 
               e esses anos serão exibidos no filtro dinâmico. Por exemplo, se um carro foi licenciado em 2020, 
               esse ano específico estará disponível para seleção no filtro dinâmico"
        )
        ) 
      )
    ),
    
    #Perfil Pará/R.I----
    box(
      title = textOutput(NS(id, "txtgeral")),
      status = "primary",
      collapsed = FALSE,
      headerBorder = TRUE,
      width = 12,
      withSpinner(
        reactableOutput(NS(id,"tab")),
        type = 8,
        color = "#007bff",
        size = 0.8
      ),
      footer = 
        list(
          div(
            style = "display: flex; justify-content: space-between;",
            div(
              tags$h6(tags$b("Fonte:", style = 'font-family: sans-serif;'), "RENAVAM/DTI/DETRAN-PA"),
              tags$h6(tags$b("Elaboração:"), "CNP/GAETRA/DETRAN-PA")
            ),
            div(
              style = "display: flex; justify-content: center; align-items: center;",
              downset_ui(NS(id, "tabdown"))
            )
          )
        )
    ),
    fluidRow(
      #Catergoria Veículos----
      box(
        title = textOutput(NS(id, "txtcat")),
        status = "primary",
        collapsed = FALSE,
        headerBorder = TRUE,
        width = 6,
        withSpinner(
          echarts4rOutput(NS(id,"catbar"),height = "600px"),
          type = 8,
          color = "#007bff",
          size = 0.8
        ),
        footer = 
          list(
            div(
              style = "display: flex; justify-content: space-between;",
              div(
                tags$h6(tags$b("Fonte:", style = 'font-family: sans-serif;'), "RENAVAM/DTI/DETRAN-PA"),
                tags$h6(tags$b("Elaboração:"), "CNP/GAETRA/DETRAN-PA")
              ),
              div(
                style = "display: flex; justify-content: center; align-items: center;",
                downset_ui(NS(id, "catdown"))
              )
            )
          )),
      #Cor dos Veículos----
      box(
        title = textOutput(NS(id, "txtcor")),
        status = "primary",
        collapsed = FALSE,
        headerBorder = TRUE,
        width = 6,
        withSpinner(
          echarts4rOutput(NS(id,"corbar"),height = "600px"),
          type = 8,
          color = "blue",
          size = 0.8
        ),
        footer = 
          list(
            div(
              style = "display: flex; justify-content: space-between;",
              div(
                tags$h6(tags$b("Fonte:", style = 'font-family: sans-serif;'), "RENAVAM/DTI/DETRAN-PA"),
                tags$h6(tags$b("Elaboração:"), "CNP/GAETRA/DETRAN-PA")
              ),
              div(
                style = "display: flex; justify-content: center; align-items: center;",
                downset_ui(NS(id, "cordown"))
              )
            )
          )),
      #Tipo de Combustível----
      box(
        title = textOutput(NS(id, "txtcombu")),
        status = "primary",
        collapsed = FALSE,
        headerBorder = TRUE,
        width = 12,
        withSpinner(
          echarts4rOutput(NS(id,"combubar"),height = "600px"),
          type = 8,
          color = "blue",
          size = 0.8
        ),
        footer = 
          list(
            div(
              style = "display: flex; justify-content: space-between;",
              div(
                tags$h6(tags$b("Fonte:", style = 'font-family: sans-serif;'), "RENAVAM/DTI/DETRAN-PA"),
                tags$h6(tags$b("Elaboração:"), "CNP/GAETRA/DETRAN-PA")
              ),
              div(
                style = "display: flex; justify-content: center; align-items: center;",
                downset_ui(NS(id, "combudown"))
              )
            )
          )),
      
      #Espécie do Veículo----
      box(
        title = textOutput(NS(id, "txtesp")),
        status = "primary",
        collapsed = FALSE,
        headerBorder = TRUE,
        width = 6,
        withSpinner(
          echarts4rOutput(NS(id,"espbar"),height = "600px"),
          type = 8,
          color = "blue",
          size = 0.8
        ),
        footer = 
          list(
            div(
              style = "display: flex; justify-content: space-between;",
              div(
                tags$h6(tags$b("Fonte:", style = 'font-family: sans-serif;'), "RENAVAM/DTI/DETRAN-PA"),
                tags$h6(tags$b("Elaboração:"), "CNP/GAETRA/DETRAN-PA")
              ),
              div(
                style = "display: flex; justify-content: center; align-items: center;",
                downset_ui(NS(id, "espdown"))
              )
            )
          )),
      #Nacionalidade do Veículo----
      box(
        title = textOutput(NS(id, "txtnac")),
        status = "primary",
        collapsed = FALSE,
        headerBorder = TRUE,
        width = 6,
        withSpinner(
          echarts4rOutput(NS(id,"nacpie"),height = "600px"),
          type = 8,
          color = "#007bff",
          size = 0.8
        ),
        footer = 
          list(
            div(
              style = "display: flex; justify-content: space-between;",
              div(
                tags$h6(tags$b("Fonte:", style = 'font-family: sans-serif;'), "RENAVAM/DTI/DETRAN-PA"),
                tags$h6(tags$b("Elaboração:"), "CNP/GAETRA/DETRAN-PA")
              ),
              div(
                style = "display: flex; justify-content: center; align-items: center;",
                downset_ui(NS(id, "nacdown"))
              )
            )
          ))
    )
  )
}

# Função do modulo servidor
tipo_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    #Atualização de entrada ano----
    #Filtro de ano dinâmico----
    anotipo <- reactive({
      frota %>% filter(tipo_veiculo == input$tipo,valor > 0)
    })
    #Sub temática
    observeEvent(anotipo(), {
      choices <- sort(unique(anotipo()[["ano"]]), decreasing = TRUE)
      updateSelectInput(inputId = "ano", choices = choices,  session) 
    })
    #Categoria dos Veículos - Gráfico Barras ----   
    #Título
    titulo1 <- renderText({
    req(input$ano)
    paste0("Categorias dos Veículos Registrados do tipo ",input$tipo," - ",input$ano)
    })
    
    output$txtcat <- renderText({
      titulo1()  
    })
    
    #Download
    downcat <- reactive({
      req(input$ano)
        frota %>% filter( tipo_veiculo == input$tipo, ano == input$ano, variavel == "Categoria do veículo por tipo") %>% 
        group_by(categoria) %>% 
        summarize( valor = sum(valor,na.rm = T),.groups = 'drop') %>% complete() %>% 
        arrange(valor) %>% mutate(tipo_veiculo = input$tipo, variavel = "Categoria do veículo", ano = input$ano) %>% 
        select(tipo_veiculo,variavel,categoria,ano,valor)  
    })
    
    #Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(downcat(),{
      titulo1()
      downset_Server("catdown", downcat(), titulo1())  
    })
    
    output$catbar <- renderEcharts4r({
      req(input$ano)
      frota %>% filter( tipo_veiculo == input$tipo, ano == input$ano, variavel == "Categoria do veículo por tipo") %>% 
        group_by(categoria) %>% 
        summarize( valor = sum(valor,na.rm = T),.groups = 'drop') %>% complete() %>% 
        arrange(valor) %>%
        e_charts(x = categoria) %>%
        e_bar(
          serie = valor,
          color = "blue",
          name = "Quantidade",
          legend = F,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = TRUE,
          itemStyle = list(barBorderRadius = 3)
        ) %>%
        e_labels(
          position = "right",
          fontWeight = "bold",
          formatter = htmlwidgets::JS(
            glue::glue(
              "function(params) {return Intl.NumberFormat('pt-BR', { style: 'decimal'}).format(params.value[{{0}}]);}",
              .open = "{{",
              .close = "}}"
            )
          )
        ) %>%
        e_y_axis(
          name = "Quantidade",
          nameTextStyle =
            list(
              fontWeight = "bold",
              padding = c(30, 0, 0, 0),
              fontSize = 14
            ),
          scale = TRUE,
          splitNumber = 4,
          nameLocation = "middle",
          axisLabel = list(
            formatter = htmlwidgets::JS(
              "
              function (value, index) {
              return value.toLocaleString('pt-BR', { minimumFractionDigits: 0, maximumFractionDigits: 0 });
              }
            "
            )
          )
        ) %>%
        e_locale("pt-Br") %>%
        e_grid(show = TRUE,containLabel = TRUE,left = "5%") %>%
        e_animation(duration = 5000) %>%
        e_toolbox_feature(feature = "saveAsImage") %>%
        e_toolbox_feature(feature = "dataZoom") %>%
        e_toolbox_feature(feature = "dataView") %>%
        e_flip_coords()
    })
    #Cor dos Veículos - Gráfico de Barras----
    #Título
    titulo2 <- renderText({
    req(input$ano)
    paste0("Cores dos Veículos Registrados do tipo ",input$tipo," - ",input$ano)
    })
    
    output$txtcor <- renderText({
      titulo2()  
    })
    
    #Download
    downcor <- reactive({
      req(input$ano)
      frota %>% filter( tipo_veiculo == input$tipo, ano == input$ano, variavel == "Cor do veículo por tipo") %>% 
        group_by(categoria) %>% 
        summarize( valor = sum(valor,na.rm = T),.groups = 'drop') %>% complete() %>% 
        arrange(valor) %>% mutate(tipo_veiculo = input$tipo, variavel = "Cor do veículo",ano = input$ano) %>% 
        select(tipo_veiculo,variavel,categoria,ano,valor)
    })
    
    #Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(downcor(),{
      titulo2()
      downset_Server("cordown", downcor(), titulo2())  
    })
    
    output$corbar <- renderEcharts4r({
      req(input$ano)
      frota %>% filter( tipo_veiculo == input$tipo, ano == input$ano, variavel == "Cor do veículo por tipo") %>% 
        group_by(categoria) %>% 
        summarize( valor = sum(valor,na.rm = T),.groups = 'drop') %>% complete() %>% 
        arrange(valor) %>% 
        e_charts(x = categoria) %>%
        e_bar(
          serie = valor,
          color = "blue",
          name = "Quantidade",
          legend = FALSE,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = TRUE,
          itemStyle = list(barBorderRadius = 2)
        ) %>%
        e_labels(
          position = "right",
          fontWeight = "bold",
          formatter = htmlwidgets::JS(
            glue::glue(
              "function(params) {return Intl.NumberFormat('pt-BR', { style: 'decimal'}).format(params.value[{{0}}]);}",
              .open = "{{",
              .close = "}}"
            )
          )
        ) %>%
        e_y_axis(
          name = "Frequência",
          nameTextStyle =
            list(
              fontWeight = "bold",
              padding = c(30, 0, 0, 0),
              fontSize = 14
            ),
          scale = TRUE,
          splitNumber = 4,
          nameLocation = "middle",
          axisLabel = list(
            formatter = htmlwidgets::JS(
              "
              function (value, index) {
              return value.toLocaleString('pt-BR', { minimumFractionDigits: 0, maximumFractionDigits: 0 });
              }
            "
            )
          )
        ) %>%
        e_locale("pt-Br") %>%
        e_grid(show = TRUE,containLabel = T,left = "5%") %>%
        e_animation(duration = 5000) %>%
        e_toolbox_feature(feature = "saveAsImage") %>%
        e_toolbox_feature(feature = "dataZoom") %>%
        e_toolbox_feature(feature = "dataView") %>%
        e_flip_coords()
    })
    
    #Tipo de Combustível - Gráfico de Barras----
    #Título
    titulo3 <- renderText({
    req(input$ano)
    paste0("Combustível utilizado pelos Veículos Registrados do tipo ",input$tipo," - ",input$ano)
    })
    
    output$txtcombu <- renderText({
      titulo3()  
    })
    
    #Download
    downcombu <- reactive({
      req(input$ano)
      frota %>% filter( tipo_veiculo == input$tipo, ano == input$ano, variavel == "Tipo de combustível(s) utilizado(s) por tipo") %>% 
        group_by(categoria) %>% 
        summarize( valor = sum(valor,na.rm = T),.groups = 'drop') %>% complete() %>% 
        arrange(valor) %>% mutate(tipo_veiculo = input$tipo, variavel = "Tipo de combustível(s) utilizado(s)",ano = input$ano) %>% 
        select(tipo_veiculo,variavel,categoria,ano,valor)
    })
    
    #Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(downcombu(),{
      titulo3()
      downset_Server("combudown", downcombu(), titulo3())  
    })
    
    output$combubar <- renderEcharts4r({
      req(input$ano)
      frota %>% filter( tipo_veiculo == input$tipo, ano == input$ano, variavel == "Tipo de combustível(s) utilizado(s) por tipo") %>% 
        group_by(categoria) %>% 
        summarize( valor = sum(valor,na.rm = T),.groups = 'drop') %>% complete() %>% 
        arrange(valor) %>%
        e_charts(x = categoria) %>%
        e_bar(
          serie = valor,
          color = "blue",
          name = "Quantidade",
          legend = F,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = TRUE,
          itemStyle = list(barBorderRadius = 3)
        ) %>%
        e_labels(
          position = "right",
          fontWeight = "bold",
          formatter = htmlwidgets::JS(
            glue::glue(
              "function(params) {return Intl.NumberFormat('pt-BR', { style: 'decimal'}).format(params.value[{{0}}]);}",
              .open = "{{",
              .close = "}}"
            ) 
          )
        ) %>%  
        e_y_axis(
          name = "Quantidade",
          nameTextStyle =
            list(
              fontWeight = "bold",
              padding = c(30, 0, 0, 0),
              fontSize = 14
            ),
          scale = TRUE,
          splitNumber = 8,
          nameLocation = "middle",
          axisLabel = list(
            formatter = htmlwidgets::JS(
              "
              function (value, index) {
              return value.toLocaleString('pt-BR', { minimumFractionDigits: 0, maximumFractionDigits: 0 });
              }
            "
            )
          )
        ) %>%
        e_locale("pt-Br") %>%
        e_grid(show = T,left = "15%") %>%
        e_animation(duration = 5000) %>%
        e_toolbox_feature(feature = "saveAsImage") %>%
        e_toolbox_feature(feature = "dataZoom") %>%
        e_toolbox_feature(feature = "dataView") %>%
        e_flip_coords()
    })
    
    #Espécie do Veículo - Gráfico de Barras----
    #Título
    titulo4 <- renderText({
    req(input$ano)
    paste0("Espécie dos Veículos Licenciados do tipo ",input$tipo," - ",input$ano)
    })
    
    output$txtesp <- renderText({
      titulo4()  
    })
    
    #Download
    downesp <- reactive({
      req(input$ano)
      frota %>% filter( tipo_veiculo == input$tipo, ano == input$ano, variavel == "Espécie de Veículo por tipo") %>% 
        group_by(categoria) %>% 
        summarize( valor = sum(valor,na.rm = T),.groups = 'drop') %>% complete() %>% 
        arrange(valor) %>% mutate(tipo_veiculo = input$tipo, variavel = "Espécie de Veículo",ano = input$ano) %>% 
        select(tipo_veiculo,variavel,categoria,ano,valor)
    })
    
    #Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(downesp(),{
      titulo4()
      downset_Server("espdown", downesp(), titulo4())  
    })
    
    output$espbar <- renderEcharts4r({
      req(input$ano)
      frota %>% filter( tipo_veiculo == input$tipo, ano == input$ano, variavel == "Espécie de Veículo por tipo") %>% 
        group_by(categoria) %>% 
        summarize( valor = sum(valor,na.rm = T),.groups = 'drop') %>% complete() %>% 
        arrange(valor) %>% 
        e_charts(x = categoria) %>%
        e_bar(
          serie = valor,
          color = "blue",
          name = "Quantidade",
          legend = FALSE,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = TRUE,
          itemStyle = list(barBorderRadius = 3)
        ) %>%
        e_labels(
          position = "top",
          fontWeight = "bold",
          formatter = htmlwidgets::JS(
            glue::glue(
              "function(params) {return Intl.NumberFormat('pt-BR', { style: 'decimal'}).format(params.value[{{1}}]);}",
              .open = "{{",
              .close = "}}"
            )
          )
        ) %>%
        e_y_axis(
          name = "",
          nameTextStyle =
            list(
              fontWeight = "bold",
              padding = c(40, 0, 0, 0),
              fontSize = 14
            ),
          scale = TRUE,
          splitNumber = 8,
          nameLocation = "middle",
          axisLabel = list(
            formatter = htmlwidgets::JS(
              "
              function (value, index) {
              return value.toLocaleString('pt-BR', { minimumFractionDigits: 0, maximumFractionDigits: 0 });
              }
            "
            )
          )
        ) %>%
        e_locale("pt-Br") %>%
        e_grid(show = TRUE,left = "15%") %>%
        e_animation(duration = 5000) %>%
        e_toolbox_feature(feature = "saveAsImage") %>%
        e_toolbox_feature(feature = "dataZoom") %>%
        e_toolbox_feature(feature = "dataView") 
    })
    
    #Nacionalidade do Veículo - Gráfico de Setor----
    #Título
    titulo5 <- renderText({
      req(input$ano)
    paste0("Nacionalidade dos Veículos Licenciados do tipo ",input$tipo," - ",input$ano)  
    })
    
    output$txtnac <- renderText({
      titulo5()  
    })
    
    #Download
    downac <- reactive({
      req(input$ano)
      frota %>% filter( tipo_veiculo == input$tipo, ano == input$ano, variavel == "Nacionalidade do Veículo por tipo") %>% 
        group_by(categoria) %>% summarize( valor = sum(valor,na.rm = T),.groups = 'drop') %>% 
        arrange(valor) %>% mutate(tipo_veiculo = input$tipo, ano = input$ano, percentual = (valor/sum(valor,na.rm = T))*100) %>% 
        select(tipo_veiculo,categoria,ano,valor,percentual)
    })
    
    #Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(downac(),{
      titulo5()
      downset_Server("nacdown", downac(), titulo5())  
    })
    
    
    output$nacpie <- renderEcharts4r({
      req(input$ano)
      frota %>% filter( tipo_veiculo == input$tipo, ano == input$ano, variavel == "Nacionalidade do Veículo por tipo") %>% 
        group_by(categoria) %>% summarize( valor = sum(valor,na.rm = T),.groups = 'drop') %>% 
        arrange(valor) %>% mutate(ano = input$ano, percentual = (valor/sum(valor,na.rm = T))*100) %>% 
        e_charts(x = categoria) %>%
        e_pie(
          serie = percentual,
          selectedMode = T,
          cursor = "pointer"
        ) %>% 
        e_tooltip(
          trigger = "item", formatter = htmlwidgets::JS("
      function(params) {
        var valor = params.data.value.toLocaleString('pt-BR');
        var percentual = params.percent.toFixed(2).replace('.', ',');
        return '<b>' + params.name + '</b>' + ' : ' + valor + ' (' + percentual + '%)';
      }
    ")
        )
    })
    
    
    #Tabelas - Perfil----
    titulo6 <- renderText({
    req(input$ano)
    paste0("Estatística Resumo dos Veículos Registrados do tipo ",input$tipo," - ",input$ano)
    })
    
    output$txtgeral <- renderText({
      titulo6()  
    })
    
    #Download
    dowtab <- reactive({
      req(input$ano)
      categoria <-
        frota %>% 
        filter(tipo_veiculo == input$tipo, variavel == "Categoria do veículo por tipo", ano == input$ano) %>% 
        group_by(categoria) %>% summarize( valor = sum(valor,na.rm = T),.groups = 'drop') %>% 
        mutate(Percentual = (valor / sum(valor, na.rm = TRUE)) * 100) %>%  
        arrange(desc(valor)) %>%  slice_head(n = 1)
      
      cor <-
        frota %>% 
        filter(tipo_veiculo == input$tipo, variavel == "Cor do veículo por tipo", ano == input$ano) %>%
        group_by(categoria) %>% summarize( valor = sum(valor,na.rm = T),.groups = 'drop') %>% 
        mutate(Percentual = (valor / sum(valor, na.rm = TRUE)) * 100) %>%   
        arrange(desc(valor)) %>% slice_head(n = 1)
      
      combustivel <-
        frota %>% 
        filter(tipo_veiculo == input$tipo,variavel == "Tipo de combustível(s) utilizado(s) por tipo", ano == input$ano) %>% 
        group_by(categoria) %>% summarize( valor = sum(valor,na.rm = T),.groups = 'drop') %>% 
        mutate(Percentual = (valor / sum(valor, na.rm = TRUE)) * 100) %>% 
        arrange(desc(valor)) %>%  slice_head(n = 1)
      
      especie <-
        frota %>% 
        filter(tipo_veiculo == input$tipo, variavel == "Espécie de Veículo por tipo", ano == input$ano) %>% 
        group_by(categoria) %>% summarize( valor = sum(valor,na.rm = T),.groups = 'drop') %>% 
        mutate(Percentual = (valor / sum(valor, na.rm = TRUE)) * 100) %>%  
        arrange(desc(valor)) %>%  slice_head(n = 1)
      
      nacionalidade <-
        frota %>% 
        filter(tipo_veiculo == input$tipo, variavel == "Nacionalidade do Veículo por tipo", ano == input$ano) %>% 
        group_by(categoria) %>% summarize( valor = sum(valor,na.rm = T),.groups = 'drop') %>% 
        mutate(Percentual = (valor / sum(valor, na.rm = TRUE)) * 100) %>%  
        arrange(desc(valor)) %>%  slice_head(n = 1)
      
      df <- rbind(categoria,cor,combustivel,especie,nacionalidade)
        # select(tipo_veiculo,variavel,categoria,ano,valor,Percentual)
    })
    
    #Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(dowtab(),{
      titulo6()
      downset_Server("tabdown", dowtab(), titulo6())  
    })
    
    output$tab <- renderReactable({
      req(input$ano)
      categoria <-
        frota %>% 
        filter(tipo_veiculo == input$tipo, variavel == "Categoria do veículo por tipo", ano == input$ano) %>% 
        group_by(categoria) %>% summarize( valor = sum(valor,na.rm = T),.groups = 'drop') %>% 
        mutate(Percentual = (valor / sum(valor, na.rm = TRUE)) * 100) %>%  
        arrange(desc(valor)) %>%  slice_head(n = 1)
      
      cor <-
        frota %>% 
        filter(tipo_veiculo == input$tipo, variavel == "Cor do veículo por tipo", ano == input$ano) %>%
        group_by(categoria) %>% summarize( valor = sum(valor,na.rm = T),.groups = 'drop') %>% 
        mutate(Percentual = (valor / sum(valor, na.rm = TRUE)) * 100) %>%   
        arrange(desc(valor)) %>% slice_head(n = 1)
      
      combustivel <-
        frota %>% 
        filter(tipo_veiculo == input$tipo,variavel == "Tipo de combustível(s) utilizado(s) por tipo", ano == input$ano) %>% 
        group_by(categoria) %>% summarize( valor = sum(valor,na.rm = T),.groups = 'drop') %>% 
        mutate(Percentual = (valor / sum(valor, na.rm = TRUE)) * 100) %>% 
        arrange(desc(valor)) %>%  slice_head(n = 1)
      
      especie <-
        frota %>% 
        filter(tipo_veiculo == input$tipo, variavel == "Espécie de Veículo por tipo", ano == input$ano) %>% 
        group_by(categoria) %>% summarize( valor = sum(valor,na.rm = T),.groups = 'drop') %>% 
        mutate(Percentual = (valor / sum(valor, na.rm = TRUE)) * 100) %>%  
        arrange(desc(valor)) %>%  slice_head(n = 1)
      
      nacionalidade <-
        frota %>% 
        filter(tipo_veiculo == input$tipo, variavel == "Nacionalidade do Veículo por tipo", ano == input$ano) %>% 
        group_by(categoria) %>% summarize( valor = sum(valor,na.rm = T),.groups = 'drop') %>% 
        mutate(Percentual = (valor / sum(valor, na.rm = TRUE)) * 100) %>%  
        arrange(desc(valor)) %>%  slice_head(n = 1)
      
      df <- rbind(categoria,cor,combustivel,especie,nacionalidade)
      
      df %>% reactable(
        defaultPageSize = 10,
        striped = FALSE,
        highlight = TRUE,
        bordered = TRUE,
        outlined = TRUE,
        resizable = TRUE,
        showSortable = TRUE,
        pagination = F,
        columns =  list(
          variavel = colDef(name = "Características"),
          categoria = colDef(name = "Predominância"),
          valor = colDef(name = "Quantidade", format = colFormat(separators = T, locales = "pt-BR")),
          Percentual = colDef(
            name = "Percentual(%)",
            format = colFormat(
              separators = T,
              locales = "pt-BR",
              digits = 2
            )
          )
        ),
        defaultColDef = colDef(
          na = "-", 
          footerStyle = list(fontWeight = "bold"),
          headerStyle = list(background = "#f7f7f8")
        ),
        language = reactableLang(
          noData = "Sem informação",
          pageInfo = "{rowStart} a {rowEnd} de {rows} linhas",
          pagePrevious = "Anterior",
          pageNext = "Próximo",
          pagePreviousLabel = "Anterior",
          pageNextLabel = "Proximo"
        )
      )
    })
    
    
  })
}
# Play do Módulo
 ui = dashboardPage(
   header = dashboardHeader(),
   sidebar = dashboardSidebar(),
   body = dashboardBody(fluidPage(tipo_ui("tipo"))))
 
 server <- function(input, output) {
   tipo_Server("tipo")
 }
 
 shinyApp(ui, server)