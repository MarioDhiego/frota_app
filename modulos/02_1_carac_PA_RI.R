
# Função de UI
loc_ui <- function(id) {
  fluidPage(
      #Controle----
      panel(
        fluidRow(
          column(2,
                 selectInput(
                   inputId = NS(id, "local"),
                   label = "LOCALIDADE",
                   choices = unique(frota_acumulado[["ri"]]),
                   width = "200px"
                 )
          ),
          column(2,
                 selectInput(
                   inputId = NS(id, "anolocal"),
                   label = "ANO",
                   choices = sort(unique(frota_acumulado[["ano"]]), decreasing = TRUE),
                   width = "200px"
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
            color = "#007bff",
            size = 0.5
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
          width = 6,
          withSpinner(
            echarts4rOutput(NS(id,"combubar"),height = "600px"),
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
        #,
      )
  )
}

# Função do modulo servidor
loc_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    #Tabelas - Perfil----
    titulo6 <- renderText({
      if (input$local == "Pará") {
        paste0("Estatística Resumo de Veículos Registrados - ",input$local," - ",input$anolocal)
      }else{
        paste0("Estatística Resumo de Veículos Registrados - Região de Integração ",input$local," - ",input$anolocal)  
      }
    })
    
    output$txtgeral <- renderText({
      titulo6()  
    })
    
    #Download
    dowtab <- reactive({
      categoria <-
        db02_categoria_acumulado %>% 
        filter(ri == input$local, ano == input$anolocal) %>% 
        group_by(variavel,categoria) %>% summarize( valor = sum(valor,na.rm = TRUE),.groups = 'drop') %>% 
        mutate(Percentual = (valor / sum(valor, na.rm = TRUE)) * 100) %>%  
        arrange(desc(valor)) %>%  slice_head(n = 1)
      
      cor <-
        db04_cores_acumulado %>% 
        filter(ri == input$local, ano == input$anolocal) %>%
        group_by(variavel,categoria) %>% summarize( valor = sum(valor,na.rm = TRUE),.groups = 'drop') %>% 
        mutate(Percentual = (valor / sum(valor, na.rm = TRUE)) * 100) %>%   
        arrange(desc(valor)) %>% slice_head(n = 1)
      
      combustivel <-
        db05_combustivel_acumulado %>% 
        filter(ri == input$local, ano == input$anolocal) %>% 
        group_by(variavel,categoria) %>% summarize( valor = sum(valor,na.rm = TRUE),.groups = 'drop') %>% 
        mutate(Percentual = (valor / sum(valor, na.rm = TRUE)) * 100) %>% 
        arrange(desc(valor)) %>%  slice_head(n = 1)
      
      especie <-
        db06_especie_veiculo_acumulado %>% 
        filter(ri == input$local, ano == input$anolocal) %>% 
        group_by(variavel,categoria) %>% summarize( valor = sum(valor,na.rm = T),.groups = 'drop') %>% 
        mutate(Percentual = (valor / sum(valor, na.rm = TRUE)) * 100) %>%  
        arrange(desc(valor)) %>%  slice_head(n = 1)
      
      nacionalidade <-
        db07_nacionalidade_acumulado %>% 
        filter(ri == input$local, ano == input$anolocal) %>% 
        group_by(variavel,categoria) %>% summarize( valor = sum(valor,na.rm = TRUE),.groups = 'drop') %>% 
        mutate(Percentual = (valor / sum(valor, na.rm = TRUE)) * 100) %>%  
        arrange(desc(valor)) %>%  slice_head(n = 1)
      
      df <- rbind(categoria,cor,combustivel,especie,nacionalidade)
      df <- df %>% mutate(ri = input$local,ano = input$anolocal) %>% 
        select(ri,variavel,categoria,ano,valor,Percentual)
    })
    
    #Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(dowtab(),{
      titulo6()
      downset_Server("tabdown", dowtab(), titulo6())  
    })
    
    output$tab <- renderReactable({
      categoria <-
        db02_categoria_acumulado %>% 
        filter(ri == input$local, ano == input$anolocal) %>% 
        group_by(variavel,categoria) %>% summarize( valor = sum(valor,na.rm = TRUE),.groups = 'drop') %>% 
        mutate(Percentual = (valor / sum(valor, na.rm = TRUE)) * 100) %>%  
        arrange(desc(valor)) %>%  slice_head(n = 1)
      
      cor <-
        db04_cores_acumulado %>% 
        filter(ri == input$local, ano == input$anolocal) %>%
        group_by(variavel,categoria) %>% summarize( valor = sum(valor,na.rm = TRUE),.groups = 'drop') %>% 
        mutate(Percentual = (valor / sum(valor, na.rm = TRUE)) * 100) %>%   
        arrange(desc(valor)) %>% slice_head(n = 1)
      
      combustivel <-
        db05_combustivel_acumulado %>% 
        filter(ri == input$local, ano == input$anolocal) %>% 
        group_by(variavel,categoria) %>% summarize( valor = sum(valor,na.rm = T),.groups = 'drop') %>% 
        mutate(Percentual = (valor / sum(valor, na.rm = TRUE)) * 100) %>% 
        arrange(desc(valor)) %>%  slice_head(n = 1)
      
      especie <-
        db06_especie_veiculo_acumulado %>% 
        filter(ri == input$local, ano == input$anolocal) %>% 
        group_by(variavel,categoria) %>% summarize( valor = sum(valor,na.rm = TRUE),.groups = 'drop') %>% 
        mutate(Percentual = (valor / sum(valor, na.rm = TRUE)) * 100) %>%  
        arrange(desc(valor)) %>%  slice_head(n = 1)
      
      nacionalidade <-
        db07_nacionalidade_acumulado %>% 
        filter(ri == input$local, ano == input$anolocal) %>% 
        group_by(variavel,categoria) %>% summarize( valor = sum(valor,na.rm = TRUE),.groups = 'drop') %>% 
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
        pagination = FALSE,
        columns =  list(
          variavel = colDef(name = "Características"),
          categoria = colDef(name = "Predominância"),
          valor = colDef(name = "Quantidade", format = colFormat(separators = TRUE, locales = "pt-BR")),
          Percentual = colDef(
            name = "Percentual(%)",
            format = colFormat(
              separators = TRUE,
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

 #Categoria dos Veículos - Gráfico Barras ----   
    #Título
    titulo1 <- renderText({
      if (input$local == "Pará") {
        paste0("Principais Categorias dos Veículos Registrados - ",input$local," - ",input$anolocal)
        
      }else{
        paste0("Principais Categorias dos Veículos Registrados - Região de Integração ",input$local," - ",input$anolocal)  
      }
      
    })
    
    output$txtcat <- renderText({
      titulo1()  
    })
    
    #Download
    downcat <- reactive({
      db02_categoria_acumulado %>% filter( ri == input$local, ano == input$anolocal) %>% 
        group_by(ri,categoria) %>% 
        summarize( valor = sum(valor,na.rm = TRUE),.groups = 'drop') %>% complete() %>% 
        arrange(valor) %>% mutate(variavel = "Categoria do veículo",ano = input$anolocal) %>% 
        select(ri,variavel,categoria,ano,valor)  
    })
    
    #Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(downcat(),{
      titulo1()
      downset_Server("catdown", downcat(), titulo1())  
    })
    
    output$catbar <- renderEcharts4r({
      db02_categoria_acumulado %>% filter( ri == input$local, ano == input$anolocal) %>% 
        group_by(ri,categoria) %>% 
        summarize( valor = sum(valor,na.rm = TRUE),.groups = 'drop') %>% complete() %>% 
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
        e_grid(show = T,containLabel = T,left = "5%") %>%
        e_tooltip(trigger = "item")%>%
        e_animation(duration = 5000) %>%
        e_toolbox_feature(feature = "saveAsImage") %>%
        e_toolbox_feature(feature = "dataZoom") %>%
        e_toolbox_feature(feature = "dataView") %>%
        e_flip_coords()
    })
    #Cor dos Veículos - Gráfico de Barras----
    #Título
    titulo2 <- renderText({
      if (input$local == "Pará") {
      paste0("Principais Cores dos Veículos Registrados - ",input$local," - ",input$anolocal)
        
      }else{
      paste0("Principais Cores dos Veículos Registrados Região de Integração - ",input$local," - ",input$anolocal)  
      }
    })
    
    output$txtcor <- renderText({
      titulo2()  
    })
    
    #Download
    downcor <- reactive({
     db04_cores_acumulado %>% filter( ri == input$local, ano == input$anolocal) %>% 
        group_by(ri,categoria) %>% 
        summarize( valor = sum(valor,na.rm = TRUE),.groups = 'drop') %>% complete() %>% 
        arrange(valor) %>% mutate(variavel = "Cor do veículo",ano = input$anolocal) %>% 
        select(ri,variavel,categoria,ano,valor)  
    })
    
    #Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(downcor(),{
      titulo2()
      downset_Server("cordown", downcor(), titulo2())  
    })
    
    output$corbar <- renderEcharts4r({
     db04_cores_acumulado %>% filter( ri == input$local, ano == input$anolocal) %>% 
        group_by(ri,categoria) %>% 
        summarize( valor = sum(valor,na.rm = TRUE),.groups = 'drop') %>% complete() %>% 
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
    
    #Tipo de Combustível - Gráfico de Barras----
    #Título
    titulo3 <- renderText({
      if (input$local == "Pará") {
      paste0("Tipo de Combustível utilizado pelos Veículos Registrados - ",input$local," - ",input$anolocal)
        
      }else{
        paste0("Tipo de Combustível utilizado pelos Veículos Registrados - Região de Integração ",input$local," - ",input$anolocal)  
      }
    })

    output$txtcombu <- renderText({
      titulo3()  
    })
    
    #Download
    downcombu <- reactive({
     db05_combustivel_acumulado %>% filter( ri == input$local, ano == input$anolocal) %>% 
        group_by(ri,categoria) %>% 
        summarize( valor = sum(valor,na.rm = T),.groups = 'drop') %>% 
        arrange(valor) %>% mutate(variavel = "Tipo de combustível(s) utilizado(s)",ano = input$anolocal) %>% 
        select(ri,variavel,categoria,ano,valor)  
    })
    
    #Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(downcombu(),{
      titulo3()
      downset_Server("combudown", downcombu(), titulo3())  
    })
    
    output$combubar <- renderEcharts4r({
      db05_combustivel_acumulado %>% filter( ri == input$local, ano == input$anolocal) %>% 
        group_by(ri,categoria) %>% 
        summarize( valor = sum(valor,na.rm = TRUE),.groups = 'drop') %>% 
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
        e_grid(show = TRUE,left = "15%") %>%
        e_animation(duration = 5000) %>%
        e_toolbox_feature(feature = "saveAsImage") %>%
        e_toolbox_feature(feature = "dataZoom") %>%
        e_toolbox_feature(feature = "dataView") %>%
        e_flip_coords()
    })
    
    #Espécie do Veículo - Gráfico de Barras----
    #Título
    titulo4 <- renderText({
      if (input$local == "Pará") {
      paste0("Espécie dos Veículos Registrados - ",input$local," - ",input$anolocal)
      }else{
        paste0("Espécie dos Veículos Registrados - Região de Integração ",input$local," - ",input$anolocal)  
      }
    })
    
output$txtesp <- renderText({
      titulo4()  
    })
    
    #Download
    downesp <- reactive({
     db06_especie_veiculo_acumulado %>% filter( ri == input$local, ano == input$anolocal) %>% 
        group_by(ri,categoria) %>% 
        summarize( valor = sum(valor,na.rm = TRUE),.groups = 'drop') %>% 
        arrange(valor) %>% mutate(variavel = "Espécie de Veículo",ano = input$anolocal) %>% 
        select(ri,variavel,categoria,ano,valor)  
    })
    
    #Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(downesp(),{
      titulo4()
      downset_Server("espdown", downesp(), titulo4())  
    })

    output$espbar <- renderEcharts4r({
      db06_especie_veiculo_acumulado %>% filter( ri == input$local, ano == input$anolocal) %>% 
        group_by(ri,categoria) %>% 
        summarize( valor = sum(valor,na.rm = TRUE),.groups = 'drop') %>% 
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
      if (input$local == "Pará") {
        paste0("Nacionalidade dos Veículos Registrados - ",input$local," - ",input$anolocal)  
      }else{
        paste0("Nacionalidade dos Veículos Registrados Região de Integração - ",input$local," - ",input$anolocal)  
      }
    })
    
    output$txtnac <- renderText({
      titulo5()  
    })
    
    #Download
    downac <- reactive({
     db07_nacionalidade_acumulado %>% filter( ri == input$local, ano == input$anolocal) %>% 
        group_by(ri,categoria) %>% summarize( valor = sum(valor,na.rm = T),.groups = 'drop') %>% 
        arrange(valor) %>% mutate(ano = input$anolocal, percentual = (valor/sum(valor,na.rm = T))*100) %>% 
        select(ri,categoria,ano,valor,percentual)
    })
    #Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(downac(),{
      titulo5()
      downset_Server("nacdown", downac(), titulo5())  
    })
    output$nacpie <- renderEcharts4r({
      db07_nacionalidade_acumulado %>% filter( ri == input$local, ano == input$anolocal) %>% 
        group_by(ri,categoria) %>% summarize( valor = sum(valor,na.rm = TRUE),.groups = 'drop') %>% 
        arrange(valor) %>% mutate(percentual = (valor/sum(valor,na.rm = TRUE))*100) %>% 
        e_charts(x = categoria) %>%
        e_pie(
          serie = percentual,
          selectedMode = TRUE,
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
  })
}
# Play do Módulo
 ui = dashboardPage(
   header = dashboardHeader(),
            sidebar = dashboardSidebar(),
            body = dashboardBody(fluidPage(loc_ui("loc"))))
 
 server <- function(input, output) {
   loc_Server("loc")
 }
 
 shinyApp(ui, server)