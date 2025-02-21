# Funções de módulo de Infraestrutura - Municipal
# Função de UI
frotamuni_ui <- function(id) {
  fluidPage(
  ## Controle----
    panel(
      fluidRow(
      selectInput(
        inputId = NS(id, "muni"),
        label = "MUNICÍPIO",
        choices = frota2 %>%
          filter(municipio != "Pará") %>%
          pull(municipio) %>% unique(),
        width = "200px"
       )
     )
    ),
  fluidRow(
    ## Gráfico - Total da Frota de Veículos Subdivididos em Licenciados e Não Licenciados----
    box(
      title = textOutput(NS(id, "txtbar")),
      status = "primary",
      collapsible = F,
      collapsed = F,
      headerBorder = T,
      width = 12,
      fluidRow(
      column(
        2,
        selectInput(
          inputId = NS(id, "municomp1"),
          label = "Comparar Município",
          choices = NULL,
          width = "300px"
         )
      ),
      column(
        2,
        selectInput(
          inputId = NS(id, "ano"),
          label = "ANO",
          choices = sort(unique(frota2[["ano"]]), decreasing = T),
          width = "100px"
        )
      )
    ),
      column(
        12,
        withSpinner(
          echarts4rOutput(NS(id, "grafbar"), height = "326px"),
          type = 8,
          color = "#c800c8",
          size = 0.5
        )
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
      downset_ui(NS(id, "grafdow")))
            )
          )
    ),
    ## Tabela - Total da Frota de Veículos Subdivididos em Licenciados e Não Licenciados----
    box(
      title = textOutput(NS(id, "txttab")),
      status = "primary",
      collapsible = F,
      collapsed = F,
      headerBorder = T,
      width = 12,
      withSpinner(
        reactableOutput(NS(id, "tab")),
        type = 8,
        color = "#c800c8",
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
              downset_ui(NS(id, "tabdow"))
            )
          )
        )
    ),
    ## Gráfico - Total da Frota de Veículos Subdivididos em Licenciados e Não Licenciados----
    box(
      title = textOutput(NS(id, "txtline")),
      status = "primary",
      collapsible = F,
      collapsed = F,
      headerBorder = T,
      width = 12,
      selectInput(
        inputId = NS(id, "municomp2"),
        label = "Comparar Município",
        choices = NULL,
        width = "300px"
      ),
      withSpinner(
        echarts4rOutput(NS(id, "grafline")),
        type = 8,
        color = "#c800c8",
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
              downset_ui(NS(id, "linedow"))
            )
          )
        )
     )
   )
  )
}

# Função do modulo servidor
frotamuni_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ## Gráfico de barras - Total da Frota de Veículos Subdivididos em Licenciados e Não Licenciados----
    #Título
    titulo1 <- reactive({
      req(input$municomp1)
      if (input$municomp1 == "Selecione um município") {
        paste0("Total da Frota de Veículos: Licenciados e Não Licenciados, ", input$muni, " - ", input$ano)
      } else {
        paste0("Total da Frota de Veículos: Licenciados e Não Licenciados, ", input$muni, " x ", input$municomp1, " - ", input$ano)
      }
    })
    # Filtra os dados
    frotabardown <- reactive({
      req(input$municomp1)
      if (input$municomp1 == "Selecione um município") {
        a <- frota2 %>% filter(municipio == input$muni, 
                               ano == input$ano, categoria != "Frota")
      } else {
        a <- frota2 %>% filter(municipio == input$muni, 
                               ano == input$ano, categoria != "Frota")
        b <- frota2 %>% filter(municipio == input$municomp1, 
                               ano == input$ano, categoria != "Frota")
        df <- rbind(a,b)
      }
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(frotabardown(), {
      titulo1()
      downset_Server("grafdow", frotabardown(), titulo1())
    })
    # Atualização da entrada
    frota2comp1 <- reactive({
      input$muni
    })
    observeEvent(frota2comp1(), {
      x <- frota2 %>% filter(municipio != "Pará")
      x <- x %>% filter(municipio != frota2comp1())
      
      choices <- x$municipio %>% unique()
      updateSelectInput(inputId = "municomp1", choices = c("Selecione um Município", choices), session)
    })
    
    ## Título
    output$txtbar <- renderText({
      titulo1()
    })
    #Gráfico bar
    output$grafbar <- renderEcharts4r({
      req(input$municomp1)
      if (input$municomp1 == "Selecione um município") {
        a <- frota2 %>% filter(municipio == input$muni, 
                               ano == input$ano, categoria != "Frota")
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            color = "#c800c8",
            name = "Quantidade",
            legend = FALSE,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = TRUE,
            barWidth = "20%",
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 0, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = TRUE, fontSize = 11),
            name = "Tipo de Frota",
            nameLocation = "middle",
            nameTextStyle = list(fontWeight = "bold", padding = c(10, 0, 0, 0), fontSize = 14)
          ) %>%
          e_y_axis(
            name = "Quantidade",
            nameTextStyle =
              list(
                fontWeight = "bold",
                fontSize = 14
              ),
            scale = T,
            axisLabel = list(
              formatter = htmlwidgets::JS("
              function (value, index) {
              return value.toLocaleString('pt-BR', { minimumFractionDigits: 0, maximumFractionDigits: 0 });
              }
            ")
            )
          ) %>%
          e_locale("pt-Br") %>%
          e_grid(show = T, height = "70%")
      } else {
        a <- frota2 %>% filter(municipio == input$muni, 
                               ano == input$ano, categoria != "Frota")
        b <- frota2 %>% filter(municipio == input$municomp1, 
                               ano == input$ano, categoria != "Frota")
        a %>%
          e_charts(x = categoria) %>%
          e_bar(
            serie = valor,
            name = input$muni,
            legend = TRUE,
            symbol = "roundRect",
            symbolSize = 8,
            legendHoverLink = TRUE,
            barWidth = "20%",
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, categoria) %>%
          e_bar(
            serie = valor,
            name = input$municomp1,
            legend = TRUE,
            symbol = "roundRect",
            symbolSize = 8,
            legendHoverLink = TRUE,
            barWidth = "25%",
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 0, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = TRUE, fontSize = 12),
            name = "Tipo de Frota",
            nameLocation = "middle",
            nameTextStyle = list(fontWeight = "bold", padding = c(10, 0, 0, 0), fontSize = 14)
          ) %>%
          e_y_axis(
            name = "Quantidade",
            nameTextStyle =
              list(
                fontWeight = "bold",
                fontSize = 14
              ),
            scale = TRUE,
            axisLabel = list(
              formatter = htmlwidgets::JS("
              function (value, index) {
              return value.toLocaleString('pt-BR', { minimumFractionDigits: 2, maximumFractionDigits: 2 });
              }
            ")
            )
          ) %>%
          e_locale("pt-Br") %>%
          e_grid(show = TRUE, height = "70%") %>%
          e_tooltip(trigger = "item") %>%
          e_animation(duration = 5000) %>%
          e_toolbox_feature(feature = "saveAsImage") %>%
          e_toolbox_feature(feature = "dataZoom") %>%
          e_toolbox_feature(feature = "dataView") 
      }
    })
    ## Tabela - Total da Frota de Veículos Subdivididos em Licenciados e Não Licenciados----
    titulo2 <- reactive({
      paste0("Frota de Veículos no Município de ",
             input$muni, " - ", min(frota2$ano), " a ", max(frota2$ano))
    })
    # Filtra os dados
    frotatabdow <- reactive({
      x <- frota2 %>%
        filter(municipio == input$muni) %>%
        pivot_wider(names_from = categoria, values_from = valor) 
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(frotatabdow(), {
      titulo2()
      downset_Server("tabdow", frotatabdow(), titulo2())
    })
    output$txttab <- renderText({
      titulo2()
    })
    #Tabela
    output$tab <- renderReactable({
      x <- frota2 %>%
        filter(municipio == input$muni) %>%
        pivot_wider(names_from = categoria, values_from = valor) %>% 
        select(-ri,-variavel,-municipio) 
      x %>% reactable(
        height = 400,
        defaultSorted = list(ano = "desc"),
        defaultPageSize = 10,
        striped = FALSE,
        highlight = TRUE,
        bordered = TRUE,
        outlined = TRUE,
        resizable = TRUE,
        showSortable = TRUE,
        pagination = FALSE,
        columns = list(
          ano =colDef(name = "Ano"),
          Frota = colDef(name = "Total")
        ),
        defaultColDef =
          colDef(
            footerStyle = list(fontWeight = "bold"),
            format = colFormat(separators = T),
            headerStyle = list(background = "#f7f7f8"),
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
    
    ## Gráfico de linha - Total da Frota de Veículos Subdivididos em Licenciados e Não Licenciados----
    titulo3 <- reactive({
      req(input$municomp2)
      if (input$municomp2 == "Selecione um município") {
        paste0("Total da Frota de Veículos, ", input$muni, " - ", min(frota2$ano), " a ", max(frota2$ano))
      } else {
        paste0("Total da Frota de Veículos, ", input$muni, " x ", input$municomp2, " - ", min(frota2$ano), " a ", max(frota2$ano))
      }
    })
    # Atualização da entrada
    frota2comp2 <- reactive({
      input$muni
    })
    observeEvent(frota2comp2(), {
      x <- frota2 %>% filter(municipio != "Pará")
      x <- x %>% filter(municipio != frota2comp2())

      choices <- x$municipio %>% unique()
      updateSelectInput(inputId = "municomp2", choices = c("Selecione um Município", choices), session)
    })
    ## Título
    output$txtline <- renderText({
      titulo3()
    })
    # Filtra os dados
    inf1_3 <- reactive({
      req(input$municomp2)
      if (input$municomp2 == "Selecione um município") {
        a <- frota2 %>% filter(municipio == input$muni, 
                               categoria == "Frota")
      } else {
        a <- frota2 %>% filter(municipio == input$muni, 
                               categoria == "Frota")
        b <- frota2 %>% filter(municipio == input$municomp2, 
                               categoria == "Frota")}
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(inf1_3(), {
      titulo3()
      downset_Server("linedow", inf1_3(), titulo3())
    })
    #Gráfico
    output$grafline <- renderEcharts4r({
      req(input$municomp2)
      if (input$municomp2 == "Selecione um município") {
        a <- frota2 %>% filter(municipio == input$muni, 
        categoria == "Frota")
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#c800c8",
            name = "Frota",
            legend = F,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = TRUE,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 2, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = TRUE, fontSize = 11),
            name = "Ano",
            splitLine = list(show = TRUE),
            nameTextStyle = list(fontWeight = "bold", fontSize = 14)
          ) %>%
          e_y_axis(
            name = "Quantidade",
            nameTextStyle = list(fontWeight = "bold", fontSize = 14),
            scale = TRUE,
            axisLabel = list(
              formatter = htmlwidgets::JS("
              function (value, index) {
              return value.toLocaleString('pt-BR', { minimumFractionDigits: 0, maximumFractionDigits: 0 });
              }
            ")
            )
          ) %>%
          e_locale("pt-Br") %>%
          e_datazoom(toolbox = F, fillerColor = "#E5F5F9") %>%
          e_grid(show = TRUE)
      } else {
        a <- frota2 %>% filter(municipio == input$muni, categoria == "Frota")
        b <- frota2 %>% filter(municipio == input$municomp2, categoria == "Frota")
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$muni,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = T,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            serie = valor,
            name = input$municomp2,
            legend = TRUE,
            symbol = "roundRect",
            symbolSize = 10,
            legendHoverLink = TRUE,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter = e_tooltip_pointer_formatter("decimal", digits = 2, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = TRUE, fontSize = 12),
            name = "Ano",
            splitLine = list(show = TRUE),
            nameTextStyle = list(fontWeight = "bold", fontSize = 14)
          ) %>%
          e_y_axis(
            name = "Quantidade",
            nameTextStyle = list(fontWeight = "bold", fontSize = 14),
            scale = TRUE,
            axisLabel = list(
              formatter = htmlwidgets::JS("
              function (value, index) {
              return value.toLocaleString('pt-BR', { minimumFractionDigits: 0, maximumFractionDigits: 0 });
              }
            ")
            )
          ) %>%
          e_locale("pt-Br") %>%
          e_datazoom(toolbox = F, fillerColor = "#E5F5F9") %>%
          e_grid(show = TRUE) %>%
          e_tooltip(trigger = "item")%>%
          e_animation(duration = 5000) %>%
          e_toolbox_feature(feature = "saveAsImage") %>%
          e_toolbox_feature(feature = "dataZoom") %>%
          e_toolbox_feature(feature = "dataView") 
          
      }
    })

  })
}

# Play do Módulo
ui <- dashboardPage(
  header = dashboardHeader(),
  sidebar = dashboardSidebar(),
  body = dashboardBody(fluidPage(frotamuni_ui("frotamuni")))
)


server <- function(input, output) {
  frotamuni_Server("frotamuni")
}

shinyApp(ui, server)
