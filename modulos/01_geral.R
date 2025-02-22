
# Função de UI
total_ui <- function(id) {
  fluidPage(tags$head(
    tags$head(
      tags$script(
        "$(function() {
              $('[data-card-widget=\"maximize\"]').on('click', function() {
                setTimeout(function() {
                  var isMaximized = $('html').hasClass('maximized-card');
                  if (isMaximized) {
                    $('#map').css('height', '100%');
                  } else {
                    $('#map').css('height', '400px');
                  }
                }, 300);
                $('#map').trigger('resize')

              });
            });
            "
      )
    )
  ),
  div(class = "navbar_dash",
      #Infobox----
      fluidRow(
      #Total Pará acumulado
      bs4InfoBoxOutput(NS(id,"total_pa"),width = 3),
      #Total Pará do ano
      bs4InfoBoxOutput(NS(id,"total_pa_acumulado"),width = 3),
      #RI com maior quantidade
      bs4InfoBoxOutput(NS(id,"max_ri"),width = 3),
      #Tipo de Véiculo com maior Frota
      bs4InfoBoxOutput(NS(id,"max_tipo"),width = 3),
      ),
      
      #Controle----
      panel(
            fluidRow(
              column(2,
              selectInput(
                inputId = NS(id, "local"),
                label = "LOCALIDADE",
                choices = unique(frota[["ri"]]),
                width = "200px"
              )
              ),
              column(2,
              selectInput(
                inputId = NS(id, "tipo"),
                label = "VEÍCULOS TOTAIS/TIPO",
                choices =
                  list(
                    "Total de Veículos" = frota %>% filter(tipo_veiculo == "Total de Veículos") %>% 
                      pull(tipo_veiculo) %>% unique(),
                    Tipo = frota %>% filter(tipo_veiculo != "Total de Veículos") %>% pull(tipo_veiculo) %>% unique()
                  ),
                width = "200px"
              )
              ),
              selectInput(
                inputId = NS(id, "ano"),
                label = "ANO",
                choices = sort(unique(frota[["ano"]]),decreasing = TRUE),
                width = "100px"
              )
            ),
      fluidRow(
        column(6,
        #Mapa----
        box(
          title = textOutput(NS(id, "txt1")),
          # maximizable = TRUE,
          status = "primary",
          collapsed = FALSE,
          headerBorder = TRUE,
          width = 12,
          withSpinner(
            leafletOutput(NS(id,"map")),
            type = 8,
            color = "#3C8DBD",
            size = 0.8
          ),
          footer = list(tags$h6(
            tags$b("Fonte:", style = 'font-family: sans-serif;'), "RENAVAM/DTI/DETRAN-PA"
          ),
          tags$h6(tags$b("Elaboração:"), "CNP/GAETRA/DETRAN-PA"))
        )),
        column(6,
        #Tabela Total----
        box(
          title = textOutput(NS(id, "txt2")),
          status = "primary",
          collapsed = FALSE,
          headerBorder = TRUE,
          # maximizable = TRUE,
          width = 12,
          withSpinner(
            reactableOutput(NS(id,"tab"),height = "400px"),
            type = 8,
            color = "#3C8DBD",
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
          )
        ),

      )      
      ),
      fluidRow(
          #Gráfico de Barras----     
          box(title = textOutput(NS(id, "txt3")),
              status = "primary",
              collapsed = FALSE,
              headerBorder = TRUE,
              width = 12,
              selectInput(
                inputId = NS(id, "anografbar"),
                label = "ANO",
                choices = sort(unique(frota[["ano"]]),decreasing = TRUE),
                width = "100px"
              ),
              withSpinner(
                echarts4rOutput(NS(id, "grafbar"), height = "600px"),
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
                  downset_ui(NS(id, "grafbardown"))
                )
              )
            )
           ),
        #Gráfico de linha, comparação----
        box(
          title = textOutput(NS(id, "txt4")),
          status = "primary",
          collapsed = FALSE,
          headerBorder = TRUE,
          width = 12,
          fluidRow(
          column(3,
                 #Selecionar pará ou Municipio
                 selectInput(
                   inputId = NS(id, "localc1"),
                   label = "LOCALIDADE 1",
                   choices = unique(frota[["local"]]),
                   width = "200px"
                 )),
          column(3,
                 #Selecionar apenas município
                 selectInput(
                   inputId = NS(id, "localc2"),
                   label = "LOCALIDADE 2",
                   choices = NULL,
                   width = "250px"
                 ))
            
          ),
          column(12,
                 withSpinner(
                   echarts4rOutput(NS(id,"graf"),height = "500px"),
                   type = 8,
                   color = "#3C8DBD",
                   size = 0.8
                 )),
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
                  downset_ui(NS(id, "grafdown"))
                )
              )
            )
        ),
        #Gráfico de TreeMap----     
        box(title = textOutput(NS(id, "txt5")),
            status = "primary",
            collapsed = FALSE,
            headerBorder = TRUE,
            width = 12,
            selectInput(
              inputId = NS(id, "anografmap"),
              label = "ANO",
              choices = sort(unique(frota[["ano"]]),decreasing = TRUE),
              width = "100px"
            ),
            withSpinner(
              echarts4rOutput(NS(id, "graftreemap"), height = "600px"),
              type = 8,
              color = "blue",
              size = 0.5
            ),
            footer = 
              list(
                div(
                  style = "display: flex; justify-content: space-between;",
                  div(
                    tags$h6(tags$b("Fonte:", style = 'font-family: sans-serif;'), "RENAVAM/DTI/DETRAN-PA"),
                    tags$h6(tags$b("Elaboração:"), "CNP/GAETRA/EDetran-PA")
                  ),
                  div(
                    style = "display: flex; justify-content: center; align-items: center;",
                    downset_ui(NS(id, "graftreedown"))
                  )
                )
              )
          )
      )
  )
)
}

# Função do modulo servidor
total_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    #Caixas de Valor----
    #Total Pará acumulado
    output$total_pa <- renderInfoBox({
      valor <- frota %>% filter(local == "Pará",
                                variavel == "Total Acumulado de veículo Licenciados",
                                ano == input$ano) %>% select(valor)
      bs4InfoBox(
        title = tags$strong("PARÁ"),
        value = tags$h2(tags$strong(
          prettyNum(
            valor$valor,
            big.mark = ".",
            decimal.mark = ",",
            scientific = FALSE
          )
        )),
        subtitle = paste0("Total de Veículos Registrados - ", input$ano),
        color = "primary",
        fill = TRUE,
        gradient = TRUE,
        iconElevation = 2,
        icon = icon("car")
      )
    })
    #Total Pará no ano
    output$total_pa_acumulado <- renderInfoBox({
      valor <- frota %>% filter(local == "Pará",
                                variavel == "Total de veículo Licenciados",
                                ano == input$ano) %>% select(valor)
      bs4InfoBox(
        title = tags$strong("PARÁ"),
        value = tags$h2(tags$strong(
          prettyNum(
            valor$valor,
            big.mark = ".",
            decimal.mark = ",",
            scientific = FALSE
          )
        )),
        subtitle = paste0("Total de Veículos Registrados no ano - ", input$ano),
        color = "danger",
        fill = TRUE,
        gradient = TRUE,
        iconElevation = 2,
        icon = icon("car")
      )
    })
    
    #Maximo RI
    output$max_ri <- renderInfoBox({
      valor <- frota %>%
        filter(variavel == "Total de veículo Licenciados",
               ri != "Pará",
               ano == input$ano) %>%
        group_by(ri) %>%
        summarise(valor = sum(valor, na.rm = TRUE))
      valor <- valor %>% filter(valor == max(valor))
      
      bs4InfoBox(
        title = tags$strong(paste0("REGIÃO DE INTEGRAÇÃO ", valor$ri )),
        value = tags$h2(tags$strong(prettyNum(valor$valor, big.mark = ".", decimal.mark = ",", scientific = FALSE))),
        subtitle = paste0("Maior Nº de Veículos Registrados - ", input$ano),
        fill = TRUE,
        gradient = TRUE,
        iconElevation = 2,
        color = "success",
        icon = icon("globe")
      )
    })
    
    #Maximo Tipo
    output$max_tipo <- renderInfoBox({
      valor <- frota %>% filter(ri == "Pará",variavel == "Tipo de Veículo", ano == input$ano)
      valor <- valor %>% filter(valor == max(valor))
      bs4InfoBox(
        title = tags$strong(paste0("TIPO ", valor$tipo_veiculo)),
        value = tags$h2(tags$strong(prettyNum(valor$valor, big.mark = ".", decimal.mark = ",", scientific = FALSE))),
        subtitle = paste0("Tipo de Veículo maior Quantidadde - ", input$ano),
        fill = TRUE,
        gradient = TRUE, 
        iconElevation = 2,
        color = "purple",
        icon = icon("chart-bar")
      )
    })
    
    #Mapa----
    #Título do Mapa
    titulo1 <- reactive({
      if (input$tipo == "Total de Veículos") {
      if (input$local == "Pará") {
        paste0(
          "Distribuição do Total de Veículos Registrados - Pará - ",
          input$ano
        )
      }else{
        paste0(
          "Distribuição do Total Veículos Registrados - Região de Integração ",
          input$local,
          " - ",
          input$ano)
      }
      }else{
        if (input$local == "Pará") {
          paste0(
            "Distribuição de ",input$tipo," - Pará - ",
            input$ano
          )
        }else{
          paste0(
            "Distribuição de ",input$tipo, "- Região de Integração ",
            input$local,
            " - ",
            input$ano)
        }  
      }
    })
    output$txt1 <- renderText({
      titulo1()  
    })
    
    #Mapa
    output$map <- renderLeaflet({
      #Tratamento da informação
      ##Filtrando informação
      if (input$tipo == "Total de Veículos") {
        if (input$local == "Pará") {
          df <- frota %>%
            filter(ri != "Pará", ano == input$ano, variavel == "Total Acumulado de veículo Licenciados", tipo_veiculo == input$tipo) %>%
            select(ri, tipo_veiculo, local, ano, valor)
          x <- cbind(geopa, df)
        } else {
          df <- frota %>%
            filter(ri != "Pará", ano == input$ano, variavel == "Total Acumulado de veículo Licenciados", tipo_veiculo == input$tipo) %>%
            select(ri, tipo_veiculo, local, ano, valor)
          x <- cbind(geopa, df) %>%
            filter(ri == input$local)
        }
      } else {
        if (input$local == "Pará") {
          df <- frota %>%
            filter(ri != "Pará", ano == input$ano, variavel == "Tipo de Veículo", tipo_veiculo == input$tipo) %>%
            select(ri, tipo_veiculo, local, ano, valor)
          x <- cbind(geopa, df)
        } else {
          df <- frota %>%
            filter(ri != "Pará", ano == input$ano, variavel == "Tipo de Veículo", tipo_veiculo == input$tipo) %>%
            select(ri, tipo_veiculo, local, ano, valor)
          x <- cbind(geopa, df) %>%
            filter(ri == input$local)
        }
      }
      
      z <- x$valor[x$valor > 0]
      bk <- unique(getJenksBreaks(z, 6, subset = NULL))
      x$valor[x$valor == 0] <- NA
      bins <- c(bk)
      if (length(bins) < 2 || length(bins) < 5) {
        bins <- 5
      }
      
      pal <-
        colorBin(
          c("#B6EDF0", "#74B4E8", "#1F83E0", "#1D44B8", "#090991"),
          domain = x$valor,
          bins = bins
        )
      conteudo <-
        sprintf(
          "<strong>%s</strong><br/> <b>%s:</b> %s",
          x$name_muni,
          input$tipo,
          ifelse(
            is.na(x$valor),
            "Não disponível",
            format(
              x$valor,
              big.mark = ".",
              decimal.mark = ",",
              digits = 4
            )
          )
        ) %>% lapply(htmltools::HTML)
      #Mapas com leafleft
      leaflet(
        x, options = leafletOptions(minZoom = 0, maxZoom = 15),width = "100%",height = "") %>%
        
        addTiles() %>%
        addProviderTiles(providers$Esri.NatGeoWorldMap)%>%
        #addProviderTiles(providers$Esri.WorldStreetMap)%>%
        addPolygons(
          weight = 2,
          opacity = 1,
          color = "black",
          fillOpacity = 1,
          fillColor = ~ pal(valor),
          dashArray = 1,
          smoothFactor = 1.5,
          highlightOptions =
            highlightOptions(
              weight = 3,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.5,
              bringToFront = TRUE
            ),
          label = conteudo,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"
          )
        ) %>% addLegend(
          pal = pal,
          values = ~ valor,
          opacity = 0.7,
          title = "Frota",
          position = "bottomright",
          na.label = "Não disponível",
          labFormat = labelFormat_decimal(
            big.mark = ".",
            decimal.mark = ",",
            digits = 2
          )
        )
    })
    
    #Tabela_total----
    #Título Tabela
    titulo2 <- reactive({
      if (input$tipo == "Total de Veículos") {
        if (input$local == "Pará") {
          paste0(
            "Distribuição dos Veículos Registrados Total - Pará - ",
            input$ano
          )
        }else{
          paste0(
            "Distribuição dos Veículos Registrados Total - Região de Integração ",
            input$local,
            " - ",
            input$ano)
        }
      }else{
        if (input$local == "Pará") {
          paste0(
            "Distribuição de ",input$tipo," por Município - ",
            input$ano
          )
        }else{
          paste0(
            "Distribuição de ",input$tipo, "- por Município Região de Integração ",
            input$local,
            " - ",
            input$ano)
        }  
      }
    })
    output$txt2 <- renderText({
      titulo2()  
    })
    
    #Download
    downtab <- reactive({
      if (input$local == "Pará") {
        df <-
          frota %>%
          filter(ri !="Pará",variavel == "Total Acumulado de veículo Licenciados", ano == input$ano) %>%
          rename(regiao_integracao = ri)
      }else{
        df <-
          frota %>%
          filter(ri != "Pará",variavel == "Total Acumulado de veículo Licenciados", ano == input$ano)
        df <-
          df %>% filter(ri == input$local) %>%
          rename(regiao_integracao = ri)
      }  
    })
    
    #Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(downtab(),{
      titulo2()
      downset_Server("tabdown", downtab(), titulo2())  
    })
    
    #Tabela
    output$tab <- renderReactable({
      if (input$tipo == "Total de Veículos") {
        if (input$local == "Pará") {
          df <- frota %>%
            filter(ri != "Pará", ano == input$ano, variavel == "Total Acumulado de veículo Licenciados", tipo_veiculo == input$tipo) %>%
            select(local, valor) %>%
            mutate(Percentual = (valor / sum(valor,na.rm = TRUE)) * 100)
        } else {
          df <- frota %>%
            filter(ri != "Pará", ano == input$ano, variavel == "Total Acumulado de veículo Licenciados", tipo_veiculo == input$tipo) %>%
            filter(ri == input$local) %>% 
            select(local, valor) %>%
            mutate(Percentual = (valor / sum(valor,na.rm = TRUE)) * 100)
        }
      } else {
        if (input$local == "Pará") {
          df <- frota %>%
            filter(ri != "Pará", ano == input$ano, variavel == "Tipo de Veículo", tipo_veiculo == input$tipo) %>%
            select(local, valor) %>% 
            mutate(Percentual = (valor / sum(valor,na.rm = TRUE)) * 100)
        } else {
          df <- frota %>%
            filter(ri != "Pará", ano == input$ano, variavel == "Tipo de Veículo", tipo_veiculo == input$tipo) %>%
            filter(ri == input$local) %>% 
            select(local, valor) %>%
            mutate(Percentual = (valor / sum(valor,na.rm = TRUE)) * 100)
        }
      }
      
      df %>% reactable(
        defaultPageSize = 10,
        striped = FALSE,
        highlight = TRUE,
        bordered = TRUE,
        outlined = TRUE,
        resizable = TRUE,
        showSortable = TRUE,
        pagination = F,
        defaultSorted = list(valor = "desc"),
        columns =  list(
          ri = colDef(name = "Reião de Integraçao"),
          local = colDef(name = "Municípios"),
          valor = colDef( 
            name = "Quantidade",
            format = colFormat(separators = T, locales = "pt-BR")
          ),
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
    
    #Gráfico de Barras Total Tipo ----
    #Título Gráfico de Barras
    titulo3 <- reactive({
        paste0(
          "Classificação por Tipo de Veículo - ",
          input$anografbar
        )
    })
    output$txt3 <- renderText({
      titulo3()  
    })

    #Download
    downgrafbar <- reactive({
      frota %>%
        filter(local == "Pará",variavel == "Tipo de Veículo",ano == input$anografbar) %>%
        arrange(valor)
    })
    
    #Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(downgrafbar(),{
      titulo3()
      downset_Server("grafbardown", downgrafbar(), titulo3())  
    })

    output$grafbar <- renderEcharts4r({
      a <- frota %>%
        filter(local == "Pará",variavel == "Tipo de Veículo",ano == input$anografbar) %>%
        arrange(valor)
      a %>%
        e_charts(tipo_veiculo) %>%
        e_bar(
          serie = valor,
          color = "blue",
          name = "Quantidade",
          legend = FALSE,
          symbol = "roundRect",
          symbolSize = 6,
          legendHoverLink = TRUE,
          itemStyle = list(barBorderRadius = 0)
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
          scale = T,
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
        e_grid(
          show = TRUE,
          width = "80%",
          height = "80%",
          left = "15%"
        ) %>%
        e_tooltip(trigger = "item")%>%
        e_animation(duration = 5000) %>%
        e_toolbox_feature(feature = "saveAsImage") %>%
        e_toolbox_feature(feature = "dataZoom") %>%
        e_toolbox_feature(feature = "dataView") %>%
        e_flip_coords()
    })


    #Gráfico linha----
    # Atualização de entrada
    localcomp <- reactive({
      input$localc1
    })
    # Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(localcomp(), {
      df <- frota %>% filter(local != "Pará")
      df <- df %>% filter(local != localcomp())
      
      choices <- df$local %>% unique()
      updateSelectInput(
        inputId = "localc2",
        choices = c("Selecione um Município", choices),
        session
      )
    })
    
    #Título Gráfico
    titulo4 <- reactive({
      req(input$localc2)
      if (input$localc2 == "Selecione um município") {
        paste0("Evolução do Número de Veículos Licenciados, ", input$localc1, " - "," (", min(frota$ano), " - ", max(frota$ano),")")
      } else {
        paste0("Evolução do Número de Veículos Licenciados, ", input$localc1, " vs ", input$localc2, " - "," (", min(frota$ano), " - ", max(frota$ano),")")
      }
    })
    
    output$txt4 <- renderText({
      titulo4()  
    })
    
    #Download
    downgraf <- reactive({
      req(input$localc2)
      if (input$localc2 == "Selecione um município") {
        a <- frota %>% 
          filter(local == input$localc1, variavel == "Total Acumulado de veículo Licenciados") %>% 
          rename(regiao_integracao = ri)
      }
      else {
        a <- frota %>% filter(local == input$localc1,variavel == "Total Acumulado de veículo Licenciados")
        b <- frota %>% filter(local == input$localc2,variavel == "Total Acumulado de veículo Licenciados")
        df <- rbind(a,b)
        df <- df %>% rename(regiao_integracao = ri)
      }
      
    })
    
    #Monitora a base filtrada, defini o texto a ser baixado
    observeEvent(downgraf(),{
      titulo4()
      downset_Server("grafdown", downgraf(), titulo4())  
    })
    
    
    
    #Gráfico
    output$graf <- renderEcharts4r({
      req(input$localc2)
      if (input$localc2 == "Selecione um município") {
        a <- frota %>% 
          filter(local == input$localc1, variavel == "Total Acumulado de veículo Licenciados")
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            color = "#dd3d28",
            name = "Quantidade",
            legend = FALSE,
            symbol = "roundRect",
            symbolSize = 8,
            legendHoverLink = TRUE,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_area(x = ano) %>%
          e_tooltip(
            trigger = "axis",
            formatter =
              e_tooltip_pointer_formatter("decimal", digits = 0, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = T, fontSize = 11),
            name = "Ano",
            splitLine = list(show = T),
            nameTextStyle = list(fontWeight = "bold", fontSize = 14)
          ) %>%
          e_y_axis(
            name = "Quantidade",
            nameTextStyle = list(fontWeight = "bold", fontSize = 14),
            scale = TRUE,
            axisLabel = list(
              formatter = htmlwidgets::JS(
                "
              function (value, index) {
              return value.toLocaleString('pt-BR',
              { minimumFractionDigits: 0, maximumFractionDigits: 0 });
              }
            "
              )
            )
          ) %>%
          e_locale("pt-Br") %>%
          e_datazoom(type = "slider",toolbox = FALSE, fillerColor = "#E5F5F9") %>%
          e_grid(show = TRUE)
      } else {
        frota$valor <- ifelse(is.na(frota$valor), 0, frota$valor)
        a <- frota %>% filter(local == input$localc1,variavel == "Total Acumulado de veículo Licenciados")
        b <- frota %>% filter(local == input$localc2,variavel == "Total Acumulado de veículo Licenciados")
        a %>%
          e_charts(x = ano) %>%
          e_line(
            serie = valor,
            name = input$localc1,
            legend = T,
            symbol = "roundRect",
            symbolSize = 8,
            legendHoverLink = TRUE,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_data(b, ano) %>%
          e_line(
            y_index = 1, 
            serie = valor,
            name = input$localc2,
            legend = T,
            symbol = "roundRect",
            symbolSize = 6,
            legendHoverLink = TRUE,
            itemStyle = list(barBorderRadius = 5)
          ) %>%
          e_tooltip(
            trigger = "axis",
            formatter =
              e_tooltip_pointer_formatter("decimal", digits = 0, locale = "pt-Br"),
            axisPointer = list(type = "shadow")
          ) %>%
          e_x_axis(
            axisLabel = list(show = TRUE, fontSize = 12),
            name = "Ano",
            splitLine = list(show = TRUE),
            nameTextStyle = list(
              fontWeight = "bold",
              fontSize = 14,
              padding = c(0, 0, 0, 20)
            )
          ) %>%
          e_y_axis(
            name = "Quantidade",
            nameTextStyle = list(fontWeight = "bold", fontSize = 14),
            scale = TRUE,
            axisLabel = list(
              formatter = htmlwidgets::JS(
                "
              function (value, index) {
              return value.toLocaleString('pt-BR',
              { minimumFractionDigits: 0, maximumFractionDigits: 0 });
              }
            "
              )
            )
          ) %>%
          e_y_axis(
            index = 1,
            name = "Quantidade",
            nameTextStyle = list(fontWeight = "bold", fontSize = 14),
            scale = TRUE,
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
          #e_datazoom(toolbox = FALSE, fillerColor = "#E5F5F9") %>%
          e_grid(show = TRUE) %>%
          e_tooltip(trigger = "item")%>%
          e_animation(duration = 5000) %>%
          e_toolbox_feature(feature = "saveAsImage") %>%
          e_toolbox_feature(feature = "dataZoom") %>%
          e_toolbox_feature(feature = "dataView") 
      }
    })
    #Gráfico de Árvore----
    #Título Gráfico
    titulo5 <- reactive({
    paste0("Treemap: Veículos Registrados em Regiões de Integração e Municípios - ",input$anografmap)  
    })
    
    output$txt5 <- renderText({
      titulo5()  
    })
        output$graftreemap <- renderEcharts4r({
          mapa <- frota %>%
            filter(variavel == "Total Acumulado de veículo Licenciados",
                   ri != "Pará",
                   ano == "2023") %>%
            group_by(ri) %>%
            summarise(valor = sum(valor, na.rm = TRUE))
          mapa <- mapa %>% select(ri,valor) %>% rename(name = ri, value = valor)
          
          frotamap <- frota %>% filter(ano == input$anografmap)
          
          # Defina os nomes dos grupos
          grupos <- c("ARAGUAIA", 
                      "BAIXO AMAZONAS", 
                      "Carajás", 
                      "Guajará", 
                      "Guamá", 
                      "Lago_de_Tucuruí", 
                      "Marajó", 
                      "Rio Caeté", 
                      "Rio Capim", 
                      "Tapajós", 
                      "Tocantins", 
                      "Xingu")
          
          # Crie uma função para filtrar os dados
          filtrar_grupo <- function(grupo) {
            frotamap %>%  
              filter(ri != "Pará", variavel == "Total Acumulado de veículo Licenciados", ri == grupo) %>%
              select(local, valor) %>% rename(name = local, value = valor)
          }
          
          # Use map para aplicar a função a cada grupo
          lista_df <- map(grupos, filtrar_grupo)
          
          # Agora, lista_df é uma lista de dataframes, cada um correspondendo a um grupo
          names(lista_df) <- grupos  # nomeie os elementos da lista com os nomes dos grupos
          
          # Adicione a lista de dataframes ao dataframe 'mapa'
          mapa <- mapa %>% mutate(list = lista_df)
          mapa <- mapa %>% rename( children = list)
          
          #TreeMap
          mapa %>% 
            e_charts() %>%
            e_animation(duration = 5000) %>%
            e_toolbox_feature(feature = "saveAsImage") %>%
            e_toolbox_feature(feature = "dataZoom") %>%
            e_toolbox_feature(feature = "dataView") %>%
            e_treemap(serie = "teste",
                      label = list(show= TRUE, formatter = "{b} \n {c}"),
             leafDepth = 1,
             itemStyle = 
               list(borderWidth = 1,
                    borderColorSaturation = 0.2)
            ) %>% 
            e_legend(selectedMode = "single") %>%
            e_title("Frota Por Região de Integração") %>%
            e_tooltip(
              formatter = htmlwidgets::JS("
              function(params) {
              return params.name + ': ' + params.value.toLocaleString();}")
            )
          
        })
    
  })
}
# Play do Módulo
ui = dashboardPage(
  header = dashboardHeader(),
           sidebar = dashboardSidebar(),
           body = dashboardBody(fluidPage(total_ui("total"))))

server <- function(input, output) {
  total_Server("total")
}

shinyApp(ui, server)