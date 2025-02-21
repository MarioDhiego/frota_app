
#===============================================================================#
#1.1 Função de modulo de UI Donwload                                            ----


downset_ui <- function(id) {
  fluidPage(
    dropMenu(
      padding = "15px",
      placement = "right",
      actionButton(NS(id, "demo1down"),
                   tags$b("Download"),
                   icon = icon("download"),
                   style = "background-color: #286090; color: #fff;",
                   class="bg-success"
                   ),
      #tags$p("Escolha o formato de arquivo para download:"),
      tags$p("Excel, CSV ou RData"),
      tags$p("Obrigado!"),
      tags$hr(),
      downloadButton(
        NS(id, "xlsx"),
        label = "Excel",
        icon = icon("file-excel"),
        class = "btn btn-success active"
        #class = "btn btn-link"
      ),
      downloadButton(
        NS(id, "csv"),
        label = "CSV",
        icon = icon("file-csv"),
        class = "btn btn-warning active"
      ),
      downloadButton(
        NS(id, "rds"),
        label = "RData",
        icon = icon("database"),
        class = "btn btn-secondary active",
        datatoggle="popover"
      )
    )
  )
}

#Função de modulo de servidor
downset_Server <- function(id,df,nome) {
  moduleServer(id, function(input, output, session) {
    
    output$xlsx <- downloadHandler(
      filename = function() {
        paste0(nome,".xlsx")
      },
      content = function(file) {
        write.xlsx(df, file)
      }
    )
    output$csv <-   downloadHandler(
      filename = function() {
        paste0(nome,".csv")
      },
      content = function(file) {
        write.csv(df, file)
      }
    )
    output$rds <-  downloadHandler(
      filename = function() {
        paste0(nome,".rds")
      },
      content = function(file) {
        write_rds(df, file)
      }
    )
  }
)
}
    

# #Função de modulo de APP
# download_App <- function() {
#   ui <- fluidPage(download_ui("download"))
#   server <- function(input, output, session) {
#     download_Server("donwload",df)
#   }
#   shinyApp(ui, server)
# }



# #Play do Módulo
# ui <- dashboardPage(
#   header = dashboardHeader(),
#   sidebar = dashboardSidebar(),
#   body = dashboardBody(fluidPage(downset_ui("download")))
# 
# )
# server <- function(input, output) {
#   df <- reactive({
#     demo1
#   })
#   downset_Server("download",df())
# }
# 
# shinyApp(ui, server)

