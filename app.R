#===============================================================================#
#                                                                               #
#              DETRAN-PA - Departamento de Trânsito do Estado do Pará           #
#                                                                               #
#===============================================================================#
#==== DESCRIÇÃO:    Dashboard de Frota Veicular - Pará
#==== ESCRITO POR:  Mário Diego Valente
#====               Tito Félix de Oliveira
#====               Kleber Bezerra Salim
#==== SITE:         https://www.detran.pa.gov.br/
#==== LICENÇA:      MIT
#==== PROJETO:      https://github.com/MarioDhiego/
#===============================================================================#



# Interface do usuário---------------------------------------
#Carregando----
source('global.R')
shinyApp(
  ui = dashboardPage(
    header = dashboardHeader(
      
      #tags$li(class = "dropdown",
      #        a(href = "https://www.facebook.com/detranPARA",
      #          class = "fa fa-facebook",
      #          target = "_blank"
      #        )),
      #tags$li(class = "dropdown",
      #        a(href = "https://www.instagram.com/detranpa_",
      #          class = "fa fa-instagram",
      #          target = "_blank"
      #        )
      #),
      #tags$li(class = "dropdown",
      #        a(href = "https://twitter.com/DETRAN_PA",
      #          class = "fa fa-twitter",
      #          target = "_blank"
      #        )),
      #tags$li(
      #  class="dropdown",
      #  tags$a(href="https://github.com/MarioDhiego",
      #         icon("github"), "Suporte", target = "_blank")),
      
      title = dashboardBrand(
        title = tags$strong("Detran - PA"),
        href = "https://www.detran.pa.gov.br/index_.php",
        image = "detran1.jpeg",
        #width = 230, 
        #heigth = 100,
        color ="gray",
      )
    ), 
# dbHeader,
skin = "black",
scrollToTop = TRUE,
fullscreen = TRUE,
help = TRUE,
options = list(sidebarExpandOnHover = TRUE),
sidebar = dashboardSidebar(
  disable = FALSE,
  width = NULL,
  skin = "dark",
  status = "primary",
  collapsed = FALSE,
  minified = TRUE,
  sidebarMenu(
    id = "tabs",
    menuItem("ANUÁRIO", tabName="anuario", icon=icon("address-card"),
             menuSubItem("Sobre Anuário", tabName="sobre1", icon=icon("book")),
             menuSubItem("Vídeo Institucional", tabName="video1", icon=icon("video"))),
    menuItem("GLOSSÁRIO", tabName="conceitos", icon=icon("book")),
    menuItem("Visão Geral", tabName = "visao_geral",icon = icon("display")),
    menuItem("Perfil dos Veículos", tabName = "carac_veiculo",icon = icon("magnifying-glass"),
             menuSubItem("Pará e R.I's",tabName = "carac_loc",icon = icon("globe")),
             menuSubItem("Municípios",tabName = "carac_muni",icon = icon("location-dot")),
             menuSubItem("Por Tipo de Veículo",tabName = "carac_tipo",icon = icon("car"))),
    menuItem("Frota",tabName = "frota",icon = icon("car"),
             menuSubItem("Pará",tabName = "pa"),
             menuSubItem("Municípios",tabName = "muni")),
    menuItem("Sobre", tabName = "sobre",icon = icon("book"))
    )      
  ),
body = dashboardBody(
  fluidPage(
    tags$head(
      tags$link(rel = "shortcut icon", href = "icons8-favicon-94.png", type = "image/x-icon"), 
      tags$link(
        rel = "stylesheet",
        href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css"
        )
      )
    ),
  tabItems(
    tabItem(tabName="sobre1",
            tabBox(id="t1", width=12,
                   tabPanel("ANUÁRIO WEB",
                            icon = icon("address-card"),
                            fluidRow(
                              column(
                                width = 8,
                                position = "left",
                                tags$img(
                                  id = "foto1",
                                  src = "frota.jpg",
                                  controls = "controls",
                                  width = 750,
                                  height = 500
                                ),
                                tags$br(),
                                tags$a("Photo by Asdecom"),
                                align = "left"
                              ),
                              column(
                                width = 4,
                                tags$br(),
                                tags$p(
                                  style = "text-align:justify;font-si20pt",
                                  strong(
                                    "A Assembléia Geral da Organização das Nações Unidas(ONU), por meio da Resolução A/RES/74/299, de 31 de agosto de 2020, lançou a Década de Ação pela Segurança Viária 2021/2030, com objetivo de reduzir em pelo menos 50/% de vítimas fatais e feridos graves no trânsito, bem como solicita aos Estados-Membros que continuem adotando medidas até 2030 para alcançar as metas dos Objetivos de Desenvolvimento Sustentável relacionados a Segurança Viária, em especial a meta 3.6."
                                  )
                                ),
                                tags$br(),
                                tags$p(
                                  style = "text-align: justify;font-si20pt",
                                  strong(
                                    "O DETRAN-PA, com sua missão fundamentada nos princípios da Política Nacional de Trânsito(Resolução do CONTRAN Nº514, de 18 de dezembro de 2014), apresenta, neste Painél, o Processo de Integração de Múltiplas Bases de Dados (Pareamento) sobre vítimas fatais por acidentes de trânsito."
                                  )
                                ),
                                tags$br(),
                                tags$p(
                                  style = "text-align: justify;font-si20pt",
                                  strong(
                                    "O Processo de Integração de Dados foi Realizado junto aos 144 municípios que compõem o Estado do Pará. Como Resultados foi criado uma base de dados de frota registrada."
                                  )
                                )
                              )
                            )), 
  tabPanel("LINHA DE BASE", icon = icon("hospital"),
           fluidRow(
             column(
               width = 4,
               position = "center",
               solidHeader = TRUE,
               tags$br(),
               tags$p(
                 style = "text-align:justify;font-si20pt",
                 strong(
                   "A Definição de Mortes e VÍtimas Graves foi Estabelecida com Base no Padrão da Organização Mundial de São de (WHO, 2022)."
                 )
               ),
               tags$br(),
               tags$p(
                 style = "text-align:justify;font-si20pt",
                 strong(
                   "1) VÍtima Fatal: uma pessoa morta imediatamente ou até 30 dias, como resultado do acidente de trânsito."
                 )
               ),
               tags$br(),
               tags$p(
                 style = "text-align:justify;font-si20pt",
                 strong(
                   "2) VÍtima Grave: a pessoa hospitalizada por pelo menos 24 horas devido a ferimentos sofridos em acidentes de trânsito."
                 )
               )
             )
           )), 
  
  tabPanel("ALINHAMENTO TÉCNICO",
           icon = icon("layer-group"),
           fluidRow(
             column(
               width = 4,
               position = "center",
               solidHeader = TRUE,
               tags$br(),
               tags$p(
                 style = "text-align: justify;font-si20pt",
                 strong(
                   "A Lei nº 13.614/2018 criou Plano Nacional de Redução de Mortes e Lesões no Trânsito(PNATRANS), acrescentando o artigo 326-A ao Código de Trânsito Brasileiro (CTB), e propôs um novo desafio para a gestão de trânsito no Brasil e para os orgãos integrados do Sistema Nacional de Trânsito(SNT)."
                 )
               ),
               tags$br(),
               tags$a("Pnatrans",
                      href = "https://www.gov.br/transportes/pt-br/assuntos/transito/arquivos-senatran/anexo_i_pnatrans_2.pdf"),
               tags$br(),
               tags$p(
                 style = "text-align: justify;font-si20pt",
                 strong(
                   "Em 2020, foi realizado um diagnóstico, envolvendo todas as Unidades Federativas com o objetivo de estabelecer uma metodologia padronizada de coleta e tratamento dos dados sobre acidentes de trânsito e uma gestão baseada em análises e melhoria do sistema de segurança viária para todo Brasil. A fim de definir dados mínimos sobre acidentes de forma padronizada, alinhar as Unidades Federativas sobre o processo e sobre a implantação do modelo e, por fim, garantir análises e atuação sobre as causas críticas com intuito de minimizar os acidentes de trânsito no país."
                 )
               ),
               tags$br(),
               tags$p(
                 style = "text-align: justify;font-si20pt",
                 strong(
                   "Nesse Contexto, o Anuário Estatístico de Acidentes de Trânsito, será construído em Alinhamento ao Manual de Gestão do RENAEST (Resolução do CONTRAN Nº808/2020), utilizando metodologias factíveis com estatísticas de trânsito padronizada para todo o Território Nacional, e aos Objetivos de Desenvolvimento Sustentáveis (Resolução da ONU Nº70/2015)."
                 )
               )
             )
           )),
  tabPanel("SOFTWARE'S", icon=icon("computer"),
           fluidRow(
             column(width=4,
                    position="center",solidHeader = TRUE,
                    tags$br(),
                    tags$p(style="text-align: justify;font-si20pt",
                           strong("Para Criação do Anuário em Formato Web com Dasboard Interativos, foi Desenvolvido um script em Linguagem de Programação R-PROJECT Versão 4.2.2, no formato de Projeto de Software Livre de Código Aberto (open source), ou seja, pode ser utilizado sem custos de licença (R DEVELOPMENT CORE TEAM, 2022)")),
                    tags$br(),
                    tags$img(
                      id = "foto2",
                      src = "R.jpg",
                      controls = "controls",
                      width = 200,height = 200),
                    tags$br(),
                    tags$a("Software R",
                           href = "https://cran.r-project.org/bin/windows/base/R-4.3.2-win.exe"),
                    tags$br(),
             ),
             column(width=4,
                    position="center",solidHeader = TRUE,
                    tags$br(),
                    tags$p(style="text-align: justify;font-si20pt",
                           strong("Foi utilizado um Ambiente de Desenvolvmento Integrado (IDE) Chamado Rstudio Versão 1.4.1.7, utilizando um Processo de Extração-Transformação-Carga(ETL) com uso de Várias bibliotecas (library), para o Ambiente Windows")),
                    tags$br(),
                    tags$img(
                      id="foto3",
                      src="RStudio.png",
                      controls="controls",
                      width=200,height=200),
                    tags$br(),
                    tags$a("RStudio",
                           href = "https://download1.rstudio.org/electron/windows/RStudio-2023.09.1-494.exe"),
                    tags$br(),
             )
           )
  ),
  tabPanel("MATERIAL E MÉTODOS", 
           icon=icon("book"),
           fluidRow(
             column(width = 4, 
                    position = "center",
                    tags$br(),
                    tags$br("Metodologia"),
                    tags$br(),
                    tags$p(
                      style = "text-align:justify;font-si20pt",
                      strong(
                        "A Metodologia Adotada para o Planejamento e execução do Projeto foi apoiada na Estratégia de Proatividade e Parceria Desenvolvida pela GRSP (CARDITA e DI PIETRO, 2010)."
                      )
                    ),
                    tags$br(),
                    tags$p(
                      style = "text-align:justify;font-si20pt",
                      strong(
                        "A Estratégia de Proatividade e Parceria (EPP) consiste em um Modelo Desenvolvido para Tratar das questões de Segurança no Trânsito."
                      )
                    ),
                    tags$br(),
                    tags$p(style = "text-align:justify;font-si20pt",
                           strong(" As Etapas a Serem Desenvolvidas Durante Aplicação do Projeto são:")),
                    tags$br(),
                    tags$p(style = "text-align:justify;font-si20pt",
                           strong("1) Articulação Intersetorial e Formação")),
                    tags$p(style = "text-align: justify;font-si20pt",
                           strong("2) Qualificação, Integração e Análise de Dados")),
                    tags$p(style = "text-align: justify;font-si20pt",
                           strong("3) Ações Integradas de Segurança no Trânsito")),
                    tags$p(style = "text-align: justify;font-si20pt",
                           strong("4) Monitoramento, Avaliação de Desenpenhp e Reconhecimento")),
                    tags$p(style = "text-align: justify;font-si20pt",
                           strong("5) Revisão Geral Anual")),
                    tags$p(style = "text-align: justify;font-si20pt",
                           strong("6) Renovação e Expansão"))
             ),
             tags$br(),
             column(
               width = 4,
               position = "center",
               tags$br("Pareamento"),
               tags$br(),
               tags$p(
                 style = "text-align:justify;font-si20pt",
                 strong(
                   "Para o Relacionamento das Múltiplas Bases de Dados(pareamento), utilizou-se o Método Probabilístico de Relacionamento de Registro desenvolvido por Fellegi e Sunter (1969)."
                 )
               ),
               tags$br(),
               tags$p(
                 style = "text-align:justify;font-si20pt",
                 strong(
                   "A principal dificuldade do pareamento é a não existência de um identificador único que permita vincular um Boletim de Ocorrência à uma Autorização de Internação Hospitalar ou Declaração de Òbito."
                 )
               ),
               tags$br(),
               tags$p(
                 style = "text-align:justify;font-si20pt",
                 strong(
                   "O Processo de Padronização de Variáveis, utilizando o Método Probabilístico, foi realizado para homogeneizar as variáveis das diferentes bases de dados, visando minimizar erros no processo de pareamento, com a alocação de registros da mesma vítima num bloco lógico para evitar: erros fonéticos, perda de informação, etc."
                 )
               )
             )
           )
  ),
  tabPanel(
    "CRÉDITOS",
    icon = icon("phone"),
    fluidRow(
      column(
        width = 4,
        position = "center",
        solidHeader = TRUE,
        tags$br(),
        tags$p(
          style = "text-align: justify;font-si20pt",
          strong("DEPARTAMENTO DE TRÂNSITO DO ESTADO DO PARÁ - DETRAN/PA")
        ),
        tags$p(style = "text-align: justify;font-si20pt",
               strong("RENATA MIRELA COELHO")),
        tags$p(style = "text-align: justify;font-si20pt",
               strong("AVENIDA: AUGUSTO MONTENEGRO KM 03 S/N")),
        tags$p(style = "text-align: justify;font-si20pt",
               strong("CEP: 66635-918 - PARQUE VERDE - BELÉM - PARÁ")),
        tags$a("https://www.detran.pa.gov.br",
               href = "https://www.detran.pa.gov.br"),
        tags$br(),
        tags$br(),
        tags$p(
          style = "text-align: justify;font-si20pt",
          strong(
            "Esta publicação deve ser citada como: Departamento de Trânsito do Estado do Pará (DETRAN-PA), Anuário Estatístico de Acidentes de Trânsito, 2025 (LC/PUB.2025/1-P), Belém, 2025."
          )
        ),
        tags$br(),
        tags$p(
          style = "text-align: justify;font-si20pt",
          strong(
            "A autorização para a reprodução total ou parcial deste trabalho deve ser solicitada ao Departamento de Trânsito do Estado do Pará, Gerência de Análise e Estatística de Trânsito, gerest@detran.pa.gov.br. Os Estados membros das Nações Unidas e suas instituições governamentais podem reproduzir este trabalho sem autorização prévia. Solicita-se apenas que mencionem a fonte e informem ao DETRAN-PA de tal reprodução."
          )
        ),
        tags$br(),
        
      ),
      column(width = 4,
             position = "center",
             solidHeader = TRUE,
               tags$br(),
               leafletOutput("mapa"),
             )
             
           )        
  ),
  tabPanel("SUGESTÕES",
           fluidRow(column(
             width = 4,
             position = "center",
             tags$br(),
             tags$p(
               style = "text-align: justify;font-si20pt",
               strong(
                 "Reclamações, sugestões, críticas e elogios relacionados ao Anuário
Estatístico de Acidentes de Trânsito do DETRAN-PA podem ser registrados na Gerência de Análise Estatística de Trânsito, por intermédio do "
               )
             ),
             tags$a("estatisticadetransito@detran.pa.gov.br",
                    href = "gerest@detran.pa.gov.br"),
           ))
  )
            )
    ),
  tabItem(tabName = "video1",
          tabBox(
            id = "t2",
            width = 12,
            tabPanel(
              "Video Institucional",
              icon = icon("video"),
              fluidRow(
                column(
                  width = 8,
                  position = "center",
                  tags$br("Projeto Strengthening Road Traffic Enforcement in Brazil"),
                  tags$video(
                    id = "videoID",
                    type = "video/mp4",
                    src = "video_detran.mp4",
                    width = 750,
                    height = 500,
                    controls = "controls"
                  ),
                  tags$br() ,
                  tags$a("Video: by Asdecom"),
                  align = "left"
                ),
                column(
                  width = 4,
                  tags$br(),
                  tags$p(
                    style = "text-align:justify;font-si20pt",
                    strong(
                      "O Departamento de Trânsito do Estado do Pará obteve o Projeto “Strengthening Road Traffic Enforcement
in Brazil” aprovado e financiado pela (United Road Safety Fund), com duração de 12 meses, se constituindo
o único selecionado do Brasil, que somado as propostas de alguns outros países, formam o conjunto de projetos
nacionais administrados pelo Fundo, coordenado e supervisionados por diversas Agências e Comissões
Regionais das Nações Unidas."
                    )
                  ),
                  tags$br(),
                  tags$p(
                    style = "text-align: justify;font-si20pt",
                    strong(
                      "Concomitantemente, o Projeto Brasileiro é supervisionado pela Comissão Econômica das Nações
Unidas para América Latina e Caribe (CEPAL), coordenado e implementado pelo DETRAN-PA
em parceria com Conselho Estadual de Trânsito do Estado do Pará (CETRAN-PA), e tem como objetivo
contribuir para a redução de mortes e lesões no Trânsito através das atividades de Educação, Engenharia e
Fiscalização em nível Estadual."
                    )
                  )
                )
              )
            )
          )),
  tabItem(tabName = "conceitos",
          tabBox(id = "t3", width = 12,
                 tabPanel("Documentação Veicular",
                          icon = icon("calendar"),
                          fluidRow(
                            column(width = 4,
                                   tags$br(),
                                   tags$p(style = "text-align:justify;font-si20pt",
                                          strong("Este item relaciona a Documentação e Registro do Veículo, definidos no Anexo I, do Regulamento do Código de Trânsito Brasileiro e Convenção de Trânsito Viário de Viena, bem como da Organização Mundial de Saúde.")),
                                   tags$br(),
                                   tags$p(style = "text-align:justify;font-si20pt",
                                          strong("1) REGISTRO:  Para transitar em vias públicas todo veículo automotor, deve ser registrado no Departamento de Trânsito do estados e Distrito Ferederal ou Circunscrição Regional de Trânsito (CIRETRAN) com jurisdição sob o domicílio ou residência do seu proprietátio, quando receberá o Certificado de Registro de Veículo (CRV), devendo possuir e estarem funcionado os equipamentos obrigatórios.")),
                                   tags$br(),
                                   tags$p(style = "text-align:justify;font-si20pt",
                                          strong("2) LICENCIAMENTO: os veículos automotores estão sujeitos a licenciamento anual, comprovado mediante Certificado de Registro e Licenciamento de Veículo (CRLV), documento de Porte obrigatório e, é expedito pelos  Departamento de Trânsito ou suas Circunscrições Regionais de Trânsito (CIRETRAN).")),
                                   tags$br(),   
                                   tags$p(style = "text-align:justify;font-si20pt",
                                          strong("2) TRANSFERÊNCIA: a transferência de propriedade do veículo ou qualquer alteração de suas características, bem como mudança de domocílio do seu proprietário, devem ser anotadas no registro inicial, sendo expedido novo Certificado de Registro de Veículo (CRV). A comunicação de mudança de endereço deverá ser feita no prazo de 30 dias, ao órgão executivo de trânsito."))
                                   
                            )
                          )
                 ),    
                tabPanel("Registro da Frota",
                         icon = icon("calendar"),
                         fluidRow(
                           column(width = 4,
                                  tags$br(),
                                  tags$p(style = "text-align:justify;font-si20pt",
                                         strong("Este item relaciona a Situação com relação ao Registro do Veículo, definidos no Anexo I, do Regulamento do Código de Trânsito Brasileiro e
Convenção de Trânsito Viário de Viena, bem como da Organização Mundial de Saúde.")),
                                  tags$br(),
                                  tags$p(style = "text-align:justify;font-si20pt",
                                         strong("1) EM CIRCULAÇÃO:  veículo que está regularmente cadastrado no Estado do Pará;")),
                                  tags$br(),
                                  tags$p(style = "text-align:justify;font-si20pt",
                                         strong("2) REGISTRO DESATIVADO: - veículo com placa antiga, sem movimentação no cadastro desde 2003")),
                                  tags$br(),
                                  tags$p(style = "text-align:justify;font-si20pt",
                                         strong("3) BAIXADO: veículo tornado sucata ou definitivamente desmontado, cuja condição consta assim assentada no cadastro")),
                                  tags$br(),
                                  tags$p(style = "text-align:justify;font-si20pt",
                                         strong("4) TRANSFERIDO P/ OUTRA UF: veículo que foi transferido para outro Estado e cujo registro passa a ser responsabilidade
do mesmo"))
                           )
                         )
                ),
 tabPanel("Tipos de Veículos", 
          icon = icon("car"),
          fluidRow(
            column(width = 4,
                   tags$br(),
                   tags$p(style = "text-align:justify;font-si20pt",
                          strong("Este item relaciona os tipos de veículos definidos no Anexo I, do Regulamento do Código de Trânsito Brasileiro e
Convenção de Trânsito Viário de Viena, bem como da Organização Mundial de Saúde.")),
                   tags$br(),
                   tags$p(style = "text-align:justify;font-si20pt",
                          strong("1) AUTOMÓVEL: Veículo destinado ao transporte de passageiros, com capacidade para até oito pessoas, mais o condutor;")),
                   tags$br(),
                   tags$p(style = "text-align:justify;font-si20pt",
                          strong("2) BICICLETA: veículo de propulsão humana, dotado de duas rodas, cujo condutor dirige em posição montada, não sendo similar à motocicleta, motoneta e ciclomotor;")),
                   tags$br(),
                   tags$p(style = "text-align:justify;font-si20pt",
                          strong("3) CAMINHÃO: Veículo automotor destinado ao transporte de carga com peso bruto total ou superior a 3.500kg, podendo transitar ou arrastar outro veículo, respeitad a capacidade máxima de tração;")),
                   tags$br(),
                   tags$p(style = "text-align:justify;font-si20pt",
                          strong("4) CAMINHONETE: Veículo destinado ao transporte de carga com peso bruto total de até três mil e quinhentos quilogramas;")),
                   tags$br(),
                   tags$p(style = "text-align:justify;font-si20pt",
                          strong("5) CAMIONETA: Veículo misto destinado ao transporte de passageiros e carga no mesmo compartimento;")),
                   tags$br(),
                   tags$p(style = "text-align:justify;font-si20pt",
                          strong("6) CICLOMOTOR: Veículo de duas ou três rodas, provido de um motor de combustão interna, cuja cilindrada não exceda a cinquenta centímetros cúbicos (3,05 polegadas cúbicas) e cuja velocidade máxima de fabricação não exceda a cinquenta quilômetros por hora;")),
            ),
            column(width = 4,
                   tags$br(),
                   tags$br(),
                   tags$br(),
                   tags$br(),
                   tags$br(),
                   tags$p(style = "text-align:justify;font-si20pt",
                          strong("7) MICRO-ÔNIBUS: veículo automotor de transporte coletivo com capacidade oara até 20 passageiros. As vans se enquadram nessa categoria de veículos;")),
                   tags$br(),
                   tags$p(style = "text-align:justify;font-si20pt",
                          strong("8) MOTOCICLETA: veículo automotor de duas rodas, dirigido por condutor em posição montada;")),
                   tags$br(),
                   tags$p(style = "text-align:justify;font-si20pt",
                          strong("9) MOTONETA: veículo automotor de duas rodas, dirigido por condutor em posição sentada;")),
                   tags$br(),
                   tags$p(style = "text-align:justify;font-si20pt",
                          strong("10) TRICICLO: veículo automotor de três rodas com ou sem cabine, dirigido por condutor em posição sentada ou montada, que não possui as casracterísticas de ciclomotor;")),
                   tags$br(),
                   tags$p(style = "text-align:justify;font-si20pt",
                          strong("11) QUADRICICLO: veículo automotor de 4 rodas, com ou sem cabine, com massa de ordem de marcha superior a 450kg, para o transporte de passageiros, ou não superior a 600kg para o transporte de cargas;")),
                   tags$br(),
                   tags$p(style = "text-align:justify;font-si20pt",
                          strong("12) ÔNIBUS: veículo automotor de transporte coletivo com capacidade para mais de 20 passageiros, ainda que, em virtude de adaptações com vista à maior comodidade destes, transporte número menor;")),
                                  tags$br(),
                                  tags$p(style = "text-align:justify;font-si20pt",
                                         strong("13) VEÍCULO ESPECIAL: veículo de passageiro, de carga, de tração, de coleção ou misto que possui características diferenciadas para realização de função especial para a qual são necessários arranjos específicos de carroceria e/ou equipamento;"))
                           )
                         )
                ),
 
 
 tabPanel("Espécie dos Veículos", 
          icon = icon("car"),
          fluidRow(
            column(width = 4,
                   tags$br(),
                   tags$p(style = "text-align:justify;font-si20pt",
                          strong("Este item relaciona as Espécies de veículos definidos no Anexo I, do Regulamento do Código de Trânsito Brasileiro e
Convenção de Trânsito Viário de Viena, bem como da Organização Mundial de Saúde.")),
                   tags$br(),
                   tags$p(style = "text-align:justify;font-si20pt",
                          strong("1) PASSAGEIRO: designa veículo destinado ao transporte de pessoas e suas bagagens;")),
                   tags$br(),
                   tags$p(style = "text-align:justify;font-si20pt",
                          strong("2) CARGA: designa veículos destinado ao transporte de carga, podendo transportar dois passageiros, exclusive o
condutor.;")),
                   tags$br(),
                   tags$p(style = "text-align:justify;font-si20pt",
                          strong("3) MISTO: designa veículos automotores destinado ao transporte simultâneo de carga e passageiro;")),
                   tags$br(),
                   tags$p(style = "text-align:justify;font-si20pt",
                          strong("4) CORRIDA/COMPETIÇÃO: designa veículos destinado ao uso esportivo de acordo com artigo 110 do CTB, somente poderão circular
nas vias com autorização da autoridade de trânsito;"))
                   
            ),
           column(
             width = 4,
             tags$br(),
             tags$br(),
             tags$br(),
             tags$br(),
             tags$br(),
             tags$br(),
             tags$p(style = "text-align:justify;font-si20pt",
                    strong("5) TRAÇÃO: designa veículo automotor destinado a tracionar outro veículo;")),
             tags$br(),
             tags$p(style = "text-align:justify;font-si20pt",
                    strong("6) ESPECIAL: designa veículos cujas características ou finalidade não permitem enquadrá-lo em uma das demais espécie;")),
             tags$br(),
             tags$p(style = "text-align:justify;font-si20pt",
                    strong("7) COLEÇÃO: designa veículos que, mesmo tendo sido fabricados há mais de trinta anos, conservam suas características
originais de fabricação e possui valor histórico próprio;"))
           ) 
          )
 ),
 tabPanel("Categoria de Veículos", 
          icon = icon("car"),
          fluidRow(
            column(width = 4,
                   tags$br(),
                   tags$p(style = "text-align:justify;font-si20pt",
                          strong("Este item relaciona as Categorias de veículos definidos no Anexo I, do Regulamento do Código de Trânsito Brasileiro e
Convenção de Trânsito Viário de Viena, bem como da Organização Mundial de Saúde.")),
                   tags$br(),
                   tags$p(style = "text-align:justify;font-si20pt",
                          strong("1) ALUGUEL: corresponde a veículos que serão usados comercialmente, para prestação de serviços a terceiros;")),
                   tags$br(),
                   tags$p(style = "text-align:justify;font-si20pt",
                          strong("2) APRENDISAGEM: corresponde a veículos para ensino de direção pelos centros de formação de condutores;")),
                   tags$br(),
                   tags$p(style = "text-align:justify;font-si20pt",
                          strong("3) CHEFE DE MISSÃO DIPLOMÁTICA: corresponde ao uso de veículos
de uso de Chefes de Missão Diplomática e de Delegações Especiais;")),
                   tags$br(),
                   tags$p(style = "text-align:justify;font-si20pt",
                          strong("4) CONSULARES DE CARREIRA: corresponde ao uso de veículos pertencentes a Repartições
Consulares de Carreira e a agentes consulares de carreira;"))
                   
                  
            ),
            column(
              width = 4,
              tags$br(),
              tags$br(),
              tags$br(),
              tags$br(),
              tags$br(),
              tags$br(),
              tags$p(style = "text-align:justify;font-si20pt",
                     strong("5) FABRICANTE: veículo em fase de desenvolvimento pelo fabricante;")),
              tags$br(),
              tags$p(style = "text-align:justify;font-si20pt",
                     strong("6) OFICIAL: veículos de propriedade de órgãos públicos dos governos federal, estadual ou municipa;")),
              tags$br(),
              tags$p(style = "text-align:justify;font-si20pt",
                     strong("7) PARTICULAR: veículos de propriedade de pessoas físicas ou jurídicas que não se enquadram nas demais categorias;"))
            )
          )
 ),
 tabPanel("Combustível dos Veículos", 
          icon = icon("car"),
          fluidRow(
            column(width = 4,
                   tags$br(),
                   tags$p(style = "text-align:justify;font-si20pt",
                          strong("Este item relaciona os Tipos de Combustível dos veículos definidos no Anexo I, do Regulamento do Código de Trânsito Brasileiro e Convenção de Trânsito Viário de Viena, bem como da Organização Mundial de Saúde.")),
                   tags$br(),
                   withMathJax(),
                   tags$p(style = "text-align:justify;font-si20pt",
                          strong("Àlcool ou Etanol: Entre os tipos de combustíveis mais notáveis o etanol, popurlarmente chamado de álcool, é uma substância química e sua produção ocorre principalmente pela fermentação de acúcares oriundos da cana-de-açúcar. È um biocombústível utilizado em motores de combustão interna com ignição por centelha. Esta modalidade subdividi-se na forma de etanol anidro como componente de mistura na formação da gasolina, ou etanol hidratado, comercializado em todo o páis como um combustível acabado. As especificações do etanol anidro e hidratado comercializado no país são estabelecidas pela Resolução ANP nº 907/2022.")),
                   tags$br(),
                   tags$p(style = "text-align:justify;font-si20pt",
                          strong("Diesel: O chamado gasóleo ou popularmente de óleo diesel é um combustível derivado apartir do refino do petróleo bruto. O diesel rebeceu este nome em homenagem ao engenheiro alemão Rudolf Diesel. O diesel é um produto pouco inflamável, medianamente tóxico, pouco volátil, límpido, isento de material em suspensão.")),
                   tags$br()
  
            ),
            column(width = 4,
                   tags$br(),
                   tags$br(),
                   tags$br(),
                   tags$br(),
                   tags$br(),
                   tags$br(),
                   tags$br(),
                   #tags$br(),
                   tags$p(style = "text-align:justify;font-si20pt",
                          strong("Gasolina: É um combustível constituído basicamente por hidrocarbonetos derivado apartir do refino do petróleo bruto. No Brasil, a Agência Nacional do Petróleo especifica três tipos de gasolinas automotivas, tipo A, tipo B e tipo C, sendo a gasolina do tipo B de uso exclusivo das Forças Armadas.")),
                   tags$br(),
                   tags$p(style = "text-align:justify;font-si20pt",
                          strong("Àlcool/Gasolina: O chamado veículo flex ou veículo de combustível duplo está equipado com um motor de combustão interna a quatro tempos que tem a capacidade de ser reabastecido e funcionar com mais de um tipo de combustível, misturados no mesmo tanque e queimados na câmara de conbustão simultaneamente."))
            )
          )
 ),                
 tabPanel("CONVENÇÕES", 
          icon = icon("landmark"),
          fluidRow(
            column(width = 4,
                   tags$br(),
                   tags$p(style = "text-align:justify;font-si20pt",
                          strong("O Sistema segue a orientação das Normas da ABNT - NBR 10697.")),
                   tags$br(),
                   tags$p(style = "text-align:justify;font-si20pt",
                          strong("(-): Dado numérico igual a zero não resultante de arredondamento;")),
                   tags$br(),
                   tags$p(style = "text-align:justify;font-si20pt",
                          strong("NI: Referem-se aos acidentes, vítimas ou atributos não informados.")),
                   tags$br(),
                   tags$p(style = "text-align:justify;font-si20pt",
                          strong("(…): Dado numérico não disponível."))
            )
          )
 )              
 )
         ),
  
        tabItem(tabName = "visao_geral",fluidPage(total_ui("geral"))),
        tabItem(tabName = "carac_loc",fluidPage(loc_ui("loc"))),
        tabItem(tabName = "carac_muni",fluidPage(muni_ui("muni"))),
        tabItem(tabName = "carac_tipo",fluidPage(tipo_ui("tipo"))),
        tabItem(tabName = "pa",fluidPage(frotapa_ui("frotapa"))),
        tabItem(tabName = "muni",fluidPage(frotamuni_ui("frotamuni"))),
        tabItem(tabName = "sobre",fluidPage(
          panel(
            title = "Sobre R, ShinyR e bs4dash",
            width = 6,  # Ajuste a largura conforme necessário
            status = "primary",
            solidHeader = TRUE,
            align = "center",
            justify = "justify",
            fluidRow(
              column(4,
              div(
                style = "text-align: justify; text-justify: inter-word;",  # Adiciona estilo CSS para centralizar e justificar o texto
                HTML("
                <h2>Sobre as tecnologias usadas</h2>
                <p>R é uma linguagem de programação estatística amplamente utilizada. 
             ShinyR é um pacote para criar aplicativos web interativos usando R. 
             bs4dash é uma extensão do shinydashboard que utiliza o Bootstrap 4. 
             Ele fornece uma variedade de componentes e funcionalidades para criar dashboards modernos e responsivos em R. 
             O pacote é particularmente útil para a criação de aplicações web interativas e visualmente atraentes, 
             aproveitando as capacidades do Bootstrap 4 para o design de interface do usuário. 
             Além disso, a integração com o Shiny facilita a construção de dashboards dinâmicos e interativos 
             que respondem às interações do usuário em tempo real. 
             Ao combinar R, ShinyR e bs4dash, os desenvolvedores têm à disposição uma poderosa combinação 
             para a criação de aplicações web estatísticas avançadas e dashboards envolventes.</p>")
              )),
              column(8, align = "center", img(src = "Dashboard.gif", height = 350))
              
            ),
            footer = fluidRow(
              column(4, align = "center", tags$img(src = "R_logo.svg.png", height = 100)),
              column(4, align = "center", tags$img(src = "hex-shiny.png", height = 100)),
              column(4, align = "center", tags$img(src = "bs4dash.png", height = 100))
            )
          )        ))
        )
    ),
#===============================================================================
    controlbar = dashboardControlbar(
      id = "controlebar",
      #skin = "light",
      #pinned = TRUE,
      disable = FALSE,
      width = 250,
      collapsed = TRUE,
      overlay = FALSE, 
      controlbarMenu(
        id = "controlebarmenu",
        controlbarItem(
          title = "Instagram",
          tags$li(class = "dropdown",
                          a(href = "https://www.instagram.com/detranpa_",
                          class = "fa fa-instagram",
                          target = "_blank"
                          )
                  )
          
        ),
        controlbarItem(
          title = "Facebook",
          tags$li(class = "dropdown",
                          a(href = "https://www.facebook.com/detranPARA",
                            class = "fa fa-facebook",
                            target = "_blank"
                          ))
        ),
        controlbarItem(
          title = "Twitter",
          tags$li(class = "dropdown",
                          a(href = "https://twitter.com/DETRAN_PA",
                            class = "fa fa-twitter",
                            target = "_blank"
                          ))
        ),
        controlbarItem(
          title = "Suporte",
          tags$li(
            class="dropdown",
            tags$a(href="https://github.com/MarioDhiego",
                   icon("github"), "Suporte", target = "_blank"))
        )
        ),
      skin = "ligth",
      pinned = FALSE
    ),
    footer = dashboardFooter(
      left = tags$b("DETRAN-PA"), 
      right = tags$b("BELÉM-PA, 2025 v.1")
    ),
    title = "Frota Registrada"
  ),


#===============================================================================
  server = function(input, output,session) {
    
    detran_location <- data.frame(
      lat = -1.37843,
      lon = -48.44034
    )
    
    output$mapa <- renderLeaflet({
    df <- read.csv(textConnection(
      "Nome, lat, lon,
      DETRAN-PA, -1.37843, -48.44034" ))
    leaflet::leaflet(df) %>%
      addTiles() %>%
      addMarkers(~lon, ~lat, label= ~htmlEscape(Nome),
                 labelOptions = labelOptions(noHide = FALSE,
                                textsize = "15px")) %>%
      addProviderTiles(providers$OpenSeaMap) %>%
      setView(lng = detran_location$lon,
              lat = detran_location$lat,
              zoom = 15)
    })
    
#===============================================================================
    total_Server("geral")
    loc_Server("loc")
    muni_Server("muni")
    tipo_Server("tipo")
    frotapa_Server("frotapa")
    frotamuni_Server("frotamuni")
  }
)




