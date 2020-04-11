source('library.R')


theme_demo <- function(theme) {

  shinyUI(

    fluidPage(

      theme = shinythemes::shinytheme(theme),
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      ),


      navbarPage("Legal Intelligence",
                 tabPanel("Sobre",

                          fluidRow(
                            column(9,
                                   wellPanel(

                                     #shinythemes::themeSelector(),

                                     h1("Legal Intelligence",align = "center"),

                                     h3(em(strong("Utilizando os dados e estatística para gerar conhecimento")),align = "center"),

                                     h5("Desenvolvido por: ",
                                        a("Djonathan Luiz de Oliveira Quadras",
                                          href = "http://lattes.cnpq.br/9927407566061611"), align="center"),
                                     h6("Contato: oliveira.ind.eng@gmail.com",align = "center"),
                                     br(),
                                     h2("O que é?"),
                                     p("A ", em("ciência de dados")," “É uma disciplina que permite transformar dados brutos em entendimento, insight e conhecimento.” (WICKHAM & GROLEMUND, 2017).
                                             A aplicação da ciência de dados, bem como da estatística, no âmbito jurídico é chamado de ", strong("jurimetria"), " (ANDRADE, 2018).
                                             A metodologia mais utilizada para este tipo de análise é a proposta por Wickham apresentada na figura abaixo."),
                                     br(),

                                     div(img(src = "https://d33wubrfki0l68.cloudfront.net/571b056757d68e6df81a3e3853f54d3c76ad6efc/32d37/diagrams/data-science.png"), style="text-align: center;"),
                                     br(),
                                     p("Esta aplicação busca desenvolver todas as etpas dessa metodologia, que consistem em: ", strong("Importanção dos dados"), ", que ocorre na aba
                                             'Inserir bases' e onde o programa é alimentado (não ficando salvas informações nesta aplicação); ", strong("Visualização"), ", disponível nas abas 'Tabelas'
                                             de forma tabular e 'Gráficos' por meio de gráficos estatísticos adaptativos e responsivos; ", strong("Modelagem"), ", disponível na aba 'Previsões',
                                             onde é possível que sejam feitas as previsões desejadas. Note que a etapa 'Transformação' é realizada internamente pelo sistema, e que 'Comunicar'
                                             é exatamente o objetivo dessa aplicação, concluindo assim o ciclo da ciência de dados."),

                                     h2("Referências:"),
                                     br(),
                                     p("WICKHAM, Hadley; GROLEMUND, Garrett. R for data science. O'Reilly Media, 2017"),
                                     p("DE ANDRADE, Mariana Dionísio. A utilização do sistema R-studio e da jurimetria como ferramentas complementares à pesquisa jurídica. REVISTA QUAESTIO IURIS, v. 11, n. 2, p. 680-692, 2018."),
                                     br()
                                   )

                            )
                          )

                 ),

                 tabPanel("Inserir Bases",

                          # Sidebar with a slider input for number of bins
                          sidebarLayout(
                            sidebarPanel(
                              tags$h2("Importar Bases"),
                              br(),
                              tags$p("Nesta aba são inseridas as tabelas de Apontamentos Tabular e Time Sheet.
                                 Para que o programa funcione corretamente, insira os arquivos nos lugares selecionados e depois clique em 'Enviar'."),
                              br(),

                              selectInput(inputId = "selecionaTipoEntrada",
                                          label = ("Escolha a base desejada"),
                                          choices = c("Apontamento Tabular e Time Sheet", "Base Completa")),

                              uiOutput("apontamento"),
                              uiOutput("timesheet"),
                              uiOutput("basecompleta"),

                              actionButton(inputId = "enable", label = "Enviar.", size = 1),
                              br(),
                              br(),
                              uiOutput("download1")


                            ), #Fim do sidebarPanel

                            # Show a plot of the generated distribution
                            mainPanel(

                              tags$style(type="text/css",
                                         ".shiny-output-error { visibility: hidden; }",
                                         ".shiny-output-error:before { visibility: hidden; }"
                              ),
                              withSpinner(DT::dataTableOutput(outputId = "dataset"), type = 4)

                            )

                          )#Fim do sidebarLayout

                 ), #Fim do tabPanel


                 ####################################
                 ##########################   Tabelas
                 ####################################

                 navbarMenu("Tabelas",

                            tabPanel(title = "Tabelas",

                                     sidebarLayout(
                                       sidebarPanel(
                                         tags$h2("Tabelas por Períodos"),

                                         br(),
                                         selectInput(inputId = "selectTableInformation",
                                                     label = ("Qual informação você deseja?"),
                                                     choices = c("Total", "Por Cliente Específico", "Por Envolvido Específico")),

                                         uiOutput("select1"),
                                         uiOutput("select2"),
                                         uiOutput("select3"),

                                         selectInput(inputId = "selectTablePeriod",
                                                     label = ("Qual período você deseja?"),
                                                     choices = c("Por Ano", "Por Mês", "Período Específico")),



                                         uiOutput("select4"),
                                         uiOutput("select5"),
                                         uiOutput("select6"),
                                         uiOutput("select7"),


                                         downloadButton('downloadTabelasPeriodos', 'Download')


                                       ), #Fim do SidebarPanel


                                       mainPanel(

                                         tags$style(type="text/css",
                                                    ".shiny-output-error { visibility: hidden; }",
                                                    ".shiny-output-error:before { visibility: hidden; }"
                                         ),
                                         withSpinner(DT::dataTableOutput(outputId = "datasetPeriods"), type = 4)

                                       )#Fim do mainPanel
                                     )#Fim do Sidebar layout
                            ), #Fim do tab panel



                            tabPanel(title = "Banco de Horas",

                                     sidebarLayout(
                                       sidebarPanel(
                                         tags$h2("Banco de Horas"),
                                         br(),
                                         tags$p("Aqui são apresentadas tabelas de acordo com o banco de horas relacionado aos clientes ou envolvidos."),
                                         br(),

                                         selectInput(inputId = "bancoDeHoras",
                                                     label = "Cliente ou Envolvido?",
                                                     choices = c("Cliente", "Envolvido")),

                                         uiOutput("inputcliente"),
                                         uiOutput("selecionado"),
                                         uiOutput("periodo1"),
                                         uiOutput("periodo2"),

                                         selectInput(inputId = "tipoDeContrato",
                                                     label = "Qual o tipo de Contrato Desejado?",
                                                     choices = c("Total", "Personalizado")),

                                         uiOutput("contratos"),


                                         downloadButton('downloadTabelasBanco', 'Download')



                                       ), #Fim do SidebarPanel

                                       mainPanel(

                                         tags$style(type="text/css",
                                                    ".shiny-output-error { visibility: hidden; }",
                                                    ".shiny-output-error:before { visibility: hidden; }"
                                         ),
                                         withSpinner(DT::dataTableOutput(outputId = "datasethour"), type = 4)


                                       ) #Fim do mainPanel
                                     )#Fim do Sidebar layout
                            ) #Fim do tab panel
                 ), #Fim do navbarMenu




                 ######################################
                 ##########################   GRAFICOS
                 ######################################


                 navbarMenu("Gráficos",

                            tabPanel("Tipo de Contrato",

                                     sidebarLayout(
                                       sidebarPanel(
                                         tags$h2("Análises"),
                                         br(),

                                         selectInput(inputId = "selectTemporaltypeG1",
                                                     label = ("Selecione por Total, Cliente ou Envolvido"),
                                                     choices = c("Total", "Por Cliente", "Por Envolvido")),

                                         uiOutput("graph1UI1"),
                                         uiOutput("graph1UI2"),
                                         uiOutput("graph1UI11"),


                                         selectInput(inputId = "selectgrafico1",
                                                     label = ("Qual visualização você deseja?"),
                                                     choices = c("Total", "Anual", "Mensal", "Período Específico")),

                                         uiOutput("graph1UI3"),
                                         uiOutput("graph1UI4"),
                                         uiOutput("graph1UI5"),
                                         uiOutput("graph1UI6"),
                                         selectInput(inputId = "tipoGrafico1",
                                                     label = ("Qual o tipo de gráfico?"),
                                                     choices = c("Barras", "Pizza")),
                                         uiOutput("graph1UI7"),
                                         uiOutput("graph1UI8"),
                                         uiOutput("graph1UI9"),
                                         uiOutput("graph1UI12")


                                       ),




                                       mainPanel(

                                         tabsetPanel(

                                           tabPanel(title = "Gráfico",
                                                    br(),
                                                    withSpinner(plotlyOutput(outputId = "grafico1"), type = 4)),
                                           tabPanel(title = "Tabela",
                                                    br(),
                                                    withSpinner(dataTableOutput(outputId = "tabelaGrafico1"), type = 4))
                                         )

                                       )

                                     )

                            ),

                          tabPanel("Series Temporais",

                                     sidebarLayout(
                                       sidebarPanel(
                                         tags$h2("Evoluções temporais"),
                                         br(),

                                         selectInput(inputId = "selectTemporaltype",
                                                     label = ("Selecione o tipo de Série Temporal"),
                                                     choices = c("Total", "Por Cliente", "Por Envolvido")),


                                         uiOutput("temporalSel1"),
                                         uiOutput("temporalSel2"),
                                         uiOutput("temporalSel3"),

                                         selectInput(inputId = "selectTemporalperiod",
                                                     label = ("Selecione o período"),
                                                     choices = c("Total", "Ano Específico", "Período Específico")),


                                         uiOutput("temporalSel4"),
                                         uiOutput("temporalSel5"),
                                         uiOutput("temporalSelPE1"),
                                         uiOutput("temporalSelPE2"),
                                         uiOutput("escalaEmY"),
                                         uiOutput("escalaEmX"),
                                         uiOutput("selTipo")


                                       ),

                                       mainPanel(

                                         tabsetPanel(



                                           tabPanel(title = "Gráfico",
                                                    br(),
                                                    withSpinner(plotlyOutput(outputId = "grafico2"), type = 4)),
                                           tabPanel(title = "Tabela",
                                                    br(),
                                                    withSpinner(dataTableOutput(outputId = "tabelaGrafico2"), type = 4))

                                         )

                                       )

                                     ) #Fim SidebarLayout

                            ) #Fim Tab Panel


                 )

      )

    )
  )

}

theme_demo("flatly")
