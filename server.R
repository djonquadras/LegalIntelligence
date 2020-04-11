source('library.R')


shinyServer(function(input, output, session) {

    options(shiny.maxRequestSize = 30*1024^2)

    #Parte inicial
    ###################################
    #Funcao que estrutura a tabela

    total <- function(apontTab, timeSh){

        # ------------------------------Importando as bases
        apontamentosTabular <- apontTab
        timeSheet <- timeSh


        #------------------------------Corrigindo apontamentosTabular e TimeSheet
        names(apontamentosTabular) <- apontamentosTabular[3,]
        apontamentosTabular <- apontamentosTabular %>% select(-'NA', -'Contexto', -'Pasta', -'Valor/Hora', -'Valor apont.', -'Tipo')
        apontamentosTabular <- apontamentosTabular[1:(nrow(apontamentosTabular)-1),]
        apontamentosTabular <- apontamentosTabular[4:nrow(apontamentosTabular),]



        #------------------------------Para Tibble
        apontamentosTabular <- as_tibble(apontamentosTabular)
        timeSheet <- as_tibble(timeSheet)

        #------------------------------Mudar Cliente
        apontamentosTabular <- apontamentosTabular %>% mutate(Cliente = apontamentosTabular$'Cliente/Entidade')
        apontamentosTabular <- apontamentosTabular %>% select(-'Cliente/Entidade')

        #------------------------------Convertendo para Date
        apontamentosTabular <- apontamentosTabular %>% mutate(Data = dmy(Data))
        apontamentosTabular <- apontamentosTabular %>% mutate(Duração = chron(times = apontamentosTabular$Duração))
        #timeSheet <- timeSheet %>% mutate(Data = dmy(Data))
        timeSheet <- timeSheet %>% mutate(Duração = chron(times = timeSheet$Duração))

        #Corrigindo Tipo de Contrato
        timeSheet$`Tipo de Contrato`[is.na(timeSheet$`Tipo de Contrato`)] <- 'Não Registrado'
        timeSheet$`Tipo de Contrato`[timeSheet$`Tipo de Contrato` == 'NÃO UTILIZAR - Consultoria Geral'] <- 'Consultoria Antigo'
        timeSheet$`Tipo de Contrato`[timeSheet$`Tipo de Contrato` == 'NÃO UTILIZAR - Contencioso Geral'] <- 'Contencioso Antigo'


        apontamentosTabular <- apontamentosTabular %>% arrange(Data, Envolvido, Duração, Descrição)
        timeSheet <- timeSheet %>% arrange(Data, Envolvido, Duração, Descrição)
        timeSheet <- timeSheet %>% select(-`Data de cadastro`, - `Última modificação`, -`Cadastrado Por`, - Contexto)

        #Mutando
        apontamentosTabular <- apontamentosTabular %>% mutate(Ordem = 1:nrow(apontamentosTabular))
        timeSheet <- timeSheet %>% mutate(Ordem = 1:nrow(timeSheet))

        #Juntando
        T <- tibble("Tipo de Contrato" = timeSheet$`Tipo de Contrato`,
                    "Ordem" = timeSheet$Ordem)
        dataset <- merge(apontamentosTabular, T, by = intersect(x = "Ordem", y = "Ordem"), no.dups = TRUE)
        dataset <- dataset %>% select(-"Ordem")
        for(i in 1:nrow(dataset)){
            dataset$`Tipo de Contrato`[i] <- gsub('[0-9]+ ', '', dataset$`Tipo de Contrato`[i])
        }
        return(dataset)
    }

    #Alocar variaveis de acordo com as funcoes

    at <- reactive({
        if(is.null(input$at))
            return(NULL)
        if(is.null(input$ts))
            return(NULL)
        aat <- input$at
        at <- read_excel(aat$datapath)
    })
    ts <- reactive({

        if(is.null(input$at))
            return(NULL)
        if(is.null(input$ts))
            return(NULL)
        tss <- input$ts
        ts <- read_excel(tss$datapath)
    })
    bc <- reactive({

        if(is.null(input$bc))
            return(NULL)

        bbc <- input$bc
        bc <- read_excel(bbc$datapath)
    })

    datasetTotal <- reactive({

        if(input$selecionaTipoEntrada == "Apontamento Tabular e Time Sheet"){
            dataset <- datasetTotal1 <- total(at(), ts())
            return(dataset)
        }

        if(input$selecionaTipoEntrada == "Base Completa"){
            dataset <- bc()
            dataset <- as_tibble(dataset)
            dataset <- dataset %>% mutate(Data = as_date(Data))
            dataset <- dataset %>% mutate(Duração = chron(times = dataset$Duração))
            return(dataset)
        }

    })
    datasetDownload <- reactive({
        dataset <- datasetTotal()
        dataset <- dataset %>% mutate(Data = as.character(Data))
        return(dataset)
    })

    #Tabelas de Períodos
    datasetTabelas1 <- reactive({

        dataset <- datasetTotal()
        if(input$selectTableInformation == "Total"){
            return(dataset)
        }

        if(input$selectTableInformation == "Por Cliente Específico"){
            if(input$selectListaDigitado == "Lista"){
                dataset <- dataset %>% filter(Cliente %in% input$listClients)
                return(dataset)
            }
            if(input$selectListaDigitado == "Digitado"){
                dataset <- dataset %>% filter(str_detect(string = Cliente, pattern = as.character(input$clientName)))
                return(dataset)
            }
        }

        if(input$selectTableInformation == "Por Envolvido Específico"){
            dataset <- dataset %>% filter(Envolvido %in% input$listEnvolvido)
            return(dataset)
        }


    })
    datasetTabelas2 <- reactive({

        dataset <- datasetTabelas1()
        dataset <- as.tibble(dataset)


        if(input$selectTablePeriod == "Por Ano"){
            dataset <- dataset %>% filter(year(Data) == input$tabelaAnos)
            return(dataset)
        }

        if(input$selectTablePeriod == "Por Mês"){
            dataset <- dataset %>% filter(year(Data) == input$tabelaAnos)
            dataset <- dataset %>% filter(month(Data) == input$tabelaMes)
            return(dataset)
        }

        if(input$selectTablePeriod == "Período Específico"){
            dataset <- dataset %>% filter(Data >= input$dataInicial)
            dataset <- dataset %>% filter(Data <= input$dataFinal)
            return(dataset)
        }
    })

    #Banco de Dados
    datasetBanco <- reactive({

        dataset <- datasetTotal()

        if(input$bancoDeHoras == "Cliente"){
            if(input$selectListaDigitadoBanco == "Digitar"){
                dataset <- dataset %>% filter(str_detect(string = Cliente, pattern = as.character(input$clientNameBanco)))
            }

            if(input$selectListaDigitadoBanco == "Lista"){
                dataset <- dataset %>% filter(Cliente %in% input$clientNameBanco)
            }
        }

        dataset <- dataset %>% filter(Data >= input$firstdate2)
        dataset <- dataset %>% filter(Data <= input$lastdate2)
        dataset <- dataset %>% mutate(Duração = chron(times = dataset$Duração))

        if(nrow(dataset) == 0)
            return(NULL)

        Total <- tibble()

        if(input$tipoDeContrato == "Total"){

            if(input$bancoDeHoras == "Envolvido"){

                nomes <- sort(c(unique(dataset$Envolvido)))
                horastotais <- vector()
                consultoria <- vector()
                Contencioso <- vector()
                sst <- vector()
                clientes <- vector()

                for(i in 1:length(nomes)){

                    df2 <- dataset %>% filter(dataset$Envolvido == nomes[i])

                    datasetconsultoria <- as_tibble(df2 %>% filter(str_detect(string = `Tipo de Contrato`, pattern = "Consultoria")))
                    datasetContencioso <- as_tibble(df2 %>% filter(str_detect(string = `Tipo de Contrato`, pattern = "Contencioso")))
                    datasetsst <- as_tibble(df2 %>% filter(df2$'Cliente' == 'SST - Silva, Santana & Teston Advogados'))
                    datasetclientes <- as_tibble(df2 %>% filter(df2$'Cliente' != 'SST - Silva, Santana & Teston Advogados'))


                    somaTotal <- sum((as.double(df2$Duração)), na.rm = TRUE)
                    somaConsultoria <- sum(as.double(datasetconsultoria$Duração), na.rm = TRUE)
                    somaContencioso <- sum(as.double(datasetContencioso$Duração), na.rm = TRUE)
                    somasst <- sum(as.double(datasetsst$Duração), na.rm = TRUE)
                    somacliente <- sum(as.double(datasetclientes$Duração), na.rm = TRUE)

                    horastotais[i] <- round((24*somaTotal),2)
                    consultoria[i] <- round((24*somaConsultoria),2)
                    Contencioso[i] <- round((24*somaContencioso),2)
                    sst[i] <- round((24*somasst),2)
                    clientes[i] <-round((24*somacliente),2)


                }
                Total <- tibble("Nome" = nomes,
                                "Total de Horas Trabalhadas" = horastotais,
                                "Para SST" = sst,
                                "Para Clientes" = clientes,
                                "Consultivo" = consultoria,
                                "Contencioso" = Contencioso)
                return(Total)

            }

            if(input$bancoDeHoras == "Cliente"){

                nomes <- sort(c(unique(dataset$Cliente)))
                horastotais <- vector()
                consultoria <- vector()
                Contencioso <- vector()
                interno <- vector()
                projetos <- vector()

                for(i in 1:length(nomes)){

                    somaConsultoria <- 0
                    somaContencioso <- 0
                    somaInterno <- 0
                    somaProjetos <- 0
                    somaTotal <- 0

                    df2 <- dataset %>% filter(dataset$Cliente == nomes[i])

                    datasetconsultoria <- as_tibble(df2 %>% filter(str_detect(string = `Tipo de Contrato`, pattern = "Consultoria")))
                    datasetContencioso <- as_tibble(df2 %>% filter(str_detect(string = `Tipo de Contrato`, pattern = "Contencioso")))
                    datasetinterno <- as_tibble(df2 %>% filter(df2$'Tipo de Contrato' == 'Interno'))
                    datasetprojetos <- as_tibble(df2 %>% filter(df2$'Tipo de Contrato' == 'Projetos Específicos'))

                    somaConsultoria <- sum((datasetconsultoria$Duração), na.rm = TRUE)
                    somaContencioso <- sum((datasetContencioso$Duração), na.rm = TRUE)
                    somaInterno <- sum((datasetinterno$Duração), na.rm = TRUE)
                    somaProjetos <- sum((datasetprojetos$Duração), na.rm = TRUE)
                    somaTotal <- sum((df2$Duração), na.rm = TRUE)


                    consultoria[i] <- round((24*(as.double(somaConsultoria))),2)
                    Contencioso[i] <- round((24*(as.double(somaContencioso))),2)
                    interno[i] <- round((24*(as.double(somaInterno))),2)
                    projetos[i] <-round((24*(as.double(somaProjetos))),2)
                    horastotais[i] <- round((24*(as.double(somaTotal))),2)

                }
                Total <- tibble("Cliente" = nomes,
                                "Horas totais dedicadas" = horastotais,
                                "Total de Consultoria" = consultoria,
                                "Total de Contencioso" = Contencioso,
                                "Total de Projetos Específicos" = projetos,
                                "Total de Interno" = interno)

                return(Total)

            }

        }
        if(input$tipoDeContrato == "Personalizado"){

            if(length(input$tipos) == 0)
                return(NULL)

            if(input$bancoDeHoras == "Envolvido"){

                nomes <- sort(unique(dataset$Envolvido))
                horasL <- list()
                tipo <- input$tipos

                for(i in 1:length(tipo)){
                    dataset2 <- dataset %>% filter(`Tipo de Contrato` == tipo[i])
                    horasV <- vector()
                    for(j in 1:length(nomes)){
                        dataset3 <- dataset2 %>% filter(Envolvido == nomes[j])
                        horasV[j] <- as.double(round((24*(sum((as.double(dataset3$Duração)), na.rm = TRUE))),2))
                    }
                    horasL[[i]] <- horasV
                }

                Total <- nomes

                for(i in 1:length(tipo)){
                    vetor <- horasL[[i]]
                    Total <- cbind(Total, horasL[[i]])
                }
                Total <- as_tibble(Total)

                names(Total) <- c("Envolvido", tipo)
                Total[, 2:ncol(Total)] <- apply(Total[, 2:ncol(Total)], 2,            # Specify own function within apply
                                                function(x) as.numeric(as.character(x)))
                return(Total)

            }

            if(input$bancoDeHoras == "Cliente"){

                nomes <- sort(unique(dataset$Cliente))
                horasL <- list()
                tipo <- input$tipos

                for(i in 1:length(tipo)){
                    dataset2 <- dataset %>% filter(`Tipo de Contrato` == tipo[i])
                    horasV <- vector()
                    for(j in 1:length(nomes)){
                        dataset3 <- dataset2 %>% filter(Cliente == nomes[j])
                        horasV[j] <- as.double(round((24*(sum((as.double(dataset3$Duração)), na.rm = TRUE))),2))
                    }
                    horasL[[i]] <- horasV
                }

                Total <- nomes

                for(i in 1:length(tipo)){
                    vetor <- horasL[[i]]
                    Total <- cbind(Total, horasL[[i]])
                }
                Total <- as_tibble(Total)
                names(Total) <- c("Cliente", tipo)

                Total[, 2:ncol(Total)] <- apply(Total[, 2:ncol(Total)], 2,            # Specify own function within apply
                                                function(x) as.numeric(as.character(x)))

                return(Total)

            }
        }
    })

    ###################################


    #Output - User Interface

    #Entradas
    #####
    output$apontamento = renderUI({

        if(input$selecionaTipoEntrada != "Apontamento Tabular e Time Sheet")
            return(NULL)

        if(input$selecionaTipoEntrada == "Apontamento Tabular e Time Sheet"){
            apontamentos <- fileInput(inputId = "at",
                                      label = "Apontamentos Tabular",
                                      multiple = FALSE,
                                      accept = ".xlsx",
                                      buttonLabel = "Buscar",
                                      placeholder = "Sem arquivo.")
            return(apontamentos)
        }

    })

    output$timesheet = renderUI({

        if(input$selecionaTipoEntrada != "Apontamento Tabular e Time Sheet")
            return(NULL)

        if(input$selecionaTipoEntrada == "Apontamento Tabular e Time Sheet"){
            timesheet <- fileInput(inputId = "ts",
                                   label = "Time Sheet",
                                   multiple = FALSE,
                                   accept = ".xlsx",
                                   buttonLabel = "Buscar",
                                   placeholder = "Sem arquivo.")
            return(timesheet)
        }

    })

    output$basecompleta = renderUI({

        if(input$selecionaTipoEntrada != "Base Completa")
            return(NULL)

        if(input$selecionaTipoEntrada == "Base Completa"){
            basecompleta <- fileInput(inputId = "bc",
                                      label = "Base Completa",
                                      multiple = FALSE,
                                      accept = ".xlsx",
                                      buttonLabel = "Buscar",
                                      placeholder = "Sem arquivo.")
            return(basecompleta)
        }

    })
    #####


    # Tabelas
    #####

    #Select 1 - Select Lista ou Digitado
    output$select1 = renderUI({

        if(input$selectTableInformation != "Por Cliente Específico")
            return(NULL)

        if(input$selectTableInformation == "Por Cliente Específico"){

            retorno <- selectInput(inputId = "selectListaDigitado",
                                   label = ("Por Lista de Nomes ou Digitado?"),
                                   choices = c("Lista", "Digitado"))
            return(retorno)
        }
    })

    #Select 2 - Lista ou TextInput
    output$select2 = renderUI({

        if(input$selectTableInformation != "Por Cliente Específico")
            return(NULL)

        if(input$enable == 0)
            return(NULL)

        dataset <- datasetTotal()

        if(input$selectListaDigitado == "Lista"){

            retorno <- selectInput(inputId = "listClients",
                                   label = ("Selecione o Cliente"),
                                   choices = sort(unique(dataset$Cliente)),
                                   multiple = TRUE,
                                   selected = sort(unique(dataset$Cliente))[1])

            return(retorno)


        }

        if(input$selectListaDigitado == "Digitado"){

            retorno <- textInput(inputId = "clientName",
                                 label = ("Digite qual o cliente desejado."))
            return(retorno)
        }
    })

    #Select 3 - Lista Envolvidos
    output$select3 = renderUI({

        if(input$selectTableInformation != "Por Envolvido Específico")
            return(NULL)

        if(input$enable == 0)
            return(NULL)

        dataset <- datasetTotal()

        if(input$selectTableInformation == "Por Envolvido Específico"){

            retorno <- selectInput(inputId = "listEnvolvido",
                                   label = ("Selecione o Envolvido"),
                                   choices = sort(unique(dataset$Envolvido)),
                                   multiple = TRUE,
                                   selected = sort(unique(dataset$Envolvido))[1])
            return(retorno)
        }
    })

    #Select 4 - Ano
    output$select4 = renderUI({

        if(input$selectTablePeriod == "Período Específico")
            return(NULL)

        if(input$enable == 0)
            return(NULL)

        dataset <- datasetTabelas1()
        vetor <- c(sort(unique(year(dataset$Data))))

        if(length(vetor) == 0)
            return(NULL)

        if(input$selectTablePeriod != "Período Específico"){

            retorno <- selectInput(inputId = "tabelaAnos",
                                   label = ("Selecione o ano desejado."),
                                   choices = vetor)

            return(retorno)
        }
    })

    #Select 5- Mês
    output$select5 = renderUI({

        if(input$selectTablePeriod != "Por Mês")
            return(NULL)

        if(input$enable == 0)
            return(NULL)

        dataset <- datasetTabelas1()
        dataset <- dataset %>% filter(year(Data) == input$tabelaAnos)
        vetor <- c(sort(unique(month(dataset$Data))))

        if(length(vetor) == 0)
            return(NULL)

        if(input$selectTablePeriod == "Por Mês"){

            retorno <- selectInput(inputId = "tabelaMes",
                                   label = ("Selecione o mês desejado."),
                                   choices = vetor)
            return(retorno)
        }
    })

    #Select 6 - Período Inical
    output$select6 = renderUI({

        if(input$selectTablePeriod != "Período Específico")
            return(NULL)

        if(input$enable == 0)
            return(NULL)

        dataset <- datasetTabelas1()

        if(is.null(dataset$Data))
            return(NULL)

        if(input$selectTablePeriod == "Período Específico"){

            retorno <- dateInput(inputId = "dataInicial",
                                 value = ymd(min(dataset$Data)),
                                 min = ymd(min(dataset$Data)),
                                 max = ymd(max(dataset$Data)),
                                 label = "Data Inicial")
            return(retorno)
        }
    })

    #Select 7 - Periodo Final
    output$select7 = renderUI({

        if(input$selectTablePeriod != "Período Específico")
            return(NULL)

        if(input$enable == 0)
            return(NULL)

        dataset <- datasetTabelas1()

        if(is.null(dataset$Data))
            return(NULL)

        if(input$selectTablePeriod == "Período Específico"){

            retorno <- dateInput(inputId = "dataFinal",
                                 value = ymd(max(dataset$Data)),
                                 min = ymd(min(dataset$Data)),
                                 max = ymd(max(dataset$Data)),
                                 label = "Data Final")
            return(retorno)
        }
    })


    #Banco

    #Lista ou Nome?
    output$inputcliente = renderUI({

        if(input$enable == 0)
            return(NULL)

        if(input$bancoDeHoras != "Cliente")
            return(NULL)

        retorno <- selectInput(inputId = "selectListaDigitadoBanco",
                               label = ("Tudo ou Deseja Selecionar?"),
                               choices = c("Tudo", "Lista", "Digitar"))
        return(retorno)

    })


    #Seleção
    output$selecionado = renderUI({

        if(input$enable == 0)
            return(NULL)

        dataset <- datasetTotal()

        if(nrow(dataset) == 0)
            return(NULL)

        if(input$bancoDeHoras != "Cliente")
            return(NULL)

        if(input$selectListaDigitadoBanco == "Tudo")
            return(NULL)

        if(input$selectListaDigitadoBanco == "Digitar"){
            retorno <- textInput(inputId = "clientNameBanco",
                                 label = ("Digite qual o cliente desejado."))
            return(retorno)
        }

        if(input$selectListaDigitadoBanco == "Lista"){
            retorno <- selectInput(inputId = "clientNameBanco",
                                   label = ("Digite qual o cliente desejado."),
                                   choices = sort(unique(dataset$Cliente)),
                                   multiple = TRUE,
                                   selected = sort(unique(dataset$Cliente))[1])
            return(retorno)
        }

    })

    #Período 1
    output$periodo1 = renderUI({

        if(input$enable == 0)
            return(NULL)

        dataset <- datasetTotal()

        if(nrow(dataset) == 0)
            return(NULL)

        if(input$bancoDeHoras == "Cliente"){

            if(input$selectListaDigitadoBanco == "Digitar"){
                dataset <- dataset %>% filter(str_detect(string = Cliente, pattern = as.character(input$clientNameBanco)))
            }

            if(input$selectListaDigitadoBanco == "Lista"){
                dataset <- dataset %>% filter(Cliente %in% input$clientNameBanco)
            }
        }

        if(length(dataset$Data) == 0)
            return(NULL)

        retorno <- dateInput(inputId = "firstdate2",
                             value = ymd(min(dataset$Data)),
                             min = ymd(min(dataset$Data)),
                             max = ymd(max(dataset$Data)),
                             label = "Data Inicial")
        return(retorno)

    })

    #Período 2
    output$periodo2 = renderUI({

        if(input$enable == 0)
            return(NULL)

        dataset <- datasetTotal()

        if(nrow(dataset) == 0)
            return(NULL)

        if(input$bancoDeHoras == "Cliente"){
            if(input$selectListaDigitadoBanco == "Digitar"){
                dataset <- dataset %>% filter(str_detect(string = Cliente, pattern = as.character(input$clientNameBanco)))
            }

            if(input$selectListaDigitadoBanco == "Lista"){
                dataset <- dataset %>% filter(Cliente %in% input$clientNameBanco)
            }
        }

        if(length(dataset$Data) == 0)
            return(NULL)

        retorno <- dateInput(inputId = "lastdate2",
                             value = ymd(max(dataset$Data)),
                             min = ymd(min(dataset$Data)),
                             max = ymd(max(dataset$Data)),
                             label = "Data Final")
        return(retorno)

    })

    #Contratos
    output$contratos = renderUI({

        if(input$enable == 0)
            return(NULL)

        if(input$tipoDeContrato != "Personalizado")
            return(NULL)

        dataset <- datasetTotal()

        if(nrow(dataset) == 0)
            return(NULL)

        if(input$bancoDeHoras == "Cliente"){
            if(input$selectListaDigitadoBanco == "Digitar"){
                dataset <- dataset %>% filter(str_detect(string = Cliente, pattern = as.character(input$clientNameBanco)))
            }

            if(input$selectListaDigitadoBanco == "Lista"){
                dataset <- dataset %>% filter(Cliente %in% input$clientNameBanco)
            }
        }

        dataset <- dataset %>% filter(Data >= input$firstdate2)
        dataset <- dataset %>% filter(Data <= input$lastdate2)

        vetor <- sort(unique(dataset$'Tipo de Contrato'))
        retorno <- checkboxGroupInput(inputId = "tipos",
                                      label = "Faça a seleção.",
                                      choices = vetor)
        return(retorno)

    })

    #####

    #Download
    #####
    output$download1 = renderUI({

        if(input$enable == 0)
            return(NULL)

        downloadButton('datasetTotalDownload', 'Download')
    })
    #####

    # Gráfico 1
    #####
    #Selecao por Cliente 1
    output$graph1UI1 = renderUI({

        if(input$enable == 0)
            return(NULL)
        if(input$selectTemporaltypeG1 != "Por Cliente")
            return(NULL)

        if(input$selectTemporaltypeG1 == "Por Cliente"){
            selecionado <- selectInput(inputId = "listOrName",
                                       label = ("Seleção por lista ou nome?"),
                                       choices = c("Lista", "Nome"))
            return(selecionado)
        }
    })

    #Seleção Cliente ou Envolvido
    output$graph1UI2 = renderUI({

        if(input$enable == 0)
            return(NULL)

        if(input$selectTemporaltypeG1 == "Total"){
            return(NULL)
        }

        dataset <- datasetTotal()

        if(input$selectTemporaltypeG1 == "Por Cliente"){

            if(input$listOrName == "Nome"){
                ui <- textInput(inputId = "selectionG1",
                                label = ("Selecione o Cliente"))
                return(ui)
            }

            if(input$listOrName == "Lista"){
                ui <- selectInput(inputId = "selectionG1",
                                  label = ("Selecione o Cliente"),
                                  choices = sort(c(unique(dataset$Cliente))),
                                  multiple = TRUE,
                                  selected = sort(c(unique(dataset$Cliente)))[1])
                return(ui)
            }
        }

        if(input$selectTemporaltypeG1 == "Por Envolvido"){

            ui <-selectInput(inputId = "selectionG1",
                             label = ("Selecione o Envolvido"),
                             choices = sort(c(unique(dataset$Envolvido))),
                             multiple = TRUE,
                             selected = sort(c(unique(dataset$Envolvido)))[1])
            return(ui)
        }

    })

    #Nome do Grupo
    output$graph1UI11 = renderUI({

        if(input$enable == 0)
            return(NULL)

        if(input$selectTemporaltypeG1 == "Por Cliente"){

            if(input$listOrName == "Lista"){

                if(length(input$selectionG1) > 1){
                    ui <- textInput(inputId = "groupName",
                                    label = ("Qual o nome do Grupo?"),
                                    value = "Grupo 1")
                    return(ui)
                } else {return(NULL)}


            }

        } else if(input$selectTemporaltypeG1 == "Por Envolvido"){

            if(length(input$selectionG1) > 1){
                ui <- textInput(inputId = "groupName",
                                label = ("Qual o nome do Grupo?"),
                                value = "Grupo 1")
                return(ui)
            } else {return(NULL)}
        } else {return(NULL)}

    })

    #Tabela Pós Seleção
    tabelaG11 <- reactive({

        dataset <- datasetTotal()

        if(input$selectTemporaltypeG1 == "Total"){
            return(dataset)
        }

        if(input$selectTemporaltypeG1 == "Por Cliente"){

            if(input$listOrName == "Lista"){
                dataset <- dataset %>% filter(Cliente %in% input$selectionG1)
                return(dataset)
            }

            if(input$listOrName == "Nome"){
                dataset <- dataset %>% filter(str_detect(string = Cliente, pattern = as.character(input$selectionG1)))
                return(dataset)
            }
        }

        if(input$selectTemporaltypeG1 == "Por Envolvido"){
            dataset <- dataset %>% filter(Envolvido %in% input$selectionG1)
            return(dataset)
        }

    })

    #Selecao do Ano
    output$graph1UI3 = renderUI({


        if(input$enable == 0)
            return(NULL)

        if(input$selectgrafico1 == "Anual" | input$selectgrafico1 == "Mensal" ){

            dataset <- tabelaG11()

            op <- selectInput(inputId = "selectYearG1",
                              label = ("Selecione o Ano"),
                              choices = sort(c(unique(year(dataset$Data)))))
            return(op)

        } else {
            return(NULL)
        }
    })

    #Selecao por Mês
    output$graph1UI4 = renderUI({


        if(input$selectgrafico1 != "Mensal")
            return(NULL)

        if(input$enable == 0)
            return(NULL)

        if(input$selectgrafico1 == "Mensal"){

            dataset <- tabelaG11()
            dataset <- dataset %>% filter(year(Data) == input$selectYearG1)
            ui <- selectInput(inputId = "selectMonthG1",
                              label = ("Selecione o Mês"),
                              choices = sort(c(unique(month(dataset$Data)))))
            return(ui)

        }
    })

    #Selecao do Primeiro Período Específico
    output$graph1UI5 = renderUI({

        if(input$selectgrafico1 == "Período Específico"){

            dataset <- tabelaG11()

            ui <- dateInput(inputId = "firstdateGrafico",
                            value = ymd(min(dataset$Data)),
                            min = ymd(min(dataset$Data)),
                            max = ymd(max(dataset$Data)),
                            label = "Data Inicial")

            return(ui)
        }

        if(input$selectgrafico1 != "Período Específico"){
            return(NULL)
        }
    })

    #Selecao do Segundo Período Específico
    output$graph1UI6 = renderUI({

        if(input$selectgrafico1 == "Período Específico"){

            dataset <- tabelaG11()
            selecionado <- dateInput(inputId = "lastdateGrafico",
                                     value = ymd(max(dataset$Data)),
                                     min = ymd(min(dataset$Data)),
                                     max = ymd(max(dataset$Data)),
                                     label = "Data Final")
            return(selecionado)


        }

        if(input$selectgrafico1 != "Período Específico"){
            return(NULL)
        }
    })

    #Comparativo?
    output$graph1UI7 = renderUI({

        if(input$selectTemporaltypeG1 == "Total")
            return(NULL)

        if(input$selectTemporaltypeG1 != "Total"){
            if(input$tipoGrafico1 == "Barras"){

                ui <- selectInput(inputId = "comparativo",
                                  choices = c("Não", "Sim"),
                                  label = "Deseja fazer comparativos?")
                return(ui)


            }else{
                return(NULL)
            }

        }

    })

    #Lista ou Nome?
    output$graph1UI8 = renderUI({

        if(input$enable == 0)
            return(NULL)
        if(input$tipoGrafico1 == "Pizza")
            return(NULL)

        if(input$selectTemporaltypeG1 == "Por Cliente"){
            if(input$comparativo == "Sim"){

                selecionado <- selectInput(inputId = "listOrName2",
                                           label = ("Seleção por lista ou nome?"),
                                           choices = c("Lista", "Nome"))
                return(selecionado)

            } else{return(NULL)}
        }else{return(NULL)}

    })

    #Tabela filtrada por período
    tabelaG1B1 = reactive({
        if(input$enable == 0)
            return(NULL)

        if(input$tipoGrafico1 == "Pizza")
            return(NULL)

        dataset <- datasetTotal()

        dataset <- as_tibble(dataset)
        dataset <- dataset %>% mutate(Data = as_date(Data))
        dataset <- dataset %>% mutate(Duração = chron(times = dataset$Duração))

        #Total
        if(input$selectgrafico1 == "Total"){
            return(dataset)
        }

        # Anual
        if(input$selectgrafico1 == "Anual") {

            dataset <- dataset %>% filter(year(Data) == input$selectYearG1 )
            return(dataset)

        }

        #Mensal
        if(input$selectgrafico1 == "Mensal"){

            dataset <- dataset %>% filter(year(Data) == input$selectYearG1 )
            dataset <- dataset %>% filter(month(Data) == input$selectMonthG1 )
            return(dataset)

        }

        # Período Específico
        if(input$selectgrafico1 == "Período Específico") {

            dataset <- dataset %>% filter(Data >= input$firstdateGrafico)
            dataset <- dataset %>% filter(Data <= input$lastdateGrafico)
            return(dataset)

        }
    })

    #Selecionar
    output$graph1UI9 = renderUI({

        if(input$enable == 0)
            return(NULL)


        if(input$tipoGrafico1 == "Pizza")
            return(NULL)

        dataset <- tabelaG1B1()

        if(input$selectTemporaltypeG1 == "Por Cliente"){



            if(input$comparativo == "Sim"){
                if(input$listOrName2 == "Nome"){
                    ui <- textInput(inputId = "selectionG1B",
                                    label = ("Selecione o Cliente"))
                    return(ui)
                }
                if(input$listOrName2 == "Lista"){
                    ui <- selectInput(inputId = "selectionG1B",
                                      label = ("Selecione o Cliente"),
                                      choices = sort(c(unique(dataset$Cliente))),
                                      multiple = TRUE,
                                      selected = sort(c(unique(dataset$Cliente)))[2])
                    return(ui)
                }
            }
        } else if(input$selectTemporaltypeG1 == "Por Envolvido"){

            if(input$comparativo == "Sim"){

                ui <-selectInput(inputId = "selectionG1B",
                                 label = ("Selecione o Envolvido"),
                                 choices = sort(c(unique(dataset$Envolvido))),
                                 multiple = TRUE,
                                 selected = sort(c(unique(dataset$Envolvido)))[2])
                return(ui)
            }

        } else{
            return(NULL)
        }
    })

    #Nome do Grupo
    output$graph1UI12 = renderUI({

        if(input$enable == 0)
            return(NULL)

        if(input$tipoGrafico1 == "Pizza")
            return(NULL)

        if(input$selectTemporaltypeG1 == "Por Cliente"){

            if(input$listOrName == "Lista"){

                if(length(input$selectionG1B) > 1){
                    ui <- textInput(inputId = "groupNameB",
                                    label = ("Qual o nome do Grupo?"),
                                    value = "Grupo 2")
                    return(ui)
                } else {return(NULL)}


            }

        } else if(input$selectTemporaltypeG1 == "Por Envolvido"){

            if(length(input$selectionG1B) > 1){
                ui <- textInput(inputId = "groupNameB",
                                label = ("Qual o nome do Grupo?"),
                                value = "Grupo 1")
                return(ui)
            } else {return(NULL)}
        } else {return(NULL)}

    })

    #Tabela filtrada por Cliente ou Enovolvido
    tabelaG1B2 = reactive({

        if(input$enable == 0)
            return(NULL)

        if(input$tipoGrafico1 == "Pizza")
            return(NULL)


        dataset <- tabelaG1B1()

        dataset <- as_tibble(dataset)
        dataset <- dataset %>% mutate(Data = as_date(Data))
        dataset <- dataset %>% mutate(Duração = chron(times = dataset$Duração))

        if(input$tipoGrafico1 == "Barras"){

            if(input$comparativo == "Sim"){

                if(input$selectTemporaltypeG1 == "Por Cliente"){

                    if(input$listOrName2 == "Nome"){
                        dataset <- dataset %>% filter(str_detect(string = Cliente, pattern = as.character(input$selectionG1B)))
                        return(dataset)
                    }

                    if(input$listOrName2 == "Lista"){

                        dataset <- dataset %>% filter(Cliente %in% input$selectionG1B)
                        return(dataset)
                    }
                } else if(input$selectTemporaltypeG1 == "Por Envolvido"){
                    dataset <- dataset %>% filter(Envolvido %in% input$selectionG1B)
                    return(dataset)
                } else {return(NULL)}
            } else {return(NULL)}
        } else {return(NULL)}


    })

    #####

    #Gráfico 2
    #####
    #Selecao por Cliente 1
    output$temporalSel1 = renderUI({

        if(input$enable == 0)
            return(NULL)
        if(input$selectTemporaltype != "Por Cliente")
            return(NULL)

        if(input$selectTemporaltype == "Por Cliente"){
            selecionado <- selectInput(inputId = "selectListOrName",
                                       label = ("Seleção por lista ou nome?"),
                                       choices = c("Lista", "Nome"))
            return(selecionado)
        }
    })

    #Selecao por Cliente 2
    output$temporalSel2 = renderUI({

        if(input$selectTemporaltype != "Por Cliente")
            return(NULL)
        if(input$enable == 0)
            return(NULL)

        if(input$selectListOrName == "Lista"){


            dataset2 <- datasetTotal()
            selecionado <-selectInput(inputId = "selectionG1grafico2",
                                      label = ("Selecione o Cliente"),
                                      choices = sort(c(unique(dataset2$Cliente))),
                                      multiple = TRUE,
                                      selected = sort(c(unique(dataset2$Cliente)))[1])
            return(selecionado)
        }

        if(input$selectListOrName == "Nome"){
            selecionado <- textInput(inputId = "clienteselecionadografico2",
                                     label = "Digite o nome do cliente")
            return(selecionado)
        }




    })

    #Selecao por Envolvido
    output$temporalSel3 = renderUI({

        if(input$enable == 0)
            return(NULL)
        if(input$selectTemporaltype != "Por Envolvido")
            return(NULL)


        if(input$selectTemporaltype == "Por Envolvido"){

            dataset2 <- datasetTotal()
            ui <-selectInput(inputId = "selectEnvolvidoGrafico2",
                             label = ("Selecione o Envolvido"),
                             choices = sort(c(unique(dataset2$Envolvido))),
                             multiple = TRUE,
                             selected = sort(c(unique(dataset2$Envolvido)))[1])
            return(ui)
        }

    })

    #Selecao Anual ou Mensal
    output$temporalSel4 = renderUI({

        if(input$selectTemporalperiod != "Total")
            return(NULL)

        if(input$selectTemporalperiod == "Total"){
            op <- selectInput(inputId = "anualouMensal",
                              label = ("Deseja ver o avanço anual ou mensal?"),
                              choices = c("Anual", "Mensal"))

        }

    })

    #Selecao do Ano
    output$temporalSel5 = renderUI({

        if(input$enable == 0)
            return(NULL)
        if(input$selectTemporalperiod != "Ano Específico")
            return(NULL)

        if(input$selectTemporalperiod == "Ano Específico"){

            dataset2 <- datasetTotal()

            if(input$selectTemporaltype == "Por Cliente"){
                if(input$selectListOrName == "Lista") {
                    dataset2 <- dataset2 %>% filter(Cliente %in% input$selectionG1grafico2)
                }
                if(input$selectListOrName == "Nome") {
                    dataset2 <- dataset2 %>% filter(str_detect(string = Cliente, pattern = as.character(input$clienteselecionadografico2)))
                }
            }
            if(input$selectTemporaltype == "Por Envolvido"){
                dataset2 <- dataset2 %>% filter(Envolvido %in% input$selectEnvolvidoGrafico2)

            }
            op <- selectInput(inputId = "anoEspecifico",
                              label = ("Selecione o Ano"),
                              choices = sort(c(unique(year(dataset2$Data)))))
            return(op)


        }
    })

    #Selecao do Primeiro Período Específico
    output$temporalSelPE1 = renderUI({

        if(input$enable == 0)
            return(NULL)

        if(input$selectTemporalperiod != "Período Específico")
            return(NULL)

        if(input$selectTemporalperiod != "Período Específico")
            return(NULL)

        if(input$selectTemporalperiod == "Período Específico"){

            dataset2 <- datasetTotal()

            if(input$selectTemporaltype == "Total"){
                op <- dateInput(inputId = "data1",
                                value = ymd(min(dataset2$Data)),
                                min = ymd(min(dataset2$Data)),
                                max = ymd(max(dataset2$Data)),
                                label = "Data Inicial")
                return(op)
            }

            if(input$selectListOrName == "Lista") {
                dataset2 <- dataset2 %>% filter(Cliente %in% input$selectionG1grafico2)
            }
            if(input$selectListOrName == "Nome") {
                dataset2 <- dataset2 %>% filter(str_detect(string = Cliente, pattern = as.character(input$clienteselecionadografico2)))
            }
            if(input$selectTemporaltype == "Por Envolvido"){
                dataset2 <- dataset2 %>% filter(Envolvido %in% input$selectEnvolvidoGrafico2)
            }

            op <- dateInput(inputId = "data1",
                            value = ymd(min(dataset2$Data)),
                            min = ymd(min(dataset2$Data)),
                            max = ymd(max(dataset2$Data)),
                            label = "Data Inicial")
            return(op)
        }
    })

    #Selecao do Segundo Período Específico
    output$temporalSelPE2 = renderUI({

        if(input$enable == 0)
            return(NULL)

        if(input$selectTemporalperiod != "Período Específico")
            return(NULL)

        if(input$selectTemporalperiod == "Período Específico"){

            dataset2 <- datasetTotal()

            if(input$selectTemporaltype == "Total"){
                op <- dateInput(inputId = "data2",
                                value = ymd(max(dataset2$Data)),
                                min = ymd(min(dataset2$Data)),
                                max = ymd(max(dataset2$Data)),
                                label = "Data Final")
                return(op)
            }

            if(input$selectListOrName == "Lista") {
                dataset2 <- dataset2 %>% filter(Cliente %in% input$selectionG1grafico2)
            }
            if(input$selectListOrName == "Nome") {
                dataset2 <- dataset2 %>% filter(str_detect(string = Cliente, pattern = as.character(input$clienteselecionadografico2)))
            }
            if(input$selectTemporaltype == "Por Envolvido"){
                dataset2 <- dataset2 %>% filter(Envolvido %in% input$selectEnvolvidoGrafico2)
            }

            op <- dateInput(inputId = "data2",
                            value = ymd(max(dataset2$Data)),
                            min = ymd(min(dataset2$Data)),
                            max = ymd(max(dataset2$Data)),
                            label = "Data Final")
            return(op)
        }
    })

    #Seleção Tipo de Contrato
    output$selTipo = renderUI({
        if(input$enable == 0)
            return(NULL)

        dataset <- tablegraph21()

        vetor <- sort(unique(dataset$'Tipo de Contrato'))
        retorno <- checkboxGroupInput(inputId = "tiposG2",
                                      label = "Faça a seleção dos Tipos de Contrato.",
                                      selected = vetor,
                                      choices = vetor)
        return(retorno)
    })

    #Escala em Y
    output$escalaEmY = renderUI({

        if(input$enable == 0)
            return(NULL)

        dataset <- tablegraph22()


        valor <- as.numeric(10^(str_length(as.integer(max(dataset$Duração) / 10))-1))
        passo <- valor / 4
        maior <- max(dataset$Duração)

        saida <- numericInput(inputId = "escalaY",
                              label = "Escala em Y (duração)",
                              min = passo,
                              max = maior,
                              value = 4*valor,
                              step = passo)
        return(saida)
    })

    #Escala em X
    output$escalaEmX = renderUI({

        if(input$enable == 0)
            return(NULL)

        dataset <- tablegraph22()

        valor <- 1

        if(input$anualouMensal == "Anual"){
            valor <- 12
        } else if(input$anualouMensal != "Anual"){
            valor <- 1
        }
        if(input$selectTemporalperiod != "Total"){
            valor <- 1
        }

        saida <- numericInput(inputId = "escalaX",
                              label = "Escala em X (Meses)",
                              min = 1,
                              max = 12,
                              value = valor,
                              step = 1)
        return(saida)
    })
    #####

    #Tabela Gráfico 1
    tabelaG12 <- reactive({

        if(input$enable == 0)
            return(NULL)


        dataset <- tabelaG11()

        dataset <- as_tibble(dataset)
        dataset <- dataset %>% mutate(Data = as_date(Data))
        dataset <- dataset %>% mutate(Duração = chron(times = dataset$Duração))

        if(input$selectTemporaltypeG1 == "Total" | input$tipoGrafico1 == "Pizza"){
            #Total
            if(input$selectgrafico1 == "Total"){

                dataset <- dataset %>% group_by("Tipo de Contrato" = `Tipo de Contrato`) %>% summarize(Duração = 24*sum(Duração))
                return(dataset)
            }

            # Anual
            if(input$selectgrafico1 == "Anual") {

                dataset <- dataset %>% filter(year(Data) == input$selectYearG1 )
                dataset <- dataset %>% group_by("Tipo de Contrato" =`Tipo de Contrato`) %>% summarize(Duração = 24*sum(Duração))
                return(dataset)

            }

            #Mensal
            if(input$selectgrafico1 == "Mensal"){

                dataset <- dataset %>% filter(year(Data) == input$selectYearG1 )
                dataset <- dataset %>% filter(month(Data) == input$selectMonthG1 )

                dataset <- dataset %>% group_by("Tipo de Contrato" =`Tipo de Contrato`) %>% summarize(Duração = 24*sum(Duração))
                return(dataset)

            }

            # Período Específico
            if(input$selectgrafico1 == "Período Específico") {

                dataset <- dataset %>% filter(Data >= input$firstdateGrafico)
                dataset <- dataset %>% filter(Data <= input$lastdateGrafico)

                dataset <- dataset %>% group_by("Tipo de Contrato" =`Tipo de Contrato`) %>% summarize(Duração = round(24*sum(Duração)),2)
                return(dataset)

            }
        } else if(input$comparativo != "Sim"){
            #Total
            if(input$selectgrafico1 == "Total"){

                dataset <- dataset %>% group_by("Tipo de Contrato" = `Tipo de Contrato`) %>% summarize(Duração = 24*sum(Duração))
                return(dataset)
            }

            # Anual
            if(input$selectgrafico1 == "Anual") {

                dataset <- dataset %>% filter(year(Data) == input$selectYearG1 )
                dataset <- dataset %>% group_by("Tipo de Contrato" =`Tipo de Contrato`) %>% summarize(Duração = 24*sum(Duração))
                return(dataset)

            }

            #Mensal
            if(input$selectgrafico1 == "Mensal"){

                dataset <- dataset %>% filter(year(Data) == input$selectYearG1 )
                dataset <- dataset %>% filter(month(Data) == input$selectMonthG1 )

                dataset <- dataset %>% group_by("Tipo de Contrato" =`Tipo de Contrato`) %>% summarize(Duração = 24*sum(Duração))
                return(dataset)

            }

            # Período Específico
            if(input$selectgrafico1 == "Período Específico") {

                dataset <- dataset %>% filter(Data >= input$firstdateGrafico)
                dataset <- dataset %>% filter(Data <= input$lastdateGrafico)

                dataset <- dataset %>% group_by("Tipo de Contrato" =`Tipo de Contrato`) %>% summarize(Duração = round(24*sum(Duração)),2)
                return(dataset)

            }
        }

        if(input$comparativo == "Sim"){
            dataset2 <- tabelaG1B2()
            dataset2 <- as_tibble(dataset2)
            dataset2 <- dataset2 %>% mutate(Data = as_date(Data))
            dataset2 <- dataset2 %>% mutate(Duração = chron(times = dataset2$Duração))
            nome1 <- input$selectionG1
            nome2 <- input$selectionG1B

            # Anual
            if(input$selectgrafico1 == "Anual") {

                dataset <- dataset %>% filter(year(Data) == input$selectYearG1)

            }

            #Mensal
            if(input$selectgrafico1 == "Mensal"){

                dataset <- dataset %>% filter(year(Data) == input$selectYearG1 )
                dataset <- dataset %>% filter(month(Data) == input$selectMonthG1 )

            }

            # Período Específico
            if(input$selectgrafico1 == "Período Específico") {

                dataset <- dataset %>% filter(Data >= input$firstdateGrafico)
                dataset <- dataset %>% filter(Data <= input$lastdateGrafico)

            }

            dataset <- dataset %>% group_by("Tipo de Contrato" =`Tipo de Contrato`) %>% summarize(Duração = 24*sum(Duração))
            dataset2 <- dataset2 %>% group_by("Tipo de Contrato" =`Tipo de Contrato`) %>% summarize(Duração = 24*sum(Duração))

            if(length(input$selectionG1) > 1){
                nome1 <- input$groupName
            }
            if(length(input$selectionG1B) > 1){
                nome2 <-  input$groupNameB
            }

            dataset <- dataset %>% mutate(Duração = round(as.double(Duração),2))
            dataset2 <- dataset2 %>% mutate(Duração = round(as.double(Duração),2))

            names(dataset)[names(dataset) == "Duração"] <- nome1
            names(dataset2)[names(dataset2) == "Duração"] <- nome2

            datasetFinal <- full_join(dataset, dataset2, by= "Tipo de Contrato")
            datasetFinal <- as_tibble(datasetFinal)
            datasetFinal <- datasetFinal %>% replace(list = is.na(datasetFinal), values = 0)




            return(datasetFinal)

        }
    })

    #Tabela Gráfico 2 - Parte 1
    tablegraph21 <- reactive({
        if(input$enable == 0)
            return(NULL)

        dataset <- datasetTotal()

        if(input$selectTemporaltype == "Por Cliente"){

            if(input$selectListOrName == "Lista"){
                dataset <- dataset %>% filter(Cliente %in% input$selectionG1grafico2)
            }

            if(input$selectListOrName == "Nome"){
                dataset <- as_tibble(dataset %>% filter(str_detect(string = Cliente, pattern = as.character(input$clienteselecionadografico2))))
            }
        }

        if(input$selectTemporaltype == "Por Envolvido"){

            dataset <- dataset %>% filter(Envolvido %in% input$selectEnvolvidoGrafico2)
        }

        dataset <- as_tibble(dataset)
        dataset <- dataset %>% mutate(Data = as_date(Data))
        dataset <- dataset %>% mutate(Duração = chron(times = dataset$Duração))


        if(input$selectTemporalperiod == "Total"){

            if(input$anualouMensal == "Anual"){
                dataset <- dataset %>% group_by(Data = floor_date(Data, "year"), "Tipo de Contrato" =`Tipo de Contrato`) %>% summarize(Duração = 24*sum(Duração))
                return(dataset)

            }

            if(input$anualouMensal == "Mensal"){
                dataset <- dataset %>% group_by(Data = floor_date(Data, "month"), "Tipo de Contrato" =`Tipo de Contrato`) %>% summarize(Duração = 24*sum(Duração))
                return(dataset)
            }

        }

        if(input$selectTemporalperiod == "Ano Específico"){

            dataset <- dataset %>% filter(year(Data) == input$anoEspecifico)
            dataset <- dataset %>% group_by(Data = floor_date(Data, "month"), "Tipo de Contrato" =`Tipo de Contrato`) %>% summarize(Duração = 24*sum(Duração))
            return(dataset)


        } #For do Ano Específico

        if(input$selectTemporalperiod == "Período Específico"){

            dataset <- dataset %>% filter(Data >= input$data1)
            dataset <- dataset %>% filter(Data <= input$data2)
            dataset <- dataset %>% group_by(Data = floor_date(Data, "month"), "Tipo de Contrato" =`Tipo de Contrato`) %>% summarize(Duração = 24*sum(Duração))
            return(dataset)

        } #For do Período Específico

    })

    #Tabela Gráfico 2 - Parte 2
    tablegraph22 <- reactive({

        dataset <- tablegraph21()
        dataset <- dataset %>% filter(`Tipo de Contrato` %in% input$tiposG2)

    })




    titulo1 <- reactive({

        periodo <- "periodo"

        if(input$selectgrafico1 == "Anual"){
            periodo <- input$selectYearG1
        } else if(input$selectgrafico1 == "Mensal"){
            periodo <- paste(input$selectMonthG1, "/", input$selectYearG1, sep = "")
        } else if (input$selectgrafico1 == "Período Específico"){
            periodo <- paste(input$firstdateGrafico, "-", input$lastdateGrafico, sep = "")
        } else if(input$selectgrafico1 == "Total"){
            periodo <- "Total"
        }

        titulo <- paste("Duração por Tipo de Contrato para Período: ", periodo, sep = "")


    })

    titulo2 <- reactive({

        if(input$selectTemporalperiod == "Total"){
            titulo <- "Evolução Temporal - Período total"
            return(titulo)
        } else if(input$selectTemporalperiod == "Ano Específico"){
            titulo <- paste("Evolução Temporal - Período: ", input$anoEspecifico, sep = "")
            return(titulo)
        } else if (input$selectTemporalperiod == "Período Específico"){
            titulo <- paste("Evolução Temporal - Período: desde ", input$data1, " até ", input$data2, sep = "")
            return(titulo)
        }

    })


    #Outputs
    #####

    #Tabela total

    output$dataset <- DT::renderDataTable({

        input$enable

        isolate({

            if(input$enable == 0)
                return(NULL)

            dataset <- datasetTotal()
            dataset <- dataset %>% mutate(Duração = as.character(dataset$Duração))


        }) #Isolate
    }) #Fim

    #Tabela Períodos
    output$datasetPeriods <- DT::renderDataTable({

        if(input$enable == 0)
            return(NULL)

        dataset <- datasetTabelas2()
        dataset <- dataset %>% mutate(Duração = as.character(dataset$Duração))

    }) #Fim


    #Banco de Horas
    output$datasethour <- DT::renderDataTable({

        if(input$enable == 0)
            return(NULL)

        dataset <- datasetBanco()

        if(length(dataset) == 0)
            return(NULL)

        if(length(dataset) != 0)
            return(dataset)
    }) #Fim


    output$grafico1 = renderPlotly({

        if(input$enable == 0)
            return(NULL)

        dataset <- tabelaG12()
        titulo <- titulo1()

        if(input$tipoGrafico1 == "Barras"){

            dataset <- dataset %>% arrange(`Tipo de Contrato`)


            p <- plot_ly(dataset, x = ~`Tipo de Contrato`, y = ~Duração, type = 'bar',
                         marker = list(color = 'rgb(158,202,225)',
                                       line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
                layout(title = titulo,
                       xaxis = list(title = "Tipo de Contrato"),
                       yaxis = list(title = "Duração"))


            if(input$selectTemporaltypeG1 == "Total"){
                return(p)
            }else{
                if(input$comparativo == "Sim"){

                    nomes <- names(dataset)
                    names(dataset) <- c("Tipo de Contrato", "Nome1", "Nome2")

                    p <- dataset %>%
                        plot_ly() %>%
                        add_trace(x = ~`Tipo de Contrato`, y = ~Nome1, type = 'bar', name = nomes[2],
                                  marker = list(color = 'rgb(158,202,225)',
                                                line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
                        add_trace(x = ~`Tipo de Contrato`, y = ~Nome2, type = 'bar', name = nomes[3],
                                  marker = list(color = 'rgb(58,200,225)',
                                                line = list(color = 'rgb(8,48,107)', width = 1.5))) %>%
                        layout(barmode = 'group',
                               title = titulo,
                               xaxis = list(title = "Tipo de Contrato"),
                               yaxis = list(title = "Duração"))
                    return(p)
                } else {return(p)}
            }
        }

        if(input$tipoGrafico1 == "Pizza"){

            dataset <- dataset %>% arrange(desc(Duração))

            p <- plot_ly(dataset, labels = ~`Tipo de Contrato`, values = ~Duração, type = 'pie') %>%
                layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                       yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                       title = titulo)

            return(p)
        }

    })

    output$tabelaGrafico1 = renderDataTable({

        if(input$enable == 0)
            return(NULL)

        tabelaG12()


    })

    output$tabelaGrafico2 = renderDataTable({

        if(input$enable == 0)
            return(NULL)

        tablegraph22()

    })

    output$grafico2 = renderPlotly({

        if(input$enable == 0)
            return(NULL)

        dataset2 <- tablegraph22()
        titulo <- titulo2()

        escalax <- paste(input$escalaX, "months", sep = " ")
        form <- "%b/%Y"

        if(input$escalaX == 12){
            escalax <- "1 year"
            form <- "%Y"
        }

        p <- ggplot(dataset2, aes(x = Data, y = Duração, color = `Tipo de Contrato`))+
            geom_line()+
            scale_x_date(date_breaks = escalax,
                         labels = date_format(form))+
            scale_y_continuous(breaks = seq(0, (max(dataset2$Duração) + input$escalaY), input$escalaY),
                               limits=c(0, (max(dataset2$Duração) + input$escalaY))) +
            geom_point(size = 1)+
            labs(title = titulo)
        p <- ggplotly(p)
        return(p)
    })

    #Base Total
    output$datasetTotalDownload <- downloadHandler(

        filename = function() {"TimeSheetFinal.xlsx"},
        content = function(file) {write.xlsx(datasetDownload(), file = file, asTable = TRUE)}
    )

    #Tabelas
    output$downloadTabelasPeriodos <- downloadHandler(

        filename = function() {"TimeSheetPeriodo.xlsx"},
        content = function(file) {write.xlsx(datasetTabelas2(), file = file, asTable = TRUE)}
    )


    #Base Banco
    output$downloadTabelasBanco <- downloadHandler(

        filename = function() {"TimeSheetBancoDeHoras.xlsx"},
        content = function(file) {write.xlsx(datasetBanco(), file = file, asTable = TRUE)}
    )
    ######

}) #Encerra o Server Shiny
