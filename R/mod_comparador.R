#' comparador UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import bslib
#' @import htmltools
#' @import shinyWidgets
#' @importFrom shinyalert shinyalert

mod_comparador_ui <- function(id){
  ns <- NS(id)
  tagList(

     fluidRow(
       column(12,uiOutput(ns('myCard')),
              uiOutput(ns('grafico1'))
       ))
  )
}

#' comparador Server Functions
#'
#' @noRd
mod_comparador_server <- function(id,datos,saberPro,saber11){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    datos_clean_1 <- reactive({

      clean_resultados(datos, grupo = input$grupo_referencia)

    })

    mediaPro <- reactive({
      calculate_mean_pro(saberPro, grupo = input$grupo_referencia)
    })

    media11 <- reactive({

      calculate_mean_11(saber11,periodo = input$periodo)

    })

    output$myCard <- renderUI({

      card(
           card_header(
             class = "bg-dark",
             "Configuración"
           ),
           card_body(border_radius = 'all',


                     fluidRow(
                       column(4,
                              shiny::selectInput(inputId = ns('grupo_referencia'),
                                  label = HTML('Grupo de Referencia:',
                                               as.character(actionLink(ns('info_gr'),
                                                                       label = 'Ayuda'
                                                                       )
                                                            )
                                               ),
                                  choices = resultados %>% distinct(!!sym("GRUPOREFERENCIA")) %>% pull(),
                                  size = 3,selectize=F,multiple = F,selected = resultados$GRUPOREFERENCIA[1])

                              ),
                       column(4,
                              shiny::selectInput(inputId = ns('prueba'),
                                  label = HTML('Prueba:',
                                                        as.character(actionLink(ns('info_prueba'),
                                                                                label = 'Ayuda'
                                                        )
                                                        )
                                  ),
                                  choices = c('Puntaje Global','Razonamiento Cuantitativo','Inglés','Lectura Crítica'),
                                  size = 3,selectize=F)),
                       column(4,
                              shiny::selectInput(inputId = ns('periodo'),
                                         label = HTML('Periodo Prueba Saber 11:',
                                                                                as.character(actionLink(ns('info_periodo'),
                                                                                                        label = 'Ayuda'
                                                                                )
                                                                                )
                                         ),
                                         choices = mediasSaber11 %>% distinct(!!sym("periodoAux")) %>% pull(),
                                  selected = mediasSaber11$periodoAux,
                                  size = 3,selectize=F,multiple = T)
                              )
               ),
               fluidRow(
                 column(6,style = "text-align: center;",
                        shiny::selectInput(inputId = ns('incluir_universidad_programa'),
                                           label = HTML('Filtrar Por:',
                                                                      as.character(actionLink(ns('info_filtro'),
                                                                                              label = 'Ayuda'
                                                                      )
                                                                      )
                                           ),
                                           choices = c('Universidad','Programa','Sin Filtro'),
                                           selected = 'Sin Filtro',selectize = F,size = 3)),
                 column(6,style = "text-align: center;",shiny::uiOutput(ns('output_universidad_programa'))
               ),
      )
           ),
      card_footer("Para seleccionar más de un elemento, mantener presionado Ctrl.")
      )

    })

    observeEvent(input$info_gr, {
      # Show a modal when the button is pressed
      shinyalert("Grupo de Referencia", "Información sobre el grupo de referencia.", type = "info")
    })

    observeEvent(input$info_periodo, {
      # Show a modal when the button is pressed
      shinyalert("Periodo Prueba Saber 11", "Información sobre el Periodo Prueba Saber 11", type = "info")
    })

    observeEvent(input$info_filtro, {
      # Show a modal when the button is pressed
      shinyalert("Filtrar Por", "Información sobre el filtro", type = "info")
    })

    observeEvent(input$info_prueba, {
      # Show a modal when the button is pressed
      shinyalert("Prueba", "Información sobre la Prueba", type = "info")
    })


    output$output_universidad_programa <- renderUI({

      if(input$incluir_universidad_programa=='Sin Filtro'){

        return()

      }
      else if(input$incluir_universidad_programa=="Universidad"){

        selectizeInput(inputId = ns("filtro_universidad"),
                    label = "Universidad:",
                    choices = datos_clean_1() %>% distinct(!!sym("INST_NOMBRE_INSTITUCION")) %>% pull(),
                    selected = datos_clean_1()$INST_NOMBRE_INSTITUCION[1],
                    multiple = T,
                    options = list(maxOptions = 3)
                    # selectize=F
                    )

      }

      else if(input$incluir_universidad_programa=="Programa"){

        selectizeInput(inputId = ns("filtro_programa"),
                    label = "Programa:",
                    choices = datos_clean_1() %>% distinct(!!sym("ESTU_PRGM_ACADEMICO")) %>% pull(),
                    selected = datos_clean_1()$ESTU_PRGM_ACADEMICO[1],
                    multiple = T,
                    # selectize = F,
                    options = list(maxOptions =  3)
                    )

      }

    })


    df_clean_final <- reactive({

      if(input$incluir_universidad_programa=="Universidad"){
        req(input$filtro_universidad)
        datos_clean_1() %>% dplyr::filter(!!sym("INST_NOMBRE_INSTITUCION") %in% input$filtro_universidad)

      }
      else if(input$incluir_universidad_programa=="Programa"){

        req(input$filtro_programa)

        datos_clean_1() %>% dplyr::filter(!!sym("ESTU_PRGM_ACADEMICO") %in% input$filtro_programa)

      }
      else{
        datos_clean_1()
      }

    })

    output$grafico1 <- renderUI({

      card(full_screen = TRUE,
           card_header(
             class = "bg-dark",
             paste("Relación puntajes de las pruebas Saber Pro y Saber 11")
           ),

             plotOutput(ns('grafico_general')),

           card_footer(

             shiny::textOutput(ns('text_footer'))

           )
      )

    })

    output$text_footer <- shiny::renderText({

      if (input$incluir_universidad_programa == 'Universidad'){

        universidades = paste(input$filtro_universidad,collapse = ", ")

        texto2 <- paste(ifelse(length(input$filtro_universidad)>1," // Universidades: "," // Universidad: "), universidades)
      }
      else if (input$incluir_universidad_programa == 'Programa'){

        programas = paste(input$filtro_programa,collapse = ", ")

        texto2 <- paste(ifelse(length(input$filtro_programa)>1," // Programas: "," // Programa: "), programas)
      }
      else{
        texto2 <- ""
      }

      texto1 <- paste("Grupo de referencia: ", input$grupo_referencia, " - ", input$prueba)

      paste(texto1,texto2)

    })

    output$grafico_general <- renderPlot({

      if (input$prueba=='Puntaje Global'){
        sal = 'PUNT_GLOBAL.x'
      }
      if (input$prueba=='Razonamiento Cuantitativo'){
        sal = 'MOD_RAZONA_CUANTITAT_PUNT'
      }
      if (input$prueba=='Inglés'){
        sal = 'MOD_INGLES_PUNT'
      }
      if (input$prueba=='Lectura Crítica'){
        sal = 'MOD_LECTURA_CRITICA_PUNT'
      }

        create_graph_general_var(df_clean_final(), media11(), mediaPro(), input$grupo_referencia, prueba = sal)

    })

  })
}

