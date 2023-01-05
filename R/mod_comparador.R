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

    # datos_clean <- reactive({
    #   clean_resultados(datos, grupo = input$grupo_referencia)
    # })

    datos_clean_1 <- reactive({

      sal_1 = clean_resultados(datos, grupo = input$grupo_referencia)

      if(input$incluir_universidad==T & input$incluir_programa==T){

        sal_1 = sal_1 %>% dplyr::filter(!!sym("ESTU_PRGM_ACADEMICO")==input$filtro_programa,
                                        !!sym("INST_NOMBRE_INSTITUCION")==input$filtro_universidad)

      }
      if(input$incluir_universidad==T){

          sal_1 = sal_1 %>% dplyr::filter(!!sym("INST_NOMBRE_INSTITUCION")==input$filtro_universidad)
      }
      if(input$incluir_programa==T){
        sal_1 = sal_1 %>% dplyr::filter(!!sym("ESTU_PRGM_ACADEMICO")==input$filtro_programa)
      }
      sal_1
    })

    mediaPro <- reactive({
      calculate_mean_pro(saberPro, grupo = input$grupo_referencia)
    })

    media11 <- reactive({

      calculate_mean_11(saber11,periodo = input$periodo)

    })

    output$myCard <- renderUI({

      caja1 <- card(
           card_header(
             class = "bg-dark",
             "Configuración"
           ),
           card_body(border_radius = 'all',
                          fluidRow(
                            column(12,
                                   fluidRow(
                                     column(6,style = "text-align: center;",
                                            shiny::checkboxInput(inputId = ns('incluir_universidad'),
                                                                   label = "Inluir Universidad",
                                                                   value = F)
                                            ),
                                     column(6,style = "text-align: center;",
                                            shiny::checkboxInput(inputId = ns('incluir_programa'),
                                                                   label = "Incluir Programa",
                                                                   value = F)
                                     )
                                   )
                                   )
                          ),
      fluidRow(
        column(4,
               shiny::selectInput(inputId = ns('grupo_referencia'),
                                  label = 'Grupo de Referencia:',
                                  choices = resultados %>% distinct(!!sym("GRUPOREFERENCIA")) %>% pull(),
                                  size = 3,selectize=F,multiple = T,selected = resultados$GRUPOREFERENCIA[1]),
               ),
        column(4,
               shiny::selectInput(inputId = ns('prueba'),
                                  label = 'Prueba:',
                                  choices = c('Puntaje Global','Razonamiento Cuantitativo','Inglés','Lectura Crítica'),
                                  size = 3,selectize=F)
               ),
        column(4,
               shiny::selectInput(inputId = ns('periodo'),
                                         label = 'Periodo:',
                                         choices = mediasSaber11 %>% distinct(!!sym("periodoAux")) %>% pull(),
                                  selected = mediasSaber11$periodoAux,
                                  size = 3,selectize=F,multiple = T)
               )
               ),
      fluidRow(
        column(12,
               fluidRow(
                 column(6,style = "text-align: center;",
                        shiny::uiOutput(ns('output_universidad'))),
                 column(6,style = "text-align: center;",
                        shiny::uiOutput(ns('output_programa')))
               ))
      )
           ),
      card_footer("Para seleccionar más de un elemento, mantener presionado Ctrl.")
      )

      # layout_column_wrap(
      #   width = 1,fill = TRUE,
      #   heights_equal = "row",
      #   caja1
      # )

      caja1
    })

    # toListen <- reactive({
    #   list(!is.null(input$incluir_universidad),!is.null(input$incluir_programa))
    # })
    #
    # observeEvent(toListen(), {
    #
    #   choices_uni = datos_clean_1() %>% distinct(!!sym("INST_NOMBRE_INSTITUCION")) %>% pull()
    #   choices_pro = datos_clean_1() %>% distinct(!!sym("ESTU_PRGM_ACADEMICO")) %>% pull()
    #
    #   updateSelectInput(inputId = "filtro_universidad", choices = choices_uni)
    #   updateSelectInput(inputId = "filtro_programa", choices = choices_pro)
    #
    # })

    # observe({
    #   req(input$incluir_universidad)
    #
    #   choices_pro = datos_clean_1() %>% distinct(!!sym("ESTU_PRGM_ACADEMICO")) %>% pull()
    #   updateSelectInput(inputId = "filtro_programa", choices = choices_pro)
    # })

    output$output_universidad <-renderUI({
      req(input$incluir_universidad,datos_clean_1())

      choices = datos_clean_1() %>% distinct(!!sym("INST_NOMBRE_INSTITUCION")) %>% pull()

        selectInput(inputId = ns("filtro_universidad"),
                    label = "Universidad:",
                    choices = choices,
                    multiple = T,
                    size = 3,
                    selectize=F,
                    selected = datos_clean_1()$INST_NOMBRE_INSTITUCION[1])
          })


    output$output_programa <-renderUI({
      req(input$incluir_programa,datos_clean_1())

      choices = datos_clean_1() %>% distinct(!!sym("ESTU_PRGM_ACADEMICO")) %>% pull()

        selectInput(inputId = ns("filtro_programa"),
                    label = "Programa:",
                    choices = choices,
                    multiple = T,
                    selectize = F,
                    size =  3,
                    selected = datos_clean_1()$ESTU_PRGM_ACADEMICO[1])
    })

    output$grafico1 <- renderUI({

      card(full_screen = TRUE,
           card_header(
             class = "bg-dark",
             paste("Relación puntajes de las pruebas Saber Pro y Saber 11")
           ),
           plotOutput(ns('grafico_general')),
           card_footer(
             paste("Grupo de referencia ", input$grupo_referencia, " - ", input$prueba)
           )
      )

    })


  output$datos_clean_final <- reactive({



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

      create_graph_general_var(datos_clean_1(), media11(), mediaPro(), input$grupo_referencia, prueba = sal)

    })

    output$myCard3 <- renderUI({

      card(title = "Grafico", body = 'text texto texto'

      )

    })


    output$myCard4 <- renderUI({

      # card(
      #   card_header('Configuración',class = "bg-dark"),
      #   card_body_fill(

          shinyWidgets::selectizeGroupUI(
            id = "my-filters",
            params = list(
              GRUPOREFERENCIA = list(inputId = "GRUPOREFERENCIA", title = "Grupo de Referencia:"),
              INST_NOMBRE_INSTITUCION = list(inputId = "INST_NOMBRE_INSTITUCION", title = "Universidad:"),
              ESTU_PRGM_ACADEMICO = list(inputId = "ESTU_PRGM_ACADEMICO", title = "Programa:")
            ),inline = FALSE)
        # )

      # )


    })

    res_mod <- callModule(
      module = selectizeGroupServer,
      id = "my-filters",
      data = datos,
      vars = names(datos)
    )

  })
}

