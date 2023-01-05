#' name_of_module2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom dplyr mutate select sample_n pull distinct
#' @importFrom rlang sym

mod_name_of_module2_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12,
             fluidRow(
               column(4,
                      shiny::sliderInput(inputId = ns('muestra'),
                         label = 'Muestra:',
                         min = 1000,
                         max = nrow(resultados),
                         value = nrow(resultados))
                      ),
               column(4,
                      shiny::selectInput(inputId = ns("programa"),
                                         label = "Grupo Referencia:",
                                         choices = resultados %>% distinct(!!sym("GRUPOREFERENCIA")) %>% pull())
                      ),
               column(4,
                      shiny::selectizeInput(inputId = ns("periodo"),
                                            label = "Periodos:",
                                            choices = mediasSaber11 %>% distinct(!!sym("periodoAux")) %>% pull(),
                                            multiple = T,
                                            selected = mediasSaber11$periodoAux)
                      )
               ),
      fluidRow(
        column(12,
               shiny::HTML(txt_info_m2_p1),
               shiny::HTML(txt_info_m2_p2),
               br(),
               plotOutput(ns('grafico_general')),
               )
        )
      )
      )
  )
}

#' name_of_module2 Server Functions
#'
#' @noRd
mod_name_of_module2_server <- function(id,datos, saberPro, saber11){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    datos_clean_sample <- reactive({
      clean_resultados(datos, n_sample = input$muestra, grupo = input$programa)
    })

    mediaPro <- reactive({
      calculate_mean_pro(saberPro, grupo = input$programa)
    })

    media11 <- reactive({

      calculate_mean_11(saber11,periodo = input$periodo)

    })

    output$grafico_general <- renderPlot({

      create_grs(resultados = datos_clean_sample(), mediasSaber11 = media11(), mediasSaberPro = mediaPro(), grupo = input$programa)

    })

    observeEvent(input$help_periodos,{
      showModal(
        modalDialog(
        title = "Periodos",
        HTML(txt_info_periodos),
        easyClose = TRUE,
        size = "xl",
        footer = NULL
      )
      )
    })

  })
}

