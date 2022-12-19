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
    shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::sliderInput(inputId = ns('muestra'),
                         label = 'Seleccione la muestra que desea visualizar:',
                         min = 1000,
                         max = nrow(resultados),
                         value = nrow(resultados)),
    shiny::selectInput(inputId = ns("programa"),
                       label = "Seleccione el grupo de referencia que desea visualizar:",
                       choices = resultados %>% distinct(!!sym("GRUPOREFERENCIA")) %>% pull()),
    shiny::selectizeInput(inputId = ns("periodo"),
                label = "Seleccione los periodos a visualizar:",
                choices = mediasSaber11 %>% distinct(!!sym("periodoAux")) %>% pull(),
                multiple = T,
                selected = c(1,2)),
    actionLink(ns('help_periodos'),
                 icon = icon('circle-info'),
                 label = ''
                 ),
    br()
    ),
    shiny::mainPanel(
      shiny::HTML(txt_info_m2_p1),
      br(),
      plotOutput(ns('grafico_general')),
      br(),
      shiny::HTML(txt_info_m2_p2))
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
        size = "s",
        footer = NULL
      )
      )
    })

  })
}

