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

mod_comparador_ui <- function(id){
  ns <- NS(id)
  tagList(

    h3("Comparador de Resultados"),

    fluidRow(
      column(12,
             fluidRow(
               column(6,uiOutput(ns('myCard'))
               ),
               column(width = 6,uiOutput(ns('myCard4'))
                      )
             )
      )
    )

  )
}

#' comparador Server Functions
#'
#' @noRd
mod_comparador_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

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
                                  choices = c(1,2,3,4)),

               ),
        column(4,
               shiny::selectInput(inputId = ns('prueba'),
                                  label = 'Prueba:',
                                  choices = c('A','B','C','D'))
               ),
        column(4,
               shiny::selectInput(inputId = ns('periodo'),
                                         label = 'Periodo:',
                                         choices = c(1,2,3,4))
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
           )
      )

      layout_column_wrap(
        width = 1,fill = TRUE,
        heights_equal = "row",
        caja1
      )

    })

    output$output_universidad <-renderUI({
      req(input$incluir_universidad)
      selectInput(inputId = ns("filtro_universidad"),
                  label = "Universidad:",
                  choices = c('a', 'b', 'c'),multiple = T,size = 3,selectize=F)
    })

    output$output_programa <-renderUI({
      req(input$incluir_programa)
      selectInput(inputId = ns("filtro_programa"),
                  label = "Programa:",
                  choices = c('a', 'b', 'c'),multiple = T,selectize = F,size = 3)
    })


    output$myCard2 <- renderUI({

    })


    output$myCard3 <- renderUI({

      card(title = "Grafico", body = 'text texto texto'

      )

    })


    output$myCard4 <- renderUI({

      card(
        card_header('Configuración',class = "bg-dark"),
        card_body_fill(

          checkboxInput(ns("test_check"), "Do you want to this?", value = FALSE),
          uiOutput(ns('id2'))

        )

      )


    })

    output$id2 <-
      renderUI({
        req(input$test_check)
        selectInput(inputId = "id2",
                    label = "something2",
                    choices = c('a', 'b', 'c'))
      })


  })
}

