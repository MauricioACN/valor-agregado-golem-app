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
             "Fluid 12",
             fluidRow(
               column(6,uiOutput(ns('myCard'))
               ),
               column(width = 6,
                      "Fluid 6")
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

      fluidRow(
        column(6,
               card(height = 300,

                    card_header(
                      class = "bg-dark",
                      "Configuración"
                    ),
                    card_body_fill(
                      shiny::selectInput(inputId = ns('grupo_referencia'),
                                         label = 'Grupo de Referencia',
                                         choices = c(1,2,3,4)),
                      shiny::checkboxInput(inputId = ns('seleccion'),
                                           label = "seleccionar programa",
                                           value = T)
                    ),collapsible=T
               )
               ),
        column(6,
               card(height = 300,

                    card_header(
                      class = "bg-dark",
                      "Configuración"
                    ),
                    card_body_fill(
                      shiny::selectInput(inputId = ns('grupo_referencia1'),
                                         label = 'Universidad',
                                         choices = c(1,2,3,4)),
                      shiny::checkboxInput(inputId = ns('seleccion1'),
                                           label = "seleccionar programa",
                                           value = T)
                    ),collapsible=T
               )

               )
      )


    })

    output$myCard2 <- renderUI({

    })


    output$myCard3 <- renderUI({

      card(title = "Grafico", body = 'text texto texto'

      )

    })


    output$myCard4 <- renderUI({

      card(title = "Grafico", body = 'text texto texto'

      )

    })


  })
}

