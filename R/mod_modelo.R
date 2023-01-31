#' modelo UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_modelo_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidRow(

      column(4,

             fluidRow(

               column(12, uiOutput(ns("filtros")))

             ),
             fluidRow(
               column(12,uiOutput(ns("texto_ayuda")))
             )

             ),

      column(8, uiOutput(ns("graficos")))

    )


    )

}

#' modelo Server Functions
#'
#' @noRd
mod_modelo_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$filtros <- renderUI({

      list(

        card(

          card_header(
            class = "bg-dark",
            paste("Filtros")),

          shiny::selectizeInput(inputId = ns('universidad'),
                                label = 'Universidad:',
                                choices = resultados_modelos %>% distinct(!!sym("INST_NOMBRE_INSTITUCION")) %>% pull(),
                                multiple = F,
                                selected = resultados_modelos$INST_NOMBRE_INSTITUCION[1],
                                options = list(maxOptions = 3)),

          shiny::selectInput(inputId = ns('variable_modelo'),
                                label = 'Variable en Modelo:',
                                choices = colnames(resultados_modelos)[1:7],
                                multiple = F,
                                selected = colnames(resultados_modelos)[1]),

          card_footer(
            "Texto"
          )

        )


      )

    })


    output$texto_ayuda <- renderUI({

      list(

        card(

          card_header(
            class = "bg-dark",
            paste("Texto")),

          card_footer(
            "Texto"
          )


        )


      )

    })


    output$graficos <- renderUI({

      list(

        card(

          card_header(
            class = "bg-dark",
            paste("Modelado")),

          card_footer(
            "Texto"
          )


        )


      )

    })


  })
}


