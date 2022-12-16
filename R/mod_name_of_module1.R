#' name_of_module1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_name_of_module1_ui <- function(id){
  ns <- NS(id)
  tagList(
    h3("prueba de salida"),
    textOutput(ns('texto'))
  )
}

#' name_of_module1 Server Functions
#'
#' @noRd
mod_name_of_module1_server <- function(id, datos){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$texto <- renderText({

      paste("esto es un texto: ", nrow(datos))

    })

  })
}

