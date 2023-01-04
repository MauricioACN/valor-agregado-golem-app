#' comparador UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import htmltools

mod_comparador_ui <- function(id){
  ns <- NS(id)
  tagList(

    h3("Comparador de Resultados"),

  )
}

#' comparador Server Functions
#'
#' @noRd
mod_comparador_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns



  })
}

