#' introduccion UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_introduccion_ui <- function(id){
  ns <- NS(id)
  tagList(

    h1('Presentación'),
    fluidRow(
      column(6,
             HTML(introduccion_app)
      ),
      column(6,
             fluidRow(
               column(12,
                      div(img(src='www/logo-poli.png',height='150px'),style="text-align: center;")
                      )
             ),
             fluidRow(
               column(12,
                      HTML(integrantes)
                      )
             )
      )
    )

  )
}

#' introduccion Server Functions
#'
#' @noRd
mod_introduccion_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


  })
}
