#' modelo_programas UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_modelo_programas_ui <- function(id){
  ns <- NS(id)
  tagList(
    golem_add_external_resources(),
    tags$style(HTML(css_variable)),
    tags$h1("Ranking de Programas", class = "center-text"),
    tags$p("Descubre cómo se clasifican los programas de educación superior en Colombia mediante el uso de dos metodologías (PMS (Propensity Score Matching) y un modelo lineal jerárquico), que analizan los resultados de las pruebas ICFES Saber 11 y Saber Pro. Estas herramientas permiten evaluar y cuantificar el valor agregado de cada institución educativa, considerando tanto los resultados académicos como las características demográficas de los estudiantes.", class = "center-text"),
    tabsetPanel(
      id = "my-nav",
      type = "pills",
      pill(
        "Modelo PSM",
        uiOutput(ns("output_table_psm"))
      ),
      pill("Modelo Jerarquico",
           uiOutput(ns("output_table_jerarquico"))
      ))
  )
}

#' modelo_programas Server Functions
#'
#' @noRd
mod_modelo_programas_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}
