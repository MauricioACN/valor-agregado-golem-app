#' modelo_v2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_modelo_v2_ui <- function(id){
  ns <- NS(id)
  tagList(
    golem_add_external_resources(),
    tags$style(HTML(css_variable)),
    tags$h1("Ranking de Universidades", class = "center-text"),
    tags$p("Descubre cómo se clasifican las instituciones de educación superior en Colombia mediante el uso de dos metodologías (PMS (Propensity Score Matching) y un modelo lineal jerárquico), que analizan los resultados de las pruebas ICFES Saber 11 y Saber Pro. Estas herramientas permiten evaluar y cuantificar el valor agregado de cada institución educativa, considerando tanto los resultados académicos como las características demográficas de los estudiantes.", class = "center-text"),
    tabsetPanel(
      id = "my-nav",
      type = "pills",
      pill(
        "Modelo PSM",
        uiOutput(ns("output_table_psm"))
      ),
      pill("Modelo Jerarquico"))
  )
}

#' modelo_v2 Server Functions
#'
#' @noRd
mod_modelo_v2_server <- function(id,datos){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$output_table_psm <- renderUI({

      table = DT::renderDataTable(datos,
                                  style = "bootstrap5",
                                  # options = list(height = "400px")
                                  )

      output_ = tagList(
        tags$h2("Propensity Score Matching", class = "center-text"),
        tags$p("El Propensity Score Matching es una técnica estadística que permite comparar dos grupos de individuos que comparten características similares. En este caso, se compara el desempeño de los estudiantes en la prueba Saber 11 con su desempeño en la prueba Saber Pro.", class = "center-text"),
        bs5_card(table,title = "Ranking Universitario bajo PSM"),
        tags$br(),
        tags$h3("Metodología", class = "center-text"),
        tags$br(),
        HTML(detalle_modelo_2)
      )

      output_

    })

    output$intro <- renderUI({

    })

    output$cartas <- renderUI({})

      # layout_column_wrap(
      #   width = 1/3,
      #   height = 350,
      #   layout_column_wrap(
      #     width = 1,
      #     heights_equal = "row",
      #     card1,card2
      #     ),
      #   card3,
      #   layout_column_wrap(
      #     width = 1,
      #     heights_equal = "row",
      #     card4,card5
      #   ),
      # )

    # card1 = fill_card()
    # card2 = fill_card()
    # card3 = fill_card()
    # card4 = fill_card()
    # card5 = fill_card()

  })
}
