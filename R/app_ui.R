#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bslib bs_theme
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    shiny::fluidPage(
    # Your application UI logic
    shiny::navbarPage(title = 'IVA',
                      id = 'navbar',collapsible = TRUE,
                      theme = bs_theme(
                        version = 5,
                        bootswatch = 'cerulean',
                        base_font = font_google("Oxanium"
                                                # , local = TRUE
                                                )

                      ) %>%
                        bs_add_rules("#my-nav { @extend .justify-content-center }"),

    shiny::tabPanel(title = 'Introducción',value = 'inicio',
                    mod_introduccion_ui("introduccion_1")
                    ),
    # shiny::tabPanel(title = 'Descriptivo',value = 'grupo_ref',
    #                 mod_descriptivo_ui("descriptivo_1")
    #                 ),
    # shiny::tabPanel(title = 'Modelo',value = 'modelo',
    #                 shiny::selectizeInput(inputId = 'modelos_universidad_prog',
    #                                       label = 'Seleccione nivel de detalle:',
    #                                       choices = c("Universidad",'Programa'),
    #                                       multiple = F,
    #                                       selected = "Universidad",
    #                                       options = list(maxOptions = 3)),
    #
    #                 shiny::uiOutput("salida_modelo")
    # ),
    shiny::tabPanel(title = "Ranking Universidades",value = 'modelo',
                    mod_modelo_v2_ui("modelo_v2")),
    shiny::tabPanel(title = 'Comparador',value = 'compara',
                    h1("Comparador de Resultados",class = "center-text"),
                    tags$p("Bienvenidos a la sección Comparador. Aquí, podrás explorar y comparar la tendencia individual de cada estudiante a través de gráficos interactivos. ¿Te preguntas cómo se dispersaron los resultados de los estudiantes en las pruebas Saber 11 y Saber Pro? ¡Este es el lugar donde encontrarás las respuestas!" ,class = "justify-text"),
                    tags$p("Con solo unos pocos clics, podrás seleccionar los filtros que desees y visualizar gráficos dinámicos que te mostrarán cómo se distribuyeron los resultados de los estudiantes en ambas pruebas." ,class = "justify-text"),
                    tags$p("Ya sea que estés interesado en ver la tendencia de un grupo específico o comparar resultados entre diferentes universidades, programas incluso modalidades, esta herramienta te brinda la flexibilidad para explorar los datos de manera fácil y comprensible.",class = "justify-text"),
                    fluidRow(
                      column(12,
                             fluidRow(
                               column(6,mod_comparador_ui("comparador_1")),
                               column(6,mod_comparador_ui("comparador_2"))))
                      )
                    )
  )
  )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Valor Agregado"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
