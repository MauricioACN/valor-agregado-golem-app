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
    shiny::tabPanel(title = "Ranking Programas",value = 'modelo_programas',
                    mod_modelo_programas_ui("modelo_programas_1")),
    shiny::tabPanel(title = 'Comparador',value = 'compara',
                    h1("Comparador de Resultados",class = "center-text"),
                    tags$p("Bienvenidos a la sección Comparador. Aquí, podrás explorar y comparar la tendencia individual de cada estudiante a través de gráficos interactivos. ¿Te preguntas cómo se dispersaron los resultados de los estudiantes en las pruebas Saber 11 y Saber Pro? ¡Este es el lugar donde encontrarás las respuestas!" ,class = "justify-text"),
                    tags$p("Con solo unos pocos clics, podrás seleccionar los filtros que desees y visualizar gráficos dinámicos que te mostrarán cómo se distribuyeron los resultados de los estudiantes en ambas pruebas." ,class = "justify-text"),
                    tags$p("Ya sea que estés interesado en ver la tendencia de un grupo específico o comparar resultados entre diferentes universidades, programas incluso modalidades, esta herramienta te brinda la flexibilidad para explorar los datos de manera fácil y comprensible.",class = "justify-text"),
                    accordion(
                      accordion_panel("¿Cómo funciona?",
                                      icon = bs_icon("info-circle"),
                                      tags$p("Tienes dos secciones idénticas a tu disposición, las cuales generan gráficos basados en los filtros que elijas. Esto te permite analizar diferentes grupos de estudiantes y comparar su desempeño en cada una de las pruebas.",class = "justify-text"),
                                      tags$h4("Paso 1"),
                                      tags$p("Haz clic en 'Nueva Configuración de Gráfico'. Aparecerá una ventana emergente con los filtros disponibles. Si no se muestra ninguna información en los filtros, intenta actualizar la página.",class = "justify-text"),
                                      tags$h4("Paso 2"),
                                      tags$p("Una vez que hayas filtrado los datos según tus preferencias, haz clic en 'OK'. Esto cerrará la ventana emergente, y luego podrás hacer clic en 'Generar Gráfico' para visualizar el gráfico correspondiente.",class = "justify-text"),
                                      ),
                      accordion_panel("¿Como interpretar los gráficos?",
                                      icon = bs_icon("graph-up"),
                                      tags$p("Los gráficos que se generan en esta sección te permiten visualizar la distribución de los resultados de los estudiantes en las pruebas Saber 11 y Saber Pro. Cada gráfico muestra la tendencia de los resultados de los estudiantes en una escala de 0 a 500 puntos.",class = "justify-text"),
                                      tags$div(
                                        "Este gráfico representa la distribución de estudiantes en cuatro cuadrantes, determinados por los promedios nacionales en las pruebas Saber 11 y Saber Pro (Lineas punteadas en color azul oscuro horizontal y vertical respectivamente):",
                                        tags$style(".list-container { margin-top: 10px; }",
                                                   ".list-container li { margin-bottom: 5px; }"),
                                        tags$ul(
                                          class = "list-container",
                                          tags$li("Cuadrante superior derecho: Estudiantes que superaron el promedio en ambas pruebas."),
                                          tags$li("Cuadrante superior izquierdo: Estudiantes que superaron el promedio en Saber Pro pero no en Saber 11."),
                                          tags$li("Cuadrante inferior izquierdo: Estudiantes que no superaron el promedio en ninguna de las pruebas."),
                                          tags$li("Cuadrante inferior derecho: Estudiantes que superaron el promedio en Saber 11 pero no en Saber Pro.")
                                        ))
                                      )
                    ),
                    tags$br(),
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
