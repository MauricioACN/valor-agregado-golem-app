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
                        bootswatch = 'sketchy',
                        base_font = font_google("Oxanium"
                                                # , local = TRUE
                                                )

                      ),

    shiny::tabPanel(title = 'Introducción',value = 'inicio',
                    h1('Aqui va la introduccion'),
                    HTML(introduccion_app)),
    shiny::tabPanel(title = 'Descriptivo',value = 'grupo_ref',
                    mod_descriptivo_ui("descriptivo_1")
                    ),
    shiny::tabPanel(title = 'Modelo',value = 'modelo'),
    shiny::tabPanel(title = 'Comparador',value = 'compara',
                    h3("Comparador de Resultados"),
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
      app_title = "valoragregado"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
