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
                      theme = bs_theme(bootswatch = 'solar'),

    shiny::tabPanel(title = 'Inicio',value = 'inicio'),
    shiny::tabPanel(title = 'Grupo Referencia',value = 'grupo_ref',
                    h1("valoragregado"),
                    mod_name_of_module2_ui("name_of_module2_1")),
    shiny::tabPanel(title = 'Modelo',value = 'modelo'),
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
