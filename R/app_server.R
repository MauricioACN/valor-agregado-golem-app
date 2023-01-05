#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom dplyr mutate
#' @importFrom stringr str_to_title
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  load("Data/basedatos.rda")
  load("Data/medias.rda")

  mod_name_of_module1_server("name_of_module1_1", datos = resultados)
  mod_name_of_module2_server("name_of_module2_1", datos = resultados, saberPro = mediasSaberPro, saber11 = mediasSaber11)
  mod_comparador_server("comparador_1",datos = resultados, saberPro = mediasSaberPro, saber11 = mediasSaber11)
  mod_comparador_server("comparador_2",datos = resultados, saberPro = mediasSaberPro, saber11 = mediasSaber11)
}
