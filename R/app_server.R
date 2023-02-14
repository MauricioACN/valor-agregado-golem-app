#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom dplyr mutate
#' @importFrom stringr str_to_title
#' @noRd
app_server <- function(input, output, session) {

  #introduccion
  mod_introduccion_server("introduccion_1")

  # Comparadores
  mod_comparador_server("comparador_1",datos = resultados, saberPro = mediasSaberPro, saber11 = mediasSaber11)
  mod_comparador_server("comparador_2",datos = resultados, saberPro = mediasSaberPro, saber11 = mediasSaber11)

  #estadistica descriptiva
  mod_descriptivo_server("descriptivo_1", datos = resultados)

  #modelo
  mod_modelo_server("modelo_1", datos= resultados_modelos)

}
