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

  output$salida_modelo <- renderUI({

    req(input$modelos_universidad_prog)

    if (input$modelos_universidad_prog=="Universidad") {

      mod_est_modelos_ui("est_modelos_1")

    }

    else{

      mod_est_modelos_ui("est_modelos_2")

    }

  })


  mod_est_modelos_server("est_modelos_1", datos = resultados_modelos, resumen_modelo_universidad = ATTdf, detalle = "Universidad",resumen_modelo_universidad_jerq = valor_agregado_jearquico_IES)
  mod_est_modelos_server("est_modelos_2", datos = resultados_modelos_prog, resumen_modelo_universidad = ATTprog, detalle = "Programa", resumen_modelo_universidad_jerq = valor_agregado_jearquico_IES)


}
