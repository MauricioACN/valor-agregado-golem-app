#' comparador UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import bslib
#' @import htmltools
#' @import shinyWidgets
#' @importFrom shinyalert shinyalert
#' @importFrom dplyr sample_n
#' @importFrom shinyWidgets selectizeGroupUI selectizeGroupServer
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyalert useShinyalert

mod_comparador_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns('myCard')),
    uiOutput(ns('grafico1')),
    uiOutput(ns('description_graph'))
    )

}

#' comparador Server Functions
#'
#' @noRd
mod_comparador_server <- function(id,datos,saberPro,saber11){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$preview, {

      shinyalert(
        inputId = "conf_grafico",
                 html = T,
                 type = "info",
                 showConfirmButton = TRUE,
                 showCancelButton = T,
                 text =
      tagList(

        fluidRow(
          column(
            12,
            p("Haga clic en 'OK' directamente si desea utilizar todos los datos disponibles."),
            hr(),
            selectizeGroupUI(
              id = ns("my-filters"),
              params = list(
                Preg1 = list(inputId = "INST_NOMBRE_INSTITUCION", title = "Universidad:"),
                Preg2 = list(inputId = "ESTU_METODO_PRGM", title = "Modalidad:"),
                Preg3 = list(inputId = "ESTU_PRGM_MUNICIPIO", title = "Sede Oferta del Programa:"),
                Preg4 = list(inputId = "GRUPOREFERENCIA", title = "Grupo de Referencia:"),
                Preg5 = list(inputId = "ESTU_PRGM_ACADEMICO", title = "Programa:")
              ),inline = FALSE, btn_label = "Resetear Filtros")
          )
        )

      )
      )

    })

    df_filter <- callModule(
      module = selectizeGroupServer,
      id = "my-filters",
      data = datos,
      vars = names(datos),
      inline = FALSE
    )

    output$mensajeOutput <- renderText({
      req(df_filter())

      # texto_salida_filtros = "Filtros Aplicados:\n"
      #
      # vars_comparate = list(INST_NOMBRE_INSTITUCION='Universidad: ',
      #                       ESTU_METODO_PRGM="Modalidad: ",
      #   ESTU_PRGM_MUNICIPIO='Sede Oferta del Programa:',
      #   GRUPOREFERENCIA='Grupo de Referencia: ',
      #   ESTU_PRGM_ACADEMICO='Programa: ')
      #
      # for (column in names(vars_comparate)) {
      #   valores = df_filter() %>% select(column)
      #   reales = datos[,column]
      #
      #   if (nrow(valores)==nrow(reales)) {
      #     texto_salida_filtros = paste0(texto_salida_filtros, paste0(vars_comparate[[column]], "Todos", collapse = ""), "\n")
      #   }else if(nrow(valores)>=3) {
      #     texto_salida_filtros = paste0(texto_salida_filtros, paste0(vars_comparate[[column]], "Varios (", as.character(length(valores)),")", collapse = " "), "\n")
      #   }else {
      #     texto_salida_filtros = paste0(texto_salida_filtros, paste0(vars_comparate[[column]], paste(valores, collapse = ", ")), "\n")
      #   }
      # }

      number_registers <- format(nrow(df_filter()), nsmall=1, big.mark=",")
        HTML(
          "Se usarán un total de: ", as.character(number_registers), " observaciones para construir el gráfico."
          )
    })

    datos_clean_1 <- reactive({

      if (input$generate_graph > 0) {
        # Actualizar solo cuando se hace clic en "Actualizar Gráfico"
        if (nrow(df_filter()) >8000) {
          df_filter() %>% sample_n(size = 8000)
        } else {
          df_filter()
        }
      } else {
        # No actualizar automáticamente
        req(df_filter())
      }
    })

    mediaPro <- reactive({
      calculate_mean_pro(saberPro, grupo = unique(datos_clean_1()$GRUPOREFERENCIA))
    })

    media11 <- reactive({

      calculate_mean_11(saber11,periodo = input$periodo)

    })
    output$myCard <- renderUI({

      card(
        height = 150,
           card_header(
             class = "bg-dark",
             "Configuración"
           ),
           card_body(border_radius = 'all',

        fluidRow(
          column(6,actionButton(ns("preview"),"Nueva Configuración del Gráfico",style = "width:100%;")),
          column(6,actionButton(ns("generate_graph"),"Generar Gráfico",style = "width:100%;"))
      )
      ),
      card_footer(textOutput(ns("mensajeOutput")))
      )

      })

    lista_cuadrantes_grap = reactive({

      req(input$prueba)

      if (input$prueba=='Puntaje Global'){
        sal = 'PUNT_GLOBAL.x'
      }
      if (input$prueba=='Razonamiento Cuantitativo'){
        sal = 'MOD_RAZONA_CUANTITAT_PUNT'
      }
      if (input$prueba=='Inglés'){
        sal = 'MOD_INGLES_PUNT'
      }
      if (input$prueba=='Lectura Crítica'){
        sal = 'MOD_LECTURA_CRITICA_PUNT'
      }

      calculate_values_for_text_herp(prueba = sal,
                                     mediasSaberPro = mediaPro(),
                                     mediasSaber11 = media11(),
                                     datos = datos_clean_1())


    })

    output$description_graph <- renderUI({

      card(
        height = 150,
        card_header(
          class = "bg-dark",
          "Interpretación"
        ),
        card_body(border_radius = 'all',
                  paste0("Cuadrante 1: ",round(lista_cuadrantes_grap()[['pc1']]),"\n",
                         "Cuadrante 2: ",round(lista_cuadrantes_grap()[['pc2']]),"\n",
                         "Cuadrante 3: ",round(lista_cuadrantes_grap()[['pc3']]),"\n",
                         "Cuadrante 4: ",round(lista_cuadrantes_grap()[['pc4']]),"\n")
                  )
      )

    })

#     observeEvent(input$info_gr, {
#       # Show a modal when the button is pressed
#       shinyalert("Grupo de Referencia", help_grupo_referencia, type = "info")
#     })
#
    observeEvent(input$info_periodo, {
      # Show a modal when the button is pressed
      shinyalert("Periodo Prueba Saber 11", help_periodos, type = "info")
    })
#
#     observeEvent(input$info_filtro, {
#       # Show a modal when the button is pressed
#       shinyalert("Filtrar Por", "Información sobre el filtro", type = "info")
#     })
#
    observeEvent(input$info_prueba, {
      # Show a modal when the button is pressed
      shinyalert("Prueba", help_prueba, type = "info")
    })
#
#
#     output$output_universidad_programa <- renderUI({
#
#       if(input$incluir_universidad_programa=='Sin Filtro'){
#
#         return()
#
#       }
#       else if(input$incluir_universidad_programa=="Universidad"){
#
#         selectizeInput(inputId = ns("filtro_universidad"),
#                     label = "Universidad:",
#                     choices = datos_clean_1() %>% distinct(!!sym("INST_NOMBRE_INSTITUCION")) %>% pull(),
#                     selected = datos_clean_1()$INST_NOMBRE_INSTITUCION[1],
#                     multiple = T,
#                     options = list(maxOptions = 3)
#                     # selectize=F
#                     )
#
#       }
#
#       else if(input$incluir_universidad_programa=="Programa"){
#
#         selectizeInput(inputId = ns("filtro_programa"),
#                     label = "Programa:",
#                     choices = datos_clean_1() %>% distinct(!!sym("ESTU_PRGM_ACADEMICO")) %>% pull(),
#                     selected = datos_clean_1()$ESTU_PRGM_ACADEMICO[1],
#                     multiple = T,
#                     # selectize = F,
#                     options = list(maxOptions =  3)
#                     )
#
#       }
#
#     })
#
#
    # df_clean_final <- reactive({
    #
    #   if(input$incluir_universidad_programa=="Universidad"){
    #     req(input$filtro_universidad)
    #     datos_clean_1() %>% dplyr::filter(!!sym("INST_NOMBRE_INSTITUCION") %in% input$filtro_universidad)
    #
    #   }
    #   else if(input$incluir_universidad_programa=="Programa"){
    #
    #     req(input$filtro_programa)
    #
    #     datos_clean_1() %>% dplyr::filter(!!sym("ESTU_PRGM_ACADEMICO") %in% input$filtro_programa)
    #
    #   }
    #   else{
    #     datos_clean_1()
    #   }
    #
    # })

      output$grafico1 <- renderUI({

        card(full_screen = TRUE,
             card_header(
               class = "bg-dark",
               paste("Relación puntajes de las pruebas Saber Pro y Saber 11")
             ),
             fluidRow(
               column(6,shiny::selectInput(inputId = ns('prueba'),
                                           label = HTML('Prueba:',
                                                        as.character(actionLink(ns('info_prueba'),
                                                                                label = 'Ayuda'
                                                        )
                                                        )
                                           ),
                                           choices = c('Puntaje Global','Razonamiento Cuantitativo','Inglés','Lectura Crítica'),
                                           size = 3,selectize=F)),
               column(6,shiny::selectInput(inputId = ns('periodo'),
                                          label = HTML('Periodo Prueba Saber 11:',
                                                       as.character(actionLink(ns('info_periodo'),
                                                                               label = 'Ayuda'
                                                       )
                                                       )
                                          ),
                                          choices = mediasSaber11 %>% distinct(!!sym("periodoAux")) %>% pull(),
                                          selected = mediasSaber11$periodoAux,
                                          size = 3,selectize=F,multiple = T))
             ),
             fluidRow(
               column(12,plotOutput(ns('grafico_general')))
                      ),

             card_footer(

               shiny::textOutput(ns('text_footer'))

             )
        )

      })

#
#     output$text_footer <- shiny::renderText({
#
#       if (input$incluir_universidad_programa == 'Universidad'){
#
#         universidades = paste(input$filtro_universidad,collapse = ", ")
#
#         texto2 <- paste(ifelse(length(input$filtro_universidad)>1," // Universidades: "," // Universidad: "), universidades)
#       }
#       else if (input$incluir_universidad_programa == 'Programa'){
#
#         programas = paste(input$filtro_programa,collapse = ", ")
#
#         texto2 <- paste(ifelse(length(input$filtro_programa)>1," // Programas: "," // Programa: "), programas)
#       }
#       else{
#         texto2 <- ""
#       }
#
#       texto1 <- paste("Grupo de referencia: ", input$grupo_referencia, " - ", input$prueba)
#
#       paste(texto1,texto2)
#
#     })
#

    observeEvent(input$generate_graph ,{

    output$grafico_general <- renderPlot({

      if (input$prueba=='Puntaje Global'){
        sal = 'PUNT_GLOBAL.x'
      }
      if (input$prueba=='Razonamiento Cuantitativo'){
        sal = 'MOD_RAZONA_CUANTITAT_PUNT'
      }
      if (input$prueba=='Inglés'){
        sal = 'MOD_INGLES_PUNT'
      }
      if (input$prueba=='Lectura Crítica'){
        sal = 'MOD_LECTURA_CRITICA_PUNT'
      }

        create_graph_general_var(datos_clean_1(), media11(), mediaPro(), prueba = sal)

    })

    })


  })
}

