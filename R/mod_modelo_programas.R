#' modelo_programas UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyWidgets selectizeGroupUI selectizeGroupServer
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
        uiOutput(ns("output_table_psm_"))
      ),
      pill("Modelo Jerarquico",
           uiOutput(ns("output_table_jerarquico_"))
      ))
  )
}

#' modelo_programas Server Functions
#'
#' @noRd
mod_modelo_programas_server <- function(id, datos, datos_demograficos, detalle, resumen_jeq){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Desarrollo de la tabla de resultados del modelo PSM
    vars <- setNames(
      object = colnames(datos_demograficos)[1:5],
      nm = c("Indice Socioeconomico","Nivel de Educación de los Padres","Puntaje Global de la prueba saber 11","Modalidad","Tenencia de Internet en la Familia")
    )

    datos_filtrados <- reactive({

      req(input$programa_)

      data_clean = datos_demograficos %>% filter(input$programa_==!!sym('ESTU_PRGM_ACADEMICO')) %>% filter(input$universidad_==!!sym('INST_NOMBRE_INSTITUCION'))

      if (input$variable_modelo_ %in% c('GRUPOREFERENCIA','FAMI_EDUCACIONPADRE.y','ESTU_ESTADOCIVIL')) {

        data_clean[,input$variable_modelo_] <- str_wrap(data_clean[,input$variable_modelo_], width = 15)
        data_clean
      }else{
        data_clean
      }

  })

    datos_con_muestra <- reactive({

      req(input$variable_modelo_)

      data_clean = datos_filtrados() %>% filter(submuestra==1)

    })

    output$output_table_psm_ <- renderUI({

      table = DT::renderDataTable(datos,
                                  style = "bootstrap5",
                                  filter = "top",
                                  options = list(
                                    autoWidth = F,
                                    searchHighlight = TRUE
                                  )
      )

      output_ = tagList(
        tags$h2("Propensity Score Matching", class = "center-text"),
        tags$p("El Propensity Score Matching es una técnica estadística que permite comparar dos grupos de individuos que comparten características similares. En este caso, se compara el desempeño de los estudiantes en la prueba Saber 11 con su desempeño en la prueba Saber Pro.", class = "justify-text"),
        tags$p("Explicaicon de para que se uso el modelo para hacer el ranking de valor agregado.", class = "justify-text"),
        tags$br(),
        bs5_card(table,title = "Ranking de Programas por PSM"),
        tags$br(),
        tags$h3("Calidad de Emparejamiento", class = "center-text"),
        tags$p("Los graficos que se muestran a continuacion permiten identificar la calidad del emparejamiento basado en las variables demograficas de los estuidantes. Para eliminar el sesgo de selección, cada estudiante del tratamiento se compara con una muestra aleatoria de diez mil estudiantes de otras IES o programas según sea el caso. Posteriormente se encuentran parejas o grupos de estudiantes muy similares: así se establece un grupo de tratamiento y otro de control con la particularidad de que todos tienen probabilidades similares de pertenecer al tratamiento.", class = "justify-text"),
        tags$br(),
        layout_column_wrap(
          width = 1/3,
          shiny::selectizeInput(inputId = ns('universidad_'),
                                label = 'Universidad:',
                                choices = datos_demograficos_filtros() %>% distinct(!!sym("INST_NOMBRE_INSTITUCION")) %>% pull(),
                                multiple = F,
                                # options = list(maxOptions = 8)
          ),
          shiny::selectizeInput(inputId = ns('programa_'),
                                label = 'Programa:',
                                choices = NULL,
                                multiple = F,
                                # options = list(maxOptions = 8)
          ),
          shiny::selectInput(inputId = ns('variable_modelo_'),
                             label = 'Variable en Modelo:',
                             choices = vars,
                             multiple = F,
                             selected = vars[1])
        ),
        layout_column_wrap(
          width = 1/2,
          bs5_card(plotOutput(ns("grafico_sin_correccion")), title = "Sin Emparejamiento"),
          bs5_card(plotOutput(ns("grafico_con_correccion")), title = "Con Emparejamiento")
        ),
        tags$p("El gráfico de la izquierda representa cómo se distribuye la variable entre los estudiantes que no fueron emparejados, mientras que el gráfico de la derecha muestra la distribución de la misma variable para los estudiantes que sí fueron emparejados. Estos resultados muestran que, tras el proceso de emparejamiento, es posible comparar estudiantes de otras universidades que tienen características demográficas similares.", class = "justify-text"),
        tags$br(),
        tags$h3("Detalle Técnico", class = "center-text"),
        tags$br(),
        HTML(detalle_modelo_2)
      )

      output_

    })

    observe({
      req(input$universidad_)
      updateSelectizeInput(session, 'programa_', choices = datos_demograficos_filtros() %>% filter(input$universidad_ == !!sym('INST_NOMBRE_INSTITUCION')) %>% distinct(!!sym("ESTU_PRGM_ACADEMICO")) %>% pull())
    })


    datos_demograficos_filtros <- reactive({

      datos_demograficos %>% select('INST_NOMBRE_INSTITUCION',"ESTU_PRGM_ACADEMICO") %>% distinct()

    })

    output$grafico_sin_correccion <- renderPlot({

      req(input$variable_modelo_)

      graficos_distribucion_modelado(datos_filtrados(),input$variable_modelo_,detalle)

    })

    output$grafico_con_correccion <- renderPlot({

      req(input$variable_modelo_)

      graficos_distribucion_modelado(datos_con_muestra(),input$variable_modelo_, detalle)

    })

  })
}
