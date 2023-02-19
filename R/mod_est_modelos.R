#' est_modelos UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shiny isTruthy
#' @importFrom plotly plot_ly renderPlotly plotlyOutput layout
#' @importFrom stats setNames
#'
mod_est_modelos_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidRow(

      column(3,

             fluidRow(

               column(12, uiOutput(ns("filtros")))

             ),
             fluidRow(
               column(12,uiOutput(ns("texto_ayuda")))
             )

      ),

      column(9, uiOutput(ns("graficos")))

    )

  )
}

#' est_modelos Server Functions
#'
#' @noRd
mod_est_modelos_server <- function(id, datos, resumen_modelo_universidad, detalle){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    if (detalle == "Universidad") {

      vars <- setNames(
        object = colnames(datos)[1:7],
        nm = c("Residencia Urbana o Rural","Estado Civil","Grupo Referencia","Indice Socioeconomico","Nivel de Educación de los Padres","Puntaje Global de la prueba saber 11","Tenencia de Internet en la Familia")
      )

    }
    else{

      vars <- setNames(
        object = colnames(datos)[1:5],
        nm = c("Indice Socioeconomico","Nivel de Educación de los Padres","Puntaje Global de la prueba saber 11","Modalidad","Tenencia de Internet en la Familia")
      )

    }

    output$filtros <- renderUI({

      list(

        card(height = 500,

             card_header(
               class = "bg-dark",
               paste("Filtros")),

             card_body_fill(

               shiny::selectInput(inputId = ns('tipo_modelo'),
                                  label = 'Modelo:',
                                  choices = c("Modelo lineal jerarquico","Propensity Score Matching"),
                                  multiple = F,
                                  selected = "Propensity Score Matching"),

               if (detalle == "Universidad") {

                 shiny::selectizeInput(inputId = ns('universidad'),
                                       label = 'Universidad:',
                                       choices = datos %>% distinct(!!sym("INST_NOMBRE_INSTITUCION")) %>% pull(),
                                       multiple = F,
                                       selected = datos$INST_NOMBRE_INSTITUCION[1],
                                       options = list(maxOptions = 3))

               }
               else{

                 shiny::selectizeInput(inputId = ns('programa'),
                                       label = 'Programa:',
                                       choices = datos %>% distinct(!!sym("llave_snies_prog")) %>% pull(),
                                       multiple = F,
                                       selected = datos$llave_snies_prog[1],
                                       options = list(maxOptions = 3))

               }


               ,shiny::uiOutput(ns('variable_modelado'))


             )

        )


      )

    })


    output$variable_modelado <- renderUI({

      if (input$tipo_modelo == 'Propensity Score Matching') {

        shiny::selectInput(inputId = ns('variable_modelo'),
                           label = 'Variable en Modelo:',
                           choices = vars,
                           multiple = F,
                           selected = vars[1])

      }

    })

    output$texto_modelos <- renderText({

      req(input$tipo_modelo)

      if (input$tipo_modelo == 'Modelo lineal jerarquico') {

        detalle_modelo_1

      }

      else if (input$tipo_modelo == 'Propensity Score Matching'){

        detalle_modelo_2

      }

    })

    output$texto_ayuda <- renderUI({

      list(

        card(full_screen = TRUE,height = 400,

             card_header(
               class = "bg-dark",
               paste("Explicación del Modelo")),

             htmlOutput(ns("texto_modelos"))

        )


      )

    })


    output$grafico_att <- renderPlotly({

      req(isTruthy(input$universidad) || isTruthy(input$programa))

      if (detalle == "Universidad"){

        m <- resumen_modelo_universidad[resumen_modelo_universidad$INST_NOMBRE_INSTITUCION==input$universidad, ]

      }

      else{

        m <- resumen_modelo_universidad[resumen_modelo_universidad$llave_snies_prog==input$programa, ]

      }

      if (m$Estimate<0) {

        valor_1 = -20
        valor_2 = 40

      }

      else{

        valor_1 = 20
        valor_2 = -40

      }

      a <- list(
        x = ifelse(detalle == "Universidad",m$INST_NOMBRE_INSTITUCION,m$ESTU_PRGM_ACADEMICO),
        y = m$Estimate,
        text = ifelse(detalle == "Universidad",m$INST_NOMBRE_INSTITUCION,m$llave_snies_prog),
        xref = "x",
        yref = "y",
        showarrow = TRUE,
        arrowhead = 7,
        ax = valor_1,
        ay = valor_2
      )

      if (detalle == "Universidad") {

        fig <- plot_ly(resumen_modelo_universidad, type='bar', x = ~reorder(INST_NOMBRE_INSTITUCION,-Estimate),
                       y = ~Estimate,
                       text = ~INST_NOMBRE_INSTITUCION, name="",
                       hovertemplate = paste('%{x}', '<br>Score: %{y}<br>'))
      }
      else{

        fig <- plot_ly(resumen_modelo_universidad, type='bar', x = ~reorder(llave_snies_prog,-Estimate),
                       y = ~Estimate,
                       text = ~llave_snies_prog, name="",
                       hovertemplate = paste('%{x}', '<br>Score: %{y}<br>'))

      }

      fig <- fig %>% layout(uniformtext=list(minsize=8, mode='hide'),
                            xaxis= list(showticklabels = FALSE, title = ifelse(detalle == "Universidad","Universidades","Programas")),
                            yaxis = list(title = 'Valor Agregado por PSM'),
                            annotations = a)

      fig

    })


    output$grafico_general <- renderUI({

      list(

        card(

          card_header(
            class = "bg-dark",
            paste("Modelado")),

          plotlyOutput(ns("grafico_att"))
        )
      )

    })

    output$graficos <- renderUI({

      req(input$tipo_modelo)

      fluidRow(

        column(12,shiny::uiOutput(ns("grafico_general"))),

        if (input$tipo_modelo == 'Propensity Score Matching') {

          column(12,shiny::uiOutput(ns("detalle_grafico")))

        }

      )


    })

    datos_filtrados <- reactive({

      req(isTruthy(input$universidad) || isTruthy(input$programa))

      if (detalle == "Universidad") {

        data_clean = datos %>% filter(input$universidad==!!sym('INST_NOMBRE_INSTITUCION'))

        if (input$variable_modelo %in% c('GRUPOREFERENCIA','FAMI_EDUCACIONPADRE.y','ESTU_ESTADOCIVIL')) {

          data_clean[,input$variable_modelo] <- str_wrap(data_clean[,input$variable_modelo], width = 15)
          data_clean
        }

        else{

          data_clean

        }

      }
      else{

        data_clean = datos %>% filter(input$programa==!!sym('llave_snies_prog'))

        if (input$variable_modelo %in% c('GRUPOREFERENCIA','FAMI_EDUCACIONPADRE.y','ESTU_ESTADOCIVIL')) {

          data_clean[,input$variable_modelo] <- str_wrap(data_clean[,input$variable_modelo], width = 15)
          data_clean
        }

        else{

          data_clean

        }

      }




    })

    datos_con_muestra <- reactive({

      req(input$variable_modelo)

      data_clean = datos_filtrados() %>% filter(submuestra==1)

    })


    output$detalle_grafico <- renderUI({

      fluidRow(

        column(6,

               card(

                 card_header(
                   class = "bg-dark",
                   paste("Sin Muestreo")),

                 plotOutput(ns("grafico_sin_correccion"))
               )

        ),
        column(6,

               card(

                 card_header(
                   class = "bg-dark",
                   paste("Con Muestreo")),

                 plotOutput(ns("grafico_con_correccion"))
               )
        )

      )

    })


    output$grafico_sin_correccion <- renderPlot({

      req(input$variable_modelo)

      graficos_distribucion_modelado(datos_filtrados(),input$variable_modelo)

    })

    output$grafico_con_correccion <- renderPlot({

      req(input$variable_modelo)

      graficos_distribucion_modelado(datos_con_muestra(),input$variable_modelo)

    })


  })
}
