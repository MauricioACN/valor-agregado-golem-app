#' modelo UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_modelo_ui <- function(id){
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

#' modelo Server Functions
#'
#' @noRd
mod_modelo_server <- function(id,datos){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    vars <- stats::setNames(
      object = colnames(datos)[1:7],
      nm = c("Residencia Urbana o Rural","Estado Civil","Grupo Referencia","Indice Socioeconomico","Nivel de Educación de los Padres","Puntaje Global de la prueba saber 11","Tenencia de Internet en la Familia")
    )

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

          shiny::selectizeInput(inputId = ns('universidad'),
                                label = 'Universidad:',
                                choices = datos %>% distinct(!!sym("INST_NOMBRE_INSTITUCION")) %>% pull(),
                                multiple = F,
                                selected = datos$INST_NOMBRE_INSTITUCION[1],
                                options = list(maxOptions = 3)),

          shiny::uiOutput(ns('variable_modelado'))

          ),

          card_footer(
            "Texto"
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

            htmlOutput(ns("texto_modelos")),

          card_footer(
            "Texto"
          )


        )


      )

    })


    output$grafico_att <- renderPlot({

      ggplot(ATTdf,aes(x=reorder(cods_ies,-Estimate),y=Estimate))+ geom_col(fill="#87CEEB")+theme(axis.text.x = element_text(angle=90,vjust = 0.1,hjust=0.5,size=1))+labs(x="Código SNIES IES",y="Valor agreagado por PSM")

    })


    output$grafico_general <- renderUI({

      list(

        card(

          card_header(
            class = "bg-dark",
            paste("Modelado")),

          plotOutput(ns("grafico_att")),

          card_footer(
            "Texto"
          )


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

      req(input$universidad)

      data_clean = datos %>% filter(input$universidad==!!sym('INST_NOMBRE_INSTITUCION'))

      if (input$variable_modelo %in% c('GRUPOREFERENCIA','FAMI_EDUCACIONPADRE.y','ESTU_ESTADOCIVIL')) {

        data_clean[,input$variable_modelo] <- stringr::str_wrap(data_clean[,input$variable_modelo], width = 15)
        data_clean
      }

      else{

        data_clean

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

                 plotOutput(ns("grafico_sin_correccion")),

                 card_footer(
                   "Texto"
                 )
               )

               ),
      column(6,

             card(

               card_header(
                 class = "bg-dark",
                 paste("Con Muestreo")),

               plotOutput(ns("grafico_con_correccion")),

               card_footer(
                 paste(input$variable_modelo)
               )
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


