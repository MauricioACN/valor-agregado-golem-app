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
mod_modelo_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    vars <- stats::setNames(
      object = colnames(resultados_modelos)[1:7],
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
                                choices = resultados_modelos %>% distinct(!!sym("INST_NOMBRE_INSTITUCION")) %>% pull(),
                                multiple = F,
                                selected = resultados_modelos$INST_NOMBRE_INSTITUCION[1],
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


    output$graficos <- renderUI({

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


  })
}


