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

    output$filtros <- renderUI({

      list(

        card(

          card_header(
            class = "bg-dark",
            paste("Filtros")),


          shiny::selectInput(inputId = ns('tipo_modelo'),
                             label = 'Modelo:',
                             choices = c("Modelo 1","Modelo 2"),
                             multiple = F,
                             selected = "Modelo 2"),

          shiny::selectizeInput(inputId = ns('universidad'),
                                label = 'Universidad:',
                                choices = resultados_modelos %>% distinct(!!sym("INST_NOMBRE_INSTITUCION")) %>% pull(),
                                multiple = F,
                                selected = resultados_modelos$INST_NOMBRE_INSTITUCION[1],
                                options = list(maxOptions = 3)),

          shiny::selectInput(inputId = ns('variable_modelo'),
                                label = 'Variable en Modelo:',
                                choices = colnames(resultados_modelos)[1:7],
                                multiple = F,
                                selected = colnames(resultados_modelos)[1]),

          card_footer(
            "Texto"
          )

        )


      )

    })


    output$texto_ayuda <- renderUI({

      list(

        card(

          card_header(
            class = "bg-dark",
            paste("Texto")),



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


