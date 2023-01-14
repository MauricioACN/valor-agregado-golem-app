#' descriptivo UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import dplyr
#' @import ggplot2
#' @importFrom DT DTOutput renderDataTable
mod_descriptivo_ui <- function(id){
  ns <- NS(id)
  tagList(

    fluidRow(
      column(6,
             uiOutput(ns("grafico1"))
             ),
      column(6,
             fluidRow(
               column(12,uiOutput(ns('grafico2'))
             ),
             fluidRow(
               column(12,uiOutput(ns('grafico3'))
             )
             )
             )
             )
    )

  )
}

#' descriptivo Server Functions
#'
#' @noRd
mod_descriptivo_server <- function(id,datos){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$grafico1 <- renderUI({

      card(full_screen = TRUE,height = 1200,
           card_header(
             class = "bg-dark",
             paste("Estudiantes en cada grupo de referencia")
           ),
           plotOutput(ns('plot_grupo_ref'),height = '1000px'),

           card_footer(
             "Esto es un texto"
           )
      )

    })

    output$plot_grupo_ref <- renderPlot({

      nivel <- summary_plot(datos,'grupoReferencia')

      grafico_bar_hor(nivel,'grupoReferencia')


    })

    output$grafico3 <- renderUI({

      card(full_screen = TRUE,height = 591,
           card_header(
             class = "bg-dark",
             paste("Índice de nivel socioeconómico (INSE)")
           ),

           plotOutput(ns('plot_inse')),

           card_footer(
             "Esto es un texto"
           )
      )

    })

    output$plot_inse <- renderPlot({

      nivel <- datos %>% select(inse_imputado)

      g_inse <- ggplot(nivel, aes(x = inse_imputado)) +
        geom_histogram(bins = 30, col = "white", fill = "#87CEEB") +
        theme(
          axis.text.y = element_text(face="bold", size=10, angle=0),
          axis.text.x = element_text(face="bold", size=10, angle=0),
          panel.background = element_rect(fill = "#FAFAFA"),
          plot.title = element_text(size = 10, face = "bold")) +
        labs(
          # title = "Índice de nivel socioeconómico (INSE)",
             x = "INSE",
             y = "Estudiantes")

      g_inse

    })

    output$grafico2 <- renderUI({

      navs_tab_card(height = 590,
        nav(
          card_header("Genero",class = 'bg-dark'),
            plotOutput(ns('plot_genero')),
        ),
        nav(
          card_header("Residencia",class = 'bg-dark'),
          card_body_fill(
            plotOutput(ns('plot_resi')),
          )
        ),
        nav(
          card_header("Modalidad",class = 'bg-dark'),
          card_body_fill(
            plotOutput(ns('plot_modalidad')),
          )
        ),
        nav(
          card_header("Semestre",class = 'bg-dark'),
          card_body_fill(
            plotOutput(ns('plot_semestre')),
          )
        ),
        nav(
          card_header("Promedio Nacional",class = 'bg-dark'),
          card_body_fill(
            DTOutput(ns('plot_tablas')),
          )
        )
      )

  })

  output$plot_tablas <- DT::renderDataTable({

    DT::datatable(medias_nal,
                  class="cell-border stripe",
                  rownames = FALSE,
                  options = list(dom = 't'))

  })

  output$plot_semestre <- renderPlot({

    nivel <- summary_plot(datos,"Semestre")

    grafico_bar_hor(nivel, "Semestre")

  })


  output$plot_genero <- renderPlot({

    nivel <- summary_plot(datos,"Genero")

    grafico_bar_hor(nivel, "Genero")

  })

  output$plot_resi <- renderPlot({

    nivel <- summary_plot(datos,"areaResidencia")

    grafico_bar_hor(nivel, "areaResidencia")

  })

  output$plot_modalidad <- renderPlot({

    nivel <- summary_plot(datos,"modalidad")

    grafico_bar_hor(nivel, "modalidad")

  })


})
}
