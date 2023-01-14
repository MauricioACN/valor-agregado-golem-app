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
             paste("Esto es un titulo")
           ),
           plotOutput(ns('plot_grupo_ref'),height = '1000px'),

           card_footer(
             "Esto es un texto"
           )
      )

    })

    output$plot_grupo_ref <- renderPlot({

      nivel <- datos %>% group_by(GRUPOREFERENCIA) %>% summarise(n = n()) %>% arrange(n) %>% mutate(GRUPOREFERENCIA = factor(GRUPOREFERENCIA, GRUPOREFERENCIA))
      colnames(nivel) <- c("grupoReferencia", "Estudiantes")
      nivel <- na.omit(nivel)
      total <- sum(nivel$Estudiantes)
      nivel <- nivel %>% mutate(porcentaje = Estudiantes / total *100)

      g_grupoReferencia <- ggplot(nivel, aes(x = grupoReferencia, y = Estudiantes)) +
        geom_bar(stat = "identity", fill="#87CEEB") +
        coord_flip() +
        theme(axis.text.y = element_text(face="bold", size=8, angle=0),
              panel.background = element_rect(fill = "#FAFAFA"),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              plot.title = element_text(size = 15, face = "bold")) +
        geom_text(aes(label = paste(Estudiantes, "(",round(porcentaje,1), "%",")", sep = "")), hjust = -0.1, size = 3) +
        labs(title = "Estudiantes en cada grupo de referencia") +
        scale_y_continuous(limit = c(0,1.3*max(nivel$Estudiantes)))
      rm(nivel, total)

      g_grupoReferencia

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
        theme(#axis.title.y=element_blank(),
          axis.text.y = element_text(face="bold", size=10, angle=0),
          #axis.text.y=element_blank(),
          #axis.ticks.y=element_blank(),
          #axis.title.x=element_blank(),
          axis.text.x = element_text(face="bold", size=10, angle=0),
          panel.background = element_rect(fill = "#FAFAFA"),
          plot.title = element_text(size = 10, face = "bold")) +
        labs(title = "Índice de nivel socioeconómico (INSE)",
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
          card_header("Tablas",class = 'bg-dark'),
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

  output$plot_genero <- renderPlot({

    nivel <- datos %>% group_by(ESTU_GENERO) %>% summarise(n())
    colnames(nivel) <- c("Genero", "Estudiantes")
    nivel <- na.omit(nivel)
    total <- sum(nivel$Estudiantes)
    nivel <- nivel %>% mutate(porcentaje = Estudiantes / total *100) %>%
      mutate(Genero = paste("Género", Genero))

    g_genero <- ggplot(nivel, aes(x = Genero, y = porcentaje)) +
      geom_bar(stat = "identity", fill="#87CEEB") +
      coord_flip() +
      theme(axis.text.y = element_text(face="bold", size=10, angle=0),
            panel.background = element_rect(fill = "#FAFAFA"),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            plot.title = element_text(size = 10, face = "bold")) +
      geom_text(aes(label = paste(round(porcentaje,1), "%")), hjust = -0.1) +
      labs(title = "Género") +
      scale_y_continuous(limit = c(0,1.2*max(nivel$porcentaje)))

    g_genero

  })

  output$plot_resi <- renderPlot({

    nivel <- datos %>% group_by(ESTU_AREARESIDE) %>% summarise(n())
    colnames(nivel) <- c("areaResidencia", "Estudiantes")
    nivel <- na.omit(nivel)
    total <- sum(nivel$Estudiantes)
    nivel <- nivel %>% mutate(porcentaje = Estudiantes / total *100)

    g_areaResidencia <- ggplot(nivel, aes(x = areaResidencia, y = porcentaje)) +
      geom_bar(stat = "identity", fill="#87CEEB") +
      coord_flip() +
      theme(axis.text.y = element_text(face="bold", size=10, angle=0),
            panel.background = element_rect(fill = "#FAFAFA"),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            plot.title = element_text(size = 10, face = "bold")) +
      geom_text(aes(label = paste(round(porcentaje,1), "%")), hjust = 0) +
      labs(title = "Área de residencia") +
      scale_y_continuous(limit = c(0,1.3*max(nivel$porcentaje)))

    g_areaResidencia

  })

  output$plot_modalidad <- renderPlot({

    nivel <- datos %>% group_by(ESTU_METODO_PRGM) %>% summarise(n())
    colnames(nivel) <- c("modalidad", "Estudiantes")
    nivel <- na.omit(nivel)
    total <- sum(nivel$Estudiantes)
    nivel <- nivel %>% mutate(porcentaje = Estudiantes / total *100)

    g_modalidad <- ggplot(nivel, aes(x = modalidad, y = porcentaje)) +
      geom_bar(stat = "identity", fill="#87CEEB") +
      coord_flip() +
      theme(axis.text.y = element_text(face="bold", size=10, angle=0),
            panel.background = element_rect(fill = "#FAFAFA"),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            plot.title = element_text(size = 10, face = "bold")) +
      geom_text(aes(label = paste(round(porcentaje,1), "%")), hjust = -0.1) +
      labs(title = "Modalidad",
           caption = "P: Presencial, DV: Distancia virtual, D: Distancia") +
      scale_y_continuous(limit = c(0,1.3*max(nivel$porcentaje)))

    g_modalidad

  })


})
}
