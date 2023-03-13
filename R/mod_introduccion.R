#' introduccion UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_introduccion_ui <- function(id){
  ns <- NS(id)
  tagList(

    # h1('Presentación'),
    div(img(src='www/campus.jpeg',
            width = 1690,
            height = 600
            ),style="text-align: center;"),
    h2("Facultad de Ingeniería, Diseño e Innovación | Escuela de Ciencias Básicas",align = "center"),
    tags$hr(),
    tags$br(),
    fluidRow(

      column(6,

             tags$p(align = "justify",
                    "La medición de la calidad de la educación superior en Colombia utilizando las pruebas estandarizadas del ICFES es una tarea que debe hacerse cuidadosamente. En primer lugar, se debe reconocer que las pruebas no recogen la totalidad de la experiencia educativa y que las inequidades sociales, económicas y educativas deben ser tenidas en cuenta al momento de hacer mediciones comparativas entre Instituciones de Educación Superior (IES) y los programas académicos porque el logro de aprendizaje depende de las experiencias previas en educación media y el entorno."
             ),
             tags$p(align = "justify",
                    "Este trabajo auspiciado desde la Facultad de Ingeniería, Diseño e Innovación, la Escuela de Ciencias Básicas y el Centro de Investigación de datos Económicos y Sectoriales del Politécnico Grancolombiano, brinda una opción de evaluación cuantitativa desde una óptica descriptiva de los desempeños de los programas y las IES en las pruebas saber pro y dos modelos de medición de valor agregado."
             )

             ),
      column(6,div(img(src='www/analisis.png',
                       width = 400,
                       height = 250
      ),style="text-align: center;"))

    ),

    tags$br(),

    fluidRow(

      column(6,div(img(src='www/descriptivo.webp',
                       width = 400,
                       height = 250
      ),style="text-align: center;")),
      column(6,
             tags$p(align = "justify",
                    "El trabajo descriptivo trata de la presentación gráfica y comparativa de los resultados de las pruebas saber pro 2020 y saber 11. Allí se buscó, por medio de una llave provista por el ICFES, a todos los estudiantes del periodo 2020 en los repositorios de la prueba saber 11 entre 2010-1 y 2017-2. Dado que la prueba saber 11 tuvo un cambio estructural a partir del periodo 2014-2 que hizo cambiar significativamente los resultados promedio de la prueba, es posible hacer las comparaciones según los resultados de uno y otro periodo. Además del puntaje global, es posible visualizar los resultados en las pruebas de razonamiento cuantitativo, inglés y lectura crítica."
                    )
             )
    ),

    tags$br(),

    fluidRow(

      column(6,
             tags$p(align = "justify",
                    "En cuanto a los modelos de medición de valor agregado, se proponen dos: el primero es un método de regresión lineal jerárquico en el que la variabilidad de la prueba saber pro se descompone en dos fuentes: las instituciones y las personas. El uso de esta metodología implica el reconocimiento de una gran diversidad en las capacidades, recursos de las IES en Colombia y desempeños dentro de los estudiantes de una misma institución. Aislando la fuente de variabilidad de las personas dentro de una institución, queda la que corresponde a las IES. Y allí el valor agregado es el valor esperado del puntaje global para una IES con condiciones particulares de acreditación y proporción de docentes con doctorado. Este modelo presenta buen ajuste para las mediciones a nivel de IES pero no de programas; por ende solo se presentan resultados a nivel institucional."
             ),
             tags$p(align = "justify",
                    "Un segundo modelo reconoce los problemas de sesgo de selección, debido a que los estudiantes escogen la universidad con base en sus restricciones sociales y económicas. Para esto plantea la búsqueda de un grupo de personas comparables para cada IES y posteriormente se encuentra, a través de una regresión lineal, el efecto de la institución en el resultado del valor agregado medido como la diferencia entre las pruebas saber 11 y saber pro. Un procedimiento idéntico aplica para programas académicos. Es posible consultar las estadísticas descriptivas de los emparejamientos en contraste con las comparaciones cuando no se hace el emparejamiento. Este tablero incluye algunas estadísticas descriptivas que ayudan a dar un contexto global de la prueba saber pro."
             )

      ),
      column(6,
             div(img(src='www/network.png',
                     width = 500,
                     height = 350
             ),style="text-align: center;")
             )

    ),

    tags$hr(),
    h2("Investigadores",align = 'center'),
    tags$br(),

    fluidRow(

      column(2,
             HTML('
<div class="card-persona">
  <div class="card-border-top">
  </div>
  <div class="img">
  </div>
  <span> Rafael Armando Garcia</span>
  <p class="job"> Decano de la Facultad de Ingeniería</p>
</div>
         ')),
             column(2,
                    HTML('
<div class="card-persona">
  <div class="card-border-top">
  </div>
  <div class="img">
  </div>
  <span> Hugo Edver <p>Zamora</p></span>
  <p class="job"> Director de la Escuela de Ciencias Básicas</p>
</div>
         ')

             ),
      column(2,
             HTML('
<div class="card-persona">
  <div class="card-border-top">
  </div>
  <div class="img">
  </div>
  <span> Jose Wilmar Quintero</span>
  <p class="job"> Docente escuela de negocios gestion y sostenibilidad</p>
</div>
         ')

      ),
      column(2,
             HTML('
<div class="card-persona">
  <div class="card-border-top">
  </div>
  <div class="img">
  </div>
  <span> Henry David <p>Bacca</p> </span>
  <p class="job"> Docente ciencias Básicas</p>
</div>
         ')

      ),
      column(2,
             HTML('
<div class="card-persona">
  <div class="card-border-top">
  </div>
  <div class="img">
  </div>
  <span> Frederick <p>Mendoza</p></span>
  <p class="job"> Docente ciencias Básicas</p>
</div>
         ')

      ),
      column(2,
             HTML('
<div class="card-persona">
  <div class="card-border-top">
  </div>
  <div class="img">
  </div>
  <span> Claudia <p>Pico</p></span>
  <p class="job"> Docente escuela de negocios gestion y sostenibilidad</p>
</div>
         ')

      )

    ),

    tags$br(),
    tags$hr(),
    tags$br(),

    HTML("<p align='center'>Desarrollado en Shiny por <a href= https://www.linkedin.com/in/mauricioacano/ target='_blank'> Alejandro Cano</a> </p>")

  )
}

#' introduccion Server Functions
#'
#' @noRd
mod_introduccion_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


  })
}
