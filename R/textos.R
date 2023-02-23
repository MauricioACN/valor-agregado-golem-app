### textos

txt_info_m2_p1 <- '
                              <p align="justify">
                              En este apartado, se presentan gráficas de dispersión que permiten analizar si existe algún tipo de relación entre los puntajes obtenidos por los estudiantes en las pruebas Saber 11 y Saber Pro. Estas gráficas se construyeron para cada grupo de referencia, teniendo en cuenta que se comparan pruebas similares del examen Saber 11 con Saber Pro, es decir, se comparó Lectura Crítica (Saber Pro) vs. Lectura Crítica (Saber 11), Razonamiento Cuantitativo (Saber Pro) vs. Matemáticas (Saber 11), Inglés (Saber Pro) vs. Inglés (Saber 11) y por supuesto, el puntaje global de ambos exámenes.
                              En los gráficos de dispersión, el eje x corresponde a los puntajes del examen Saber 11, mientras que el eje y corresponde a los puntajes del examen Saber Pro. Se tiene una línea horizontal que corresponde al promedio nacional de la correspondiente prueba, en el grupo de referencia determinado. Por otro lado, la línea vertical, denota el promedio nacional, del correspondiente módulo.
                              Con las líneas que denotan los promedios nacionales en las gráficas se conforman cuatro cuadrantes, así:
                              <ul>
                              <li> Cuadrante 1: superior derecho </li>
                              <li> Cuadrante 2: superior izquierdo </li>
                              <li> Cuadrante 3: inferior izquierdo </li>
                              <li> Cuadrante 4: inferior derecho </li>
                              </ul>
                              </p>'

txt_info_m2_p2 <- '
                              <p align="justify">
                              En cada uno de estos cuadrantes, se muestra el porcentaje de estudiantes del correspondiente programa y prueba, que pertenece a dicho cuadrante, de manera que los porcentajes en cada cuadrante se leen así:
                              <ul>
                              <li> Cuadrante 1: estudiantes que obtuvieron puuntaje superior al promedio en Saber 11 y en Saber Pro. </li>
                              <li> Cuadrante 2: estudiantes que obtuvieron puntaje inferior al promedio en Saber 11, pero superior en Saber Pro. </li>
                              <li> Cuadrante 3: estudiantes que obtuvieron puntaje inferior al promedio en Saber 11 y en Saber Pro. </li>
                              <li> Cuadrante 4: estudiantes que obtuvieron puntaje superoir al promedio en saber 11, pero inferior en Saber Pro. </li>
                              </ul>
                              </p>'

txt_info_periodos <- '

                              <b> Periodo 1:</b> puntajes de los estudiantes que presentaron el examen Saber 11 antes del 2014-2.
                              <br></br>
                              <b>Periodo 2:</b> puntajes de los estudiantes que presentaron el examen Saber 11 después del 2014-2.
                              <br></br>
                              La anterior clasificación por periodos se realizó teniendo en cuenta por un lado que, el examen Saber Pro cambió en su estructura, agrupando algunas pruebas (<a href=https://www2.icfes.gov.co/documents/39286/2385704/Documentacion+saber+11.pdf/37c20c0a-1ea1-ac6a-8e6f-8b37e6907c40?version=1.0&t=1648072736825> mayor información <a/>); y por otro, algunas variaciones globales en los puntajes de ambos periodos.


'

help_grupo_referencia <- 'Son las áreas de estudio en las que el ICFES clasifica los programas académicos.'
help_periodos <- 'Es el periodo de prueba saber 11 que se usará para promediar y hacer las comparaciones en los gráficos descriptivos. En 2014-2 hacia adelante, los promedios suelen ser más altos, debido a un cambio estructural en la prueba.'
help_prueba <- 'Son las pruebas de competencias genéricas definidas por el ICFES.'


introduccion_app <- '
                          <p align="justify">
                          La medición de la calidad de la educación superior en Colombia utilizando las pruebas estandarizadas del ICFES es una tarea que debe hacerse cuidadosamente. En primer lugar, se debe reconocer que las pruebas no recogen la totalidad de la experiencia educativa y que las inequidades sociales, económicas y educativas deben ser tenidas en cuenta al momento de hacer mediciones comparativas entre Instituciones de Educación Superior (IES) y los programas académicos porque el logro de aprendizaje depende de las experiencias previas en educación media y el entorno.
                          </p>
                          <p align="justify">
                          Este trabajo auspiciado desde la Facultad de Ingeniería, Diseño e Innovación, la Escuela de Ciencias Básicas y el Centro de Investigación de datos Económicos y Sectoriales del Politécnico Grancolombiano, brinda una opción de evaluación cuantitativa desde una óptica descriptiva de los desempeños de los programas y las IES en las pruebas saber pro y dos modelos de medición de valor agregado.
                          </p>
                          <p align="justify">
                          El trabajo descriptivo trata de la presentación gráfica y comparativa de los resultados de las pruebas saber pro 2020 y saber 11. Allí se buscó, por medio de una llave provista por el ICFES, a todos los estudiantes del periodo 2020 en los repositorios de la prueba saber 11 entre 2010-1 y 2017-2. Dado que la prueba saber 11 tuvo un cambio estructural a partir del periodo 2014-2 que hizo cambiar significativamente los resultados promedio de la prueba, es posible hacer las comparaciones según los resultados de uno y otro periodo. Además del puntaje global, es posible visualizar los resultados en las pruebas de razonamiento cuantitativo, inglés y lectura crítica.
                          </p>
                          </p>
                          <p align="justify">
                          En cuanto a los modelos de medición de valor agregado, se proponen dos: el primero es un método de regresión lineal jerárquico en el que la variabilidad de la prueba saber pro se descompone en dos fuentes: las instituciones y las personas. El uso de esta metodología implica el reconocimiento de una gran diversidad en las capacidades, recursos de las IES en Colombia y desempeños dentro de los estudiantes de una misma institución. Aislando la fuente de variabilidad de las personas dentro de una institución, queda la que corresponde a las IES. Y allí el valor agregado es el valor esperado del puntaje global para una IES con condiciones particulares de acreditación y proporción de docentes con doctorado. Este modelo presenta buen ajuste para las mediciones a nivel de IES pero no de programas; por ende solo se presentan resultados a nivel institucional.
                          </p>
                          <p align="justify">
                          Un segundo modelo reconoce los problemas de sesgo de selección, debido a que los estudiantes escogen la universidad con base en sus restricciones sociales y económicas. Para esto plantea la búsqueda de un grupo de personas comparables para cada IES y posteriormente se encuentra, a través de una regresión lineal, el efecto de la institución en el resultado del valor agregado medido como la diferencia entre las pruebas saber 11 y saber pro. Un procedimiento idéntico aplica para programas académicos. Es posible
                          </p>
                          <p align="justify">
                          consultar las estadísticas descriptivas de los emparejamientos en contraste con las comparaciones cuando no se hace el emparejamiento. Este tablero incluye algunas estadísticas descriptivas que ayudan a dar un contexto global de la prueba saber pro.
                          </p>

'

detalle_modelo_2 <- '

                          <p align="justify">
                          En este modelo se intenta eliminar el sesgo de selección haciendo comparaciones en el valor agregado entre estudiantes comparables.
                          </p>
                          <p align="justify">
                          Procedimentalmente se procede de manera similar al enfoque metodológico de las evaluaciones de impacto; por ende, cada IES o programa es un tratamiento. Los estudiantes que ingresan tienen unas condiciones previas de la prueba saber 11, el ingreso individual y familiar, el área de residencia (urbana o rural), acceso a internet, nivel de formación del padre, estado civil. Y además se asume que pueden existir diferencias entre los grupos de referencia de cada estudiante.
                          </p>
                          <p align="justify">
                          Para eliminar el sesgo de selección, cada estudiante del tratamiento se compara con una muestra aleatoria de diez mil estudiantes de otras IES o programas según sea el caso. Posteriormente se encuentran parejas o grupos de estudiantes muy similares: así se establece un grupo de tratamiento y otro de control con la particularidad de que todos tienen probabilidades similares de pertenecer al tratamiento.
                          </p>
                          <p align="justify">
                          Finalmente se desarrolla una regresión lineal en la que La variable dependiente es la **diferencia entre los puntajes estandarizados entre los puntajes globales entre las pruebas saber pro y saber 11**. Allí se controla por las mismas variables del emparejamiento más una dummy de tratamiento. Luego, se toma el coeficiente de la variable tratamiento como el efecto promedio sobre los tratados (ATT por sus siglas en inglés): este es el valor agregado. Dado que los puntajes y sus diferencias están estandarizado la interpretación del modelo puede hacerse en términos de desviaciones estándar. Así, por ejemplo, un valor agregado de 0.5 indica un efecto promedio de 0.5 desviaciones estándar en el valor agregado de una IES o programa con respecto a otros espacios de formación que reciben estudiantes similares o comparables.
                          </p>
'


detalle_modelo_1 <- '

        <p align="justify">La variación de la prueba saber pro está en dos fuentes: una intra-institucional que se debe a los estudiantes de cada IES y están correlacionados. y otra inter-institucional debida a las características de cada IES.</p>
        <p align="justify">El propósito de este modelo es dividir las fuentes de variación. En lo referente a la fuente institucional se utilizan como variables explicativas o independientes del resultado en la prueba saber pro la condición de acreditación de alta calidad y la proporción de docentes con doctorado.</p>
        <p align="justify">En principio, el resultado de la prueba saber pro depende de las condiciones previas del estudiante en lo referente a su desempeño en la prueba saber 11 y su condición socio económica. También se tiene en cuenta el grupo de referencia (Área de conocimiento). Sin embargo, el nivel de asociación entre la prueba saber 11 y la saber pro, medido en el coeficiente b (nos centramos en esta relación para enfocarnos en el valor agregado entre pruebas) es variable entre cada IES dependiendo de sus condiciones de acreditación y proporción de docentes con doctorado.</p>
        <p align="justify">El valor agregado se refiere a los efectos aleatorios en el intercepto de cada IES . Específicamente se mide como el valor esperado o valor promedio del puntaje global en competencias genéricas de cada IES de acuerdo con la predicción del modelo jerárquico.</p>
'

integrantes <- '

        <h2 align="center">Facultad de Ingeniería, Diseño e Innovación | Escuela de Ciencias Básicas</h2>

        <h3 align="center">Investiagadores</h3>

        <ul>

            <li>Rafael Armando Garcia</li>
            <li>Hugo Edver Zamora</li>
            <li>Jose Wilmar Quintero</li>
            <li>Henry David Bacca</li>
            <li>Frederick Mendoza</li>

        </ul>

'
