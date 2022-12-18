#' helpers
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' @importFrom dplyr mutate filter select group_by summarise n
#' @importFrom tidyr all_of
#' @import ggplot2
#' @import gridExtra
#' @importFrom rlang sym
#' @importFrom thematic thematic_on

thematic::thematic_on(bg="auto",fg="auto",accent = "auto", font = "auto")

clean_resultados <- function(data, n_sample, grupo){

  datos_clean <- data %>% mutate(ESTU_PRGM_ACADEMICO = ifelse(ESTU_PRGM_ACADEMICO == "ADMINISTRACION DE EMPRESAS","ADMINISTRACIÓN DE EMPRESAS", ESTU_PRGM_ACADEMICO)) %>%
    mutate(ESTU_PRGM_ACADEMICO = ifelse(ESTU_PRGM_ACADEMICO == "ADMINISTRACIÓN DE EMPRESAS - CICLO PROFESIONAL", "ADMINISTRACIÓN DE EMPRESAS", ESTU_PRGM_ACADEMICO)) %>%
    mutate(ESTU_PRGM_ACADEMICO = ifelse(ESTU_PRGM_ACADEMICO == "INGENIERIA INDUSTRIAL", "INGENIERÍA INDUSTRIAL", ESTU_PRGM_ACADEMICO)) %>%
    mutate(ESTU_PRGM_ACADEMICO = ifelse(ESTU_PRGM_ACADEMICO == "CONTADURIA PUBLICA", "CONTADURÍA PÚBLICA", ESTU_PRGM_ACADEMICO)) %>%
    mutate(ESTU_PRGM_ACADEMICO = ifelse(ESTU_PRGM_ACADEMICO == "PSICOLOGIA", "PSICOLOGÍA", ESTU_PRGM_ACADEMICO)) %>%
    mutate(ESTU_PRGM_ACADEMICO = ifelse(ESTU_PRGM_ACADEMICO == "COMUNICACION SOCIAL- PERIODISMO", "COMUNICACIÓN SOCIAL", ESTU_PRGM_ACADEMICO)) %>%
    mutate(ESTU_PRGM_ACADEMICO = ifelse(ESTU_PRGM_ACADEMICO == "PERIODISMO", "COMUNICACIÓN SOCIAL", ESTU_PRGM_ACADEMICO)) %>%
    mutate(ESTU_PRGM_ACADEMICO = ifelse(ESTU_PRGM_ACADEMICO == "INGENIERIA DE SISTEMAS", "INGENIERÍA DE SISTEMAS", ESTU_PRGM_ACADEMICO)) %>%
    mutate(ESTU_PRGM_ACADEMICO = ifelse(ESTU_PRGM_ACADEMICO == "ECONOMIA", "ECONOMÍA", ESTU_PRGM_ACADEMICO))

  datos_clean = datos_clean %>% sample_n(size = n_sample)

  datos_clean <- datos_clean %>% filter(!!sym("GRUPOREFERENCIA") == grupo)

  return(datos_clean)

}


calculate_mean_pro <- function(mediasSaberPro,grupo){

  mediaPro <- mediasSaberPro %>% filter(!!sym("GRUPOREFERENCIA") %in% grupo)
  return(mediaPro)
}

calculate_mean_11 <- function(media11,periodo){

    media11_mean <- media11 %>% filter(periodoAux == periodo) %>% colMeans(.)

  return(media11_mean)
}

create_graph_general <- function(datos, mediasSaber11, mediasSaberPro, grupo, prueba){

  y <- prueba
  if(y == "MOD_LECTURA_CRITICA_PUNT"){
    x <- "Lectura_critica.11"
    subtitulo <- "Lectura crítica"}
  if(y == "MOD_RAZONA_CUANTITAT_PUNT"){
    x <- "Matematicas.11"
    subtitulo <- "Razonamiento cuantitativo"}
  if(y == "MOD_INGLES_PUNT"){
    x <- "Ingles.11"
    subtitulo <- "Inglés"}
  if(y == "PUNT_GLOBAL.x"){
    x <- "Global.11"
    subtitulo <- "Puntaje global"}

  mediaPro <- mediasSaberPro %>% select(all_of(prueba))
  mediaPro <- as.numeric(mediaPro)

  # if(is.na(mediaPro)){
  #   mediaPro <- mean(mediasSaberPro[prueba])
  #   mediaPro <- as.numeric(mediaPro)
  # }

  # media11 <- mediasSaber11 %>% select(all_of(c(x)))
  media11 <- mediasSaber11[x]
  media11 <- as.numeric(media11)

  nombres <- c(x, y)
  datos <- datos %>% select(all_of(nombres), periodoAux)

  datos$periodoAux <- as.character(datos$periodoAux)
  colnames(datos) <- c("x", "y", "periodoAux")

  datos <- datos %>% mutate(cuadrante = ifelse(x >= media11 & y >= mediaPro, "c1",
                                               ifelse(x < media11 & y >= mediaPro, "c2",
                                                      ifelse(x < media11 & y < mediaPro, "c3",
                                                             ifelse(x >= media11 & y < mediaPro, "c4", NA)))))


  datos <- na.omit(datos)
  nivel <- datos %>% group_by(cuadrante) %>% summarise(n())
  colnames(nivel) <- c("cuadrante", "Estudiantes")
  total <- sum(nivel$Estudiantes)
  nivel <- nivel %>% mutate(porcentaje = Estudiantes / total *100)

  pc1 <- as.numeric(nivel %>% filter(cuadrante == "c1") %>% select(porcentaje))
  pc2 <- as.numeric(nivel %>% filter(cuadrante == "c2") %>% select(porcentaje))
  pc3 <- as.numeric(nivel %>% filter(cuadrante == "c3") %>% select(porcentaje))
  pc4 <- as.numeric(nivel %>% filter(cuadrante == "c4") %>% select(porcentaje))

  if(is.na(pc1)){pc1 <- 0}
  if(is.na(pc2)){pc2 <- 0}
  if(is.na(pc3)){pc3 <- 0}
  if(is.na(pc4)){pc4 <- 0}

  theme_set(theme_bw())

  ##Puntaje global##
  if(y == "PUNT_GLOBAL.x"){
    ggplot(datos, aes(x = x , y = y)) +
      geom_point(aes(color = periodoAux, shape = periodoAux)) +
      labs(subtitle = paste("Grupo de referencia ", grupo, " - ", subtitulo),
           y = "Saber Pro",
           x = "Saber 11",
           title = "Relación puntajes de las pruebas Saber Pro y Saber 11",
           col = "Periodo",
           shape = "Periodo"
      ) +
      geom_hline(yintercept = mediaPro, linetype="dashed", color = "#0B355C", size = 1.05, show.legend = T) +
      geom_vline(xintercept = media11, linetype="dashed", color = "#0B355C", size = 1.05) +
      geom_text(aes(x = media11, label = paste(round(media11, 0)), y = min(y) - 10),  #vertical
                colour = "#0B355C", angle = 0, hjust = 1.1,
                size = 5) +
      geom_text(aes(x = min(x) - 5, label = paste(round(mediaPro,0)), y = mediaPro), #horizontal
                colour = "#0B355C", angle = 0, vjust = 1.5,
                size = 5) +
      geom_text(aes(x = max(x), y = max(y), label = paste(round(pc1, 0), "%")),  #Proporción cuadrante1
                colour = "#0B355C", angle = 0, hjust = 1,
                size = 5) +
      geom_text(aes(x = min(x), y = max(y), label = paste(round(pc2, 0), "%")),  #Proporción cuadrante2
                colour = "#0B355C", angle = 0, hjust = 0,
                size = 5) +
      geom_text(aes(x = min(x), y = min(y), label = paste(round(pc3, 0), "%")),  #Proporción cuadrante3
                colour = "#0B355C", angle = 0, hjust = 0,
                size = 5) +
      geom_text(aes(x = max(x), y = min(y), label = paste(round(pc4, 0), "%")),  #Proporción cuadrante4
                colour = "#0B355C", angle = 0, hjust = 1,
                size = 5)
  } else{
    ##Pruebas##
    ggplot(datos, aes(x = x , y = y)) +
      geom_point(aes(color = periodoAux, shape = periodoAux)) +
      theme(legend.position="none") +
      #scale_x_continuous(limits = c(0, 100)) +
      #scale_y_continuous(limits = c(0, 300)) +
      labs(subtitle = paste(subtitulo),
           y = "Saber Pro",
           x = "Saber 11",
           col = "Periodo",
           shape = "Periodo"

      ) +
      geom_hline(yintercept = mediaPro, linetype="dashed", color = "#0B355C", size = 1.05, show.legend = T) +
      geom_vline(xintercept = media11, linetype="dashed", color = "#0B355C", size = 1.05) +

      geom_text(aes(x = media11, label = paste(round(media11, 0)), y = min(y) - 10),  #vertical
                colour = "#0B355C", angle = 0, hjust = 1.1,
                size = 5) +

      geom_text(aes(x = min(x)+1, label = paste(round(mediaPro,0)), y = mediaPro), #horizontal
                colour = "#0B355C", angle = 0, vjust = 1.5,
                size = 5) +
      geom_text(aes(x = max(x), y = max(y), label = paste(round(pc1, 0), "%")),  #Proporción cuadrante1
                colour = "#0B355C", angle = 0, hjust = 1,
                size = 5) +
      geom_text(aes(x = min(x), y = max(y), label = paste(round(pc2, 0), "%")),  #Proporción cuadrante2
                colour = "#0B355C", angle = 0, hjust = 0,
                size = 5) +
      geom_text(aes(x = min(x), y = min(y), label = paste(round(pc3, 0), "%")),  #Proporción cuadrante3
                colour = "#0B355C", angle = 0, hjust = 0,
                size = 5) +
      geom_text(aes(x = max(x), y = min(y), label = paste(round(pc4, 0), "%")),  #Proporción cuadrante4
                colour = "#0B355C", angle = 0, hjust = 1,
                size = 5)
  }
}


create_grs <- function(resultados, mediasSaber11,mediasSaberPro, grupo){

  g1 <- create_graph_general(resultados, mediasSaber11, mediasSaberPro, grupo, prueba = 'MOD_LECTURA_CRITICA_PUNT')
  g2 <- create_graph_general(resultados, mediasSaber11, mediasSaberPro, grupo, prueba = 'MOD_RAZONA_CUANTITAT_PUNT')
  g3 <- create_graph_general(resultados, mediasSaber11, mediasSaberPro, grupo, prueba = 'MOD_INGLES_PUNT')
  g4 <- create_graph_general(resultados, mediasSaber11, mediasSaberPro, grupo, prueba = 'PUNT_GLOBAL.x')
  grid.arrange(g1, g2, g3, g4,
               widths = c(2, 2, 2),
               heights = c(1.8,1, 0.4),
               layout_matrix = rbind(c(4, 4, NA),
                                     c(1, 2, 3),
                                     c(NA, NA, NA)))
}

