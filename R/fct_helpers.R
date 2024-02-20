#' helpers
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' @importFrom dplyr mutate filter select group_by summarise n arrange group_by_
#' @importFrom tidyr all_of
#' @import ggplot2
#' @importFrom rlang sym
#' @importFrom thematic thematic_on
#' @importFrom stringr str_to_title
#' @importFrom stats na.omit
#' @importFrom ggplot2 ggplot aes coord_flip theme element_text geom_text scale_y_continuous

thematic::thematic_on(bg="auto",fg="auto",accent = "auto", font = "auto")

grafico_bar_hor <- function(nivel,variable_x){

  theme_set(theme_bw())

  ggplot(nivel, aes_string(x = variable_x, y = "porcentaje")) +
    geom_bar(stat = "identity",
             fill="#87CEEB"
    ) +
    coord_flip() +
    theme(axis.text.y = element_text(face="bold", size=10, angle=0),
          panel.background = element_rect(fill = "#FAFAFA"),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 10, face = "bold")
    ) +
    geom_text(aes(label = paste(round(porcentaje,1), "%")), hjust = -0.1) +
    # labs(title = "Modalidad",
    #      caption = "P: Presencial, DV: Distancia virtual, D: Distancia") +
    scale_y_continuous(limit = c(0,1.3*max(nivel$porcentaje)))
}

summary_plot <- function(datos, name_variable){

  if (name_variable=="Genero"){

    nivel <- datos %>% group_by(ESTU_GENERO) %>% summarise(n())
  }
  else if (name_variable=="areaResidencia"){

    nivel <- datos %>% group_by(ESTU_AREARESIDE) %>% summarise(n())
  }
  else if (name_variable=="modalidad"){

    nivel <- datos %>% group_by(ESTU_METODO_PRGM) %>% summarise(n())
  }
  else if (name_variable=="grupoReferencia"){

    nivel <- datos %>%
      group_by(GRUPOREFERENCIA) %>%
      summarise(conteo = n()) %>%
      arrange(conteo) %>%
      mutate(GRUPOREFERENCIA = factor(GRUPOREFERENCIA, GRUPOREFERENCIA))
  }
  else if (name_variable=="Semestre"){

    nivel <- datos %>% group_by(ESTU_SEMESTRECURSA) %>% summarise(n())
  }

  colnames(nivel) <- c(name_variable, "Estudiantes")
  nivel <- na.omit(nivel)
  total <- sum(nivel$Estudiantes)
  nivel <- nivel %>% mutate(porcentaje = Estudiantes / total *100)

  if (name_variable=='Genero'){
    nivel <- nivel %>% mutate(Genero = paste("Género", Genero))
  }

  nivel

}


clean_resultados <- function(data, n_sample=NA, grupo=NA){

  if (!is.na(n_sample)){
    datos_clean = data %>% sample_n(size = n_sample) %>% filter(!!sym("GRUPOREFERENCIA") == grupo)
  }
  else{
    datos_clean <- data %>% filter(!!sym("GRUPOREFERENCIA") == grupo)
  }

  return(datos_clean)

}


calculate_mean_pro <- function(mediasSaberPro,grupo){
  ### si se selecciona mas de un grupo de referencia se va a comprar contra el promedio Nacional
  if (length(grupo)>1) {

    mediaPro = mediasSaberPro %>% filter(GRUPOREFERENCIA=='Grupo Referencia Nacional')

  }

  else{
    mediaPro <- mediasSaberPro %>% filter(!!sym("GRUPOREFERENCIA") %in% grupo)
  }

  return(mediaPro)
}

calculate_mean_11 <- function(media11,periodo){

    # media11_mean <- media11 %>% filter(periodoAux == periodo) %>% colMeans(.)
    media11_mean <- media11 %>%
      filter(periodoAux == periodo) %>%
      dplyr::select(-c(periodoAux)) %>%
      colMeans(.)


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


create_graph_general_var <- function(datos, mediasSaber11, mediasSaberPro, prueba){

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
    ggplot(datos, aes(x = x , y = y)) +
      geom_point(aes(color = periodoAux, shape = periodoAux)) +
      labs(
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

}

# create_grs <- function(resultados, mediasSaber11,mediasSaberPro, grupo){
#
#   g1 <- create_graph_general(resultados, mediasSaber11, mediasSaberPro, grupo, prueba = 'MOD_LECTURA_CRITICA_PUNT')
#   g2 <- create_graph_general(resultados, mediasSaber11, mediasSaberPro, grupo, prueba = 'MOD_RAZONA_CUANTITAT_PUNT')
#   g3 <- create_graph_general(resultados, mediasSaber11, mediasSaberPro, grupo, prueba = 'MOD_INGLES_PUNT')
#   g4 <- create_graph_general(resultados, mediasSaber11, mediasSaberPro, grupo, prueba = 'PUNT_GLOBAL.x')
#   grid.arrange(g1, g2, g3, g4,
#                widths = c(2, 2, 2),
#                heights = c(1.8,1, 0.4),
#                layout_matrix = rbind(c(4, 4, NA),
#                                      c(1, 2, 3),
#                                      c(NA, NA, NA)))
# }

graficos_distribucion_modelado <- function(datos, variable,nivel_modelo){

  if (variable %in% c('inse_imputado','Global.11','Global.11.Z')) {

    ggplot(datos, aes_string(x=variable, fill = 'treat',colour = 'treat')) +
      geom_density(alpha = 0.5,bw = "bcv")
    }

  else{

    ggplot(datos, aes_string(x=variable, fill = 'treat')) +
      geom_bar(aes(y = (..count..)/sum(..count..)), position = "dodge")+
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
      scale_y_continuous(name = "Proporción")+
      scale_fill_discrete(name = "Contrastes",
                          labels=c(
                            'Grupo Comparación',
                            nivel_modelo))

  }

}


calculate_values_for_text_herp = function(prueba,mediasSaberPro,mediasSaber11,datos) {
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

  return(list(pc1= pc1, pc2 = pc2, pc3 = pc3, pc4 = pc4))
}

