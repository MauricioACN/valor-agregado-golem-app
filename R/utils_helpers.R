#' helpers
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
#' @import shiny


card <- function(body, title) {
  div(class = "card",
      div(icon("chart-line", style = "color:white"), class = "card-header bg-success text-white text-center font-weight-bold", title),
      div(class = "card-body d-flex justify-content-center", body)
  )
}
