#' Launch the packaged Shiny app
#' 
#' @export
run_yarra_rap <- function() {
  app_dir <- system.file("app", package = "yarrariver")
  shiny::runApp(app_dir, display.mode = "normal")
}