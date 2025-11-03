## File: R/run_yarra_app.R

#' Launch the Yarra River Shiny app
#'
#' Opens the packaged Shiny app found under inst/app.
#'
#' @export
run_yarra_app <- function(...) {
  app_dir <- system.file("app", package = "yarrariver")
  if (identical(app_dir, "") || !dir.exists(app_dir)) {
    stop("Could not find app directory in the installed package. ",
         "Ensure `inst/app/` exists before install.", call. = FALSE)
  }
  shiny::runApp(app_dir, display.mode = "normal", launch.browser = TRUE)
}

