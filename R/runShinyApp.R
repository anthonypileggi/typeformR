#' Run a Shiny App included within this package
#'
#' @param myapp The name of the app you want to run.
#'    Submit `runShinyApp()` to see all available apps.
#'
#' @export
runShinyApp <- function(myapp) {
  # locate all the shiny app examples that exist
  validExamples <- list.files(system.file("shiny-apps", package = "typeformR"))

  validExamplesMsg <-
    paste0(
      "Valid apps are: \n '",
      paste(validExamples, collapse = "'\n '"),
      "'")

  # if an invalid example is given, throw an error
  if (missing(myapp) || !nzchar(myapp) ||
      !myapp %in% validExamples) {
    stop(
      'Please run `runShinyApp()` with a valid example app as an argument. \n',
      validExamplesMsg,
      call. = FALSE)
  }

  # find and launch the app
  appDir <- system.file("shiny-apps", myapp, package = "typeformR")
  shiny::runApp(appDir, display.mode = "normal")
}