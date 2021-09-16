#' @importFrom shiny addResourcePath
#'
.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath("www", system.file("www", package = "axelRod"))
}