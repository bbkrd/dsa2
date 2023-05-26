#' Functions to be run when package is loaded
#'
#' Functions to be run when package is loaded
#' @author Daniel Ollech

.onLoad <- function(libname, pkgname){
  Sys.setenv("JAVA_HOME"="C:\\Workspace\\Java\\JDK\\jdk-17.0.3+7")
  .libPaths("C:\\Workspace\\R\\JD_lib")
  message("Your JAVA_HOME has been set to")
  message(Sys.getenv("JAVA_HOME"))
  message("\nYour .libPaths has been changed to")
  message("C:\\Workspace\\R\\JD_lib")
}