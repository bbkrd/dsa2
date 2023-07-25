#' Functions to be run when package is loaded
#'
#' Functions to be run when package is loaded
#' @param libname name of library
#' @param pkgname name of package


.onLoad <- function(libname, pkgname){
  result <- rJava::.jpackage(pkgname, lib.loc = libname)
  if (!result) stop("Loading java packages failed")
  
  
    jversion <- rJava::.jcall('java.lang.System','S','getProperty','java.version')
  if (jversion < "17") {
    stop(paste("Your java version is ", jversion,
               ".  N or higher. Use Sys.setenv to set the path", sep = ""))
  }

  
}