#' @importFrom stats acf pacf spectrum
#' @noRd  
NULL 

#' Functions to be run when package is loaded
#'
#' Functions to be run when package is loaded.
#' @param libname A character value containing the name of the library.
#' @param pkgname A character value containing the name of the package.
#' @keywords internal

.onLoad <- function(libname, pkgname) {
  
  result <- tryCatch(rJava::.jpackage(pkgname, lib.loc = libname),
                     error = function(e) e)
  
  if (inherits(result, "error")) {
    stop("Loading java packages failed. You probably need to specify where your
         Java installation is stored using something like
         Sys.setenv('JAVA_HOME' = 'C:/Workspace/Java/JDK/jdk-17.0.3+7')")
  }
  
  # if (!result) stop("Loading java packages failed")
  
  jversion <- rJava::.jcall('java.lang.System','S','getProperty','java.version')
  if (jversion < "17") {
    stop(paste("Your java version is ", jversion,
               ".  We need 17 or higher. Use Sys.setenv to set the path", sep = ""))
  }

  classes <- system.file("java", package=pkgname, lib.loc=libname)
  jars    <- "splines-0.3.jar"
  rJava::.jaddClassPath(paste(classes, jars, sep = .Platform$file.sep))
}
