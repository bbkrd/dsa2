#' Interpolator
#' 
#' Interpolates values for non-existent dates e.g. 29.02.2000
#' @param series time series
#' @param interpolator Either "CUBIC_SPLINE" or "NONE"
#' @author Thomas Witthohn
#' @export

interpolate31 <- function(series, interpolator = "CUBIC_SPLINE") {
  first <- xts::first(series)
  year <- xts::.indexyear(first) + 1900
  month <- xts::.indexmon(first) + 1
  day <- xts::.indexmday(first)
  date <-
    rJava::.jcall(
      "java/time/LocalDate",
      "Ljava/time/LocalDate;",
      "of",
      as.integer(year),
      as.integer(month),
      as.integer(day)
    )
  
  calculator <- rJava::.jnew("de/bundesbank/splines/Calculator")
  values <- rJava::.jarray(as.double(series))
  jrslt <- calculator$interpolate(date, values, interpolator)
  return(jrslt)
}


#' Invert interpolation
#' 
#' Removes interpolated values for non-existent dates e.g. 29.02.2000
#' @param originalSeries reference series
#' @param interpolatedVector series including interpolated values
#' @author Thomas Witthohn
#' @export

reduce31 <- function(originalSeries, interpolatedVector) {
  first <- xts::first(originalSeries)
  year <- xts::.indexyear(first) + 1900
  month <- xts::.indexmon(first) + 1
  day <- xts::.indexmday(first)
  date <-
    rJava::.jcall(
      "java/time/LocalDate",
      "Ljava/time/LocalDate;",
      "of",
      as.integer(year),
      as.integer(month),
      as.integer(day)
    )
  
  calculator <- rJava::.jnew("de/bundesbank/splines/Calculator")
  values <- rJava::.jarray(interpolatedVector)
  jrslt <- calculator$reduce(date, values)
  return(jrslt)
}