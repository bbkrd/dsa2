#' Interpolator
#' 
#' Interpolates values for non-existent dates e.g. 29.02.2000.
#' @param series An `xts` object containing the time series to be adjusted.
#' @param interpolator A character value set to `"CUBIC_SPLINE"` or `"NONE"` (`"NONE"` if STL is used, `"CUBIC_SPLINES"` else) to define the 
#'  interpolation method for the day-of-the-month effect (see details).
#' @author Thomas Witthohn
#' @export


interpolate31 <- function(series, interpolator = "CUBIC_SPLINE") {
  
  first <- xts::first(series)
  year  <- xts::.indexyear(first) + 1900
  month <- xts::.indexmon(first) + 1
  day   <- xts::.indexmday(first)
  date  <- rJava::.jcall(
      "java/time/LocalDate",
      "Ljava/time/LocalDate;",
      "of",
      as.integer(year),
      as.integer(month),
      as.integer(day)
      )
  
  calculator <- rJava::.jnew("de/bundesbank/splines/Calculator")
  values     <- rJava::.jarray(as.double(series))
  jrslt      <- calculator$interpolate(date, values, interpolator)
  return(jrslt)
}


#' Invert interpolation
#' 
#' Removes interpolated values for non-existent dates e.g. 29.02.2000.
#' @param originalSeries An `xts` object containing the reference time series.
#' @param interpolatedVector An `xts` object containing the reference time series including interpolated values.
#' @author Thomas Witthohn
#' @export
#' 

reduce31 <- function(originalSeries, interpolatedVector) {
  
  first <- xts::first(originalSeries)
  year  <- xts::.indexyear(first) + 1900
  month <- xts::.indexmon(first) + 1
  day   <- xts::.indexmday(first)
  date  <- rJava::.jcall(
      "java/time/LocalDate",
      "Ljava/time/LocalDate;",
      "of",
      as.integer(year),
      as.integer(month),
      as.integer(day)
      )
  
  calculator <- rJava::.jnew("de/bundesbank/splines/Calculator")
  values     <- rJava::.jarray(interpolatedVector)
  jrslt     <- calculator$reduce(date, values)
  return(jrslt)
}
