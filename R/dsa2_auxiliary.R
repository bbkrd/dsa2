delete_29 <- function(x) {
  x[format(zoo::index(x), "%m-%d") == "02-29"] <- NA
  x <- x[!is.na(x)]
}


Descaler <- function (x, y = NA, Diff = 0,  log = FALSE, Lag = NA) { # Copied from {dsa}
  .diffinv_xts <- function(x, y, lag = 1, differences = 1, 
                           stepsize = "days", ...) {
    if (all(class(y) != "xts")) {
      stop("The time series y needs to be an xts")
    }
    values = stats::diffinv(x[stats::complete.cases(x)], 
                            xi = y[1:(lag * differences)], lag = lag, differences = differences, 
                            ...)
    series = xts::xts(values, order.by = seq.Date(from = as.Date(stats::start(y)), 
                                                  by = stepsize, length.out = length(values)))
    return(series)
  }
  if (log) {
    ysave = y
    y = log(y)
  }
  if (Diff > 0) {
    if (any(class(x) == "ts")) {
      x <- stats::diffinv(x, differences = Diff, xi = y[1:(1 * 
                                                             Diff)])
    }
    else {
      x <- .diffinv_xts(x, y, differences = Diff)
    }
  }
  if (log) 
    x = exp(x)
  if (any(class(x) == "numeric")) {
    x <- xts::xts(x, order.by = seq.Date(from = as.Date(stats::start(y)), 
                                         by = "days", length.out = length(x)))
  }
  return(x)
}


Scaler <- function (x, Diff = 0, log = FALSE) { # Copied from {dsa}
  if (log) 
    x = log(x)
  if (Diff > 0) 
    x = diff(x, differences = Diff)
  return(stats::na.omit(x))
}


plot.dsa2 <- function(dsa2_object) {
  plot(dsa2_object$series, main = "Netter Titel")
}

print.dsa2 <- function(dsa2_object) {
  cat("Something interesting")
  cat("\n") ## New line
  cat(dsa2_object$parameters$h)
}


summary.dsa2 <- function() {
  
}


compare_plot <- function(dsa2_object1, dsa2_object2) {
  result1 <- head(dsa2_object1$series, 
                  nrow(dsa2_object1$series)-dsa2_object1$parameters$h)
  result2 <- head(dsa2_object2$series, 
                  nrow(dsa2_object2$series)-dsa2_object2$parameters$h)
  
  if (all(result1[,1] == result2[,2])) {
    result <- xts::merge.xts(result1, result2[,2])
  } else {
    result <- xts::merge.xts(result1, result2)
  }
  
  plot(result[,ncol(result):1], col=c("darkgrey", "red", "blue", "orange")[1:ncol(result)],
       main="Comparison")
}







