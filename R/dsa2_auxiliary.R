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


#' Plot generic for dsa2
#' 
#' Plot generic for dsa2
#' @param ... additional parameters from plot() function
#' @author x
#' @export

plot.dsa2 <- function(dsa2_object, main = "Result for seasonal adjustment of daily time series", ...) {
  opar <- par(no.readonly = TRUE)
  par(mar=c(10, 1.75, 1.75, 0.5), xpd=TRUE)
  dates <- zoo::index(dsa2_object$series)
  series1 <- as.numeric(dsa2_object$series[,1])
  series2 <- as.numeric(dsa2_object$series[,2])
  plot(dates, series1,type = "l", xlab = "", ylab = "", cex.axis = 0.75, bty = "n", ...)
  legend("bottom", inset=c(0, -0.3), col=c("#2F4858", "#D54444"), lty=c(1,1), 
         legend=c("Original", "Adjusted"), box.lty=0, horiz=TRUE)
  par(xpd = FALSE, cex.axis=0.75)
  abline(v = axis.Date(1,dates), col = "#949098", lty = 1, xaxt = "n")
  axis(2, tck = 1, col = "#949098", lty = 1)
  par(new = TRUE)
  plot(dates, series1, type = "l", xlab = "", ylab = "", 
       main = main, col="#2F4858", bty= "n")
  lines(dates, series2, col="#D54444")
  par(col.axis="transparent")
  axis(1, col.ticks = "#949098", axis.Date(1,dates))
  axis(2, col.ticks = "#949098")
  box(col = "#949098")
  on.exit(par(opar))
}


#' Summary generic for dsa2
#' 
#' Summary generic for dsa2
#' @param x
#' @author x
#' @export
summary.dsa2 <- function() {
  
}


#' Print generic for dsa2
#' 
#' Print generic for dsa2
#' @param x
#' @author x
#' @export

print.dsa2 <- function(dsa2_object) {
  cat("Something interesting")
  cat("\n") ## New line
  cat(dsa2_object$parameters$h)
}



#' Function to compare two dsa2 results
#' 
#' Create a plot of the original and adjusted series from two different adjustments
#' @param dsa2_object1 first dsa2 output object
#' @param dsa2_object2 second dsa2 output object
#' @example set.seed(2358)
#' all <- tssim::sim_daily(N=5)
#' series <- all$original
#' result <- dsa2(series, outliers=NULL)
#' result2 <- dsa2(series,s7 = "x11", pre_processing = result)
#' compare_plot(result, result2)
#' @author Daniel Ollech
#' @export

compare_plot <- function(dsa2_object1, dsa2_object2, include_forecasts=FALSE) {
  if (include_forecasts) {
    minus_h <- 0
  } else {
    minus_h <- dsa2_object1$parameters$h
  }
  result1 <- head(dsa2_object1$series, 
                  nrow(dsa2_object1$series)-minus_h)
  result2 <- head(dsa2_object2$series, 
                  nrow(dsa2_object2$series)-minus_h)
  
  plot(result1[,1], type="l", col="#2F4858", main="Comparison", lwd=2) # https://mycolor.space/
  if (!all(result1[,1] == result2[,1])) {
    lines(result2[,1], col="#BFA5A2", lwd=2)
  }
  lines(result1[,2], col="#D54444", lwd=2)
  lines(result2[,2], col="#80AFE1", lwd=2)
}




