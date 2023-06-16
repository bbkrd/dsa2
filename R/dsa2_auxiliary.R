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


# Color palette
# https://mycolor.space/
.dsa2color <- function(color, ...){
 if (missing(...)){
   bc <- color
 } 
  else{
    bc <- c(color, ...)
  }
  colorset <- data.frame(blue = c("#3a6699"), 
  darkblue = c("#2f4858"),                      
  grey = c("#949098"), 
  gray = c("#949098"), 
  darkgrey = c("#727073"), 
  darkgray = c("#727073"), 
  yellow = c("#f9f34d"), 
  orange = c("#ef972c"), 
  brown = c("#98724d"), 
  red = c("#d54444"),
  violet = c("#934884"), 
  petrol = c("#006e7f"), 
  green = c("#00ac54"), 
  darkgreen = c("#007460"), 
  lightgreen = c("#91dc69"), 
  black = c("#000000"),  
  pink = c("#ff79a3"), stringsAsFactors = F)
  for (j in 1:length(bc)) {
    if (bc[j] %in% colnames(colorset)) {
      bc[j] <- gsub(bc[j], colorset[bc[j]], bc[j])
    }
  }
  bc <- gsub("\\t","",bc)
  return(bc)
}
  

#' Plot generic for dsa2
#' 
#' Plot generic for dsa2
#' @param ... additional parameters from plot() function
#' @author x
#' @export

.add_legend <- function(...) {
  opar <- par(fig=c(0, 1, 0, 1), oma=c(0, 0, 0, 0),
              mar=c(0, 0, 0, 0), new=TRUE)
  on.exit(par(opar))
  plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
  legend(...)
}

plot.dsa2 <- function(dsa2_object, main = "Result for seasonal adjustment of daily time series", ...) {
  opar <- par(no.readonly = TRUE)
  par(mar=c(4, 2, 2, 0.5), xpd=TRUE)
  dates <- zoo::index(dsa2_object$series)
  series1 <- as.numeric(dsa2_object$series[,1])
  series2 <- as.numeric(dsa2_object$series[,2])
  plot(dates, series1,type = "l", xlab = "", ylab = "", cex.axis = 0.75, bty = "n", ...)
  par(xpd = FALSE, cex.axis=0.75)
  abline(v = axis.Date(1,dates), col = .dsa2color("grey"), lty = 1, xaxt = "n")
  axis(2, tck = 1, col = .dsa2color("grey"), lty = 1)
  par(new = TRUE)
  plot(dates, series1, type = "l", xlab = "", ylab = "", 
       main = main, col = .dsa2color("darkblue"), bty= "n")
  lines(dates, series2, col = .dsa2color("red"))
  par(col.axis = "transparent")
  axis(1, col.ticks = dsa2color("grey"), axis.Date(1,dates))
  axis(2, col.ticks = dsa2color("grey"))
  box(col = .dsa2color("grey"))
  .add_legend("bottom", legend=c("Original", "Adjusted"), lty = c(1,1),
              col = .dsa2color("darkblue", "red"),
              horiz=TRUE, bty='n', cex=0.8)
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
#' @param dsa_object dsa2 output object
#' @author x
#' @export
.outOutlier <- function(dsa2_object){
  for (i in length(dsa2_object$preProcessing$model$variables)) {
    t_value <- dsa2_object$preProcessing$model$b / sqrt(dsa2_object$preProcessing$model$bcov[i,i])
  }
  dsa2_object$preProcessing$model$t <- t_value
  df <- rbind(dsa2_object$preProcessing$model$variables, dsa2_object$preProcessing$model$b,dsa2_object$preProcessing$model$t)
  df <- t(df)
  lookup <- data.frame(substr(dsa2_object$preProcessing$model$variables,4,nchar(dsa2_object$preProcessing$model$variables)))
  names(lookup) <- c("id")
  df <- cbind(df, lookup)
  names(df) <- c("o","coefficient","t_value","id")
  out <- subset(df, !grepl("x-", df$o) )
  dates <- zoo::index(dsa2_object$series)
  dates <- data.frame(dates)
  dates$id <- seq.int(nrow(dates))
  base <- (merge(lookup, dates, by = "id" ))
  df2 <- (merge(out, base, by = "id"))
  result <- data.frame(substr(df2$o,1,2))
  names(result) <- c("outliertype")
  df2 <- cbind(df2, result)
  df2 <- subset(df2, select=c("outliertype", "dates", "coefficient", "t_value"))  
  return(df2)
  }

.outCalendar <- function(dsa2_object){
  for (i in length(dsa2_object$preProcessing$model$variables)) {
    t_value <- dsa2_object$preProcessing$model$b / sqrt(dsa2_object$preProcessing$model$bcov[i,i])
  }
  dsa2_object$preProcessing$model$t <- t_value
  df <- rbind(dsa2_object$preProcessing$model$variables, dsa2_object$preProcessing$model$b,dsa2_object$preProcessing$model$t)
  df <- t(df)
  lookup <- data.frame(substr(dsa2_object$preProcessing$model$variables,4,nchar(dsa2_object$preProcessing$model$variables)))
  names(lookup) <- c("id")
  df <- cbind(df, lookup)
  names(df) <- c("o","coefficient","t_value","id")
  cal <- subset(df, grepl("x-", df$o) )
  cal2 <- subset(cal, select=c("o", "coefficient", "t_value"))  
  names(cal2) <- c("regressor", "coefficient", "t_value")
  return(cal2)  # TO DO: give regressors their proper titles
}

print.dsa2 <- function(dsa2_object) {
  cat("Pre-processing")
  cat("\n") ## New line
  cat("Fractional Airline Coefficients:")
  cat(dsa2_object$preProcessing$estimation$parameters)
  cat("\n") ## New line
  cat("\n") ## New line
  cat("Calendar Regressors") 
  cat("\n") ## New line
  print(.outCalendar(dsa2_object))
  cat("\n") ## New line
  cat("Outliers")
  cat("\n") ## New line
  print(.outOutlier(dsa2_object))
  cat("\n")
  # cat("Seasonality Test")
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
  
  opar <- par(no.readonly = TRUE)
  par(mar=c(4, 2, 2, 0.5), xpd=TRUE)
  dates <- zoo::index(result1)
  series1 <- as.numeric(result1[,1])
  series2 <- as.numeric(result1[,2])
  series3 <- as.numeric(result2[,2])
  plot(dates, series1,type = "l", xlab = "", ylab = "", cex.axis = 0.75, bty = "n")
  par(xpd = FALSE, cex.axis=0.75)
  abline(v = axis.Date(1,dates), col = .dsa2color("grey"), lty = 1, xaxt = "n")
  axis(2, tck = 1, col = .dsa2color("grey"), lty = 1)
  par(new = TRUE)
  plot(dates, series1, type = "l", xlab = "", ylab = "", 
       main = "Comparison", col = .dsa2color("darkblue"), bty= "n")
  lines(dates, series2, col = .dsa2color("red"))
  lines(dates, series3, col = .dsa2color("orange"))
  par(col.axis = "transparent")
  axis(1, col.ticks = dsa2color("grey"), axis.Date(1,dates))
  axis(2, col.ticks = dsa2color("grey"))
  box(col = .dsa2color("grey"))
  .add_legend("bottom", legend=c("Original", "Adjusted Series 1", "Adjusted Series 2"), lty = c(1,1),
              col = .dsa2color("darkblue", "red", "orange"),
              horiz=TRUE, bty='n', cex=0.8)
  on.exit(par(opar))
}


