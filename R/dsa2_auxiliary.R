#' Delete February 29
#' 
#' Delete the observation on February 29 from a time series
#' @param x time series
#' @author Daniel Ollech


delete_29 <- function(x) {
  x[format(zoo::index(x), "%m-%d") == "02-29"] <- NA
  x <- x[!is.na(x)]
}


#' Internal function to invert taking logs and differences of a time series
#' 
#' For a series that has been logged and/or differenced, this function reverses these transformations.
#' @param x time series
#' @param y reference time series for inverting differencing
#' @param Diff number of differences to be taken
#' @param log should time series be logarithmised
#' @param Lag which Lag of differencing needs to be inverted
#' @details Function is used in dsa to handle the users choice of logs and levels.
#' @author Daniel Ollech


descaler <- function(x, y = NA, Diff = 0,  log = FALSE, Lag = NA) { # Copied from {dsa}
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


#' Internal function to take logs and differences of  a time series
#' 
#' Logarithmise and / or difference a time series 
#' @param x time series
#' @param Diff number of differences to be taken
#' @param log Should time series be logarithmised
#' @details Function is used in dsa to handle the users choice of logs and levels.
#' @author Daniel Ollech


scaler <- function(x, Diff = 0, log = FALSE) { # Copied from {dsa}
  if (log) 
    x = log(x)
  if (Diff > 0) 
    x = diff(x, differences = Diff)
  return(stats::na.omit(x))
}


#' Defining dsa2 colors
#' 
#' This function defines the colors to be used troughout the dsa2 package
#' @param color name of color
#' @param ... additional colors
#' @author Daniel Ollech
#' @examples dsa2:::.dsa2color("darkblue")


.dsa2color <- function(color, ...) {
 if (missing(...)) {
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
#' Creates a plot of original and seasonally adjusted series.
#' @param x dsa2 output object
#' @param include_forecasts display forecast data
#' @param main title of the plot
#' @param ... additional parameters from plot() function
#' @details The function uses the base plot package. 
#' @author Sindy Brakemeier, Daniel Ollech
#' @export

plot.dsa2 <- function(x, main = "Result for seasonal adjustment of daily time series", include_forecasts = FALSE, ...) {
  opar <- graphics::par(no.readonly = TRUE)
  graphics::par(mar = c(4, 2, 2, 0.5), xpd = TRUE)
  
  if (include_forecasts) {
    minus_h <- 0
  } else {
    minus_h <- x$parameters$h
  }
  x$series <- utils::head(x$series, 
                  nrow(x$series) - minus_h)
  
  dates <- zoo::index(x$series)
  series1 <- as.numeric(x$series[,1])
  series2 <- as.numeric(x$series[,2])
  plot(dates, series1,type = "l", xlab = "", ylab = "", cex.axis = 0.75, bty = "n", ...)
  graphics::par(xpd = FALSE, cex.axis = 0.75)
  graphics::abline(v = graphics::axis.Date(1,dates), col = .dsa2color("grey"), lty = 1, xaxt = "n")
  graphics::axis(2, tck = 1, col = .dsa2color("grey"), lty = 1)
  graphics::par(new = TRUE)
  plot(dates, series1, type = "l", xlab = "", ylab = "", 
       main = main, col = .dsa2color("darkblue"), bty = "n")
  graphics::lines(dates, series2, col = .dsa2color("red"))
  graphics::par(col.axis = "transparent")
  graphics::axis(1, col.ticks = .dsa2color("grey"), graphics::axis.Date(1,dates))
  graphics::axis(2, col.ticks = .dsa2color("grey"))
  graphics::box(col = .dsa2color("grey"))
  .add_legend("bottom", legend = c("Original", "Adjusted"), lty = c(1,1),
              col = .dsa2color("darkblue", "red"),
              horiz = TRUE, bty = 'n', cex = 0.8)
  on.exit(graphics::par(opar))
}

.add_legend <- function(...) {
  opar <- graphics::par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0),
              mar = c(0, 0, 0, 0), new = TRUE)
  on.exit(graphics::par(opar))
  plot(0, 0, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n')
  graphics::legend(...)
}


#' Summary generic for dsa2
#' 
#' Summary.dsa2 lists the coefficients of the fractional airline model as well as the coefficients and t-values of all outliers and calendar effects.
#' @param object dsa2 output object
#' @param ... further arguments to print
#' @author Sindy Brakemeier, Lea Hengen
#' @export
summary.dsa2 <- function(object, ...) {
  print(object)
}


#' Print generic for dsa2
#' 
#' Print generic for dsa2, lists the coefficients of the fractional airline model as well as the coefficients and t-values of all outliers and calendar effects.
#' @param x dsa2 output object
#' @param ... further arguments to print
#' @author Sindy Brakemeier, Lea Hengen
#' @export


print.dsa2 <- function(x, ...) {
  handle <- list(...) # Just used to ensure consistency with generic print
  frac <- toString(round(x$preProcessing$estimation$parameters,3))
  if (is.null(x$parameters$xreg)) {
    calends <- .outCalendar(x)
  } else {
    calends <- .outCalendar(x)
    the_name <- ifelse(max(nchar(colnames(x$parameters$xreg))) < 8, # ensures that the regression results are printed nicely
                       "\tregs",
                       "\tregressors")
    calends <- rbind(c(the_name, "coef", "t-value") ,
                     calends)
    calends$sep <- "\n"
    calends <- paste(t(calends), collapse = "\t")
  }
  
  if (all(x$preProcessing$model$component_outliers == 0)) {
    outlier <- .outOutlier(x)
  } else
  { outlier <- .outOutlier(x)
    outlier$dates <- as.character(outlier$dates)
    outlier <- rbind(c("\ttype", "dates", "\tcoef", "t-value"), 
                   outlier)
    outlier$sep <- "\n"
    outlier <- paste(t(outlier), collapse = "\t")
  }
  
  out <- paste0("Pre-processing\n
Fractional Airline Coefficients: ", 
                frac,"\n
Calendar Regressors:\n\n",
                calends,"\n
Outliers:\n\n",
                outlier)
  cat(out)
  invisible(out)
}

#' Internal function for outliers
#' 
#' Internal function to polish the output for outliers
#' @param dsa2_object dsa2 output object
#' @author Sindy Brakemeier, Lea Hengen

.outOutlier <- function(dsa2_object) {
  if (all(dsa2_object$preProcessing$model$component_outliers == 0)) {
    return("No outliers found")
  } else {
    for (i in length(dsa2_object$preProcessing$model$variables)) {
      t_value <- dsa2_object$preProcessing$model$b / sqrt(dsa2_object$preProcessing$model$bcov[i,i])
    }
    dsa2_object$preProcessing$model$t <- t_value
    df <- rbind(dsa2_object$preProcessing$model$variables, sprintf("%.3f",round(dsa2_object$preProcessing$model$b, 3)), sprintf("%.3f",round(dsa2_object$preProcessing$model$t,3)))
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
    df2 <- subset(df2, select = c("outliertype", "dates", "coefficient", "t_value"))  
    return(df2)
  }
}

#' Internal function for calendars
#' 
#' Internal function to polish the output for calendars
#' @param dsa2_object dsa2 output object
#' @author Sindy Brakemeier, Lea Hengen

.outCalendar <- function(dsa2_object) {
  if (is.null(dsa2_object$parameters$xreg)) {
    return("No calendar adjustment conducted")
  } else {
    for (i in length(dsa2_object$preProcessing$model$variables)) {
      t_value <- dsa2_object$preProcessing$model$b / sqrt(dsa2_object$preProcessing$model$bcov[i,i])
    }
    dsa2_object$preProcessing$model$t <- t_value
    df <- rbind(dsa2_object$preProcessing$model$variables, sprintf("%.3f",round(dsa2_object$preProcessing$model$b,3)), sprintf("%.3f",round(dsa2_object$preProcessing$model$t,3)))
    df <- t(df)
    lookup <- data.frame(substr(dsa2_object$preProcessing$model$variables,4,nchar(dsa2_object$preProcessing$model$variables)))
    names(lookup) <- c("id")
    df <- cbind(df, lookup)
    names(df) <- c("o","coefficient","t_value","id")
    cal <- subset(df, grepl("x-", df$o) )
    cal$o <- colnames(dsa2_object$parameters$xreg)
    cal2 <- subset(cal, select = c("o", "coefficient", "t_value"))  
    names(cal2) <- c("regressor", "coefficient", "t_value")
    return(cal2)  
  }
}




#' Function to compare two dsa2 results
#' 
#' Create a plot of the original and adjusted series from two different adjustments
#' @param dsa2_object1 first dsa2 output object
#' @param dsa2_object2 second dsa2 output object
#' @param include_forecasts should forecasts be depicted?
#' @examples set.seed(2358)
#' all <- tssim::sim_daily(N = 5)
#' series <- all$original
#' result <- dsa(series, outliers = NULL)
#' result2 <- dsa(series,s7 = "x11", pre_processing = result)
#' compare_plot(result, result2)
#' @author Daniel Ollech
#' @export


compare_plot <- function(dsa2_object1, dsa2_object2, include_forecasts = FALSE) {
  if (include_forecasts) {
    minus_h <- 0
  } else {
    minus_h <- dsa2_object1$parameters$h
  }
  result1 <- utils::head(dsa2_object1$series, 
                  nrow(dsa2_object1$series) - minus_h)
  result2 <- utils::head(dsa2_object2$series, 
                  nrow(dsa2_object2$series) - minus_h)
  
  name1 <- deparse(substitute(dsa2_object1))
  name2 <- deparse(substitute(dsa2_object2))
  
  opar <- graphics::par(no.readonly  =  TRUE)
  graphics::par(mar = c(4, 2, 2, 0.5), xpd = TRUE)
  dates <- zoo::index(result1)
  series1 <- as.numeric(result1[,1])
  series2 <- as.numeric(result1[,2])
  series3 <- as.numeric(result2[,2])
  plot(dates, series1,type = "l", xlab = "", ylab = "", cex.axis = 0.75, bty = "n")
  graphics::par(xpd = FALSE, cex.axis = 0.75)
  graphics::abline(v = graphics::axis.Date(1,dates), col = .dsa2color("grey"), lty = 1, xaxt = "n")
  graphics::axis(2, tck = 1, col = .dsa2color("grey"), lty = 1)
  graphics::par(new = TRUE)
  plot(dates, series1, type = "l", xlab = "", ylab = "", 
       main = "Comparison", col = .dsa2color("darkblue"), bty = "n")
  graphics::lines(dates, series2, col = .dsa2color("red"))
  graphics::lines(dates, series3, col = .dsa2color("green"))
  graphics::par(col.axis = "transparent")
  graphics::axis(1, col.ticks = .dsa2color("grey"), graphics::axis.Date(1,dates))
  graphics::axis(2, col.ticks = .dsa2color("grey"))
  graphics::box(col = .dsa2color("grey"))
  .add_legend("bottom", legend = c("Original", paste0("Adjusted Series (", name1, ")"), paste0("Adjusted Series (", name2, ")")), lty = c(1,1),
              col = .dsa2color("darkblue", "red", "green"),
              horiz = TRUE, bty = 'n', cex = 0.8)
  on.exit(graphics::par(opar))
}


#' HTML output for dsa2
#' 
#' HTML output for dsa2
#' @param dsa2_object output object
#' @param path path for HTML output
#' @details Generates a .Rmd file that is rendered into an html document saved in the working directory.
#' @author Lea Hengen, Sindy Brakemeier
#' @export

output <- function(dsa2_object, path = NULL) {
  if(is.null(path)) {
    path <- getwd()
  }
  
  filename <- paste0(path, "/", dsa2_object$parameters$name, ".Rmd")
  
  if (dsa2_object$parameters$log == TRUE) {
    model <- "multiplicative"
  } else {
    model <- "additive"
  }
  
  if (is.null(dsa2_object$parameters$xreg)) {
    regressors <- "not in use"
  } else{
    regressors <- colnames(dsa2_object$parameters$xreg)
  }
  
  if (is.null(dsa2_object$parameters$outliers)) {
    outliers <- "not in use"
  } else{
    outliers <- dsa2_object$parameters$outliers
  }
  
  if (is.null(dsa2_object$parameters$pre_processing)) {
    pre_processing_ex <- "not in use"
  } else {
    pre_processing_ex <- "in use"
  }
  
  if (is.null(dsa2_object$parameters$s31)) {
    model_s31 <- "none"
  } else{
    model_s31 <- dsa2_object$parameters$s31
  }
  
  out <- summary.dsa2(dsa2_object)
  
  cat("---
title: \"Title\"
date: \'`r Sys.Date()`\'
output: html_document
---
**Time series information**
\n
      Name: `r dsa2_object$parameters$name`       
      Length: from `r zoo::index(dsa2_object$series)[1]` to `r zoo::index(dsa2_object$series)[length(zoo::index(dsa2_object$series))-dsa2_object$parameters$h]`     
      Number of values: `r length(zoo::index(dsa2_object$series)) - dsa2_object$parameters$h` 
**Parameters**
\n
      Number of iterations: `r dsa2_object$parameters$n_iterations`
      Model: `r model`
      Length of forecast: `r dsa2_object$parameters$h` days
      Calendar regressors: `r regressors`
      Outlier types: `r outliers`
      External pre-processing: `r pre_processing_ex`
      Interpolation method: `r dsa2_object$parameters$interpolator`
      Adjustment method day-of-the-week: `r dsa2_object$parameters$s7`
      Adjustment method day-of-the-month: `r model_s31`
      Adjustment method day-of-the-year: `r dsa2_object$parameters$s365`
\n
**Summary**
\n
```{r, echo=FALSE}
summary.dsa2(dsa2_object)
``` 
\n
      
```{r, echo=FALSE}
interactive_time <- dygraphs::dygraph(dsa2_object$series, 
                                      main = 'Result for seasonal adjustment of daily time series') 
interactive_time <- dygraphs::dyRangeSelector(interactive_time)
interactive_time <- dygraphs::dyOptions(interactive_time, 
                                        colors = .dsa2color('darkblue','red'))
interactive_time
```      
      ",
      
      file = filename) 
  rmarkdown::render(filename)
  file.remove(filename) #Removes the Rmd-File
}

