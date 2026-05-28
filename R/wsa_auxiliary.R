
#' Internal function for Fractional Airline model
#'
#' Internal function to polish the output for the fractional airline model.
#' Inspired by dsa2 functions.
#' @param x A `wsa` object.
#' @param digits An integer value specifying the number of digits to be displayed.
#' @author Carolin Schaaff
#' @keywords internal


.outARIMA.wsa <- function(x, digits = 3) {

  coefs <- x$preProcessing$estimation$parameters
  covar <- x$preProcessing$estimation$covariance

  # compute standard errors and t-values
  sterrs <- sqrt(diag(covar))
  tvals  <- coefs / sterrs

  df <- data.frame(
    "Regressor"   = c("theta", "theta 52.18"),
    "Coefficient" = format(round(coefs,  digits),  n_small = digits),
    "Std.Error"   = format(round(sterrs, digits),  n_small = digits),
    "t-Value"     = format(round(tvals,  digits),  n_small = digits)
  )

  return(df)
}


#' Internal function for calendars
#'
#' Internal function to polish the output for calendars.
#' Inspired by dsa2 functions.
#' @param x A `wsa` object.
#' @param digits An integer value specifying the number of digits to be displayed.
#' @author Carolin Schaaff
#' @keywords internal


.outCalendar.wsa <- function(x, digits = 3) {

  if (is.null(x$parameters$xreg)) {
    return(NULL)
  }

  dates <- zoo::index(x$series)             # dates of time series
  xreg  <- x$parameters$xreg                # calendar matrix
  vars  <- x$preProcessing$model$variables  # outlier/calendar vars
  coefs <- x$preProcessing$model$b          # coefficients
  covar <- x$preProcessing$model$bcov       # covariance matrix

  # remove outliers
  n     <- ncol(xreg)
  vars  <- vars[1:n]
  coefs <- coefs[1:n]
  covar <- covar[1:n, 1:n]

  # compute standard errors and t-values
  if (length(coefs) == 1) {
    sterrs <- sqrt(covar)
  } else {
    sterrs <- sqrt(diag(covar))
  }
  tvals  <- coefs / sterrs

  df <- data.frame(
    "Regressor"   = colnames(xreg),
    "Coefficient" = format(round(coefs,  digits), n_small = digits),
    "Std.Error"   = format(round(sterrs, digits), n_small = digits),
    "t-Value"     = format(round(tvals,  digits), n_small = digits)
  )

  return(df)
}


#' Internal function for outliers
#'
#' Internal function to polish the output for outliers.
#' Inspired by dsa2 functions.
#' @param x A `wsa` object.
#' @param digits An integer value specifying the number of digits to be displayed.
#' @author Carolin Schaaff
#' @keywords internal


.outOutlier.wsa <- function(x, digits = 3) {

  if (x$parameters$log) {
    noOutliers <- all(x$preProcessing$model$component_outliers == 1)
  } else {
    noOutliers <- all(x$preProcessing$model$component_outliers == 0)
  }

  # no outliers found
  if (noOutliers) {
    return(NULL)
  }

  # auxiliary variables
  dates <- zoo::index(x$series)             # dates of time series
  xreg  <- x$parameters$xreg                # calendar matrix
  vars  <- x$preProcessing$model$variables  # outlier/calendar vars
  coefs <- x$preProcessing$model$b          # coefficients
  covar <- x$preProcessing$model$bcov       # covariance matrix

  # remove calendar variables
  n <- ncol(xreg)
  if (!is.null(n)) {
    vars  <- vars[-c(1:n)]
    coefs <- coefs[-c(1:n)]
    covar <- covar[-c(1:n),-c(1:n)]
  }

  # extract types and dates of outliers from 'vars'
  outlierTypes <- substr(vars, 1, 2)
  outlierDates <- dates[as.numeric(substr(vars, 4, nchar(vars)))]

  # compute standard errors and t-values
  if (length(coefs) == 1) {
    sterrs <- sqrt(covar)
  } else {
    sterrs <- sqrt(diag(covar))
  }
  tvals  <- coefs / sterrs

  df <- data.frame(
    "Date"        = as.character(outlierDates),
    "Type"        = outlierTypes,
    "Coefficient" = format(round(coefs,  digits), n_small = digits),
    "Std.Error"   = format(round(sterrs, digits), n_small = digits),
    "t-Value"     = format(round(tvals,  digits), n_small = digits)
  )
  df <- df[order(outlierDates),]

  return(df)
}


#' Print for `wsa` objects
#'
#' Lists the coefficients of the fractional airline model
#' as well as the coefficients and t-values of all outliers and calendar effects.
#' Inspired by dsa2 functions.
#' @param x A `wsa` object.
#' @param ... Further arguments to print.
#' @author Carolin Schaaff
#' @export
#' @method print wsa


print.wsa <- function(x, ...) {
  
  handle <- list(...) # Just used to ensure consistency with generic print

  # auxiliary function to format the print objects
  print_format <- function(y) {
    y     <- rbind(colnames(y), y)         # add column names as observations
    y$sep <- "\n"                          # add line breaks
    y     <- apply(y, 2, format)           # apply format over each column
    y     <- paste(t(y), collapse = "\t")  # paste together
    return(y)
  }

  # call auxiliary functions to prepare string outputs
  fracARIMA <- .outARIMA(x)
  calendar  <- .outCalendar(x)
  outliers  <- .outOutlier(x)

  # format string outputs
  fracARIMA <- print_format(fracARIMA)

  outliers  <- ifelse(is.null(outliers),
                      "No outliers found",
                      print_format(outliers))

  calendar  <- ifelse(is.null(calendar),
                      "No calendar adjustment conducted",
                      print_format(calendar))

  # paste together for output string
  out <- paste0("Pre-processing",
                "\n\n",
                "Fractional Airline Model:",
                "\n\n\t",
                fracARIMA,
                "\n\n",
                "Calendar Regressors:",
                "\n\n\t",
                calendar,
                "\n\n",
                "Outliers:",
                "\n\n\t",
                outliers)

  cat(out)
  invisible(out)
}


#' Plot for `wsa` objects
#'
#' Creates a plot of original and seasonally adjusted series.
#' Aligned with dsa2 functions.
#' @param x A `wsa` object.
#' @param include_forecasts A logical value indicating whether to display forecast data.
#' @param main A character value detailing the title of the plot.
#' @param ... Additional parameters from `plot()` function
#' @details The function uses the base plot package.
#' @author Carolin Schaaff
#' @export
#' @method plot wsa


plot.wsa <- function(x, 
                     main = "Result for seasonal adjustment of weekly time series", 
                     include_forecasts = FALSE, ...) {
  
  opar <- graphics::par(no.readonly = TRUE)
  graphics::par(mar = c(4, 2, 2, 0.5), xpd = TRUE)

  if (include_forecasts) {
    minus_h <- 0
  } else {
    minus_h <- x$parameters$h
  }
  
  x$series <- utils::head(x$series,
                          nrow(x$series) - minus_h)
  dates    <- zoo::index(x$series)
  series1  <- as.numeric(x$series[,1])
  series2  <- as.numeric(x$series[,2])
  
  plot(dates, series1, 
       type = "l", xlab = "", ylab = "", 
       cex.axis = 0.75, bty = "n", ...)
  graphics::par(xpd = FALSE, cex.axis = 0.75)
  graphics::abline(v = graphics::axis.Date(1,dates), 
                   col = .dsa2color("grey"), 
                   lty = 1, xaxt = "n")
  graphics::axis(2, tck = 1, 
                 col = .dsa2color("grey"), lty = 1)
  graphics::par(new = TRUE)
  plot(dates, series1, 
       type = "l", xlab = "", ylab = "",
       main = main, col = .dsa2color("darkblue"), 
       bty = "n")
  graphics::lines(dates, series2, 
                  col = .dsa2color("red"))
  graphics::par(col.axis = "transparent")
  graphics::axis(1, col.ticks = .dsa2color("grey"), 
                 graphics::axis.Date(1,dates))
  graphics::axis(2, col.ticks = .dsa2color("grey"))
  graphics::box(col = .dsa2color("grey"))
  
  .add_legend("bottom", legend = c("Original", "Adjusted"), 
              lty = c(1,1),
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


#' Function to compare two `wsa` results
#'
#' Create a plot of the original and adjusted series from two different
#' adjustments. Inspired by dsa2 functions.
#' @param x1 First `wsa` object.
#' @param x2 Second `wsa` object.
#' @param include_forecasts A logical value indicating whether to display forecast data.
#' @author Carolin Schaaff
#' @export
#' @method compare_plot wsa


compare_plot.wsa <- function(x1, 
                             x2, 
                             include_forecasts = FALSE) {
  
  if (include_forecasts) {
    minus_h <- 0
  } else {
    minus_h <- x1$parameters$h
  }
  
  result1 <- utils::head(x1$series,
                         nrow(x1$series) - minus_h)
  result2 <- utils::head(x2$series,
                         nrow(x2$series) - minus_h)

  name1 <- deparse(substitute(x1))
  name1 <- substring(name1,1,15)
  name2 <- deparse(substitute(x2))
  name2 <- substring(name2,1,15)

  opar    <- graphics::par(no.readonly  =  TRUE)
  graphics::par(mar = c(4, 2, 2, 0.5), xpd = TRUE)
  dates   <- zoo::index(result1)
  series1 <- as.numeric(result1[,1])
  series2 <- as.numeric(result1[,2])
  series3 <- as.numeric(result2[,2])
  
  plot(dates, series1, 
       type = "l", xlab = "", ylab = "", 
       cex.axis = 0.75, bty = "n")
  graphics::par(xpd = FALSE, cex.axis = 0.75)
  graphics::abline(v = graphics::axis.Date(1,dates), 
                   col = .dsa2color("grey"), 
                   lty = 1, xaxt = "n")
  graphics::axis(2, tck = 1, 
                 col = .dsa2color("grey"), lty = 1)
  graphics::par(new = TRUE)
  plot(dates, series1, 
       type = "l", xlab = "", ylab = "",
       main = "Comparison", col = .dsa2color("darkblue"), bty = "n")
  graphics::lines(dates, series2, 
                  col = .dsa2color("orange"))
  graphics::lines(dates, series3, 
                  col = .dsa2color("green"))
  graphics::par(col.axis = "transparent")
  graphics::axis(1, col.ticks = .dsa2color("grey"), 
                 graphics::axis.Date(1,dates))
  graphics::axis(2, col.ticks = .dsa2color("grey"))
  graphics::box(col = .dsa2color("grey"))
  
  .add_legend("bottom", legend = c("Original", paste0("Adjusted Series (", name1, ")"), 
                                   paste0("Adjusted Series (", name2, ")")), 
              lty = c(1,1),
              col = .dsa2color("darkgrey", "orange", "green"),
              horiz = TRUE, bty = 'n', cex = 0.8)
  on.exit(graphics::par(opar))
}


#' Plot the periodogram for `wsa` objects
#'
#' Plot the periodogram of the adjustment result of a weekly time series.
#' @param x A `wsa` object.
#' @param ... Further options to `par()`.
#' @details The spectrum is build around the `spec.pgram()` function
#' @author Daniel Ollech
#' @export
#' @method spectrum wsa


spectrum.wsa <- function(x, ...) {
  # Calculations before
  original_diff <- diff(x$series$original)
  original_diff <- stats::ts(original_diff[!is.na(original_diff)], 
                             frequency = 52.18)
  df            <- data.frame(freq = stats::spec.pgram(original_diff, plot = F)$freq, 
                              spectrum = (stats::spec.pgram(original_diff, plot = F)$spec))
  df$spectrum   <- 10 * log10(df$spectrum) 
  
  seasadj_diff  <- diff(x$series$seas_adj)
  seasadj_diff  <- stats::ts(seasadj_diff[!is.na(seasadj_diff)], 
                             frequency = 52.18)
  df2           <- data.frame(freq = stats::spec.pgram(seasadj_diff, plot = F)$freq, 
                              spectrum = (stats::spec.pgram(seasadj_diff, plot = F)$spec))
  df2$spectrum  <- 10 * log10(df2$spectrum) 
  
  opar <- graphics::par(no.readonly = TRUE)
  graphics::par(fig = c(0, 1, 0, 1), 
                oma = c(1, 1, 1, 1),
                mar = c(1.75, 2.75, 1.5, 0.5),
                mgp = c(1.75, 0.5, 0),
                cex.axis = 0.75,
                ...)
  graphics::par(mfrow = c(2,1))
  .single_plot_spectrum(df, ylab = "Original", 
                        title = "Spectrum", dsa2 = FALSE)
  graphics::par(mar = c(2.75, 2.75, 0.5, 0.5))
  .single_plot_spectrum(df2, ylab = "Seasonally Adjusted", 
                        title = "", dsa2 = FALSE)
  
  on.exit(graphics::par(opar))
}


#' ACF for `wsa` objects
#' 
#' ACF applied to the outcome of a call to `wsa()`
#' @param x  A `wsa` object.
#' @param ... Additional parameters. 
#' @export
#' @method acf wsa


acf.wsa <- function(x, ...) {
  
  x <- x$preProcessing$model$residuals
  stats::acf(x, ...)
}


#' PACF for `wsa` objects
#' 
#' PACF applied to the outcome of a call to `wsa()`.
#' @param x  A `wsa` object.
#' @param ... Additional parameters. 
#' @keywords internal


pacf.wsa <- function(x, ...) {
  
  x <- x$preProcessing$model$residuals
  stats::pacf(x, ...)
}
