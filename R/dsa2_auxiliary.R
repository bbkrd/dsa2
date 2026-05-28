#' Delete February 29
#' 
#' Delete the observation on February 29 from a time series
#' @param x An `xts` object containing the time series to be adjusted.
#' @author Daniel Ollech
#' @keywords internal


delete_29 <- function(x) {
  x[format(zoo::index(x), "%m-%d") == "02-29"] <- NA
  x <- x[!is.na(x)]
}


#' Internal function to invert taking logs and differences of a time series
#' 
#' For a series that has been logged and/or differenced, this function reverses these transformations.
#' @param x An `xts` object containing the time series to be adjusted.
#' @param y An `xts` object containing the reference time series for inverting differencing.
#' @param Diff An integer value detailing the number of differences to be taken.
#' @param log A logical value indicating whether the time series should be logarithmised.
#' @param Lag An integer value detailing which lag of differencing needs to be inverted.
#' @details The function is used in `dsa()` to handle the users choice of logs and levels.
#' @author Daniel Ollech
#' @keywords internal


.descaler <- function(x, 
                      y = NA, 
                      Diff = 0,  
                      log = FALSE, 
                      Lag = NA) { 
  # Copied from {dsa}
  .diffinv_xts <- function(x, 
                           y, 
                           lag = 1, 
                           differences = 1, 
                           stepsize = "days", 
                           ...) {
    
      if (!inherits(y, "xts")) {
        stop("The time series y must contain an xts object.")
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
    if (inherits(x, "ts")) {
      x <- stats::diffinv(x, differences = Diff, xi = y[1:(1 * Diff)])
    }
    else {
      x <- .diffinv_xts(x, y, differences = Diff)
    }
  }
  if (log) {
    x <- exp(x)
  }
  if (any(class(x) == "numeric") & !is.na(y)) {
    x <- xts::xts(x, order.by = seq.Date(from = as.Date(stats::start(y)), 
                                         by = "days", length.out = length(x)))
  }
  return(x)
}


#' Internal function to take logs and differences of a time series
#' 
#' Logarithmise and / or difference a time series 
#' @param x An `xts` object containing the time series to be adjusted.
#' @param Diff An integer value detailing the number of differences to be taken.
#' @param log A logical value indicating whether the time series should be logarithmised.
#' @details The function is used in `dsa()` to handle the users choice of logs and levels.
#' @author Daniel Ollech
#' @keywords internal


.scaler <- function(x, 
                    Diff = 0, 
                    log = FALSE) { # Copied from {dsa}
  
  if (log){
    x <- log(x)
  } 
  if (Diff > 0) {
    x <- diff(x, differences = Diff)
  }
    
  return(stats::na.omit(x))
}


#' Defining dsa2 colors
#' 
#' This function defines the colors to be used throughout dsa2.
#' @param color A character value specifying the name of the color.
#' @param ... Additional colors
#' @author Daniel Ollech
#' @keywords internal


.dsa2color <- function(color,  ...) {
  
  if (missing(...)) {
   bc <- color
  } else {
    bc <- c(color, ...)
  }
  
  colorset   <- data.frame(blue = c("#3a6699"), 
  darkblue   <- c("#2f4858"),                      
  grey       <- c("#949098"), 
  gray       <- c("#949098"), 
  darkgrey   <- c("#727073"), 
  darkgray   <- c("#727073"), 
  yellow     <- c("#EFB036"), 
  orange     <- c("#ef972c"), 
  brown      <- c("#996D3A"), 
  red        <- c("#993E3A"),
  violet     <- c("#993A95"),
  purple     <- c("#993A95"),
  petrol     <- c("#3A9699"), 
  green      <- c("#66993A"), 
  darkgreen  <- c("#446627"), 
  lightgreen <- c("#7FBB4B"), 
  black      <- c("#000000"),  
  pink       <- c("#ff79a3"), stringsAsFactors = F)
  
  for (j in 1:length(bc)) {
    if (bc[j] %in% colnames(colorset)) {
      bc[j] <- gsub(bc[j], colorset[bc[j]], bc[j])
    }
  }
  bc <- gsub("\\t","",bc)
  
  return(bc)
}
  

#' Plot the results of `dsa2` objects
#' 
#' Creates a plot of original and seasonally adjusted series.
#' @param x A `dsa2` object.
#' @param main A character value detailing the title of the plot.
#' @param include_forecasts A logical value indicating whether to display forecast data.
#' @param type A single character indicating the line type (see `plot()`).
#' @param xlab A character value detailing the label of x-axis.
#' @param ylab A character value detailing the label of y-axis (possibly change the margins using parameter `mar`). 
#' @param cex.axis see `par()`.
#' @param bty see `par()`.
#' @param mar A numerical vector indicating the number of lines of margin to be specified on the four sides of the plot (see `par()`).
#' @param ... Additional parameters from `plot()`
#' @details The function uses the base plot package. 
#' @author Sindy Brakemeier, Daniel Ollech
#' @export


plot.dsa2 <- function(x, 
                      main = "Result for seasonal adjustment of daily time series", 
                      include_forecasts = FALSE, 
                      type = "l", 
                      xlab = "", 
                      ylab = "", 
                      cex.axis = 0.75, 
                      bty = "n", 
                      mar = c(4, 2, 2, 0.5), 
                      ...) {
  
  opar <- graphics::par(no.readonly = TRUE)
  graphics::par(mar = mar, xpd = TRUE)
  
  if (include_forecasts) {
    minus_h <- 0
  } else {
    minus_h <- x$parameters$h
  }
  x$series <- utils::head(x$series, 
                  nrow(x$series) - minus_h)
  
  dates   <- zoo::index(x$series)
  series1 <- as.numeric(x$series[,1])
  series2 <- as.numeric(x$series[,2])
  
  plot(dates, 
       series1, 
       type = type, 
       xlab = xlab, 
       ylab = ylab, 
       cex.axis = cex.axis, 
       bty = bty, 
       ...)
  graphics::par(xpd = FALSE, 
                cex.axis = 0.75)
  graphics::abline(v = graphics::axis.Date(1,dates), 
                   col = .dsa2color("grey"), 
                   lty = 1, 
                   xaxt = "n")
  graphics::axis(2, 
                 tck = 1, 
                 col = .dsa2color("grey"), 
                 lty = 1)
  graphics::par(new = TRUE)
  plot(dates, 
       series1, 
       type = type, 
       xlab = xlab, 
       ylab = ylab, 
       main = main, 
       col = .dsa2color("darkblue"), 
       bty = bty)
  graphics::lines(dates, 
                  series2, 
                  col = .dsa2color("red"))
  graphics::par(col.axis = "transparent")
  graphics::axis(1, 
                 col.ticks = .dsa2color("grey"), 
                 graphics::axis.Date(1,dates))
  graphics::axis(2,
                 col.ticks = .dsa2color("grey"))
  graphics::box(col = .dsa2color("grey"))
  .add_legend("bottom", 
              legend = c("Original", "Adjusted"), 
              lty = c(1,1),
              col = .dsa2color("darkblue", "red"),
              horiz = TRUE, bty = 'n', cex = 0.8)
  on.exit(graphics::par(opar))
}

.add_legend <- function(...) {
  opar <- graphics::par(fig = c(0, 1, 0, 1), 
                        oma = c(0, 0, 0, 0),
                        mar = c(0, 0, 0, 0), 
                        new = TRUE)
  on.exit(graphics::par(opar))
  plot(0, 0, type = 'n', bty = 'n', xaxt = 'n', yaxt = 'n')
  graphics::legend(...)

}


#' Summary for `dsa2` objects
#' 
#' `summary.dsa2` lists the coefficients of the fractional airline model as well as the coefficients and 
#'  t-values of all outliers and calendar effects.
#' @param object A `dsa2` output object.
#' @param ... Further arguments handed to `print.dsa2`.
#' @author Sindy Brakemeier, Lea Hengen
#' @keywords internal


summary.dsa2 <- function(object, ...) {print(object, ...)}


#' Print for `dsa2` objects
#' 
#' Print for `dsa2` objects, lists the coefficients of the fractional airline model as well as the coefficients and 
#'  t-values of all outliers and calendar effects.
#' @param x A `dsa2` object.
#' @param ... Further arguments to print.
#' @author Sindy Brakemeier, Lea Hengen
#' @keywords internal


print.dsa2 <- function(x, ...) {
  
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


#' Display ARIMA results for `dsa2` and `wsa` objects
#' 
#' Display ARIMA results for `dsa2` and `wsa` objects.
#' @param x A `dsa2` or `wsa` object.
#' @param digits An integer value specifying the number of digits to be displayed.
#' @author Daniel Ollech
#' @keywords internal


.outARIMA <- function(x, digits = 3) {UseMethod(".outARIMA")} # This is how we define generics in S3


#' Internal function for Fractional Airline model
#' 
#' Internal function to polish the output for the fractional airline model.
#' @param x A `dsa2` object.
#' @param digits An integer value specifying the number of digits to be displayed.
#' @author Jakob Oberhammer, Martin Stefan, Sindy Brakemeier, Lea Hengen
#' @keywords internal


.outARIMA.dsa2 <- function(x, digits = 3) {
  
  # auxiliary variables
  coefs <- x$preProcessing$estimation$parameters
  covar <- x$preProcessing$estimation$covariance
  
  sterrs <- sqrt(diag(covar))
  tvals  <- coefs / sterrs
  
  df <- data.frame(
    "Regressor"   = c("theta", "theta 7", "theta 365"),
    "Coefficient" = format(round(coefs,  digits),  nsmall = digits),
    "Std.Error"   = format(round(sterrs, digits),  nsmall = digits),
    "t-Value"     = format(round(tvals,  digits),  nsmall = digits)
  )
  
  return(df)
}


#' Display outlier results for `dsa2` and `wsa` objects
#' 
#' Display outlier results for `dsa2` and `wsa` objects.
#' @param x A `dsa2` object.
#' @param digits An integer value specifying the number of digits to be displayed.
#' @author Daniel Ollech
#' @keywords internal

.outOutlier <- function(x, digits = 3) {UseMethod(".outOutlier")} # This is how we define generics in S3


#' Internal function for outliers
#' 
#' Internal function to polish the output for outliers.
#' @param x A `dsa2` object.
#' @param digits An integer value specifying the number of digits to be displayed.
#' @author Sindy Brakemeier, Lea Hengen
#' @keywords internal


.outOutlier.dsa2 <- function(x, digits = 3) {
  
  # detect if any outliers present
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
    "Coefficient" = format(round(coefs,  digits), nsmall = digits),
    "Std.Error"   = format(round(sterrs, digits), nsmall = digits),
    "t-Value"     = format(round(tvals,  digits), nsmall = digits)
  )
  
  df <- df[order(outlierDates),]
  
  return(df)
}


#' Display calendar regression results for `dsa2` and `wsa` objects
#' 
#' Display calendar regression results for `dsa2` and `wsa` objects.
#' @param x A `dsa2` object.
#' @param digits An integer value specifying the number of digits to be displayed.
#' @author Daniel Ollech
#' @keywords internal


.outCalendar <- function(x, digits = 3) {UseMethod(".outCalendar")} # This is how we define generics in S3


#' Internal function for calendars
#' 
#' Internal function to polish the output for calendars.
#' @param x A `dsa2` object.
#' @param digits An integer value specifying the number of digits to be displayed.
#' @author Sindy Brakemeier, Lea Hengen
#' @keywords internal


.outCalendar.dsa2 <- function(x, digits = 3) {
  
  # detect if any calendar matrix present
  if (is.null(x$parameters$xreg) & 
      (all(x$components$calComp==1) | 
       all(x$components$calComp==0))) {
    return(NULL)
  }
  
  # auxiliary variables
  dates <- zoo::index(x$series)             # dates of time series
  if (is.null(x$parameters$xreg)) {
  xreg  <- x$preProcessing$model$xreg[,-c(
    grep("^AO.", x$preProcessing$model$variables),
    grep("^LS.", x$preProcessing$model$variables),
    grep("^WO.", x$preProcessing$model$variables)
  )]                # calendar matrix
  colnames(xreg) <- x$preProcessing$model$variables[-c(
    grep("^AO.", x$preProcessing$model$variables),
    grep("^LS.", x$preProcessing$model$variables),
    grep("^WO.", x$preProcessing$model$variables)
  )]
  } else {
    xreg <- x$parameters$xreg
  } 
  vars  <- x$preProcessing$model$variables  # outlier/calendar vars
  coefs <- x$preProcessing$model$b          # coefficients
  covar <- x$preProcessing$model$bcov       # covariance matrix
  
  # remove outliers
  n <- ncol(xreg)
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
    "Coefficient" = format(round(coefs,  digits), nsmall = digits),
    "Std.Error"   = format(round(sterrs, digits), nsmall = digits),
    "t-Value"     = format(round(tvals,  digits), nsmall = digits)
  )

  return(df)
}


#' Plot a comparison between two `dsa2` or two `wsa` objects
#' 
#' Compare plot can be applied to `dsa2` and `wsa` objects, i.e. the results from a call to `dsa()` or `wsa()`.
#' @param x1 The first `dsa2` or `wsa` object.
#' @param x2 The second `dsa2` or `wsa` object.
#' @param include_forecasts A logical value indicating whether to display forecast data.
#' @author Daniel Ollech
#' @export


compare_plot <- function(x1, x2=NULL, include_forecasts = FALSE) {UseMethod("compare_plot")} # This is how we define generics in S3


#' Function to compare two `dsa2` results
#' 
#' Create a plot of the original and adjusted series from two different adjustments.
#' @param x1 The first `dsa2` object.
#' @param x2 The second `dsa2` object.
#' @param include_forecasts A logical value indicating whether to display forecast data.
#' @examples set.seed(2358)
#' all <- tssim::sim_daily(N = 5)
#' series  <- all$original
#' result  <- dsa(series, outliers = NULL)
#' result2 <- dsa(series, s7 = "x11", pre_processing = result)
#' compare_plot(result, result2)
#' @author Daniel Ollech
#' @export


compare_plot.dsa2 <- function(x1, x2, include_forecasts = FALSE) {
  
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
  
  opar <- graphics::par(no.readonly  =  TRUE)
  graphics::par(mar = c(4, 2, 2, 0.5), xpd = TRUE)
  dates <- zoo::index(result1)
  
  series1 <- as.numeric(result1[,1])
  series2 <- as.numeric(result1[,2])
  series3 <- as.numeric(result2[,2])
  
  plot(dates, series1, 
       type = "l", xlab = "", ylab = "", 
       cex.axis = 0.75, bty = "n")
  graphics::par(xpd = FALSE, 
                cex.axis = 0.75)
  graphics::abline(v = graphics::axis.Date(1,dates), 
                   col = .dsa2color("grey"), 
                   lty = 1, xaxt = "n")
  graphics::axis(2, tck = 1, 
                 col = .dsa2color("grey"), 
                 lty = 1)
  graphics::par(new = TRUE)
  
  plot(dates, series1, 
       type = "l", xlab = "", ylab = "", 
       main = "Comparison", col = .dsa2color("darkgrey"), 
       bty = "n")
  graphics::lines(dates, series2, 
                  col = .dsa2color("orange"))
  graphics::lines(dates, series3, 
                  col = .dsa2color("green"))
  graphics::par(col.axis = "transparent")
  graphics::axis(1, col.ticks = .dsa2color("grey"), 
                 graphics::axis.Date(1,dates))
  graphics::axis(2, col.ticks = .dsa2color("grey"))
  graphics::box(col = .dsa2color("grey"))
  
  .add_legend("bottom", legend = c("Original", paste0("Adjusted Series (", name1, ")"), paste0("Adjusted Series (", name2, ")")), 
              lty = c(1,1),
              col = .dsa2color("darkgrey", "orange", "green"),
              horiz = TRUE, bty = 'n', cex = 0.8)
  on.exit(graphics::par(opar))
}


#' HTML output for `dsa2` objects
#' 
#' HTML output for `dsa2` objects.
#' @param x A `dsa2` object.
#' @param fileName A character value detailing the file name for HTML output.
#' @param filePath A character value detailing the file path for HTML output.
#' @param title A character value detailing the title of the HTML document.
#' @param open A logical value indicating whether the rendered HTML is opened.
#' @details The function generates an .Rmd file that is rendered into an .html document saved in the working directory.
#' @author Lea Hengen, Sindy Brakemeier, Martin Stefan, Jan Heller, Daniel Ollech
#' @export


output <- function(x, 
                   fileName = NULL, 
                   filePath = NULL, 
                   title = NULL, 
                   open = TRUE) {
  # if no file name is specified, use series name
  if (is.null(fileName)) {
    fileName <- deparse(substitute(x))
  }
  
  # if no file path is specified, use current working directory 
  if (is.null(filePath)) {
    filePath <- getwd()
  }
  
  # if no file path is specified, use current working directory 
  if (!is.null(title)) {
    x$parameters$title <- title 
  } else {
    x$parameters$title <- x$parameters$name
  }
  
  rmarkdown::render(
    input         = paste0(system.file(package = "dsa2"), "/rmd/output.Rmd"),
    output_format = "html_document",
    output_file   = paste0(filePath, "/", fileName, ".html"),
    params        = list(x = x, fileName = fileName)#,
    #encoding     = 'UTF-8'
  )
  
  if (open) {
    utils::browseURL(paste0(filePath, "/", fileName, ".html"))
  }
}


#' Plot the ACF
#' 
#' It can be applied to both `ts` and numerics (as before), and `dsa2` and `wsa` output, i.e. the results of a call to `dsa()` or `wsa()`.
#' @param x A `dsa2` or `wsa` object for which the ACF should be plotted.
#' @param ... Additional Parameters. 
#' @author Daniel Ollech
#' @export


acf <- function(x,  ...) {UseMethod("acf")} # This is how we define generics in S3


#' Plot the spectrum
#' 
#' It can be applied to both `ts` and numerics (as before), and `dsa2` and `wsa` output, i.e. the results of a call to `dsa()` or `wsa()`. 
#'  The y-axis is a dB-type scale, analogous to `log="dB"` in `stats::spectrum()`.
#' @param x A `ts` or numeric object.
#' @param ... Additional parameters. 
#' @author Daniel Ollech
#' @export


spectrum <- function(x,  ...) {UseMethod("spectrum")} # This is how we define generics in S3


#' ACF for `ts` objects
#' 
#' See `stats::acf` for details.
#' @param x A `ts` object.
#' @param ... Additional parameters.
#' @author Daniel Ollech
#' @export
#' @keywords internal


acf.ts <- function(x, ...) {stats::acf(x, ...)}


#' ACF for numeric objects
#' 
#' See `stats::acf` for details.
#' @param x A numeric object.
#' @param ... Additional parameters.
#' @author Daniel Ollech
#' @export
#' @keywords internal


acf.numeric <- function(x, ...) {stats::acf(x, ...)}


#' Spectrum for `ts` objects
#' 
#' See `stats::spectrum` for details.
#' @param x A `ts` object.
#' @param ... Additional parameters.
#' @author Daniel Ollech 
#' @export
#' @keywords internal


spectrum.ts <- function(x, ...) {stats::spectrum(x, ...)}


#' Spectrum for numeric objects
#' 
#' See `stats::spectrum` for details.
#' @param x A numeric object.
#' @param ... Additional parameters.
#' @author Daniel Ollech
#' @keywords internal
#' @export


spectrum.numeric <- function(x, ...) {stats::spectrum(x, ...)}


#' Plot the ACF based on `dsa2` objects
#'
#' Plot the ACF for a seasonally adjusted time series extracted from a `dsa2` object. 
#' @param x A `dsa2` object.
#' @param lags An integer value specifying which lags are shown.
#' @param ylim A numeric vector indicating the limits of y-axis.
#' @param main A character value detailing the title of the plot.
#' @param xlab A character value detailing the label of x-axis.
#' @param ylab A character value detailing the label of y-axis. 
#' @param col A character value detailing the color of the line.
#' @param col2 A character value detailing the color of horizontal lines for the confidence interval.
#' @param col3 A character value detailing the color of vertical lines to separate lags of interest.
#' @param space A numeric value detailing the space before each bar (see `barplot()`).
#' @param border A character value detailing the color of the border.
#' @param ... Further arguments for `barplot()`.
#' @details Wrapper around the `stats::acf`() function. See `barplot()` for details on changing the look of the plot
#' @author Daniel Ollech
#' @examples x <- tssim::sim_daily(3)$original
#' result <- dsa(x)
#' acf(result)
#' @export
#' @method acf dsa2


acf.dsa2 <- function(x, 
                     lags = c(1:7 + 1, 30:31 + 1, 365 + 1, 730 + 1),
                     ylim = c(-1, 1),
                     main = "ACF for selected lags",
                     xlab = "lags",
                     ylab = "ACF",
                     col = .dsa2color("darkgreen"),
                     col2 = .dsa2color("blue"),
                     col3 = .dsa2color("darkgrey"),
                     space = 6,
                     border = NA,
                     ...) {
  # Calculations before
  residuals      <- x$preProcessing$model$residuals
  residuals      <- residuals[!is.na(residuals)]
  acf_result     <- stats::acf(residuals, lag.max = 366 * 2, plot = FALSE)
  acf_res        <- acf_result$acf[lags]
  names(acf_res) <- as.character(acf_result$lag[lags])
  ci             <- stats::qnorm((1 + 0.95) / 2) / sqrt(length(residuals))
  
  graphics::barplot(
    acf_res,
    ylim = ylim,
    main = main,
    xlab = xlab,
    ylab = ylab,
    col = col,
    space = space,
    border = border,
    ...)
  graphics::abline(h = ci,
                   lty = 3,
                   col = col2)
  graphics::abline(h = -ci,
                   lty = 3,
                   col = col2)
  graphics::abline(v = 52.5, col = col3)
  graphics::abline(v = 65, col = col3)
}


#' Plot the PACF based on `dsa2` objects
#'
#' Plot the PACF for a seasonally adjusted time series extracted from a `dsa2` object. 
#' @param x A `dsa2` object.
#' @param lag.max A integer value specifying the maximum lags, included for consistency with `pacf.default()`.
#' @param plot A logical value indicating whether the object is plotted, included for consistency with `pacf.default()`.
#' @param na.action see `stats::na.exclude`, handling of missing observations.
#' @param lags An integer value specifying which lags are shown.
#' @param ylim A numeric vector indicating the limits of y-axis.
#' @param main A character value detailing the title of the plot.
#' @param xlab A character value detailing the label of x-axis.
#' @param ylab A character value detailing the label of y-axis. 
#' @param col A character value detailing the color of the line.
#' @param col2 A character value detailing the color of horizontal lines for the confidence interval.
#' @param col3 A character value detailing the color of vertical lines to separate lags of interest.
#' @param space A numeric value detailing the space before each bar (see `barplot()`).
#' @param border A character value detailing the color of the border.
#' @param ... Further arguments for `barplot()`.
#' @details Wrapper around the `stats::pacf()` function. See `barplot()` for details on changing the look of the plot.
#' @author Daniel Ollech
#' @examples x <- tssim::sim_daily(3)$original
#' result <- dsa(x)
#' pacf(result)
#' @export
#' @method pacf dsa2


pacf.dsa2 <- function(x,
                     lag.max = 730,
                     plot = TRUE,
                     na.action = stats::na.exclude,
                     lags = c(1:7, 30:31, 365, 730),
                     ylim = c(-1, 1),
                     main = "PACF for selected lags",
                     xlab = "lags",
                     ylab = "PACF",
                     col = .dsa2color("darkgreen"),
                     col2 = .dsa2color("blue"),
                     col3 = .dsa2color("darkgrey"),
                     space = 6,
                     border = NA,
                     ...) {
  # Calculations before
  residuals       <- x$preProcessing$model$residuals
  residuals       <- residuals[!is.na(residuals)]
  pacf_result     <- stats::pacf(residuals, lag.max = 366 * 2, plot = FALSE)
  pacf_res        <- pacf_result$acf[lags]
  names(pacf_res) <- as.character(pacf_result$lag[lags])
  ci              <- stats::qnorm((1 + 0.95) / 2) / sqrt(length(residuals))
  
  # Plotting
  if (plot) {
  graphics::barplot(
    pacf_res,
    ylim = ylim,
    main = main,
    xlab = xlab,
    ylab = ylab,
    col = col,
    space = space,
    border = border,
    ...)
    
  graphics::abline(h = ci,
                   lty = 3,
                   col = col2)
  graphics::abline(h = -ci,
                   lty = 3,
                   col = col2)
  graphics::abline(v = 52.5, col = col3)
  graphics::abline(v = 65, col = col3)
  }
}


#' Plot the periodogram for `dsa2` objects
#'
#' Plot the periodogram of the adjustment result of a daily time series.
#' @param x A `dsa2` object.
#' @param ... Further options to `par()`.
#' @details The spectrum is build around the `spec.pgram()` function.
#' @author Daniel Ollech
#' @examples x <- tssim::sim_daily(3)$original
#' res <- dsa(x)
#' spectrum(res)
#' @export
#' @method spectrum dsa2


spectrum.dsa2 <- function(x, ...) {
  # Calculations before
  original_diff <- diff(x$series$original)
  original_diff <- stats::ts(original_diff[!is.na(original_diff)], frequency = 365.2524)
  df            <- data.frame(freq = stats::spec.pgram(original_diff, plot = F)$freq, 
                              spectrum = (stats::spec.pgram(original_diff, plot = F)$spec))
  df$spectrum   <- 10 * log10(df$spectrum) # Idea adapted from stats::plot.spec (if log = "dB")
  
  seasadj_diff  <- diff(x$series$seas_adj)
  seasadj_diff  <- stats::ts(seasadj_diff[!is.na(seasadj_diff)], frequency = 365.2524)
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
  .single_plot_spectrum(df, ylab = "Original", title = "Spectrum")
  graphics::par(mar = c(2.75, 2.75, 0.5, 0.5))
  .single_plot_spectrum(df2, ylab = "Seasonally Adjusted", title = "")
  
  on.exit(graphics::par(opar))
}

#' ACF for ts object
#' 
#' See \link[stats]{acf} for details
#' @param x input
#' @param ... parameters 
#' @export
#' @method acf ts

acf.ts <- function(x, ...) {
  stats::acf(x, ...)
}

#' ACF for numeric object
#' 
#' See \link[stats]{acf} for details
#' @param x input
#' @param ... parameters 
#' @export
#' @method acf numeric

acf.numeric <- function(x, ...) {
  stats::acf(x, ...)
}

#' Spectrum for ts object
#' 
#' See \link[stats]{spectrum} for details
#' @param x input
#' @param ... parameters 
#' @export
#' @method spectrum ts

spectrum.ts <- function(x, ...) {
  stats::spectrum(x, ...)
}

#' Spectrum for numeric object
#' 
#' See \link[stats]{spectrum} for details
#' @param x input
#' @param ... parameters 
#' @export
#' @method spectrum numeric

spectrum.numeric <- function(x, ...) {
  stats::spectrum(x, ...)
}



#' Helper function for the plotting of spectra
#'
#' The function is used in `spectrum.wsa()` and `spectrum.dsa2()`.
#' @param df A data frame with spectrum data.
#' @param ylab A character value detailing the label of y-axis. 
#' @param title A character value detailing the title of the plot.
#' @param dsa2 A logical value indicating whether the object in `spectrum()` is a `dsa2` object.
#' @author Daniel Ollech
#' @keywords internal


.single_plot_spectrum <- function(df, 
                                  ylab = "Spectrum", 
                                  title = "Spectrum", 
                                  dsa2 = TRUE) {
  
  graphics::plot(
    df,
    type = "l",
    ylab = ylab,
    xlab = "Number of cycles",
    main = title
  )
  
  if (dsa2) {
  graphics::abline(v = 12, col = .dsa2color("orange"), lty = 3)
  graphics::abline(v = 24, col = .dsa2color("orange"), lty = 3)
  graphics::abline(v = 365.2524/7, col = .dsa2color("red"), lty = 4)
  graphics::abline(v = 365.2524/7*2, col = .dsa2color("red"), lty = 4)
  graphics::abline(v = 365.2524/7*3, col = .dsa2color("red"), lty = 4)
  graphics::lines(df) 
  
  # Add text 
  graphics::text(x = 365.25/7, y = min(df$spectrum), 
                 labels = "Once a week", col=.dsa2color("darkgreen"), 
                 pos = 4, srt = 90, cex = 0.6)  
  graphics::text(x = 365.25/(7/2), y = min(df$spectrum), 
                 labels = "Twice a week", col=.dsa2color("darkgreen"), 
                 pos = 4, srt = 90, cex = 0.6)  
  graphics::text(x = 365.25/(7/3), y = min(df$spectrum), 
                 labels = "Thrice a week", col=.dsa2color("darkgreen"), 
                 pos = 4, srt = 90, cex = 0.6)  
  graphics::text(x = 12, y = min(df$spectrum), 
                 labels = "Once a month", col=.dsa2color("orange"), 
                 pos = 4, srt = 90, cex = 0.6)  
  graphics::text(x = 24, y = min(df$spectrum), 
                 labels = "Twice a month", col=.dsa2color("orange"), 
                 pos = 4, srt = 90, cex = 0.6) 
  } else {
    graphics::abline(v = 1, col = .dsa2color("orange"), lty = 3)
    graphics::abline(v = 2, col = .dsa2color("orange"), lty = 3)
    graphics::abline(v = 3, col = .dsa2color("orange"), lty = 3)
    graphics::abline(v = 4, col = .dsa2color("orange"), lty = 3)
    graphics::abline(v = 5, col = .dsa2color("orange"), lty = 3)
    graphics::abline(v = 6, col = .dsa2color("orange"), lty = 3)
    graphics::abline(v = 7, col = .dsa2color("orange"), lty = 3)
    graphics::abline(v = 8, col = .dsa2color("orange"), lty = 3)
    graphics::abline(v = 9, col = .dsa2color("orange"), lty = 3)
    graphics::abline(v = 10, col = .dsa2color("orange"), lty = 3)
    graphics::abline(v = 11, col = .dsa2color("orange"), lty = 3)
    graphics::abline(v = 12, col = .dsa2color("red"), lty = 3)
    graphics::lines(df)
  }
}


#' Interactive plots
#' 
#' The interactive plot can be applied to a `dsa2`, `wsa` and `xts` object. Is it based on dygraphs.
#' @param x An `dsa2`, `wsa` or `xts` object to be plotted.
#' @param ... Additonal parameters. 
#' @author Daniel Ollech
#' @export


plot_interactive <- function(x,  ...) {UseMethod("plot_interactive")} # This is how we define generics in S3


#' Interactive plot for `dsa2` objects
#' 
#' Creates a plot of original and seasonally adjusted series.
#' @param x An `dsa2` object to be plotted.
#' @param ... Additional parameters for `dygraphs::dyOptions`.
#' @details The function uses the dygraphs package. 
#' @author Martin Stefan, Daniel Ollech
#' @export
#' @method plot_interactive dsa2


plot_interactive.dsa2 <- function(x, ...) {
  # auxiliary variables
  dates      <- zoo::index(x$series)
  shadeEnd   <- dates[length(dates)]
  shadeStart <- dates[length(dates) - x$parameters$h]
  cols       <- c(.dsa2color("petrol"), .dsa2color("orange"))
  
  dygraphs::dygraph(x$series) |>
    dygraphs::dyOptions(colors = cols, ...) |>
    dygraphs::dyAxis("x", drawGrid = FALSE) |>
    dygraphs::dyAxis("y", axisLabelWidth = 25) |>
    dygraphs::dySeries("original", label = "Original") |>
    dygraphs::dySeries("seas_adj", label = "Adjusted") |>
    dygraphs::dyShading(from = shadeStart, to = shadeEnd) |>
    dygraphs::dyLegend(labelsSeparateLines = TRUE,
                       width = 125,
                       show = "follow") |>
    dygraphs::dyRangeSelector(height = 70, 
                              strokeColor = "", 
                              fillColor = .dsa2color("petrol"))
}


#' Interactive plot for `wsa` objects
#' 
#' Creates a plot of original and seasonally adjusted series.
#' @param x An `wsa` object to be plotted.
#' @param ... Additional parameters for `dygraphs::dyOptions`.
#' @details The function uses the dygraphs package. 
#' @author Daniel Ollech
#' @export
#' @method plot_interactive wsa


plot_interactive.wsa <- function(x, ...) {
  # auxiliary variables
  dates      <- zoo::index(x$series)
  shadeEnd   <- dates[length(dates)]
  shadeStart <- dates[length(dates) - x$parameters$h]
  cols       <- c(.dsa2color("petrol"), .dsa2color("orange"))
  
  # create plot
  dygraphs::dygraph(x$series) |>
    dygraphs::dyOptions(colors = cols, ...) |>
    dygraphs::dyAxis("x", drawGrid = FALSE) |>
    dygraphs::dyAxis("y", axisLabelWidth = 25) |>
    dygraphs::dySeries("original", label = "Original") |>
    dygraphs::dySeries("seas_adj", label = "Adjusted") |>
    dygraphs::dyShading(from = shadeStart, to = shadeEnd) |>
    dygraphs::dyLegend(labelsSeparateLines = TRUE,
                       width = 125,
                       show = "follow") |>
    dygraphs::dyRangeSelector(height = 70, 
                              strokeColor = "", 
                              fillColor = .dsa2color("petrol"))
}


#' Interactive plot for `xts` objects
#' 
#' Creates a plot of original and seasonally adjusted series.
#' @param x An `xts` object to be plotted.
#' @param ... Additional parameters for `dygraphs::dyOptions`.
#' @details The function uses the dygraphs package. 
#' @author Daniel Ollech
#' @export
#' @method plot_interactive xts


plot_interactive.xts <- function(x, ...) {

  cols <- .dsa2color("petrol", "orange", "darkgreen", 
                     "pink", "darkblue", "red", 
                     "lightgreen", "violet", 
                     "black", "blue", "brown", 
                     "darkgrey", "yellow", "petrol", 
                     "orange", "darkgreen", "pink", 
                     "darkblue", "red", "lightgreen", 
                     "violet", "black", "blue", 
                     "brown", "darkgrey", "yellow")##MR2 was mit Petrol?
  
  if (ncol(x) > 26) {
    warning("We do not encourage plotting more than 26 time series.")
  }
  
  cols <- cols[1:ncol(x)]
  
  # create plot
  dygraphs::dygraph(x) |>
    dygraphs::dyOptions(colors = cols, ...) |>
    dygraphs::dyAxis("x", drawGrid = FALSE) |>
    dygraphs::dyAxis("y", axisLabelWidth = 25) |>
    dygraphs::dyLegend(labelsSeparateLines = TRUE,
                       width = 125,
                       show = "follow") |>
    dygraphs::dyRangeSelector(height = 70, 
                              strokeColor = "", 
                              fillColor = .dsa2color("petrol"))
}


#' Output table for `dsa2` objects
#' 
#' Creates a table to be used in `output()`.
#' @param df A data frame.
#' @param ... Additional parameters for `gt::tab_options`.
#' @details The function uses the gt package. 
#' @author Martin Stefan, Daniel Ollech
#' @keywords internal

.table <- function(df, ...) {
  
  df |>
    gt::gt() |>
    gt::tab_options(column_labels.hidden = TRUE, ...) 
}


#' Helper function for MSR selection
#' 
#' Local helper: rescaling factors (Lothian, 1978).
#' @param n_observations An integer detailing the number of observations.
#' @author Karsten Webel, Lea Hengen, Daniel Ollech
#' @keywords internal

.rescaling_factor_lothian <- function(n_observations = 10L) {
  
  if (!is.integer(n_observations) || n_observations < 4L) {
    stop("Invalid length: n_observations must be an integer greater than 3.")
  }
  
  c_scale <- if (n_observations == 4L) {  
    3  
  } else if (n_observations == 5L) {  
    3 * sqrt(2) / (1 + sqrt(3))  
  } else if (n_observations == 6L) {  
    5 * sqrt(6) / (8 + sqrt(2))  
  } else {  
    (n_observations - 1) / (n_observations - 7 + sqrt(24))  
  }  
  
  fi_scale <- if (n_observations == 4L) {  
    90 / (2 * sqrt(842) + 21 * sqrt(2))  
  } else if (n_observations == 5L) {  
    60 / (sqrt(894) + 2 * sqrt(211))  
  } else if (n_observations == 6L) {  
    25 * sqrt(3) / (2 * sqrt(298) + sqrt(67))  
  } else {  
    (n_observations - 1) / (n_observations - 7 + 0.2 * sqrt(894))  
  }  
  
  fi_scale / c_scale  
}


#' Translate msr value into a filter
#' 
#' Translate msr value into a filter.
#' @param global_msr A numeric containing the global msr value of a time series.
#' @author Karsten Webel, Lea Hengen, Daniel Ollech
#' @keywords internal


.msr_rule <- function(global_msr) {
  
  thresholds <- c(1.4, 1.5, 2.4, 2.9, 3.9, 4.4, 5.3, 5.8, 6.1, 6.2) 
  
  if (global_msr <= thresholds[1]) {  
    "S3X1"  
  } else if (global_msr >= thresholds[2] && global_msr <= thresholds[3]) {  
    "S3X3"  
  } else if (global_msr >= thresholds[4] && global_msr <= thresholds[5]) {  
    "S3X5"  
  } else if (global_msr >= thresholds[6] && global_msr <= thresholds[7]) {  
    "S3X9"  
  } else if (global_msr >= thresholds[8] && global_msr <= thresholds[9]) {  
    "S3X15"  
  } else if (global_msr >= thresholds[10]) {  
    "S3X23"  
  } else {  
    NULL  
  }  
}


#' Translate msr value into filter even if it repeatedly falls in between thresholds
#' 
#' Translate msr value into filter
#' @param global_msr A numeric containing the global msr value of a time series
#' @author Daniel Ollech, Lea Hengen
#' @keywords internal
 
.msr_rule_ultimate <- function(global_msr) {
  
  thresholds <- data.frame(associated_filter = c("S3X1", "S3X3", "S3X5", "S3X9", "S3X15", "S3X23"),
                                       lower = c(NA,      1.5,     2.9,    4.4,    5.8,     6.2),
                                       upper = c(1.4,     2.4,     3.9,    5.3,    6.1,     NA)
                           )
  
  idx <- which.min(abs(global_msr - as.matrix(thresholds[,2:3]) |> t() |> as.vector()))
  filter_to_use <- thresholds$associated_filter[ceiling(idx/2)]

  return(filter_to_use)
}

#' Use msr
#' 
#' Applies new msr rule
#' @param series series to be adjusted
#' @param parameters parameters for x11plus
#' @details Calculates a preliminary d8 assuming weekly and yearly seasonality and applies the new msr rule. Returns the chosen seasonal moving average
#' @author Lea Hengen, Daniel Ollech
#' @keywords internal

.run_msr <- function(series, parameters) {
  
  parameters$seas.s0 <- "S3X9"
  parameters$seas.s1 <- "S3X9"
  
  new_series <- series
  
  if (parameters$period == 31) {
    use_period = 31
    annual_length = 372 # NOTE(DO): Ensure that the length of the year is handled correctly
  } else {
    use_period = NA
    annual_length = 365
  }
  
  if (parameters$period == 7) { # NOTE(DO): We do this additional analysis only, if the day-of-the-week is estimated, otherwise, it has already been removed from series inside of dsa
    
    parameters <- suppressMessages(.correct_filter_length(parameters, stats::ts(new_series, frequency = 7)))
    prelim_adjustment_7 <- rjd3x11plus::x11plus_trend( 
      new_series,
      period = 7,
      trend.coefs = rjd3filters::lp_filter(horizon = ceiling(8/2), degree = 3, kernel = "Henderson", 
                                           endpoints = "LC"), # NOTE(DO): As soon as rjd3x11plus::x11plus can give out d1 and d7, we want to switch away from x11plus_trend, remove the rjd3filters::lp_filter part and remove dependency on rjdfilters
      mul = parameters$mul,
      seas.s0 = parameters$seas.s0,
      seas.s1 = parameters$seas.s1, 
      userdefined = c("d1", "d7")
    )
    
    new_series <- prelim_adjustment_7$decomposition[,2] # NOTE(DO): This is the seasonally adusted 
    
    if (parameters$mul) {
      d8 <- prelim_adjustment_7$user_defined$d1 / prelim_adjustment_7$user_defined$d7
    } else {
      d8 <- prelim_adjustment_7$user_defined$d1 - prelim_adjustment_7$user_defined$d7
    }
    
    
    use_period <- ifelse(is.na(use_period), 7, use_period)
  } else {
    d8 <- NA
    use_period <- ifelse(is.na(use_period), 365, use_period)
  }
  
  parameters <- suppressMessages(.correct_filter_length(parameters, stats::ts(new_series, frequency = annual_length)))
  prelim_adjustment_365 <- rjd3x11plus::x11plus_trend(
    new_series,
    period = annual_length,
    trend.coefs = rjd3filters::lp_filter(horizon = ceiling((annual_length+2)/2), degree = 3, kernel = "Henderson", 
                                         endpoints = "LC"),
    mul = parameters$mul,
    seas.s0 = parameters$seas.s0,
    seas.s1 = parameters$seas.s1, 
    userdefined = c("d1", "d7")
  )
  
  
  if (all(is.na(d8))) {
    if (parameters$mul) {
      d8 <- prelim_adjustment_365$user_defined$d1 / prelim_adjustment_365$user_defined$d7
    } else {
      d8 <- prelim_adjustment_365$user_defined$d1 - prelim_adjustment_365$user_defined$d7
    } 
  } else {
    if (parameters$mul) {
      d8 <- d8/prelim_adjustment_365$decomposition[,4] # NOTE(DO): 4th column is seasonal component
    } else {
      d8 <- d8 - prelim_adjustment_365$decomposition[,4]
    }
  }
  
  
  sma <- .apply_msr_rule(d8, 
                        seasonal_periodicity=use_period, 
                        use_additive_formula = !parameters$mul) 
  
  return(sma)
}



#' Select an S3Xk seasonal moving-average filter
#'
#' Determine the appropriate S3Xk seasonal moving-average filter ("S3X1", "S3X3",
#' "S3X5", "S3X9", "S3X15", or "S3X23") for a given time series by computing a
#' global MSR statistic from tentative seasonal and irregular components. The
#' procedure can operate in additive or multiplicative mode.
#'
#' The method:
#' - Optionally removes user-specified backcasts and forecasts.
#' - Trims the series to an integer multiple of the minimum seasonal periodicity.
#' - Obtains tentative seasonal and irregular components via moving averages.
#' - Computes a global MSR statistic and rescales it using a factor following
#' Lothian (1978).
#' - Selects the S3Xk filter based on intersection-rule thresholds.
#'
#' If no filter is selected at the current length, the series is iteratively
#' shortened by one minimum seasonal cycle and re-evaluated. If no selection is
#' possible after shortening or the series was shortened 100 times, the function 
#' returns "S3X5" by default.
#'
#' @param series A numeric vector containing the time series.
#' @param n_backcasts An integer number of leading observations to remove 
#'  before analysis.
#' @param n_forecasts An integer number of trailing observations to remove 
#'  before analysis.
#' @param seasonal_periodicity A numeric or integer vector of length-1 specifying 
#'  the seasonal periodicity. 
#' @param tentative_seasonal_ma_length An odd integer greater than 1 that
#' gives the length of the simple moving average used for the tentative seasonal
#' estimate (default 7).
#' @param use_additive_formula A logical value whether to use additive decomposition (`TRUE`;
#' irregular = series - seasonal) or multiplicative decomposition (`FALSE`;
#' irregular = series / seasonal).
#' @author Karsten Webel, Lea Hengen, Daniel Ollech
#'
#' @return a list containing the selected seasonal_filter and the global MSR
#'
#' @details
#' - The function requires at least five seasonal cycles to evaluate the thresholds; 
#' otherwise, it progressively trims the series.
#' - Input validation ensures a feasible seasonal configuration and appropriate
#' moving-average length.
#' @references
#' Lothian, J. (1978), The Identification and Treatment of Moving Seasonality 
#'  in the X-11 Seasonal Adjustment Method, Research Paper 78-10-004, Seasonal 
#'  Adjustment and Time Series Analysis Staff, Statistics Canada.
#'
#' @keywords internal


.apply_msr_rule <- function(series,
                           n_backcasts = 0,
                           n_forecasts = 0,
                           seasonal_periodicity = 4,
                           tentative_seasonal_ma_length = 7L,
                           use_additive_formula = TRUE) {
  ## Check tentative seasonal estimate length
  if (
    !is.integer(tentative_seasonal_ma_length) ||
    (tentative_seasonal_ma_length - 1) %% 2 != 0 ||
    tentative_seasonal_ma_length < 3L
  ) {
    stop("Invalid length: tentative_seasonal_ma_length must be an odd integer greater than 1.")
  }
  
  half_length <- as.integer((tentative_seasonal_ma_length - 1) / 2)
  
  ## Remove backcasts and forecasts
  if (n_backcasts > 0) {
    series <- series[-seq_len(n_backcasts)]
  }
  if (n_forecasts > 0) {
    series <- series[-seq.int(from = length(series) - n_forecasts + 1, to = length(series))]
  }
  
  ## Check minimum length
  if (length(series) < half_length * seasonal_periodicity) {
    stop("Invalid length: series is too short.")
  }
  
  seasonal_periodicity <- round(seasonal_periodicity)

  ## Cut input series to an integer multiple of the minimum seasonal periodicity
  series <- series[seq_len(length(series) %/% seasonal_periodicity * seasonal_periodicity)]

  selected_filter <- NULL
  
  repeated_calculation <- 0
  


  while (length(series) / seasonal_periodicity >= 4) {
    # Calculate tentative seasonal and irregular components
    series_matrix <- matrix(
      c(rep(NA, half_length * seasonal_periodicity),
        series, 
        rep(NA, half_length * seasonal_periodicity)),
      byrow = TRUE,
      ncol = seasonal_periodicity
    )
    
    n_rows <- nrow(series_matrix)  
    
    for (i in seq_len(seasonal_periodicity)) {  
      series_matrix[seq_len(half_length), i] <-  
        mean(series_matrix[seq(from = half_length + 1, to = 2 * half_length), i])  
      
      series_matrix[seq(from = n_rows - half_length + 1, to = n_rows), i] <-  
        mean(series_matrix[seq(from = n_rows - 2 * half_length + 1, to = n_rows - half_length), i])  
    }  
    
    tentative_seasonal_component <- matrix(NA, nrow = n_rows, ncol = seasonal_periodicity)
    for (i in seq_len(seasonal_periodicity)) {  
      for (j in seq(from = half_length + 1, to = n_rows - half_length)) {  
        tentative_seasonal_component[j, i] <-  
          mean(series_matrix[seq(from = j - half_length, to = j + half_length), i])  
      }  
    }  
    
    if (use_additive_formula) {  
      tentative_irregular_component <- series_matrix - tentative_seasonal_component  
    } else {  
      tentative_irregular_component <- series_matrix / tentative_seasonal_component  
    }  
    
    seasonal_component_vector  <- as.vector(t(tentative_seasonal_component))  
    seasonal_component_vector  <- seasonal_component_vector[!is.na(seasonal_component_vector)]  
    
    irregular_component_vector <- as.vector(t(tentative_irregular_component))  
    irregular_component_vector <- irregular_component_vector[!is.na(irregular_component_vector)]  
    
    # Calculate global MSR  
    if (use_additive_formula) {  
      global_msr_seasonal  <- sum(abs(diff(seasonal_component_vector, lag = seasonal_periodicity, differences = 1)))  
      global_msr_irregular <- sum(abs(diff(irregular_component_vector, lag = seasonal_periodicity, differences = 1)))  
    } else {  
      global_msr_seasonal  <- sum(abs(  
        seasonal_component_vector[(seasonal_periodicity + 1):length(seasonal_component_vector)] /  
          seasonal_component_vector[1:(length(seasonal_component_vector) - seasonal_periodicity)] - 1  
      ))  
      global_msr_irregular <- sum(abs(  
        irregular_component_vector[(seasonal_periodicity + 1):length(irregular_component_vector)] /  
          irregular_component_vector[1:(length(irregular_component_vector) - seasonal_periodicity)] - 1  
      ))  
    }  

    global_msr <- global_msr_irregular / global_msr_seasonal *  
      .rescaling_factor_lothian(as.integer(n_rows - 2 * half_length))  
    
    # Select S3Xk filter  
    selected_filter <- .msr_rule(global_msr = global_msr)  
    
    # Stop or redo with shortened series  
    repeated_calculation <- repeated_calculation + 1
    
    if (is.null(selected_filter) & repeated_calculation == 55) {  
      selected_filter <- .msr_rule_ultimate(global_msr)
      
      # message(paste0("The global MSR was recalculated on a shortened span 50 times, the last global MSR was: ", round(global_msr, 4), 
      #                "\nand the filter is now set to: ", selected_filter)) # NOTE(DO): I am not sure, whether anybody will want this message
      
      break  
    }  
    
    if (!is.null(selected_filter)) { 
      break  
    }  
    
    series <- series[-seq.int(from = length(series) - (min(365, seasonal_periodicity*6)) + 1, to = length(series))] # NOTE(DO): We want to shorten the series by at least 6 weeks, otherwise, the difference for global_msr will be most unchanged
  }
  
  if (selected_filter == "S3X23") {
    selected_filter <- "S3X15"
    message("S3X23 is optimal for the seasonal component with frequency ", seasonal_periodicity," but not yet implemented, therefore S3X15 is used.")
  }
  
  filter_settings <- list(
    seasonal_filter = selected_filter,
    moving_seasonality_ratio = global_msr
  )

  return(filter_settings)
}
