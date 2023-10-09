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
  
  # auxiliary function to format the print objects
  print_format <- function(y) {
    y <- rbind(colnames(y), y)         # add column names as observations
    y$sep <- "\n"                      # add line breaks
    y <- apply(y, 2, format)           # apply format over each column
    y <- paste(t(y), collapse = "\t")  # paste together
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
  
  # print
  cat(out)
  invisible(out)
  
}




#' Internal function for Fractional Airline model
#' 
#' Internal function to polish the output for the fractional airline model
#' @param dsa2_object dsa2 output object
#' @author Jakob Oberhammer, Martin Stefan

.outARIMA <- function(dsa2_object, digits = 3) {
  
  # auxiliary variables
  coefs <- dsa2_object$preProcessing$estimation$parameters
  covar <- dsa2_object$preProcessing$estimation$covariance
  
  # compute standard errors and t-values
  sterrs <- sqrt(diag(covar))
  tvals  <- coefs / sterrs
  
  # create df
  df <- data.frame(
    "Regressor"   = c("theta", "theta 7", "theta 365"),
    "Coefficient" = format(round(coefs,  digits),  n_small = digits),
    "Std.Error"   = format(round(sterrs, digits),  n_small = digits),
    "t-Value"     = format(round(tvals,  digits),  n_small = digits)
  )
  
  # return
  return(df)
  
}



#' Internal function for outliers
#' 
#' Internal function to polish the output for outliers
#' @param dsa2_object dsa2 output object
#' @author Sindy Brakemeier, Lea Hengen

.outOutlier <- function(dsa2_object, digits = 3) {
  
  # detect if any outliers present
  if (dsa2_object$parameters$log) {
    noOutliers <- all(dsa2_object$preProcessing$model$component_outliers == 1)
  } else {
    noOutliers <- all(dsa2_object$preProcessing$model$component_outliers == 0)
  }
  
  # no outliers found
  if (noOutliers) {
    return(NULL)
  }
    
  # auxiliary variables
  dates <- zoo::index(dsa2_object$series)             # dates of time series
  xreg  <- dsa2_object$parameters$xreg                # calendar matrix
  vars  <- dsa2_object$preProcessing$model$variables  # outlier/calendar vars
  coefs <- dsa2_object$preProcessing$model$b          # coefficients
  covar <- dsa2_object$preProcessing$model$bcov       # covariance matrix
  
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
  if (length(coef) == 1) {
    sterrs <- sqrt(covar)
  } else {
    sterrs <- sqrt(diag(covar))
  }
  tvals  <- coefs / sterrs
  
  # create df
  df <- data.frame(
    "Date"        = as.character(outlierDates),
    "Type"        = outlierTypes,
    "Coefficient" = format(round(coefs,  digits), n_small = digits),
    "Std.Error"   = format(round(sterrs, digits), n_small = digits),
    "t-Value"     = format(round(tvals,  digits), n_small = digits)
  )
  
  # sort by dates
  df <- df[order(outlierDates),]
  
  # return
  return(df)
  
}




  
 
#' Internal function for calendars
#' 
#' Internal function to polish the output for calendars
#' @param dsa2_object dsa2 output object
#' @author Sindy Brakemeier, Lea Hengen

.outCalendar <- function(dsa2_object, digits = 3) {
  
  # detect if any calendar matrix present
  if (is.null(dsa2_object$parameters$xreg)) {
    return(NULL)
  }
  
  # auxiliary variables
  dates <- zoo::index(dsa2_object$series)             # dates of time series
  xreg  <- dsa2_object$parameters$xreg                # calendar matrix
  vars  <- dsa2_object$preProcessing$model$variables  # outlier/calendar vars
  coefs <- dsa2_object$preProcessing$model$b          # coefficients
  covar <- dsa2_object$preProcessing$model$bcov       # covariance matrix
  
  # remove outliers
  n <- ncol(xreg)
  vars  <- vars[1:n]
  coefs <- coefs[1:n]
  covar <- covar[1:n, 1:n]
  
  # compute standard errors and t-values
  if (length(coef) == 1) {
    sterrs <- sqrt(covar)
  } else {
    sterrs <- sqrt(diag(covar))
  }
  tvals  <- coefs / sterrs
  
  # create df
  df <- data.frame(
    "Regressor"   = colnames(xreg),
    "Coefficient" = format(round(coefs,  digits), n_small = digits),
    "Std.Error"   = format(round(sterrs, digits), n_small = digits),
    "t-Value"     = format(round(tvals,  digits), n_small = digits)
  )

  # return
  return(df)
  
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
  name1 <- substring(name1,1,15)
  name2 <- deparse(substitute(dsa2_object2))
  name2 <- substring(name2,1,15)
  
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
  graphics::lines(dates, series2, col = .dsa2color("lightgreen"))
  graphics::lines(dates, series3, col = .dsa2color("violet"))
  graphics::par(col.axis = "transparent")
  graphics::axis(1, col.ticks = .dsa2color("grey"), graphics::axis.Date(1,dates))
  graphics::axis(2, col.ticks = .dsa2color("grey"))
  graphics::box(col = .dsa2color("grey"))
  .add_legend("bottom", legend = c("Original", paste0("Adjusted Series (", name1, ")"), paste0("Adjusted Series (", name2, ")")), lty = c(1,1),
              col = .dsa2color("darkblue", "lightgreen", "violet"),
              horiz = TRUE, bty = 'n', cex = 0.8)
  on.exit(graphics::par(opar))
}


#' HTML output for dsa2
#' 
#' HTML output for dsa2
#' @param x output object
#' @param fileName name for HTML output
#' @param filePath path for HTML output
#' @details Generates an .Rmd file that is rendered into an .html document saved in the working directory.
#' @author Lea Hengen, Sindy Brakemeier
#' @export

output <- function(x, fileName = NULL, filePath = NULL) {
  
  # if no file name is specified, use series name
  if (is.null(fileName)) {
    path <- x$parameters$name
  }
  
  # if no file path is specified, use current working directory 
  if (is.null(filePath)) {
    path <- getwd()
  }
  
  # render markdown file
  rmarkdown::render(
    input       = paste0(system.file(package = "dsa2"), "/rmd/output.Rmd"),
    output_file = paste0(filePath, "/", fileName, ".html"),
    params      = list(x = x, fileName = fileName),
    encoding    = 'UTF-8'
  )
  
}


#' Generic for ACF
#' 
#' Generic for ACF
#' @param ... parameters 
#' @author Daniel Ollech
#' @export

acf <- function(dsa2_object,  ...) {UseMethod("acf")} # This is how we define generics in S3

#' Generic for PACF
#' 
#' Generic for PACF
#' @param ... parameters 
#' @author Daniel Ollech
#' @export

pacf <- function(dsa2_object,  ...) {UseMethod("pacf")} # This is how we define generics in S3

#' Generic for spectrum
#' 
#' Generic for spectrum
#' @param ... parameters 
#' @author Daniel Ollech
#' @export

spectrum <- function(dsa2_object,  ...) {UseMethod("spectrum")} # This is how we define generics in S3

#' Plot the ACF based on dsa2 object
#'
#' Plot the ACF for a seasonally adjusted time series extracted from a dsa2 object 
#' @param dsa2_object object as calculated by dsa2()
#' @param ... further arguments for barplot()
#' @details Wrapper around the stats::acf() function
#' @author Daniel Ollech
#' @examples x <- tssim::sim_daily(3)$original
#' result <- dsa(x)
#' acf(result)
#' @export

acf.dsa2 <- function(dsa2_object, ...) {
  # Calculations before
  residuals <- dsa2_object$preProcessing$model$residuals
  residuals <- residuals[!is.na(residuals)]
  acf_result <- stats::acf(residuals, lag.max = 366 * 2, plot = FALSE)
  acf_res <- acf_result$acf[c(1:7 + 1, 30:31 + 1, 365 + 1, 730 + 1)]
  names(acf_res) <-
    as.character(acf_result$lag[c(1:7 + 1, 30:31 + 1, 365 + 1, 730 + 1)])
  ci <- stats::qnorm((1 + 0.95) / 2) / sqrt(length(residuals))
  
  # Plotting
  graphics::barplot(
    acf_res,
    ylim = c(-1, 1),
    main = "ACF for selected lags",
    xlab = "lags",
    ylab = "ACF",
    col = .dsa2color("darkgreen"),
    space = 6,
    border = NA,
    ...
  )
  graphics::abline(h = ci,
                   lty = 3,
                   col = .dsa2color("blue"))
  graphics::abline(h = -ci,
                   lty = 3,
                   col = .dsa2color("blue"))
  graphics::abline(v = 52.5, col = .dsa2color("darkgrey"))
  graphics::abline(v = 65, col = .dsa2color("darkgrey"))
}

#' Plot the PACF based on dsa2 object
#'
#' Plot the PACF for a seasonally adjusted time series extracted from a dsa2 object 
#' @param dsa2_object object as calculated by dsa2()
#' @param ... further arguments for barplot()
#' @details Wrapper around the stats::pacf() function
#' @author Daniel Ollech
#' @examples x <- tssim::sim_daily(3)$original
#' result <- dsa(x)
#' pacf(result)
#' @export

pacf.dsa2 <- function(dsa2_object, ...) {
  # Calculations before
  residuals <- dsa2_object$preProcessing$model$residuals
  residuals <- residuals[!is.na(residuals)]
  acf_result <- stats::pacf(residuals, lag.max = 366 * 2, plot = FALSE)
  acf_res <- acf_result$acf[c(1:7, 30:31, 365, 730)]
  names(acf_res) <-
    as.character(acf_result$lag[c(1:7, 30:31, 365, 730)])
  ci <- stats::qnorm((1 + 0.95) / 2) / sqrt(length(residuals))
  
  # Plotting
  graphics::barplot(
    acf_res,
    ylim = c(-1, 1),
    main = "PACF for selected lags",
    xlab = "lags",
    ylab = "PACF",
    col = .dsa2color("violet"),
    space = 6,
    border = NA,
    ...
  )
  graphics::abline(h = ci,
                   lty = 3,
                   col = .dsa2color("blue"))
  graphics::abline(h = -ci,
                   lty = 3,
                   col = .dsa2color("blue"))
  graphics::abline(v = 52.5, col = .dsa2color("darkgrey"))
  graphics::abline(v = 65, col = .dsa2color("darkgrey"))
}


#' Plot the periodogram of a daily time series
#'
#' Plot the periodogram of a daily time series
#' @param x xts or ts, daily timeseries
#' @param xlog should x-axis be log transformed
#' @param size linesize
#' @param color color of line
#' @param .dsa2color("pink") color of vertical lines
#' @details Plot uses ggplot2 and can be changed accordingly. The spectrum is build around the spec.pgram() function
#' @author Daniel Ollech
#' @examples x <- tssim::sim_daily(3)$original
#' res <- dsa(x)
#' spectrum(res)
#' @export

spectrum.dsa2 <- function(dsa2_object, xlog=FALSE) {
  # Calculations before
  original_diff <- diff(dsa2_object$series$original)
  original_diff <- ts(original_diff[!is.na(original_diff)], frequency = 365.2524)
  df <- data.frame(freq = stats::spec.pgram(original_diff, plot = F)$freq, spectrum = (stats::spec.pgram(original_diff, plot = F)$spec))
  
  seasadj_diff <- diff(dsa2_object$series$seas_adj)
  seasadj_diff <- ts(seasadj_diff[!is.na(seasadj_diff)], frequency = 365.2524)
  df2 <- data.frame(freq = stats::spec.pgram(seasadj_diff, plot = F)$freq, spectrum = (stats::spec.pgram(seasadj_diff, plot = F)$spec))
  
  
  opar <- graphics::par(no.readonly = TRUE)
  graphics::par(fig = c(0, 1, 0, 1), 
                oma = c(1, 1, 1, 1),
                mar = c(1.75, 2.75, 1.5, 0.5),
                mgp = c(1.75, 0.5, 0),
                cex.axis = 0.75)
  graphics::par(mfrow = c(2,1))
  .single_plot_spectrum(df, ylab = "Original", title = "Spectrum")
  graphics::par(mar = c(2.75, 2.75, 0.5, 0.5))
  .single_plot_spectrum(df2, ylab = "Seasonally Adjusted", title = "")
  
  
  on.exit(graphics::par(opar))
}


.single_plot_spectrum <- function(df, ylab = "Spectrum", title = "Spectrum") {
  graphics::plot(
    df,
    type = "l",
    log = "y",
    ylab = ylab,
    xlab = "Number of cycles",
    main = title
  )
  graphics::abline(v = 12, col = .dsa2color("orange"), lty = 3)
  graphics::abline(v = 24, col = .dsa2color("orange"), lty = 3)
  graphics::abline(v = 365.2524/7, col = .dsa2color("red"), lty = 4)
  graphics::abline(v = 365.2524/7*2, col = .dsa2color("red"), lty = 4)
  graphics::abline(v = 365.2524/7*3, col = .dsa2color("red"), lty = 4)
  graphics::lines(df)
}

#plot_spectrum(dsa2_object) #CH: keine Ahnung was die Zeile tun soll. 

