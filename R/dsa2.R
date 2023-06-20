#' Seasonal adjustment of daily time series
#' 
#' Seasonal adjustment of daily time series
#' @param series input time series in xts format 
#' @param xreg regressors used for calendar adjustment
#' @param log should multiplicative time series model be used
#' @param s7 method or specification used for adjustment of day-of-the-week
#' @param s31 method or specification used for adjustment of day-of-the-month
#' @param s365 method or specification used for adjustment of day-of-the-year
#' @param outliers should outliers be identified using regarima models
#' @param n_iterations number of iterations of step 2 to 4 (i.e. s7, s31 and s365)
#' @param h number of days to forecast
#' @param interpolator either "default", "CUBIC_SPLINE" or "NONE" (the last two inherited from rjd3bbksplines::interpolate31). See details
#' @param pre_processing Optionally include pre-processing results computed earlier
#' @param ... additional parameters from fractionalAirlineEstimation
#' @details DSA iteratively estimates and adjusts the calendar component, the day-of-the-week effect, if selected: the day-of-the-year effect, and the day-of-the-year effect to get the seasonally adjusted series.
#' For the estimation of the day-of-the-month effect, the months are extended to include 31 days in each months. This is done by filling up the artificial days (e.g. 31st of April) by NAs and then - if so chosen - filling up the missing values using spline interpolation. By default, if STL is used, the NAs are not filled up, if X-11 or Seats is used- the NAs are filled up.
#' @author Daniel Ollech, Christiane Hofer, Martin Stefan, Thomas Witthohn
#' @examples 
#' ## Create time series 
#' set.seed(2358)
#' all <- tssim::sim_daily(N=5)
#' series <- all$original 
#' 
#' ## Default adjustment
#' result <- dsa(series)
#' plot(result)
#' 
#' ## Set STL parameters to be used
#' result2 <- dsa(series, s7 = stl_method(swindow = 31, twindow=9), pre_processing = result)
#' 
#' ## Use STL and X11 in combination
#' result3 <- dsa(series, s7 = "x11", s365 = "stl", pre_processing = result) 
#' 
#' ## Compare results
#' compare_plot(result2, result3)
#' @references Ollech, Daniel (2018). Seasonal Adjustment of Daily Time Series. 
#' Bundesbank Discussion Paper 41/2018.
#' @references Ollech, Daniel (2021). Seasonal Adjustment of Daily Time Series. 
#' Journal of Time Series Econometrics 13 (2), 235-264.
#' @export

dsa <- function(series,  
                 xreg = NULL,
                 log = TRUE,
                 s7 = c("x11", "stl", "seats")[2], # NOTE(DO): Later x11 should be default
                 s31 = NULL,
                 s365 = c("x11", "stl", "seats")[2], # NOTE(DO): Later x11 should be default
                 outliers = c("AO", "LS", "WO"),
                 n_iterations = 1,
                 h = 365,
                 interpolator = "default",
                 pre_processing = NULL,
                 ...) {
  
  parameters <- as.list(environment(), all = TRUE)
  
  # Preliminary checks -----------------------------------------------------
  .preliminary_checks(series, outliers)
  
  # Vector of dates (needed for xts conversion) -------------------------------
  dates   <- seq.Date(from = as.Date(stats::start(series)),
                      by = "days",
                      length.out = length(series) + h)
  
  
  # Pre-processing -------------------------------------------------------------
  
  # User has not provided pre-processing results
  if (is.null(pre_processing)) {
    
    # Take logs
    # NOTE(Daniel): Should be handled in Java
    if (log) {
      series <- log(series) 
    }
    
    # Run fractional airline estimation
    fracAirline <- rjd3bbkhighfreqforecast::fractionalAirlineEstimation(
      y = series, 
      periods = c(7, 365.25), 
      x = xreg, 
      nfcasts = h,
      outliers = outliers,
      ...)
    
    # Undo logs
    # NOTE(DO): Should be handled in Java
    if (log) {
      series  <- exp(series) 
      fracAirline$model$linearized <- exp(fracAirline$model$linearized)
    } 
    
    # Extract calendar adjusted series and calendar component 
    # TODO(Daniel): Find out if calendar component should be centered?
    xLinear <- fracAirline$model$linearized
    calComp <- fracAirline$model$component_userdef_reg_variables
    if (is.null(calComp)) {
      calComp <- xLinear*0 # NOTE(DO): '+ ifelse(log, 1, 0)' has to be readded, once the exp/log is handled correctly in the fractional airline
    }
    
    if (log) calComp <- exp(calComp) # NOTE(DO): Should be handled in Java
    
  } else {
    fracAirline <- pre_processing$preProcessing
    xLinear <- fracAirline$model$linearized  
    calComp <- pre_processing$components$calComp
  }
  
  # Convert to xts-format
  xLinear <- xts::xts(xLinear, order.by = dates)
  calComp <- xts::xts(calComp, order.by = dates)
  
  # Preliminary seasonal components
  seasComp7   <- 0 * calComp + ifelse(log, 1, 0)
  seasComp31  <- 0 * calComp + ifelse(log, 1, 0)
  seasComp365 <- 0 * calComp + ifelse(log, 1, 0)
  
  
  
  # Seasonal adjustment --------------------------------------------------------
  
  for (j in seq(n_iterations)) {
    
    # S7 -----------------------------------------------------------------------
    xLinx <- compute_seasadj(xLinear, 
                               seasComp7 = NULL, 
                               seasComp31 = seasComp31, 
                               seasComp365 = seasComp365, 
                               log = log)
    xLinx <- ts(xLinx, frequency = 7)

    s7Result <- adjust(method = s7, series = xLinx) 
    seasComp7 <- xts::xts(s7Result$seasComp, 
                          zoo::index(seasComp7))
    
    # S31 ----------------------------------------------------------------------
    
    zLinz <- compute_seasadj(xLinear, 
                             seasComp7 = seasComp7, 
                             seasComp31 = NULL, 
                             seasComp365 = seasComp365, 
                             log = log)
    
    if (interpolator == "default") {
      if (class(s31) == "stl_method" | (class(s31) == "character" && s31 == "stl")) {
        interpolator <- "NONE"
      } else {
        interpolator <- "CUBIC_SPLINE" 
      }
    }
    
    xLinx <- ts(rjd3bbksplines::interpolate31(zLinz, 
                           interpolator = interpolator),
                frequency = 31)
                
    s31Result <- adjust(method = s31, series = xLinx) 
    
    seasComp31 <- xts::xts(rjd3bbksplines::reduce31(zLinz, s31Result$seasComp), 
                           zoo::index(seasComp31))
    
    # S365 ---------------------------------------------------------------------
    
    xLinx <- compute_seasadj(xLinear, 
                               seasComp7 = seasComp7, 
                               seasComp31 = seasComp31, 
                               seasComp365 = NULL, 
                               log = log)
    
    zLinz <- delete_29(xLinx)
    yLiny <- ts(as.numeric(zLinz), frequency = 365)
    s365Result <- adjust(method = s365, series = yLiny)
    seasComp365 <- xts::xts(s365Result$seasComp, zoo::index(zLinz))
    seasComp365 <- zoo::na.locf(xts::merge.xts(seasComp365, xLinear)[,1]) # Filling up the seasonal component with a value for 29.Feb. Should be handled in Java ## na.locf makes that value on 29.2 = value on 28.2
  }
  
  
  # Create output --------------------------------------------------------------
  original <- xts::xts(fracAirline$model$y, 
                       seq.Date(from = as.Date(stats::start(series)), 
                                by = "days", 
                                length.out = length(fracAirline$model$y))) 
  
  if (log) original <- exp(original) # Should be handled before in fracAirline/Java
  
  seas_adj <- compute_seasadj(original, 
                                seasComp7 = seasComp7, 
                                seasComp31 = seasComp31, 
                                seasComp365 = seasComp365, 
                                calComp = calComp,
                                log = log)
  
  
  series <- xts::merge.xts(original = original, seas_adj = seas_adj)
  colnames(series) <- c("original", "seas_adj")
  
  components <- xts::merge.xts(calComp = xts::xts(calComp, 
                                                  zoo::index(original)), 
                               seasComp7 = xts::xts(seasComp7, 
                                                  zoo::index(original)), 
                               seasComp31 = xts::xts(seasComp31, 
                                                   zoo::index(original)), 
                               seasComp365 = xts::xts(seasComp365,  
                                                    zoo::index(original)))
  colnames(components) <- c("calComp",
                            "seasComp7", 
                            "seasComp31",
                            "seasComp365")
  
  
  out <- list(series = series, 
              components = components, 
              preProcessing = fracAirline, 
              parameters = parameters)
  
  class(out) <- "dsa2"
  
  return(out)
}

#' Handler for stl
#' 
#' Handler for stl
#' @param period period
#' @param swindow swindow
#' @param log log
#' @param twindow twindow
#' @param ninnerloop ninnerloop
#' @param nouterloop nouterloop
#' @param nojump nojump
#' @param weight.threshold weight.threshold
#' @param weight.function weight.function
#' @details This functions is basically a translator between the dsa2 routines 
#' and rjd3stl::stl, but its goal is to invoke the stl procedure.
#' @author Daniel Ollech
#' @export

stl_method  <- function(period = NA, 
                        swindow = 13, 
                        log = TRUE, # NOTE(DO): Umbenennung von multiplicative in rjd3stl::stl
                        twindow = 0, 
                        ninnerloop = 1, 
                        nouterloop = 15, 
                        nojump = FALSE, 
                        weight.threshold = 0.001, 
                        weight.function = c('BIWEIGHT')
) 
{
  parameters <- list(period = period,
                     swindow = swindow,
                     multiplicative = log,
                     twindow = twindow,
                     ninnerloop = ninnerloop,
                     nouterloop = nouterloop,
                     nojump = nojump,
                     weight.threshold = weight.threshold,
                     weight.function = weight.function)

  
  class(parameters) <- c("stl_method")
  
  return(parameters)  
}

#' Handler for x-11
#' 
#' Handler for x-11
#' @param period period
#' @param log log
#' @param sma sma
#' @param trend.horizon t
#' @param trend.kernel t
#' @param trend.asymmetric t
#' @param sigma sigma
#' @author Daniel Ollech
#' @export

x11_method <- function(period = NA,   # NOTE(DO): Assumes use of rjd3highfreq::x11
                       log = TRUE, # NOTE(DO): Umbenennung von mul in rjd3highfreq::x11
                       sma = c("S3X9", "S3X1", "S3X3", "S3X5", "S3X15")[1], 
                       trend.horizon = 6, # NOTE(DO): Brauchen wir?
                       trend.degree = 2, # NOTE(DO): Brauchen wir?
                       trend.kernel = c("Henderson"),
                       trend.asymmetric = c("CutAndNormalize"),
                       sigma = c(1.5,2.5)) # NOTE(DO): Umbenennung von extreme.lsig, extreme.usig
{
  if (class(sma) == "numeric") {
    sma <- paste0("S3X", sma)
  }
  
  parameters <- list(period = period, 
                     mul = log, 
                     trend.horizon = trend.horizon, 
                     trend.degree = trend.degree,
                     trend.kernel = trend.kernel,
                     trend.asymmetric = trend.asymmetric,
                     seas.s0 = toupper(sma),
                     seas.s1 = toupper(sma),
                     extreme.lsig = sigma[1], 
                     extreme.usig = sigma[2])
  
  class(parameters) <- c("x11_method")
  
  return(parameters) 
  
}

#' Handler for seats
#' 
#' Handler for seats
#' @param period period
#' @param log log
#' @param sn sn
#' @param stde stde
#' @param nbcasts nbcasts
#' @param nfcasts nfcasts
#' @author Daniel Ollech
#' @export

seats_method <- function(period = period,  # NOTE(DO): Assumes use of rjd3highfreq::fractionalAirlineDecomposition, we might want to use rjd3bbkhighfreqforecast::multiAirlineDecomposition instead
                         log = FALSE, # NOTE(DO): Currently not implemented in rjd3bbkhighfreqforecast::fractionalAirlineDecomposition
                         sn = FALSE,
                         stde = FALSE,
                         nbcasts = 0,
                         nfcasts = 0
)  {
  parameters <- list(period = period, 
                     # multiplicative = log, NOTE(DO): currently not implemented
                     sn = sn, 
                     stde = stde, 
                     nbcasts = nbcasts, 
                     nfcasts = nfcasts)
  
  stop("Seats is currently not fully implemented, but we are working to implement it.")
  
  class(parameters) <- c("seats_method")
  
  return(parameters) 
}


#' Generic for adjusting a seasonal time series
#' 
#' Generic for adjusting a seasonal time series
#' @param method method to be employed
#' @param series time series to be adjusted
#' @author Daniel Ollech
#' @export

### NOTE(DO): Maybe the following line(s) have to be loaded at package
###           start-up once this thingy becomes a dsa2 package
adjust <- function(method, series) {UseMethod("adjust")} # This is how we define generics in S3

#' Default method for adjusting a seasonal time series
#' 
#' Default method for adjusting a seasonal time series
#' @param method method to be employed
#' @param series time series to be adjusted
#' @author Daniel Ollech
#' @export

adjust.default <- function(method, series) {
  message("The method should either be one of 'x11', 'stl' or 'seats' or a call to stl_method(), x11_method() or seats_method()")
}

#' STL method for adjusting a seasonal time series
#' 
#' STL method for adjusting a seasonal time series
#' @param method method to be employed
#' @param series time series to be adjusted
#' @author Daniel Ollech
#' @export

adjust.stl_method <- function(method, series) { 
  if (is.na(method$period)) {
    method$period <- stats::frequency(series)
  }
  
  adjustment <- do.call(rjd3stl::stl, append(list(series),
                                             method))
  return(list(adjustment = adjustment, seasComp = adjustment$decomposition[,4]))
}

#' X-11 method for adjusting a seasonal time series
#' 
#' X-11 method for adjusting a seasonal time series
#' @param method method to be employed
#' @param series time series to be adjusted
#' @author Daniel Ollech
#' @export

adjust.x11_method <- function(method, series) { 
  if (is.na(method$period)) {
    method$period <- stats::frequency(series)
  }
  
  adjustment <- do.call(rjd3highfreq::x11, append(list(series),
                                                       method))
  return(list(adjustment = adjustment, seasComp = adjustment$decomposition$s))
}

#' Seats method for adjusting a seasonal time series
#' 
#' Seats method for adjusting a seasonal time series
#' @param method method to be employed
#' @param series time series to be adjusted
#' @author Daniel Ollech
#' @export

adjust.seats_method <- function(method, series) { 
  if (is.na(method$period)) {
    method$period <- stats::frequency(series)
  }
  
  adjustment <- do.call(rjd3highfreq::fractionalAirlineDecomposition, append(list(series),
                                                       method))
  return(list(adjustment = adjustment, seasComp = adjustment$decomposition[,4])) 
}

#' Character method for adjusting a seasonal time series
#' 
#' Character method for adjusting a seasonal time series
#' @param method method to be employed
#' @param series time series to be adjusted
#' @author Daniel Ollech
#' @export

adjust.character <- function(method, series) { 
  if (method == "stl") {
    adjustment <- do.call(rjd3stl::stl, append(list(series),
                                               stl_method(
                                                 period = stats::frequency(series)
                                                 )))
    return(list(adjustment = adjustment, seasComp = adjustment$decomposition[,4])) 
  }
  
  if (method == "x11") {
    adjustment <- do.call(rjd3highfreq::x11, append(list(series),
                                                         x11_method(
                                                           period = stats::frequency(series)
                                                         )))
    return(list(adjustment = adjustment, seasComp = adjustment$decomposition$s)) 
  }
  
  if (method == "seats") {
    adjustment <- do.call(rjd3highfreq::fractionalAirlineDecomposition, append(list(series),
                                                           seats_method(
                                                             period = stats::frequency(series)
                                                           )))
    return(list(adjustment = adjustment, seasComp = adjustment$decomposition$s)) 
  }
}

#' NULL method for adjusting a seasonal time series
#' 
#' NULL method for adjusting a seasonal time series
#' @param method method to be employed
#' @param series time series to be adjusted
#' @author Daniel Ollech
#' @export

adjust.NULL <- function(method, series) {
  return(list(adjustment = NULL, seasComp = series*NA)) 
}



#' Compute calendar and seasonally adjusted time series
#' 
#' Compute calendar and seasonally adjusted time series
#' @param xLinx basic series
#' @param seasComp7 day-of-the-week component
#' @param seasComp31 day-of-the-month component
#' @param seasComp365 day-of-the-year component
#' @param calComp calendar component
#' @author Daniel Ollech
#' @export

compute_seasadj <- function(xLinear, 
                            seasComp7, 
                            seasComp31, 
                            seasComp365, 
                            calComp = NULL, 
                            log = TRUE) {
  
  # Replace NULL or NA components
  if (is.null(seasComp7) | all(is.na(seasComp7))) {
    seasComp7 <- xLinear * 0 + ifelse(log, 1, 0)
  }
  if (is.null(seasComp31) | all(is.na(seasComp31))) {
    seasComp31 <- xLinear * 0 + ifelse(log, 1, 0)
  }
  if (is.null(seasComp365) | all(is.na(seasComp365))) {
    seasComp365 <- xLinear * 0 + ifelse(log, 1, 0)
  }
  if (is.null(calComp) | all(is.na(seasComp365))) {
    calComp <- xLinear * 0 + ifelse(log, 1, 0)
  }
  
  # Compute adjusted figures
  xout <- Descaler(Scaler(xLinear, log = log) - 
                   Scaler(calComp, log = log) - 
                   Scaler(seasComp7, log = log) - 
                   Scaler(seasComp31, log = log) - 
                   Scaler(seasComp365, log = log), log = log) 

  # Return
  return(xout)
  
}


.preliminary_checks <- function(series, outliers) {
  if (is.null(series)) { 
    stop("Series is NULL")
  }
  if (length(series) < 365*2 + 1) { 
    if (length(series) < 15) {
      stop("Series is too short to use dsa2 on it.")
    } else {
      warning("Series it too short to estimate a day-of-the-year effect, 
              and probably is too short for a good forecast. 
              We would like more than 2 years of observations.")
    }
  }
  
  if (any(outliers == "TC")) {
    warning("The outlier type TC is not implemented in the Fractional Airline Estimation function")
  }
}

