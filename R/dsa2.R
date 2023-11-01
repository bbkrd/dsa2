#' Seasonal adjustment of daily time series
#' 
#' Seasonal adjustment of daily time series
#' @param series input time series in xts format 
#' @param xreg regressors used for calendar adjustment
#' @param log should multiplicative time series model be used
#' @param s7 method or specification used for adjustment of day-of-the-week
#' @param s31 method or specification used for adjustment of day-of-the-month
#' @param s365 method or specification used for adjustment of day-of-the-year
#' @param outliers which outliers should be identified (LS, AO, WO). Set NULL if none shall be searched
#' @param critical_value threshold to include outliers to be used in the automatic outlier estimation
#' @param n_iterations number of iterations of step 2 to 4 (i.e. s7, s31 and s365), at least 1
#' @param h number of days to forecast
#' @param interpolator either "default" ("NONE" if stl is used, "CUBIC_SPLINES" else), "CUBIC_SPLINE" or "NONE". See details
#' @param pre_processing Optionally include pre-processing results computed earlier using dsa2 result (see examples) 
#' @param ... additional parameters from fractionalAirlineEstimation
#' @details DSA iteratively estimates and adjusts the calendar component, the day-of-the-week effect, if selected: the day-of-the-month effect, and the day-of-the-year effect to get the seasonally adjusted series.
#' For the estimation of the day-of-the-month effect, the months are extended to include 31 days in each months. This is done by filling up the artificial days (e.g. 31st of April) by NAs and then - if so chosen - filling up the missing values using spline interpolation. By default, if STL is used, the NAs are not filled up, if X-11 or Seats is used- the NAs are filled up.
#' Is is not possible to use different decomposition schemes for the single dsa2 steps. This means you cannot use a multiplicative model for the day-of-the-week and an additive model for the day-of-the-year. Therefore, the global specification og log = TRUE or log = FALSE is used for all steps.
#' @author Daniel Ollech, Christiane Hofer, Martin Stefan, Thomas Witthohn
#' @examples
#' Sys.setenv("JAVA_HOME"="C:/Workspace/Java/JDK/jdk-17.0.3+7") ## Currently start with this
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
                 critical_value = 6,
                 n_iterations = 1,
                 h = 365,
                 interpolator = "default",
                 pre_processing = NULL,
                 ...) {
  
  parameters <- as.list(environment(), all = TRUE)
  parameters$name <-  deparse(substitute(series)) # Get name of object inputted as series
  
  # Preliminary checks -----------------------------------------------------
  .preliminary_checks(series, outliers, xreg, h)
  
  if (any( (inherits(s7, "character") && s7 == "seats") |  # Note(DO): If-clause needs to be deleted once Seats can deal with multiplicative models
           inherits(s7, "seats_method") |  
           (inherits(s31, "character") && s31 == "seats") | 
           inherits(s31, "seats_method") |  
           (inherits(s365, "character") && s365 == "seats") | 
           inherits(s365, "seats_method")) & 
      log) {
    log <- FALSE
    message("The log parameter has been set to FALSE. Thus, an additive model is employed for *all* steps of DSA2. \nThis is because Seats currently cannot handle multiplicative models.")
  } 
  
  # Vector of dates (needed for xts conversion) -------------------------------
  dates   <- seq.Date(from = as.Date(stats::start(series)),
                      by = "days",
                      length.out = length(series) + h)
  
  
  # Pre-processing -------------------------------------------------------------
  
  # User has not provided pre-processing results
  if (is.null(pre_processing)) {

    # Run fractional airline estimation
    fracAirline <- rjd3highfreq::fractionalAirlineEstimation(
      y = series, 
      periods = c(7, 365.25), 
      x = xreg, 
      nfcasts = h,
      outliers = outliers,
      criticalValue = critical_value,
      log = log,
      ...)
    
    # Extract calendar adjusted series and calendar component 
    # TODO(Daniel): Find out if calendar component should be centered?
    xLinear <- fracAirline$model$linearized
    calComp <- xts::xts(fracAirline$model$component_userdef_reg_variables,
                                 order.by = dates)
} else { 
  
    if (pre_processing$parameters$name != deparse(substitute(series))){
      warning("Pre-processing used is not based on the time series to be adjusted")
    }
    
    fracAirline <- pre_processing$preProcessing
    xLinear <- pre_processing$preProcessing$model$linearized  
    calComp <- pre_processing$components$calComp
  }
  
  # Convert to xts-format
  xLinear <- .descaler(xts::xts(xLinear, order.by = dates),
                      log = log)

  # Preliminary seasonal components # with does nothing
  seasComp7   <- 0 * calComp + ifelse(log, 1, 0)
  seasComp31  <- 0 * calComp + ifelse(log, 1, 0)
  seasComp365 <- 0 * calComp + ifelse(log, 1, 0)
  
  
  
  # Seasonal adjustment --------------------------------------------------------
  
  for (j in seq(n_iterations)) {
    
    # S7 -----------------------------------------------------------------------
    xLinx <- compute_seasadj(series = xLinear, 
                             seasComp7 = NULL, 
                             seasComp31 = seasComp31, 
                             seasComp365 = seasComp365, 
                             log = log)
    xLinx <- stats::ts(xLinx, frequency = 7)

    s7Result <- .estimate_component(method = s7, series = xLinx, log = log) 
    seasComp7 <- xts::xts(s7Result$seasComp, 
                          zoo::index(seasComp7))
    
    # S31 ----------------------------------------------------------------------
    
    zLinz <- compute_seasadj(series = xLinear, 
                             seasComp7 = seasComp7, 
                             seasComp31 = NULL, 
                             seasComp365 = seasComp365, 
                             log = log)
    
    if (interpolator == "default") {
      if (inherits(s31, "stl_method") | (inherits(s31, "character") && s31 == "stl")) {
        interpolator <- "NONE"
      } else {
        interpolator <- "CUBIC_SPLINE" 
      }
    }
    
    xLinx <- stats::ts(interpolate31(zLinz, 
                           interpolator = interpolator),
                frequency = 31)
    
    s31 <- s31
                
    s31Result <- .estimate_component(method = s31, series = xLinx, log = log) 
    
    seasComp31 <- xts::xts(reduce31(zLinz, s31Result$seasComp), 
                           zoo::index(seasComp31))
    
    # S365 ---------------------------------------------------------------------
    
    xLinx <- compute_seasadj(series = xLinear, 
                             seasComp7 = seasComp7, 
                             seasComp31 = seasComp31, 
                             seasComp365 = NULL, 
                             log = log)
    
    zLinz <- delete_29(xLinx)
    yLiny <- stats::ts(as.numeric(zLinz), frequency = 365)
    s365Result <- .estimate_component(method = s365, series = yLiny, log = log)
    seasComp365 <- xts::xts(s365Result$seasComp, zoo::index(zLinz))
    seasComp365 <- zoo::na.locf(xts::merge.xts(seasComp365, xLinear)[,1]) # Filling up the seasonal component with a value for 29. Feb. Should be handled in Java ## na.locf makes that value on 29.2 = value on 28.2
  }
  
  
  # Create output --------------------------------------------------------------
  original <- xts::xts(fracAirline$model$y,
                       seq.Date(from = as.Date(stats::start(series)), 
                                by = "days", 
                                length.out = length(fracAirline$model$y))) 
  
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
  
  adjustmentResults <- list(s7Result = s7Result, 
                            s31Result = s31Result,
                            s365Result = s365Result)
  
  out <- list(series = series, 
              components = components, 
              preProcessing = fracAirline,
              adjustmentResults = adjustmentResults,
              parameters = parameters)
  
  class(out) <- "dsa2"
  
  return(out)
}

#' Translation between input to dsa() and rjd3-function
#' 
#' Handler for stl
#' @param period frequency which shall be adjusted, numeric
#' @param swindow number of observations included in the local regressions calculated to obtain the seasonal component
#' @param log log, ignored in dsa2
#' @param twindow number of observations included in local regressions for trend component
#' @param ninnerloop number of inner loops of STL
#' @param nouterloop number of outer loops of STL
#' @param nojump impacts the precision of the estimation
#' @param weight.threshold threshold for weights, see ? rjd3stl::stlplus for details
#' @param weight.function wfunction for weights, see ? rjd3stl::stlplus for details
#' @details This functions is basically a translator between the dsa2 routines 
#' and rjd3stl::stlplus, but its goal is to invoke the stl procedure.
#' It cannot be used to change the decomposition scheme (additive/multiplicative) for a single step in DSA2.
#' @author Daniel Ollech
#' @export

stl_method <- function(period = NA, 
                        swindow = 13, 
                        log = NULL, # NOTE(DO): Umbenennung von multiplicative in rjd3stl::stlplus
                        twindow = 0, 
                        ninnerloop = 1, 
                        nouterloop = 15, 
                        nojump = FALSE, 
                        weight.threshold = 0.001, 
                        weight.function = c('BIWEIGHT')
) 
{
  
  # Pre-tests ---------------------------------------------------------------

  if (!is.logical(log) & !is.null(log)) {
      warning("log needs to be a boolean")
    }
  
  if (!is.logical(nojump)) {
    warning("nojump needs to be a boolean")
  }
  
  if (!is.na(period)) {
    if (!is.numeric(period) & !is.integer(period)) {
      stop("period in stl_method() needs to be of class numeric or integer")
    }
    if (period < 2) {warning("period in stl_method() should be at least 2")}
  }
  
  if ((!is.numeric(swindow) & !is.integer(swindow))) {
    warning("swindow in stl_method() need to be of class numeric or integer")
  }
  
  if ((!is.numeric(twindow) & !is.integer(twindow)) |
      (!is.numeric(ninnerloop) & !is.integer(ninnerloop)) |
      (!is.numeric(nouterloop) & !is.integer(nouterloop)) |
      (!is.numeric(weight.threshold) & 
       !is.integer(weight.threshold))) {
    warning("twindow, ninnerloop, nouterloop and weight.threshold in 
             stl_method() need to be of class numeric or integer")
  }
  
  if (!is.character(weight.function)) {
    warning("weight.function in stl_method() need to be of class character")
  }
  
  # Translation of parameters -----------------------------------------------
  
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

#' Translation between input to dsa() and rjd3-function
#' 
#' Handler for x-11
#' @param period frequency which shall be adjusted, numeric
#' @param log log, ignored in dsa2
#' @param sma seasonal moving average used
#' @param trend.horizon band.width of trend filters, see ?rjd3x11plus::x11plus for details
#' @param trend.degree polynomial order of the local trend model, see ?rjd3x11plus::x11plus for details
#' @param trend.kernel kernel weights in objective function, see ?rjd3x11plus::x11plus for details
#' @param trend.asymmetric truncation type to obtain asymmetric from symmetric filter, see ?rjd3x11plus::x11plus for details
#' @param sigma lower and upper limit for the extreme value detection
#' @details This functions is basically a translator between the dsa2 routines 
#' and rjd3x11plus::x11plus, but its goal is to invoke the X-11 procedure.
#' It cannot be used to change the decomposition scheme (additive/multiplicative) for a single step in DSA2.
#' @author Daniel Ollech
#' @export

x11_method <- function(period = NA,   # NOTE(DO): Assumes use of rjd3x11plus::x11plus
                       log = NULL, # NOTE(DO): Renaming  mul in rjd3x11plus::x11plus for harmonization purposes
                       sma = c("S3X9", "S3X1", "S3X3", "S3X5", "S3X15")[1], 
                       trend.horizon = 6,  
                       trend.degree = 2,  
                       trend.kernel = c("Henderson"),
                       trend.asymmetric = c("CutAndNormalize"),
                       sigma = c(1.5,2.5)) # NOTE(DO): Renaming extreme.lsig, extreme.usig for harmonization purposes
{
  if (inherits(sma, "numeric")) {
    sma <- paste0("S3X", sma)
  }
  
  # Pre-tests ---------------------------------------------------------------
  if (!is.logical(log) & !is.null(log)) {
    warning("log needs to be a boolean")
  }
  
  if (!is.na(period)) {
    if (!is.numeric(period) & !is.integer(period)) {
      stop("period in x11_method() needs to be of class numeric or integer")
    }
    if (period < 2) {warning("period in x11_method() should be at least 2")}
  }

  if ((!is.numeric(trend.horizon) & !is.integer(trend.horizon)) |
      (!is.numeric(trend.degree) & !is.integer(trend.degree))) {
    warning("trend.horizon and trend.degree in x11_method() need to be of 
            class numeric or integer")
  }

  if (!is.character(trend.kernel) |
      !is.character(trend.asymmetric)) {
    warning("trend.kernel and trend.asymmetric in x11_method() need to be of 
            class character")
  }
  
  if (length(sigma) != 2) {
    warning("sigma needs to be of length 2, e.g. c(1.5, 2.5)")
  }
  
  # Translation of parameters -----------------------------------------------
  
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

#' Translation between input to dsa() and rjd3-function
#' 
#' Handler for seats
#' @param period frequency which shall be adjusted, numeric
#' @param log log, ignored in dsa2
#' @param sn shall the series be decomposed into two components (signal and noise)
#' @param stde boolean, shall standard deviations of components be computed
#' @param nbcasts number of forecasts
#' @param nfcasts number of backcasts
#' @details This functions is basically a translator between the dsa2 routines 
#' and rjd3highfreq::fractionalAirlineDecomposition, but its goal is to invoke the Seats procedure.
#' It cannot be used to change the decomposition scheme (additive/multiplicative) for a single step in DSA2.
#' @author Daniel Ollech
#' @export

seats_method <- function(period = NA,  # NOTE(DO): Assumes use of rjd3highfreq::fractionalAirlineDecomposition
                         log = NULL, # NOTE(DO): Currently not implemented 
                         sn = FALSE,
                         stde = FALSE,
                         nbcasts = 0,
                         nfcasts = 0
)  {
  

# Pre-tests ---------------------------------------------------------------
if (!is.na(period)) {
  if (!is.numeric(period) & !is.integer(period)) {
    stop("period in seats_method() needs to be of class numeric or integer")
  }
  if (period < 2) {warning("period in seats_method() should be at least 2")}
}
  
if (!is.logical(sn) | !is.logical(stde)) {
  warning("sn and stde in seats_method() should be either TRUE or FALSE")
}
  
if ((!is.numeric(nbcasts) & !is.integer(nbcasts)) |
    (!is.numeric(nfcasts) & !is.integer(nfcasts))) {
  warning("nbcasts and nfcasts in seats_method() need to be of class numeric or integer")
}
  
  if (!is.logical(log) & !is.null(log)) {
    warning("log needs to be a boolean")
  }

# Translation of parameters -----------------------------------------------

  parameters <- list(period = period, 
                     # multiplicative = log, NOTE(DO): currently not implemented
                     sn = sn, 
                     stde = stde, 
                     nbcasts = nbcasts, 
                     nfcasts = nfcasts)
  
  if (!is.null(log) && log) {stop("Seats is currently not fully implemented. Therefore, please do not use a multiplicative model, i.e. set log = FALSE.")}

  class(parameters) <- c("seats_method")
  
  return(parameters) 
}


#' Generic for estimating seasonal component
#' 
#' Generic for estimating seasonal component
#' @param method method to be employed
#' @param series time series to be adjusted
#' @param log should logs be used
#' @author Daniel Ollech
#' @keywords internal

.estimate_component <- function(method, series, log = NULL) {UseMethod(".estimate_component")} # This is how we define generics in S3

#' Default method for estimating seasonal component
#' 
#' Default method for estimating seasonal component. 
#' @param method method to be employed
#' @param series time series to be adjusted
#' @param log multiplicative models used
#' @author Daniel Ollech
#' @keywords internal

.estimate_component.default <- function(method, series, log = NULL) {
  message("The method should either be one of 'x11', 'stl' or 'seats' or a call to stl_method(), x11_method() or seats_method()")
}

#' STL method for estimating seasonal component
#' 
#' STL method for estimating seasonal component
#' @param method method to be employed
#' @param series time series to be adjusted, class should be ts
#' @param log should logs be used
#' @author Daniel Ollech
#' @keywords internal

.estimate_component.stl_method <- function(method, series, log = NULL) { 
  if (is.na(method$period)) {
    method$period <- stats::frequency(series)
  }
  
  if (!is.null(log)) {method$multiplicative <- log}
  
  adjustment <- do.call(rjd3stl::stlplus, append(list(series),
                                             method))
  return(list(adjustment = adjustment, seasComp = adjustment$decomposition[,4]))
}

#' X-11 method for estimating seasonal component
#' 
#' X-11 method for estimating seasonal component
#' @param method method to be employed
#' @param series time series to be adjusted, class should be ts
#' @param log should logs be used
#' @author Daniel Ollech
#' @keywords internal

.estimate_component.x11_method <- function(method, series, log = NULL) { 
  if (is.na(method$period)) {
    method$period <- stats::frequency(series)
  } else {
    if (method$period != stats::frequency(series)) {
      warning(paste0("The period in the method used is set to ", method$period, ", but in this step of DSA2 we usually want to estimate a periodic component with period length ", stats::frequency(series)))
    }
  }
  
  if (!is.null(log)) {method$mul <- log}
  
  adjustment <- do.call(rjd3x11plus::x11plus, append(list(series),
                                                     .correct_filter_length(method, # remove correct_filter_length once handled in Java. Then instead of correct_filter_length(..), we here just need to put in "method"
                                                                           series)))
  return(list(adjustment = adjustment, seasComp = adjustment$decomposition$s))
}

#' Check the chosen X-11 filter
#' 
#' If X-11 is used on the day-of-the-year effect, the number of observations included in the time series needs to be high enough for certain filters to be used. This function checks it.
#' @param method method to be employed
#' @param series time series to be adjusted, class should be ts
#' @author Daniel Ollech
#' @keywords internal

.correct_filter_length <- function(method, series) { # Can be removed, once handled in Java
  all_filters <- c(15, 9, 5, 3, 1)
  oldfilter <- method$seas.s1
  filter <- as.numeric(gsub("S3X",
                            "",
                            oldfilter))
  position <- grep(paste0("^", filter, "$"), all_filters)
  
  length_series <- length(series) / 365
  if (method$period >= 365) {
    if (length_series < 3) {
      stop("Series needs to have at least 3 years of observations to use X-11")
    } else {
      while (length_series < filter + 2) {
        position <- position + 1
        
        filter <- all_filters[position]
      }
      
      method$seas.s0 <- paste0("S3X", filter)
      method$seas.s1 <- paste0("S3X", filter)
      if (oldfilter != method$seas.s1) {
        message(
          paste(
            "The seasonal filter for X-11 (in the estimation of day-of-the-year) has been changed to",
            method$seas.s1
          )
        )
      }
    }
  }
  
  return(method) 
}



#' Seats method for estimating seasonal component
#' 
#' Seats method for estimating seasonal component
#' @param method method to be employed
#' @param series time series to be adjusted, class should be ts
#' @param log should logs be used
#' @author Daniel Ollech
#' @keywords internal

.estimate_component.seats_method <- function(method, series, log = NULL) { 
  if (is.na(method$period)) {
    method$period <- stats::frequency(series)
  }
  
  #if (!is.null(log)) {method$multiplicative <- log} # Note(DO): Needs to be activated once Seats can deal with multiplicative models
  
  adjustment <- do.call(rjd3highfreq::fractionalAirlineDecomposition, append(list(series),
                                                                              method))
  return(list(adjustment = adjustment, seasComp = adjustment$decomposition$s)) 
}

#' Character method for estimating seasonal component
#' 
#' Character method for estimating seasonal component
#' @param method method to be employed
#' @param series time series to be adjusted, class should be ts
#' @param log should logs be used
#' @author Daniel Ollech
#' @keywords internal

.estimate_component.character <- function(method, series, log = NULL) {
  if (method == "stl") {
    method <- stl_method()
  } else if (method == "x11") {
    method <- x11_method()
  } else if (method == "seats") {
    method <- seats_method()
  } else{
    stop("Unknown method!")
  }
  return(.estimate_component(
    method = method,
    series = series,
    log = log
  )) 
}

#' NULL method for estimating seasonal component
#' 
#' NULL method for estimating seasonal component
#' @param method method to be employed
#' @param series time series to be adjusted
#' @param log multiplicative models used
#' @author Daniel Ollech
#' @keywords internal

.estimate_component.NULL <- function(method, series, log = NULL) {
  return(list(adjustment = NULL, seasComp = series*NA)) 
}



#' Compute calendar and seasonally adjusted time series
#' 
#' Compute calendar and seasonally adjusted time series
#' @param series original, xts time series
#' @param seasComp7 day-of-the-week component, xts time series
#' @param seasComp31 day-of-the-month component, xts time series
#' @param seasComp365 day-of-the-year component, xts time series
#' @param calComp calendar component, xts time series
#' @param log do we use a multiplicative model
#' @details All time series used should be of class xts or a numeric constant. 
#' @author Daniel Ollech
#' @export

compute_seasadj <- function(series, 
                            seasComp7 = NULL, 
                            seasComp31 = NULL, 
                            seasComp365 = NULL, 
                            calComp = NULL, 
                            log = TRUE) {
  
  # Replace NULL or NA components
  if (is.null(seasComp7) | all(is.na(seasComp7))) {
    seasComp7 <- series * 0 + ifelse(log, 1, 0)
  }
  if (is.null(seasComp31) | all(is.na(seasComp31))) {
    seasComp31 <- series * 0 + ifelse(log, 1, 0)
  }
  if (is.null(seasComp365) | all(is.na(seasComp365))) {
    seasComp365 <- series * 0 + ifelse(log, 1, 0)
  }
  if (is.null(calComp) | all(is.na(calComp))) {
    calComp <- series * 0 + ifelse(log, 1, 0)
  }
  
  # Compute adjusted figures
  xout <- .descaler(.scaler(series, log = log) - 
                   .scaler(calComp, log = log) - 
                   .scaler(seasComp7, log = log) - 
                   .scaler(seasComp31, log = log) - 
                   .scaler(seasComp365, log = log), log = log) 

  # Return
  return(xout)
  
}


.preliminary_checks <- function(series, outliers, xreg, h) {
  ### Preliminary checks to check the input parameters in dsa2
  if (is.null(series)) { 
    stop("Series is NULL")
  }
  
  if (length(series) < 365*2 + 1) { 
    stop("Series is too short to use dsa2 on it")
  }
  
  if (!inherits(series, "xts")) {
    stop("Class of series should be xts")
  }
  
  if (any(outliers == "TC")) {
    warning("The outlier type TC is not implemented in the Fractional Airline Estimation function")
  }
  
  if (!is.null(xreg)) {
    if (length(series) + h != nrow(xreg)) {
      stop(
        "The number of observations included in xreg needs to be equal to the length of the series plus the number of observations to forecast (given by h)"
      )
    }
    
    if (!is.null(dim(xreg))) {
    if(any(apply(xreg[1:length(series),], 2, stats::sd) == 0)) {
      get_problem_regressor <- seq(ncol(xreg))[apply(xreg[1:length(series),], 2, stats::sd) == 0]
      stop(paste0("Some of the regressors included are constants with respect to the observations used in the fractional airline model, i.e all but the last ", h, " observations are a constant. The following columns need to be checked: ", paste0(get_problem_regressor, collapse=" ")))
    }
    }
  }
  
  
  
}

