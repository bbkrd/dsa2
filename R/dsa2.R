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
#' @param pre_processing Optionally include pre-processing results computed earlier
#' @param ... additional parameters from fractionalAirlineEstimation
#' @author Daniel Ollech, Martin Stefan
#' @examples dsa2(dsa::daily_sim(n=5, year_effect=5)$original)  
#' @details This function implements the DSA2 procedure that facilitates the 
#' seasonal adjustment of daily time series.
#' @references Ollech, Daniel (2018). Seasonal Adjustment of Daily Time Series. 
#' Bundesbank Discussion Paper 41/2018.
#' @references Ollech, Daniel (2021). Seasonal Adjustment of Daily Time Series. 
#' Journal of Time Series Econometrics 13 (2), 235-264.
#' @export



dsa2 <- function(series, 
                 xreg = NULL,
                 log = TRUE,
                 s7 = c("x11", "stl", "seats")[2], # NOTE(DO): Later x11 should be default
                 s31 = NULL,
                 s365 = c("x11", "stl", "seats")[2], # NOTE(DO): Later x11 should be default
                 outliers = c("AO", "LS", "TC"),
                 n_iterations = 1,
                 h = 365,
                 pre_processing = NULL,
                 ...) {
  
  parameters <- as.list(environment(), all=TRUE)
  
  # TODO(Martin): xts und ts stuff ####
  
  
  # pre-processing ----------------------------------------------------------
  if (is.null(pre_processing)) { # Standard_case
    
    if (log) series <- log(series) ### NOTE(DO): Should be handled in Java
    model <- rjd3bbkhighfreqforecast::fractionalAirlineEstimation(y=series, 
                                                       periods=c(7, 365.25), 
                                                       x=xreg, 
                                                       nfcasts = h,
                                                       outliers=outliers,
                                                       ...)
    
    
    
    xLinear <- xts::xts(model$model$linearized, zoo::index(series)) # calendar adjusted
    calComp <- component_userdef_reg_variables # calendar factor # NOTE(DO): Should it be centered?
    
    if (log){### NOTE(DO): Should be handled in Java
      series <- exp(series) 
      calComp <- exp(calComp)
    } 
    
  } else {
    model = pre_processing$preProcessing
    xLinear = model$model$linearized  
  }
  
  # Preliminary seasonal component
  seasComp7 <- seasComp31 <- seasComp365 <- 0 * calComp + ifelse(log, 1, 0)
  
  # Seasonal adjustment -----------------------------------------------------
  
  for (j in seq(n_iterations)) {
    
    # S7 --------------------------------------------------------------------
    xLinx <- compute_seasadj(xLinear, 
                               seasComp7=NULL, 
                               seasComp31=seasComp31, 
                               seasComp365=seasComp365, 
                               log=log)
    frequency(xLinx) <- 7
    s7Result <- adjust(method=s7, series=xLinx) 
    seasComp7 <- s7Result$seasComp
    
    # S31 --------------------------------------------------------------------
    
    zLinz <- compute_seasadj(xLinear, 
                             seasComp7=seasComp7, 
                             seasComp31=NULL, 
                             seasComp365=seasComp365, 
                             log=log)
    
    xLinx <- ts(rjd3bbksplines::interpolate31(zLinz, 
                           interpolator = "CUBIC_SPLINE"),
                frequency = 31)
                
    s31Result <- adjust(method=s31, series=zLinz) 
    
    seasComp31 <- rjd3bbksplines::reduce31(s31Result$seasComp, zLinz)
    
    # S365 --------------------------------------------------------------------
    
    xLinx <- compute_seasadj(xLinear, 
                               seasComp7=seasComp7, 
                               seasComp31=seasComp31, 
                               seasComp365=NULL, 
                               log=log)
    
    zLinz <- delete_29(xLinx)
    frequency(zLinz) <- 365
    s365Result <- adjust(method=s365, series=zLinz) 
    seasComp365 <- s365Result$seasComp
    seasComp365 <- zoo::na.locf(merge(seasComp365, seasComp7)[,1]) # Filling up the seasonal component with a value for 29.Feb. Should be handled in Java
  }
  
  
  # Create output -----------------------------------------------------------
  original <- xts::xts(model$model$y, 
                       seq.Date(from=as.Date(stats::start(series)), 
                                by="days", 
                                length.out=length(model$model$y)) 
                       
  seas_adj <- compute_seasadj(original, 
                                seasComp7=seasComp7, 
                                seasComp31=seasComp31, 
                                seasComp365=seasComp365, 
                                log=log)
  
  
  series <- xts::merge.xts(original=original, seas_adj = seas_adj)
  components <- xts::merge.xts(calComp = xts::xts(calComp, 
                                                  zoo::index(original)), 
                               seasComp7=xts::xts(seasComp7 
                                                  zoo::index(original)), 
                               seasComp31=xts::xts(seasComp31, 
                                                   zoo::index(original)), 
                               seasComp365=xts::xts(seasComp365,  
                                                    zoo::index(original)))
  
  
  out <- list(series=series, 
              components=components, 
              preProcessing=model, 
              parameters=parameters)
  
  class(out) <- "dsa2"
  
  return(out)
}



stl_method  <- function(period = stats::frequency(series), 
                        swindow = 13, 
                        log = TRUE, # NOTE(DO): Umbenennung von mul
                        twindow=0, 
                        ninnerloop=1, 
                        nouterloop=15, 
                        nojump=FALSE, 
                        weight.threshold=0.001, 
                        weight.function=c('BIWEIGHT')
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

x11_method <- function(period = stats::frequency(series),   # NOTE(DO): Assumes use of rjd3highfreq::x11
                       log=TRUE, # NOTE(DO): Umbenennung von mul
                       sma=c("S3X3", "S3X1", "S3X5", "S3X9", "S3X15"), 
                       trend.horizon=6, # NOTE(DO): Brauchen wir?
                       trend.degree=2, # NOTE(DO): Brauchen wir?
                       trend.kernel=c("Henderson"),
                       trend.asymmetric=c("CutAndNormalize"),
                       sigma=c(1.5,2.5)) # NOTE(DO): Umbenennung von extreme.lsig, extreme.usig
{
  parameters <- list(period = stats::frequency(series), 
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


seats_method <- function(period = stats::frequency(series),  # NOTE(DO): Assumes use of rjd3highfreq::fractionalAirlineDecomposition, we might want to use rjd3bbkhighfreqforecast::multiAirlineDecomposition instead
                         log=TRUE, # NOTE(DO): Currently not implemented in rjd3bbkhighfreqforecast::fractionalAirlineDecomposition
                         sn = FALSE,
                         stde = FALSE,
                         nbcasts = 0,
                         nfcasts = 0
)  {
  parameters <- list(period = stats::frequency(series), 
                     # multiplicative = log, NOTE(DO): currently not implemented
                     sn = sn, 
                     stde = stde, 
                     nbcasts = nbcasts, 
                     nfcasts = nfcasts)
  
  class(parameters) <- c("seats_method")
  
  return(parameters) 
}



### NOTE(DO): Maybe the following line(s) have to be loaded at package
###           start-up once this thingy becomes a dsa2 package
adjust <- function(method, series) {UseMethod("adjust")} # This is how we define generics in S3
adjust.default <- function(method, series) {
  message("The method should either be one of 'x11', 'stl' or 'seats' or a call to stl_method(), x11_method() or seats_method()")
}

adjust.stl_method <- function(method, series) { 
  adjustment <- do.call(rjd3stl::stl, append(list(series),
                                             method))
  return(list(adjustment=adjustment, seasComp=adjustment$seasComp))
}

adjust.x11_method <- function(method, series) { 
  adjustment <- do.call(rjd3highfreq::x11, append(list(series),
                                                       method))
  return(list(adjustment=adjustment, seasComp=adjustment$seasComp))
}

adjust.seats_method <- function(method, series) { 
  adjustment <- do.call(IRGENDEINE_seats_METHODE, append(list(series),
                                                       method))
  return(list(adjustment=adjustment, seasComp=adjustment$seasComp)) 
}

adjust.character <- function(method, series) { 
  if (method == "stl") {
    adjustment <- do.call(rjd3stl::stl, append(list(series),
                                               stl_method()))
    return(list(adjustment=adjustment, seasComp=adjustment$seasComp)) 
  }
  
  if (method == "x11") {
    adjustment <- do.call(IRGENDEINE_X11_METHODE, append(list(series),
                                                         x11_method()))
    return(list(adjustment=adjustment, seasComp=adjustment$seasComp)) 
  }
  
  if (method == "seats") {
    adjustment <- do.call(IRGENDEINE_Seats_METHODE, append(list(series),
                                                           seats_method()))
    return(list(adjustment=adjustment, seasComp=adjustment$seasComp)) 
  }
}

adjust.NULL <- function(method, series) {
  return(list(adjustment=NULL, seasComp=series*NA)) 
}




compute_seasadj <- function(xLinx, seasComp7=seasComp7, seasComp31=seasComp31, 
                            seasComp365=seasComp365, calComp=NULL, log=TRUE) {
  
  if (is.null(seasComp7) | all(is.na(seasComp7))) {seasComp7 <- series * 0 + ifelse(log, 1, 0)}
  if (is.null(seasComp31) | all(is.na(seasComp31))) {seasComp31 <- series * 0 + ifelse(log, 1, 0)}
  if (is.null(seasComp365) | all(is.na(seasComp365))) {seasComp365 <- series * 0 + ifelse(log, 1, 0)}
  if (is.null(calComp) | all(is.na(seasComp365))) {calComp <- series * 0 + ifelse(log, 1, 0)}
  
  xout <- Descaler(Scaler(xLin, log=log) - 
                   Scaler(calComp, log=log) - 
                   Scaler(seasComp7, log=log) - 
                   Scaler(seasComp31, log=log) - 
                   Scaler(seasComp365, log=log), log=log) 
  
  return(xout)
}













# Theoretical Examples -------------------------------------------------------------


#output <- dsa2(bip)

#dsa2(bip, preProcessing = output$model) # This is how we want to work in the Tagesgeschaeft with a fixed pre_processing
