dsa2 <- function(series, 
                 xreg = NULL,
                 s7 = c("x11", "stl", "seats")[1],
                 s31 = NULL,
                 s365 = c("x11", "stl", "seats")[1],
                 outliers = c("AO", "LS", "TC"),
                 n_iterations = 1,
                 pre_processing = NULL) {
  
  parameters <- as.list(environment(), all=TRUE)
  
  # General TO DO: xts und ts stuff
  
  # pre-processing ----------------------------------------------------------
  if (!is.null(pre_processing)) { # Standard_case
    model <- rjd3highfreq::fractionalAirlineEstimation(y=x, periods=c(7, 365.25), x=xreg, outliers=outliers)
    xlin <- xts::xts(model$model$linearized, zoo::index(x)) # calendar adjusted
    cal_comp <- model$cal_comp # calendar factor # not available yet
    
  } else {
    xlin = pre_processing$xlin # Put here the linearised series
  }
  
  # Preliminary seasonal component
  seas_comp7 <- seas_comp31 <- seas_comp365 <- 0 * cal_comp + ifelse(Log, 1, 0)
  
  # Seasonal adjustment -----------------------------------------------------
  
  for (j in seq(n_iterations)) {
    
    # S7 --------------------------------------------------------------------
    xlinx <- seasonally_adjust(xlin, seas_comp7=NULL, seas_comp31=seas_comp31, seas_comp365=seas_comp365, log=log)
    frequency(xlinx) <- 7
    s7_result <- adjust(method=s7, series=xlinx) 
    seas_comp7 <- s7_result$seas_comp
    
    # S31 --------------------------------------------------------------------
    ### TO DO: Adding interpolation
    xlinx <- seasonally_adjust(xlin, seas_comp7=seas_comp7, seas_comp31=NULL, seas_comp365=seas_comp365, log=log)
    frequency(xlinx) <- 31
    s31_result <- adjust(method=s31, series=xlinx) 
    seas_comp31 <- s31_result$seas_comp
    
    # S365 --------------------------------------------------------------------
    ### TO DO: Dealing with 29.2
    xlinx <- seasonally_adjust(xlin, seas_comp7=seas_comp7, seas_comp31=seas_comp31, seas_comp365=NULL, log=log)
    frequency(xlinx) <- 365
    s365_result <- adjust(method=s365, series=xlinx) 
    seas_comp365 <- s365_result$seas_comp
  }
  
  
  # Create output -----------------------------------------------------------
  original <- series # add forecasts
  seas_adj <- seasonally_adjust(xlin, seas_comp7=seas_comp7, seas_comp31=seas_comp31, seas_comp365=seas_comp365, log=log)
  
  
  series <- xts::merge.xts(original=original, seas_adj = seas_adj)
  components <- xts::merge.xts(cal_comp = cal_comp, seas_comp7=seas_comp7, seas_comp31=seas_comp31, seas_comp365=seas_comp365)
  
  
  out <- list(series=series, components=components, pre_processing=model, parameters=parameters)
  
  class(out) <- "dsa2"
  
  return(out)
}



stl_method  <- function(period = stats::frequency(series), 
                        swindow = 13, 
                        log = TRUE, # Umbenennung von multiplicative
                        twindow=0, 
                        ninnerloop=1, 
                        nouterloop=15, 
                        nojump=FALSE, 
                        weight.threshold=0.001, 
                        weight.function=c('BIWEIGHT')
)# include a series object or only pure specifications?
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

x11_method <- function(period = stats::frequency(series),   # Assumes use of rjd3highfreq::x11
                       log=TRUE, # Umbenennung von mul
                       sma=c("S3X3", "S3X1", "S3X5", "S3X9", "S3X15"), 
                       trend.horizon=6, # Brauchen wir?
                       trend.degree=2, # Brauchen wir?
                       trend.kernel=c("Henderson"),
                       trend.asymmetric=c("CutAndNormalize"),
                       sigma=c(1.5,2.5)) # Umbenennung von extreme.lsig, extreme.usig
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


seats_method <- function(period = stats::frequency(series),  # Assumes use of rjd3highfreq::fractionalAirlineDecomposition, we might want to use rjd3highfreq::multiAirlineDecomposition instead
                         log=TRUE, # Currently not implemented in rjd3highfreq::fractionalAirlineDecomposition
                         sn = FALSE,
                         stde = FALSE,
                         nbcasts = 0,
                         nfcasts = 0
)  {
  parameters <- list(period = stats::frequency(series), 
                     # multiplicative = log, currently not implemented
                     sn = sn, 
                     stde = stde, 
                     nbcasts = nbcasts, 
                     nfcasts = nfcasts)
  
  class(parameters) <- c("seats_method")
  
  return(parameters) 
}



### Maybe the following line(s) have to be loaded at packages start-up once this thingy becomes a dsa2 package
adjust <- function(method, series) {UseMethod("adjust")} # This is how we define generics in S3
adjust.default <- function(method, series) {message("The method should either be one of 'x11', 'stl' or 'seats' or a call to stl_method(), x11_method() or seats_method()")}

adjust.stl_method <- function(method, series) { 
  adjustment <- do.call(stl, append(list(series),method)) # This is just for test purposes, so that I do not have to use the VC. Later use the one below
  # do.call(rjd3stl::stl, append(list(series),method))
  return(list(adjustment=adjustment, seas_comp=adjustment$seas_comp)) # Adapt so that seas_comp works correctly
}

adjust.x11_method <- function(method, series) { 
  adjustment <- do.call(IRGENDEINE_X11_METHODE, append(list(series),method))
  return(list(adjustment=adjustment, seas_comp=adjustment$seas_comp)) # Adapt so that seas_comp works correctly
}

adjust.seats_method <- function(method, series) { 
  adjustment <- do.call(IRGENDEINE_X11_METHODE, append(list(series),method))
  return(list(adjustment=adjustment, seas_comp=adjustment$seas_comp)) # Adapt so that seas_comp works correctly
}

adjust.character <- function(method, series) { 
  if (method == "stl") {
    adjustment <- do.call(rjd3stl::stl, append(list(series),stl_method()))
    return(list(adjustment=adjustment, seas_comp=adjustment$seas_comp)) # Adapt so that seas_comp works correctly
  }
  
  if (method == "x11") {
    adjustment <- do.call(IRGENDEINE_X11_METHODE, append(list(series),x11_method()))
    return(list(adjustment=adjustment, seas_comp=adjustment$seas_comp)) # Adapt so that seas_comp works correctly
  }
  
  if (method == "seats") {
    adjustment <- do.call(IRGENDEINE_Seats_METHODE, append(list(series),seats_method()))
    return(list(adjustment=adjustment, seas_comp=adjustment$seas_comp)) # Adapt so that seas_comp works correctly
  }
}

adjust.NULL <- function(method, series) {
  return(list(adjustment=NULL, seas_comp=series*NA)) # Adapt so that seas_comp works correctly
}




seasonally_adjust <- function(xlinx, seas_comp7=seas_comp7, seas_comp31=seas_comp31, seas_comp365=seas_comp365, cal_comp=NULL, log=TRUE) {
  
  if (is.null(seas_comp7) | all(is.na(seas_comp7))) {seas_comp7 <- series * 0 + ifelse(log, 1, 0)}
  if (is.null(seas_comp31) | all(is.na(seas_comp31))) {seas_comp31 <- series * 0 + ifelse(log, 1, 0)}
  if (is.null(seas_comp365) | all(is.na(seas_comp365))) {seas_comp365 <- series * 0 + ifelse(log, 1, 0)}
  if (is.null(cal_comp) | all(is.na(seas_comp365))) {cal_comp <- series * 0 + ifelse(log, 1, 0)}
  
  xout <- Descaler(Scaler(xlin, log=log) - Scaler(cal_comp, log=log) - Scaler(seas_comp7, log=log) - Scaler(seas_comp31, log=log) - Scaler(seas_comp365, log=log), log=log) 
  return(xout)
}



### TEST ZONE ###
params <- list(s.window=7, t.window=13)
class(params) <- "stl_method"

test_series <- tsbox::ts_ts(tssim::sim_monthly(N=10)$original)
adjust(params, test_series) 
#################



plot.dsa2 <- function() {
  
}

print.dsa2 <- function() {
  
}






# Theoretical Examples -------------------------------------------------------------


output <- dsa2(bip)

dsa2(bip, pre_processing = output$model) # This is how we want to work in the Tagesgeschaeft with a fixed pre_processing
