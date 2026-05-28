#' Seasonal adjustment of weekly time series
#'
#' Seasonal adjustment of weekly time series.
#' @param x An `xts` object containing the time series to be adjusted.
#' @param sa_method A character value containing either "x11" or "seats" or a call to x11_method() or seats_method().
#' @param reiterate An integer value detailing the number of reiterations for the seasonal adjustment.
#' @param xreg Regressors used for calendar adjustment.
#' @param log A logical value indicating whether to use a multiplicative model.
#' @param outliers Identifying outliers (LS - leve shift, AO - additive outlier, WO - ???); set `NULL` if none shall be searched.
#' @param h A numeric value specifying the number of days to forecast.
#' @param critical_value A numerical value specifying the threshold to include outliers used in the automatic outlier estimation (see also 
#'  `rjd3highfreq::fractionalAirlineEstimation`).
#' @param ... Additional parameters from `rjd3highfreq::fractionalAirlineEstimation`.
#' @author Daniel Ollech, Carolin Schaaff
#' @return An object of class "wsa", a list with elements:
#' series: xts with columns original and seas_adj
#' components: xts with calendar and seasonal components
#' preProcessing: model fit from rjd3highfreq::fractionalAirlineEstimation
#' decomposition: seasonal adjustment details from the chosen method
#' specification: a list of settings (e.g., log)
#' parameters: call parameters and metadata
#'
#' @examples
#' \dontrun{
#' to_week <- function (x) {
#' ep <- xts::endpoints(x, on = "weeks")
#' out <- xts::period.apply(x, 
#'                          INDEX = ep[ep >= 0 &  ep <= length(x)], 
#'                          FUN = mean)
#' return(out)
#' }
#' seriesw <- to_week(series)
#' res <- wsa(seriesw)
#' plot(res)
#' }
#' @details This function can be used to seasonally and calendar adjust weekly time series. Currently the function uses the extended airline estimation model from the rjd3highfreq package for calendar estimation and the extended airline decomposition model from the same package for seasonality estimation.
#' @export


wsa <- function(x,
                sa_method = c("seats", "x11")[1],
                reiterate = 1, 
                xreg = NULL,
                log = TRUE,
                outliers = c("AO", "LS", "WO"),
                h = 53,
                critical_value = 5,
                ...) {

  parameters <- as.list(environment(), all = TRUE)
  parameters$name <-  deparse(substitute(x)) # Get name of object inputted as series

  dates   <- seq.Date(from = as.Date(stats::start(x)),
                      by = "weeks",
                      length.out = length(x) + h)

  # Pre-processing -------------------------------------------------------------

  # Run fractional airline estimation
  fracAirline <- rjd3highfreq::fractionalAirlineEstimation(y = x,
                                                           periods = 52.18,
                                                           x = xreg,
                                                           nfcasts = h,
                                                           outliers = outliers,
                                                           criticalValue = critical_value,
                                                           log = log,
                                                           ...)

  # Extract calendar adjusted series and calendar component
  xLinear   <- .descaler(xts::xts(fracAirline$model$linearized,
                                    order.by = dates),
                           log = log)

  calComp   <- xts::xts(fracAirline$model$component_userdef_reg_variables,
             order.by = dates)
  
  # Seasonal adjustment---------------------------------------------------------
  
  seats_log <- (any(sa_method == "seats") | inherits(sa_method, "seats_method")) & log
  x11_log   <- (any(sa_method == "x11") | inherits(sa_method, "x11_method")) & log
  xLinx     <- .scaler(stats::ts(xLinear, frequency = 52.18), log = seats_log)
  
  for (i in 1:reiterate) {
  
    if (i > 1) {
      # Subtract preliminary seasonal component
      xLinx <-  .descaler(.scaler(xLinx, log = x11_log) - .scaler(seasCompAddition, log = x11_log),
                          log = x11_log)
    }
      
    # Calculate seasonal component
    seasResult <- .estimate_component(method = sa_method, 
                                       series = xLinx, 
                                       log = x11_log)

    if (i > 1) {
      # Add seasonal components together
      seasCompAddition <- seasResult$seasComp
      seasComp <- .descaler(
                .scaler(seasComp, log = x11_log) + 
                .scaler(seasCompAddition, log = x11_log),
                log = x11_log)
    } else {
      seasCompAddition <- seasResult$seasComp
      seasComp <- seasResult$seasComp
    }
  }
  
  seasComp <- .descaler(xts::xts(seasComp, order.by = dates), 
                        log = seats_log)

  # Create output --------------------------------------------------------------
  original <- xts::xts(fracAirline$model$y,
                       seq.Date(from = as.Date(stats::start(x)),
                                by = "weeks",
                                length.out = length(fracAirline$model$y)
                                )
                       )
  
  seas_adj <- xts::merge.xts(.descaler(
    .scaler(original, log = log) - 
      .scaler(calComp, log = log) - 
      .scaler(seasComp, log = log),
    log = log)
    )
  
  series           <- xts::merge.xts(original = original, 
                                     seas_adj = seas_adj)
  colnames(series) <- c("original", "seas_adj")
  components       <- xts::merge.xts(calComp, seasComp)

  out <- list(series        = series,
              components    = components,
              preProcessing = fracAirline,
              decomposition = seasResult$adjustment,
              specification = list(log = log),
              parameters    = parameters,
              settings      = seasResult$method
              )

  class(out) <- "wsa"

  return(out)
}
