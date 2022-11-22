# Basic settings ----------------------------------------------------------

Sys.setenv("JAVA_HOME"="C:\\Workspace\\Java\\JDK\\jdk-17.0.3+7")
.libPaths("C:\\Workspace\\R\\JD_lib")

library(rjd3highfreq)
library(rjd3stl)

set.seed(135)
all <- tssim::sim_daily(N=5, sd=5)
x <- all$original# Simulated time series
#x[3] <- x[3]*2
xreg <- xts::xts(cbind(rbinom(length(x), size=1, prob=0.02), rbinom(length(x), size=1, prob=0.02)), zoo::index(x)) # Simulated dummy-style regressor
xregf <- xts::xts(cbind(rbinom(365, size=1, prob=0.02), rbinom(365, size=1, prob=0.02)), seq.Date(from=end(xreg)+1, by="days", length.out=365)) # For dsa::dsa

source("R:/Zentrale/Projekte/FB-S/Daten/DA/JDemetra/Anwendungen/DSA2/MVP_auxiliary.R")

# MVP ---------------------------------------------------------------------

## Function
dsa2 <- function(x, xreg=NULL, Log=TRUE, swindow7=13, swindow31=13, swindow365=13, 
                 fill29=c("sa", "s")[1], outliers=c("AO", "LS", "WO")) {
  
  
  # RegARIMA ----------------------------------------------------------------
  model <- rjd3highfreq::fractionalAirlineEstimation(y=x, periods=c(7, 365.25), x=xreg, outliers=outliers)
  
  xlin <- xts::xts(model$model$linearized, zoo::index(x)) # calendar adjusted
  cfac <- Descaler(Scaler(x, Log=Log)-Scaler(xlin, Log=Log), Log=Log) # calendar factor
  
  # cfac2 <- xts::xts(Descaler(xreg %*% model$model$b, Log=Log), zoo::index(x))
  # cfac2 <- xts::xts(Descaler(model$model$b * xreg, Log=Log), zoo::index(x))

  # s7 ----------------------------------------------------------------------
  if (!is.null(swindow7)) {
    s7_model <- rjd3stl::stl(xlin, period=7, multiplicative=Log, swindow=swindow7)
    
    s7 <- xts::xts(s7_model$decomposition[,2], zoo::index(x)) # seasonally adjusted
    sfac7 <- xts::xts(s7_model$decomposition[,4], zoo::index(x)) # seasonal factor
  } else {
    s7_model <- NULL
    s7 <- xlin
    sfac7 <- xlin*0+ifelse(Log, 0, 1)
  }
  
  
  # s31 ---------------------------------------------------------------------
  if (!is.null(swindow31)){
    s7filled <- dsa:::.fill31(s7, fill="spline")
    
    s31_model <- rjd3stl::stl(s7filled, period=31, multiplicative=Log, swindow=swindow31)
    
    s31 <- dsa:::.drop31(ts(s31_model$decomposition[,2],
                            start=stats::start(s7filled), 
                            frequency = stats::frequency(s7filled)), 
                         new_start=as.numeric(format(stats::start(s7), "%j")), 
                         new_end=as.numeric(format(stats::end(s7), "%j")))
    
    sfac31 <- dsa:::.drop31(ts(s31_model$decomposition[,4],
                               start=stats::start(s7filled), 
                               frequency = stats::frequency(s7filled)), 
                            new_start=as.numeric(format(stats::start(s7), "%j")), 
                            new_end=as.numeric(format(stats::end(s7), "%j")))
  } else {
    s31_model <- NULL
    s31 <- s7
    sfac31 <- s7*0+ifelse(Log, 1,0)
  }
  
  # s365 --------------------------------------------------------------------
  if (!is.null(swindow365)){
    s31x <- delete_29(s31)
    
    s365_model <- rjd3stl::stl(s31x, period=365, multiplicative=Log, swindow=swindow365)
    
    if (fill29=="sa") { # How should the Feb 29 be filled up?
      s365 <- Descaler(Scaler(x, Log=Log)-Scaler(xts::xts(s365_model$decomposition[,4], zoo::index(s31x)), Log=Log), Log=Log) # seasonally adjusted
      s365 <- xts::merge.xts(s365, x)[,1] # Add back Feb 29
      s365 <- zoo::na.spline(s365) # DO: Maybe use rjd3bench::cubicspline
      
      sfac365 <- Descaler(Scaler(x, Log=Log)-Scaler(s365, Log=Log), Log=Log)
    }
    
    
    if (fill29=="s") {
      sfac365 <- xts::xts(s365_model$decomposition[,4], zoo::index(s31x)) # seasonal factor
      sfac365 <- xts::merge.xts(sfac365, x)[,1] # Add back Feb 29
      sfac365 <- zoo::na.spline(sfac365) # DO: Maybe use rjd3bench::cubicspline
    }
  } else {
    s365_model <- NULL
    s365 <- s31
    sfac365 <- s31*0+ifelse(Log, 1,0)
  }
  
  # Re-introduce outlier effects --------------------------------------------
  
  
  # Final sa ----------------------------------------------------------------
  
  sa <- Descaler(Scaler(x, Log=Log) - Scaler(sfac365, Log=Log) - 
                   Scaler(sfac31, Log=Log)  - Scaler(sfac7, Log=Log) - 
                   Scaler(cfac, Log=Log), Log=Log)
  
  # Output ------------------------------------------------------------------
  factors <- xts::merge.xts(cfac, sfac7, sfac31, sfac365)
  colnames(factors) <- c("cfac", "sfac7", "sfac31", "sfac365")
  series <- xts::merge.xts(x, sa)
  colnames(series) <- c("original", "sa")
  
  out <- list(series=series, factors=factors, regarima=model, rjd3stl::stl=list(s7_model, s31_model, s365_model))
  return(out)
}


# Some Results ------------------------------------------------------------


system.time({
  test <- dsa2(x, xreg=xreg, outliers=NULL)
})

system.time({
  comp <- dsa::dsa(x, Log=TRUE, 
                   s.window1 = 13, s.window2 = 13, s.window3 = 13, 
                   outlier = FALSE,
                   fourier_number = 24, 
                   regressor = dsa::multi_xts2ts(xreg), 
                   forecast_regressor = xregf)
})



bbkplot::tsplot(merge(x, comp$output$seas_adj[zoo::index(x)], test$series[,2]), 
                color=c("black", "darkgrey", "green"), 
                names=c("Original", "{DSA}", "DSA2"))

mean(abs(comp$output$seas_adj[zoo::index(x)]-all$seas_adj)) # MAE of dsa::dsa vs simulated
mean(abs(test$series[,2]-all$seas_adj)) # MAE of dsa2 vs simulated

# Probleme/Missings
# - Outlier und Kalendereffekt sind nicht getrennt verfügbar
# - Forecast aus RegARIMA/Fractional fehlt
