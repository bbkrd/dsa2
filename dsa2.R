dsa2 <- function(x, 
                 xreg = NULL, 
                 Log = TRUE, 
                 swindow7 = 13, 
                 swindow31 = 13, 
                 swindow365 = 13, 
                 fill29 = c("sa", "s")[1], 
                 outliers = NULL,
                 #outliers = c("AO", "LS", "WO"),
                 n_iterations = 1) {
  
  
  # Step 1: RegARIMA ----------------------------------------------------------------
  model <- rjd3highfreq::fractionalAirlineEstimation(y=x, periods=c(7, 365.25), x=xreg, outliers=outliers)
  xlin <- xts::xts(model$model$linearized, zoo::index(x)) # calendar adjusted
  cfac <- Descaler(Scaler(x, Log=Log)-Scaler(xlin, Log=Log), Log=Log) # calendar factor
  #cfac2 <- xts::xts(Descaler(xreg %*% model$model$b, Log=Log), zoo::index(x))
  #cfac2 <- xts::xts(Descaler(model$model$b * xreg, Log=Log), zoo::index(x))
  
  # Preliminary seasonal component
  sfac7 <- sfac31 <- sfac365 <- 0 * cfac + ifelse(Log, 1, 0)
  
  # Iterative loop over steps 2 to 4
  for (iteration in 1:n_iterations) {

    
    # Step 2: s7 ---------------------------------------------------------------
    if (!is.null(swindow7)) {
      xlinx <- Descaler(Scaler(xlin, Log=Log) - Scaler(sfac31, Log=Log) - Scaler(sfac365, Log=Log), Log=Log) # DO: Changed name to avoid alle Effekte hier zu sammeln
      s7_model <- rjd3stl::stl(xlinx, period=7, multiplicative=Log, swindow=swindow7)
      s7 <- xts::xts(s7_model$decomposition[,2], zoo::index(x)) # seasonally adjusted
      s7 <- Descaler(Scaler(s7, Log=Log) + Scaler(sfac31, Log=Log) + Scaler(sfac365, Log=Log), Log=Log) # DO: Neu! Reintroducing components 
      sfac7 <- xts::xts(s7_model$decomposition[,4], zoo::index(x)) # seasonal factor
    } else {
      s7_model <- NULL
      s7 <- xlin
      sfac7 <- xlin*0+ifelse(Log, 0, 1)
    }
    
    
    # Step 3: s31 --------------------------------------------------------------
    if (!is.null(swindow31)){

      s7 <- Descaler(Scaler(s7, Log=Log)  - Scaler(sfac365, Log=Log), Log=Log)
      
      s7filled <- dsa:::.fill31(s7, fill="spline")
      
      s31_model <- rjd3stl::stl(s7filled, period=31, multiplicative=Log, swindow=swindow31)
      
      s31 <- dsa:::.drop31(ts(s31_model$decomposition[,2],
                              start=stats::start(s7filled), 
                              frequency = stats::frequency(s7filled)), 
                           new_start=as.numeric(format(stats::start(s7), "%j")), 
                           new_end=as.numeric(format(stats::end(s7), "%j")))
      
      s31 <- Descaler(Scaler(s31, Log=Log)  + Scaler(sfac365, Log=Log), Log=Log) # DO: Neu! Reintroducing components 
      
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
    

    # Step 4: s365 -------------------------------------------------------------
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
  
  out <- list(series=series, 
              factors=factors, 
              regarima=model, 
              stl=list(s7_model, s31_model, s365_model))
  
  return(out)
}