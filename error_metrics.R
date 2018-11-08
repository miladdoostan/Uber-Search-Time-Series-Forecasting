# Two functions for calculating the RMSE and MAE of errors.

rmse <- function(error){
  return (sqrt(mean(error^2, na.rm=TRUE)))
}

mae <- function(error){
  return(mean(abs(error), na.rm=TRUE))
}
