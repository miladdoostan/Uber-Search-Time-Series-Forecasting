# A function for determining the strength of the trend and seasonality of a time series. It is written based on the formulation 
# provided in  https://otexts.org/fpp2/seasonal-strength.html

trend_seasonality_strength <- function(x){
  x_decomposed <- mstl(x)
  rem <- remainder(x_decomposed)
  season <- seasonal(x_decomposed)
  season_adj <- seasadj(x_decomposed)
  trend_strength <- max(0, (1 - var(rem)/var(season_adj)))
  seasonality_strength <- max(0, (1 - var(rem) / (var(season + rem))))
  return (list(trend_strength=trend_strength, 
               seasonality_strength=seasonality_strength))
}


