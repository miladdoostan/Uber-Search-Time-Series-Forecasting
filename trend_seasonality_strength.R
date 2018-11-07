install.packages('fpp2')
install.packages('seasonal')

trend_seasonality_strength <- function(x){
  x_decomposed <- mstl(x)
  rem <- remainder(x_decomposed)
  season <- seasonal(x_decomposed)
  season_adj <- seasadj(x_decomposed)
  trend_strength <- max(0, (1 - var(rem)/var(season_adj)))
  seasonality_strngth <- max(0, (1 - var(rem) / (var(season + rem))))
  return (list(trend_strength=trend_strength, seasonality_strength=seasonality_strngth))
}