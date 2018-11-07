library('fpp2')
library('ggpubr')

# four plot
run_seq_plot <- function(x){
  autoplot(x, xlab='Time', ylab='x') + geom_point(pch=21, fill='red')
}

lag_plot <- function(x){
  cbind(x=x, y=shift(x,1)) %>% as.data.frame() %>% ggplot(aes(x=x, y=y)) + 
    geom_point(pch=21, size=3, fill='gold', alpha=0.8, color='black')  +
    geom_smooth(method='lm', color='black', linetype='longdash', se=F) + ylab('lag x')
}

hist_plot <- function(x){
  cbind(x=x) %>% as.data.frame() %>% ggplot(aes(x=x)) + 
    geom_histogram(aes(x=x), bins=8, fill='dodgerblue', color='navy')
}

acf_plot <- function(x){
  ggAcf(x, lag.max=60)
}

four_plot <- function(x){
  ggarrange(run_seq_plot(x), lag_plot(x), hist_plot(x), acf_plot(x), nrow=2, ncol=2)
}
