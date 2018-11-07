library('fpp2')
library('ggpubr')

residual_run_seq_plot <- function(model){
  autoplot(residuals(model)) + 
    geom_point(pch=21, fill='red') +
    xlab('Time') + ylab('Residuals') +
    ggtitle('Residuals versus time')
}

residual_hist_plot <- function(model){
  cbind(x=residuals(model)) %>% as.data.frame() %>% ggplot(aes(x=x)) + 
    geom_histogram(aes(x=x), bins=8, fill='dodgerblue', color='navy') + 
    xlab('Residuals') + ylab('Frequency') +
    ggtitle('Histogram of the residuals')
}

residual_normal_prob_plot <- function(model){
  ggplot() +
    geom_qq(aes(sample = residuals(model)), pch=21, fill='black')+
    geom_qq_line(aes(sample = residuals(model)), color='red')+
    xlab('Residuals') + ylab('Percent') +
    ggtitle('Normal probability plot of the residuals')
}

residual_fit_plot <- function(model){
  cbind(x=fitted(model), y=residuals(model)) %>% as.data.frame() %>% ggplot(aes(x=x, y=y)) + 
    geom_point(pch=21, fill='tan', color='black', size=3) +
    geom_hline(yintercept=0, linetype="dashed", color = "navy")+
    xlab('Fitted value') + ylab('Residuals') +
    ggtitle('Residuals versus the fitted values')
}

residual_acf_plot <- function(model){
  ggAcf(residuals(model), lag.max=60) +
    xlab('lag') + ylab('autocorrelateion')+
    ggtitle('Residuals ACF plot')
}

residual_pacf_plot <- function(model){
  ggPacf(residuals(model), lag.max=60) +
    xlab('lag') + ylab('autocorrelateion')+
    ggtitle('Residuals PACF plot')
}

residual_diognostics_plots <- function(model){
  ggarrange(residual_normal_prob_plot(model),
            residual_fit_plot(model),
            residual_hist_plot(model),
            residual_run_seq_plot(model),
            residual_acf_plot(model),
            residual_pacf_plot(model),
            nrow=2, ncol=3)
}