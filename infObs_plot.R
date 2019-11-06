infObs_plot <- function(df,fit){
  cutoff <- 4/(nrow(df)-length(fit$coefficients)-2)
  plot(fit,which = 4,cook.levels = cutoff)
  abline(h=cutoff,lty=2,col="red")
}
##观测强影响点，在红色阈值线以上的可以判断为强影响点