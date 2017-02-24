normal_plot_hyp <- function(mean, se, alpha = 0.05, sample_mean, two_tailed = TRUE) {
  ## finding the x and y coordinates for plotting
  x <- seq(-4,4,length=1000)*se + mean
  hx <- dnorm(x,mean,se)
  
  ## plot
  plot(x, hx, type="n", xlab="Sample mean given H0", ylab="", axes=FALSE)
  
  ub <- mean + (qnorm(p = 1 - (alpha/2))) * se
  lb <- mean - (qnorm(p = 1 - (alpha/2))) * se
  i_low <- x <= lb 
  i_high <- x > ub
  lines(x, hx)
  polygon(c(lb,x[i_low]), c(0,hx[i_low]), col=rgb(1,0,0, alpha = 0.75)) 
  polygon(c(ub,x[i_high]), c(0,hx[i_high]), col=rgb(1,0,0, alpha = 0.75)) 
  abline(v = sample_mean, lty = 2)
  p_val <- 1 - pnorm(abs((sample_mean - mean)/se))
  
  p_val_txt <- paste("P-value = ", round(p_val, 3))
  result <- paste(p_val_txt)
  mtext(result,3)
  axis(1, at=seq(mean - (4 * se), mean + ( 4 * se), se), pos=0)
}


