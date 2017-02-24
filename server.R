library(shiny)

## choosing random standard dev,sample size and sample mean
sd<-round(runif(1,2,15),2)
n<-round(runif(1,35,1234),0)
xbar<-round(runif(1,-5,200),1)
SE <- sd/sqrt(n)
## simulated population mean
hyp_mu <- round(xbar + (round(runif(1, -4, 4), 2) * SE), 2)

## choosing random value of alpha (0.10, 0.05 or 0.01)
alpha <- if(runif(1,0,1)<0.3) 0.10 else {
  if(runif(1,0,1)<0.05) 0.05 else 0.01
}

## choosing whether the test is one or two tailed
if(runif(1,0,1)<=0.5){
  test_type <- "2t"
  test_sgn_null = " = "
  test_sgn_alt = " \\not = "
} else {
  if (runif(1,0,1)<0.5){
    test_type <- "1tl"
    test_sgn_null = " \\geq "
    test_sgn_alt = " < "
  } else {
    test_type <- "1tu"
    test_sgn_null = " \\leq "
    test_sgn_alt = " > "
  }
}

## defining the plot function for feedback once the students answer
normal_plot_hyp <- function(mean, se, alpha = 0.05, sample_mean, test_type = "2t") {
  ## finding the x and y coordinates for plotting
  x <- seq(-4,4,length=1000)*se + mean
  hx <- dnorm(x,mean,se)
  
  ## plot
  plot(x, hx, type="n", xlab=expression("Sample mean given H"[0]), ylab="", axes=FALSE)
  lines(x, hx)
  
  if (test_type == "2t"){
    ub <- mean + (qnorm(p = 1 - (alpha/2))) * se
    lb <- mean - (qnorm(p = 1 - (alpha/2))) * se
    i_low <- x <= lb 
    i_high <- x > ub
    polygon(c(lb,x[i_low]), c(0,hx[i_low]), col=rgb(0.8,0.8,0.8, alpha = 0.75)) 
    polygon(c(ub,x[i_high]), c(0,hx[i_high]), col=rgb(0.8,0.8,0.8, alpha = 0.75)) 
  }
  else if (test_type == "1tl"){
    lb <- mean - (qnorm(p = 1 - (alpha))) * se
    i_low <- x <= lb
    polygon(c(lb,x[i_low]), c(0,hx[i_low]), col=rgb(0.8,0.8,0.8, alpha = 0.75))
  }
  else {
    ub <- mean + (qnorm(p = 1 - (alpha))) * se
    i_high <- x > ub
    polygon(c(ub,x[i_high]), c(0,hx[i_high]), col=rgb(0.8,0.8,0.8, alpha = 0.75))
  }
  
  abline(v = sample_mean, lty = 2)
  
  if (test_type == "2t"){
    p_val <- 1 - pnorm(abs((sample_mean - mean)/se))
  }
  else if (test_type == "1tl"){
    p_val <- pnorm((sample_mean - mean)/se)
  }
  else {
    p_val <- 1 - pnorm((sample_mean - mean)/se)
  }
  
  p_val_txt <- paste("P-value = ", round(p_val, 3))
  result <- paste(p_val_txt)
  mtext(result,3)
  axis(1, at=seq(mean - (4 * se), mean + ( 4 * se), se), pos=0)
}

## I just initiated the reactive values outside of the shiny server before the write_question 
## because I believe it makes use of the reactive values
v <- reactiveValues(cor_inc = "", 
                    sd = sd,
                    n = n,
                    xbar = xbar,
                    alpha = alpha,
                    correct = 0,
                    incorrect = 0,
                    question_count = 0,
                    SE = SE,
                    test_area = 0,
                    hyp_mu = hyp_mu,
                    test_type = test_type,
                    test_sgn_null = test_sgn_null,
                    test_sgn_alt = test_sgn_alt)

shinyServer(function(input, output, session){
  observeEvent(input$action, {
    v$cor_inc <- ""
    v$sd<-round(runif(1,2,15),2)
    v$n<-round(runif(1,35,1234),0)
    v$xbar<-round(runif(1,-5,200),1)
    v$SE <- v$sd/sqrt(v$n)
    v$hyp_mu <- round(v$xbar + (round(runif(1, -4, 4), 2) * v$SE), 2)
    if(runif(1,0,1)<=0.5){
      v$test_type <- "2t"
      v$test_sgn_null = " = "
      v$test_sgn_alt = " \\not = "
    } else {
      if (runif(1,0,1)<0.5){
        v$test_type <- "1tl"
        v$test_sgn_null = " \\geq "
        v$test_sgn_alt = " < "
      } else {
        v$test_type <- "1tu"
        v$test_sgn_null = " \\leq "
        v$test_sgn_alt = " > "
      }
    }
    
    v$alpha <- if(runif(1,0,1)<0.25) 0.10 else {
      if(runif(1,0,1)<0.67) 0.05 else 0.01
    }
    output$vis <- renderPlot(plot.new())
  })
  
  observeEvent(input$answer, {
    C_lower <- qnorm((v$alpha)/2,v$xbar,v$sd/sqrt(v$n))
    C_upper  <- qnorm((v$alpha)/2+(1 - (v$alpha/2)),v$xbar,v$sd/sqrt(v$n))
    ##p <- 1 - pnorm(abs((v$xbar - v$hyp_mu)/v$SE))
    
    if (v$test_type == "2t"){
      rej_thresh = v$alpha/2
      p <- 1 - pnorm(abs((v$xbar - v$hyp_mu)/v$SE))
    }
    else {
      rej_thresh = v$alpha
      if (v$test_type == "1tl"){
        p <- pnorm((v$xbar - v$hyp_mu)/v$SE)
      } else {
        p <- pnorm((v$xbar - v$hyp_mu)/v$SE)
      }
    }
    
    if (((p > rej_thresh & input$accept_reject == 1)|
        (p <= rej_thresh & input$accept_reject == 2))
        & (input$p_value < p + 0.001 & input$p_value > p - 0.001)){
      v$cor_inc <- "Correct!"
      v$correct <- v$correct + 1
    }
    else{
      v$cor_inc <- "Incorrect! Please try again. (Look at the figure below for a clue)"
      v$incorrect <- v$incorrect + 1
    }
    
    output$vis <- renderPlot(normal_plot_hyp(mean = v$hyp_mu, se = v$SE, sample_mean = v$xbar, alpha = v$alpha, test_type = v$test_type))
  })
  
  output$Question <- renderText(paste("You collect a sample of ",v$n," observations from a population with standard deviation of ",v$sd,". The value of the sample mean is ",v$xbar,". Test the following null hypothesis with alpha-level = ",v$alpha, sep=""))
  output$Hypothesis <- renderUI({withMathJax(helpText(paste('$$H_0: \\mu', v$test_sgn_null, v$hyp_mu, '$$', '$$H_a: \\mu', v$test_sgn_alt, v$hyp_mu, '$$')))})
  output$Conf_int <- renderText(paste("CI: (", v$xbar + (qnorm(v$alpha/2))*v$SE, ", ", v$xbar + (qnorm(1 - v$alpha/2))*v$SE, ")", " Test Type: ", v$test_type))
  
  output$ci <- renderText(v$cor_inc)
  output$cor <- renderText(paste("Correct: ", v$correct))
  output$incor <- renderText(paste("Incorrect: ", v$incorrect))
})