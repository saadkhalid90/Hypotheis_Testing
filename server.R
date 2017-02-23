library(shiny)

## choosing random standard dev,sample size and sample mean
sd<-round(runif(1,2,15),2)
n<-round(runif(1,35,1234),0)
xbar<-round(runif(1,-5,200),1)
SE <- sd/sqrt(n)
## simulated population mean
hyp_mu <- round(xbar + (round(runif(1, -3.8, 3.8), 2) * SE), 2)

## choosing random confidence level (90, 95 and 99 %)
alpha <- if(runif(1,0,1)<0.3) 0.10 else {
  if(runif(1,0,1)<0.05) 0.95 else 0.01
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
                    hyp_mu = hyp_mu)

shinyServer(function(input, output, session){
  observeEvent(input$action, {
    v$cor_inc <- ""
    v$sd<-round(runif(1,2,15),2)
    v$n<-round(runif(1,35,1234),0)
    v$xbar<-round(runif(1,-5,200),1)
    v$SE <- v$sd/sqrt(v$n)
    v$hyp_mu <- round(v$xbar + (round(runif(1, -10, 10), 2) * v$SE), 2)
    
    v$alpha <- if(runif(1,0,1)<0.3) 0.10 else {
      if(runif(1,0,1)<0.5) 0.05 else 0.01
    }
  })
  
  observeEvent(input$answer, {
    C_lower <- qnorm((v$alpha)/2,v$xbar,v$sd/sqrt(v$n))
    C_upper  <- qnorm((v$alpha)/2+(1 - (v$alpha/2)),v$xbar,v$sd/sqrt(v$n))
    p <- 1 - pnorm(abs((v$xbar - v$hyp_mu)/v$SE))
    
    if ((p > v$alpha/2 & input$accept_reject == 1)|
        (p <= v$alpha/2 & input$accept_reject == 2)){
      v$cor_inc <- "Correct!"
      v$correct <- v$correct + 1
    }
    else{
      v$cor_inc <- "Incorrect! Please try again"
      v$incorrect <- v$incorrect + 1
    }
  })
  
  
  output$Question <- renderText(paste("You collect a sample of ",v$n," observations from a population with standard deviation of ",v$sd,". The value of the sample mean is ",v$xbar,". Test the following null hypothesis with alpha = ",v$alpha,sep=""))
  output$Hypothesis <- renderText(paste("Null Hypothesis", ":" ," mu = ", v$hyp_mu))
  output$Conf_int <- renderText(paste("CI: (", v$xbar + (qnorm(v$alpha/2))*v$SE, ", ", v$xbar + (qnorm(1 - v$alpha/2))*v$SE, ")", "  P-value: ", 1 - pnorm(abs((v$xbar - v$hyp_mu)/v$SE))))
  output$ci <- renderText(v$cor_inc)
  output$cor <- renderText(paste("Correct: ", v$correct))
  output$incor <- renderText(paste("Incorrect: ", v$incorrect))
})