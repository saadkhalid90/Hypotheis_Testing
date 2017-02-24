library(shiny)
library(shinythemes)

## I just rearranged the UI a little bit

shinyUI(fluidPage(theme = shinytheme("lumen"),
  h3("Unlimited Test Bank - Hypotheis Testing"),
  hr(),
  h4("Enter your answer below and click Answer to check it"),
  h4("or click Next Problem for a new question"),
  hr(),
  h4("Question:"),
  withMathJax(textOutput("Question")),
  uiOutput("Hypothesis"),
  withMathJax(textOutput("test")),
  textOutput("Conf_int"),
  hr(),
  numericInput("p_value", label = "P-value: ", value = 0, min = 0, max = 1, step = 0.05,
               width = NULL),
  radioButtons("accept_reject", label = "Fail to reject or reject the Null Hypothesis",
               choices = list("Fail to reject" = 1, "Reject" = 2), 
               selected = 1),
  actionButton("answer", label = "Answer"),
  actionButton("action", label = "Next Problem"),
  hr(),
  mainPanel(
    h4(textOutput("ci")),
    h5(textOutput("cor")),
    h5(textOutput("incor")),
    plotOutput("vis", width = "100%", height = "400px")
  )
))