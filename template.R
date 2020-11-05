library(shiny)
library(shinythemes)
library("rjags")
library("car")
library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(car)
library(nortest)
library(tseries)
library(RcmdrMisc)
library(lmtest)
data("Leinhardt")
Leinhardt$loginfant = log(Leinhardt$infant)
Leinhardt$logincome = log(Leinhardt$income)
dat = na.omit(Leinhardt)
ui <- (fluidPage(theme = shinytheme("cerulean"),
                 
  titlePanel("Bayesian Hierarchical Model for predicting Infant Mortality"),
  tabPanel(icon("home"),
  p("Through this application, it is intended to develop a Bayesian Hierarchical Model to predict the Infant Mortaility in a Country, based on the several factors present in the Data Set below.",style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
  sliderInput(inputId = "num", label = "Total Number of MCMC Iterations * ", value = 25, min = 1, max = 100000),
  plotOutput(outputId = "hist1"), verbatimTextOutput("stats1"), plotOutput(outputId = "hist2"), verbatimTextOutput("stats2"),
  hr(),
  tags$style(".fa-database {color:#E87722}"),
  h3(p(em("Dataset "),icon("database",lib = "font-awesome"),style="color:black;text-align:center")),
  fluidRow(column(DT::dataTableOutput("RawData"),
                  width = 12)),
  hr(),
  p(em("Developed by"),br("Uttasarga Singh"),style="text-align:center; font-family: times"),
  
  fluidRow(column(width=2),
                    column(
                      h4(p("Distribution of Variables",style="color:black;text-align:center")),
                      width=8,style="background-color:lavender;border-radius: 10px")
           ),
  plotOutput(outputId = "hist3"),
  plotOutput(outputId = "hist4")
  
  
)))




library("car")
data("Leinhardt")
Leinhardt$loginfant = log(Leinhardt$infant)
Leinhardt$logincome = log(Leinhardt$income)
dat = na.omit(Leinhardt)
library(shiny)
library(shinythemes)
library(DT)
library(ggplot2)
library(car)
library(nortest)
library(tseries)
library(RcmdrMisc)
library(lmtest)



server <- function(input, output) 
{
model1 <- reactive({
  
mod2_string = " model {
    for (i in 1:length(y)) {
        y[i] ~ dnorm(mu[i], prec)
        mu[i] = b[1] + b[2]*log_income[i] + b[3]*is_oil[i]
    }
    
    for (i in 1:3) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }
    
    prec ~ dgamma(5/2.0, 5*10.0/2.0)
    sig = sqrt( 1.0 / prec )
} "
data2_jags = list(y=dat$loginfant, n = nrow(dat) ,log_income=dat$logincome,
                  is_oil=as.numeric(dat$oil=="yes"))

params2 = c("b", "sig")
inits2 = function() {
  inits = list("b"=rnorm(3,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}
mod2 = jags.model(textConnection(mod2_string), data=data2_jags, inits=inits2, n.chains=3)
update(mod2, 1e3)
mod2_sim = coda.samples(model=mod2,
                        variable.names=params2,
                        n.iter=input$num)
mod2_csim = as.mcmc(do.call(rbind, mod2_sim))
X2 = cbind(rep(1.0, data2_jags$n), data2_jags$log_income, data2_jags$is_oil)
pm_params2 = colMeans(mod2_csim)
yhat2 = drop(X2 %*% pm_params2[1:3])
})
  output$hist1 <- renderPlot({
    title <- "Predicted Infant Mortality"
    plot((model1()), main = title)
  })
  output$stats1 <- renderPrint({
    summary(model1())
  })
  output$hist2 <- renderPlot({
    title <- "Actual Infant Mortality"
  plot((dat$loginfant), main = title)
})
  
  output$hist3 <- renderPlot({
    title <- "Infant Mortality Distribution"
    plot((hist(dat$infant)), main = title)
  })
  
  output$hist4 <- renderPlot({
    title <- "Income Distribution"
    plot((hist(dat$income)), main = title)
  })
  output$stats2 <- renderPrint({
    summary(dat$loginfant)
  })
  output$RawData <- DT::renderDataTable(
    DT::datatable({
      dat
    },
    options = list(lengthMenu=list(c(5,15,20),c('5','15','20')),pageLength=10,
                   initComplete = JS(
                     "function(settings, json) {",
                     "$(this.api().table().header()).css({'background-color': 'moccasin', 'color': '1c1b1b'});",
                     "}"),
                   columnDefs=list(list(className='dt-center',targets="_all"))
    ),
    filter = "top",
    selection = 'multiple',
    style = 'bootstrap',
    class = 'cell-border stripe',
    rownames = FALSE,
    colnames = c("infant","income","region","is_oil","loginfant","logincome")
    ))
  
}
shinyApp(ui = ui, server = server)