# Load packages ----
library(shiny)
library(quantmod)
library(ggplot2)
library(xts)
library(moments)
library(tidyverse)
library(hrbrthemes)
library(viridis)
library(zoo)
library(Rcpp)
library(forecast)
library(shinyWidgets)
#install.packages('shinyWidgets')
prices <- c("Open", "High", "Low", "Close","","Adjusted")

# User interface ----
ui <- fluidPage(
  setBackgroundColor(
    color = c("#F7FBFF", "#808080"),
    gradient = "radial",
    direction = c("top", "left")
  ),
  
  titlePanel("InvestHunter"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Please select a ticker from Yahoo Finance,"),
      textInput("symb", "Symbol", "SPY"),
      
      dateRangeInput("dates",
                     "Date range",
                     start = "2013-01-01",
                     end = as.character(Sys.Date())),
      
      selectInput("pricetype", "Price type", c("Open", "High", "Low", "Close",
                                               "Adjusted"), "Adjusted"),
      
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel(title = "Data",
                 h2("Price"),
                 plotOutput("plot")),
        tabPanel(title = "Statistics",
                 h2("Descriptive statistics for price"),
                 tableOutput("descriptive_statistics_price"),
                 br(),
                 br(),
                 h2("Descriptive statistics for log-returns"),
                 tableOutput("descriptive_statistics_returns")),
        
        tabPanel(title = "Visualization",
                 h2("Log-returns"),
                 plotOutput("returns"),
                 h2("Density of log-returns"),
                 plotOutput("hist_returns"),
                 h2("ARCH effects"),
                 plotOutput("arch_effects")),
        tabPanel(title = "Model ARIMA",
                 h2("ARIMA Forecast"),
                 sliderInput("bins",
                             "Number of forecasted days:",
                             min = 1,
                             max = 50,
                             value = 30),
                 plotOutput("distPlot"),
                 p("Prediction based on the model with lowest information criterion AIC and BIC for chosen data.")),
        tabPanel(title = "Model CAPM",
                 h2("CAPM model"),
                 numericInput("risk_free_rate",
                              "Please provide Risk-Free rate",
                              0.01,
                              step = 0.01),
                 tableOutput("CAPM"))
        
      )
    )
  )
)

# Server logic
server <- function(input, output) {
  
  dataInput <- reactive({
    getSymbols(input$symb, src = "yahoo",
               from = input$dates[1],
               to = input$dates[2],
               auto.assign = FALSE) %>%
      na.omit()
    
  })
  
  dataInput1 <- reactive({dataInput()[,which(prices == input$pricetype)]
  })
  
  #Load SP500 data
  dataInput2 <- reactive({
    getSymbols("^GSPC", src = "yahoo",
               from = input$dates[1] %>% format("%Y-%m-%d"),
               to = input$dates[2] %>% format("%Y-%m-%d"),
               auto.assign = FALSE) %>%
      na.omit()
    
  })
  
  output$plot <- renderPlot({
    
    chartSeries(dataInput1(), theme = chartTheme("white"),
                type = "line")
  })
  
  output$descriptive_statistics_price <- renderTable({
    
    Minimum = min(dataInput1(), na.rm = TRUE)
    First_Quartile = quantile(dataInput1(), 0.25, na.rm = TRUE)
    Mean = mean(dataInput1(), na.rm = TRUE)
    Third_Quartile = quantile(dataInput1(), 0.75, na.rm = TRUE)
    Maximum = max(dataInput1(), na.rm = TRUE)
    Median = median(dataInput1(), na.rm = TRUE)
    Standard_deviation = sd(dataInput1(), na.rm = TRUE)
    Skewness = skewness(dataInput1(), na.rm = TRUE)
    Kurtosis = kurtosis(dataInput1(), na.rm = TRUE)
    
    
    df <- data.frame(Minimum, First_Quartile, Mean, Third_Quartile, Maximum, Median, Skewness, Kurtosis)
    df
    
  })
  
  output$descriptive_statistics_returns <- renderTable({
    
    daily_returns = dailyReturn(dataInput1()) %>%
      na.omit()
    
    Minimum = min(daily_returns, na.rm = TRUE)
    First_Quartile = quantile(daily_returns, 0.25, na.rm = TRUE)
    Mean = mean(daily_returns, na.rm = TRUE)
    Third_Quartile = quantile(daily_returns, 0.75, na.rm = TRUE)
    Maximum = max(daily_returns, na.rm = TRUE)
    Median = median(daily_returns, na.rm = TRUE)
    Standard_deviation = sd(daily_returns, na.rm = TRUE)
    Skewness = skewness(daily_returns, na.rm = TRUE)
    Kurtosis = kurtosis(daily_returns, na.rm = TRUE)
    
    
    df <- data.frame(Minimum, First_Quartile, Mean, Third_Quartile, Maximum, Median, Skewness, Kurtosis)
    df
    
  })
  
  output$returns <- renderPlot({
    
    daily_return <- dailyReturn(dataInput1(), type = "log")
    plot(daily_return)
    
  })
  
  
  output$hist_returns <- renderPlot({
    
    daily_return <- dailyReturn(dataInput1(), type = "log")
    mean = mean(daily_return)
    sd = sd(daily_return)
    
    plot <- ggplot(daily_return, aes(x = daily_return)) + 
      geom_density(aes(color = "Simulated")) + 
      stat_function(aes(color = "Normal"), fun = dnorm, args = list(mean = mean, sd = sd)) +
      geom_vline(aes(xintercept = mean, color = "Simulated Mean"),
                 linetype="dashed", size=0.5) +
      scale_colour_manual("Density", values = c("red", "black", "blue"))
    plot
    
  })
  
  output$arch_effects <- renderPlot({
    
    daily_return <- dailyReturn(dataInput1(), type = "log")
    
    acf(daily_return^2, 
        lag.max = 36, 
        na.action = na.pass,
        ylim = c(0,0.5), # we rescale the vertical axis
        col = "darkblue", 
        lwd = 7, 
        main = "ACF of SQUARED log-returns")
    
  })
  #ARIMA Forecast
  output$distPlot <- renderPlot({
    data2 <- getSymbols(input$symb,src = "yahoo",
                        from = input$dates[1],
                        to = input$dates[2], 
                        auto.assign = FALSE)
    data2<-as.data.frame(data2)
    data3<-data2[,which(prices == input$pricetype)]
    
    #Price = ts(data3,frequency = 252,start = 2013-01-01)
    Price = ts(data3)
    arima_fit = auto.arima(Price)
    arima_forecast = forecast(arima_fit, h = input$bins)
    autoplot(arima_forecast, include = 100)
    
  })
  #Code in C++
  #output$CAPM <- renderTable({
  #Calculate SP500 log-returns
  #market_return <- dataInput2() %>%
  # dailyReturn(type = "log") 
  
  # daily_return <- dataInput1() %>%
  #  dailyReturn(type = "log")
  
  #Calculate beta
  # beta_CAPM <- cov(daily_return, market_return) / var(market_return)
  
  #Let's preserve user's rf input
  # rf <- input$risk_free_rate
  
  #CAPM
  #CAPM <- rf + beta_CAPM * (sum(daily_return) - sum(market_return))
  
  # cppFunction("double CAPMC(rf, beta_CAPM, daily_return, market_return) {
  #       double CAPM;
  #        double beta_CAPM;
  #        double daily_return;
  #        double market_return;
  #        CAPM = rf + beta_CAPM * (sum(daily_return) - sum(market_return);
  #        return CAPM;
  #        }")
  
  #  CAPMC(rf, beta_CAPM, daily_return, market_return)
  # })
  output$CAPM <- renderTable({
    #Calculate SP500 log-returns
    market_return <- dataInput2() %>%
      dailyReturn(type = "log") 
    
    daily_return <- dataInput1() %>%
      dailyReturn(type = "log")
    
    #Calculate beta
    beta_CAPM <- cov(daily_return, market_return) / var(market_return)
    
    #Let's preserve user's rf input
    rf <- input$risk_free_rate
    
    #CAPM
    CAPM <- rf + beta_CAPM * (sum(daily_return) - sum(market_return))
    CAPM <- matrix(CAPM, nrow = 1, dimnames = list(c("CAPM"), c("CAPM")))
    CAPM
    
    
    #   cppFunction("double CAPMC(rf, beta_CAPM, daily_return, market_return) {
    #             double CAPM;
    #             double beta_CAPM;
    #             double daily_return;
    #             double market_return;
    #             CAPM = rf + beta_CAPM * (sum(daily_return) - sum(market_return);
    #             return CAPM;
    #             }")
    #   
    #   CAPMC(rf, beta_CAPM, daily_return, market_return)
    # })
    
    
  })
  
  
}

# Run the app
shinyApp(ui, server)

