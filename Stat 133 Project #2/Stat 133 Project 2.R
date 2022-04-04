# ===============================================
# Title: Retirement Withdrawal Simulator
# Description: Shiny app that runs simulations of an individual's output when 
#              given a certain input of withdrawal rate, with set rates of 
#              return and inflation.
# Author: Morgan Rhee
# Date: November 12th, 2021


# ===============================================
# Required packages
# ===============================================
library(tidyverse)
library(ggplot2)
library(shiny)

# ===============================================
# Define UserInterface "ui" for application
# ===============================================

ui <- fluidPage(
  
  titlePanel("Retirement Withdrawal Simulator"),
  
  fluidRow(
    # Inputs for initial portfolio, retirement age, and withdrawal rate
    column(3,
      numericInput(inputId= "initial", 
                   label = "Initial Portfolio Amount ($)",
                   min = 1, 
                   value = 1000000),
      sliderInput(inputId = 'age', 
                  label = 'Retirement Age (in years)', 
                  min = 1, 
                  max = 100,
                  value = 60,
                  step = 1),
      numericInput(inputId= "withdrawal", 
                   label = "Withdrawal Rate (%)",
                   min = 0, 
                   value = 4),
    ),
    
    # Inputs for mean and standard deviation of annual return rates
    column(3,
      numericInput(inputId= "avg_return", 
                   label = "Average Annual Rate of Return (%)",
                   min = 0, 
                   value = 10),
      numericInput(inputId= "return_v", 
                   label = "Average Return Volatility (%)",
                   min = 0, 
                   value = 18),
    ),
    
    # Inputs for mean and standard deviation of annual inflation rates
    column(3,
      numericInput(inputId= "avg_inflation", 
                   label = "Average Inflation Rate (%)",
                   min = 0, 
                   value = 3), 
      numericInput(inputId = "inflation_v",
                   label = "Average Inflation Volatility (%)",
                   min = 0,
                   value = 3.5)
    ),
    
    # Inputs for number of simulations, and random seed
    column(3,
      numericInput(inputId = "sim",
                   label = "Number of Simulations",
                   min = 1,
                   value = 50),
      numericInput(inputId = "seed",
                   label = "Value of Random Seed",
                   min = 1,
                   value = 12345),
      selectInput(inputId = "quantile",
                       label = h4("Displayed Quantile (%)"), 
                       choices = list("10" = '10', "25" = "25", 
                                      "50" = "50", "75" = "75", "90" = "90"), 
                      selected = "90"),
    )
  ),
  
  hr(),
  h3('Yearly Balance Graph'),
  plotOutput('plot'),
  
  hr(),
  h3('Summary Statistics'),
  tableOutput('stat_table'),
)

# ===============================================
# Define Server "server" logic
# ===============================================

server <- function(input, output) {
  
  # the main reactive function for the shiny app
  balance <- reactive({
    # set seed to save data
    set.seed(input$seed)
    
    # total number of year until 100 for retirement
    age <- 100 - input$age + 1
    
    # save data into a matrix
    dat <- matrix(nrow = age, ncol = (input$sim + 1))
    dat <- as.data.frame(dat)
    dat$Year = 1:age
    
    # first for loop: simulation
    for (n in 1:input$sim) {
      dat[1, (input$sim + 1)] = 0
      dat[1, n] <- input$initial

      # assign variable to determine remaining value each year
      remaining <- input$initial - (input$initial * input$withdrawal/100)
      
      # second for loop: years (Year 1 being the starting year)
      for (i in 2:age) {
        return_value = rnorm(1, input$avg_return/100, input$return_v/100)
        inf_value = rnorm(1, input$avg_inflation/100, input$inflation_v/100)
        
        # amount left every year
        remaining <- remaining * (1 + return_value) - 
          (input$initial * input$withdrawal/100) * (1 + inf_value)
        
        # insert remaining balance in matrix
        dat[i,n] <- remaining
        dat[i, (input$sim + 1)] <-  i - 1
        
        # label for every column 
        colnames(dat)[n] = paste0("Simulation #", n)
      }
    }
    dat
  })
  
  # pivoted data to have each simulation as a column 
  pivoted <- reactive({
    pivot_longer(balance(), cols = starts_with("sim"), 
                 names_to = c("simulation"), values_to = "amount")
  })
  
  statistic <- reactive({
    summarized <- group_by(pivoted(), Year) %>%
      summarise(Quantile_1 = quantile(amount, probs = q_value(input$quantile)), 
                Median = median(amount),
                Mean = mean(amount),
                Standard_Deviation = sd(amount))
    return(summarized)
  })
  
  q_value <- function(k) {
    if (k == "10") {
      assigned <- 0.1
    } else if (k == "25") {
      assigned <- 0.25
    } else if (k == "50") {
      assigned <- 0.5
    } else if (k == "75") {
      assigned <- 0.75
    } else if (k == "90") {
      assigned <- 0.9
    }
    assigned
  }
  
  # plot yearly graph
  output$plot <- renderPlot({
    ggplot() +
      labs(color = "Summary Statistics") +
      ggtitle(paste("Number of Simulations: ", input$sim)) +
      xlab("Years Until 100") + ylab("Remaining Balance (millions)") +
      
      geom_line(data = pivoted(), aes(x = Year, y = amount/input$initial, 
                                  group = simulation,), colour = "grey40", 
                                  size = 0.7, alpha = 0.2) +
      geom_line(data = statistic(), aes(x = Year, y = Median/input$initial, 
                                        colour = "Median"), size = 1) +
      geom_line(data = statistic(), aes(x = Year, y = Mean/input$initial, 
                                  colour = "Mean"), size = 1) +
      geom_line(data = statistic(), aes(x = Year, 
                                        y = Standard_Deviation/input$initial, 
                                        colour = "Standard Deviation"), 
                                        size = 1, linetype = "dotted") +
      geom_line(data = statistic(), aes(x = Year, y = Quantile_1/input$initial, 
                                        colour = "Displayed Quantiles"), 
                                        size = 1, linetype = "dashed") +
      scale_color_manual(values=c("turquoise1","red",'yellow',"blue")) +
      theme_bw()
  })
  
  # overall summary table
  output$stat_table <- renderTable({
    statistic()
  })
  
}

# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

