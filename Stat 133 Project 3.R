# ===============================================
# Fill in the following fields
# ===============================================
# Title: U2 Band Album Text Analysis Shiny App
# Description: A shiny app made to demonstrate a thorough text analysis for word frequency analysis 
# and sentiment analysis of the album lyrics for the Irish rock band U2
# Author: Morgan Rhee
# Date: December 3rd, 2021


# ===============================================
# Packages
# ===============================================
library(tidyverse)
library(tidytext)
library(shiny)
library(wordcloud)    
library(RColorBrewer) 
library(igraph)       
library(ggraph) 
library(textdata)
library(reshape2)  
# ===============================================
# Import data
# ===============================================
dat <- read.csv("u2-lyrics.csv")

# ===============================================
# Define UserInterface "ui" for application
# ===============================================

ui <- fluidPage(
  
  titlePanel("U2 Band Album Text Analysis"),
  fluidRow(
# widget for choosing album in U2
    column(3,
           selectInput(inputId = "alb", 
                        label = "Choose Album", 
                        choices = c("All Albums" = "All Albums",
                                    "Boy" = "Boy",
                                    "October" = "October",
                                    "War" = "War",
                                    "The Unforgettable Fire" = "The Unforgettable Fire",
                                    "The Joshua Tree" = "The Joshua Tree",
                                    "Rattle And Hum" = "Rattle And Hum",
                                    "Achtung Baby" = "Achtung Baby",
                                    "Zooropa" = "Zooropa",
                                    "Passengers" = "Passengers",
                                    "Pop" = "Pop",
                                    "The Best of 1980-1990" = "The Best Of 1980-1990",
                                    "All That You Cant Leave Behind" = "All That You Cant Leave Behind",
                                    "How To Dismantle An Atomic Bomb" = "How To Dismantle An Atomic Bomb",
                                    "Medium Rare Remastered" = "Medium Rare Remastered",
                                    "No Line On The Horizon" = "No Line On The Horizon",
                                    "Songs Of Innocence" = "Songs Of Innocence",
                                    "Songs Of Experience" = "Songs Of Experience"
                                    ), 
                        selected = "opt1")
    ),
    
    column(3,
# widget for number of top words displayed
           sliderInput(inputId = "top",
                       label = "Number of Top Words Displayed",
                       min = 1,
                       max = 50, 
                       step = 1,
                       value = 10),
# option to remove stopwords
          checkboxInput(inputId = "stop",
                        label = strong("Remove Stopwords"),
                        value = FALSE)
    ),
    column(3,
        # widget for bar arrangement order    
          radioButtons(inputId = "arrange", 
                       label = "Order Bars By", 
                       choices = list("Decreasing Frequency" = "decrease",
                                      "Increasing Frequency" = "increase"),
                       selected = "decrease")
    ),

       column(3,
# widget for choosing between bar plot or word cloud displayed
           radioButtons(inputId = "display", 
                        label = "Visual Type", 
                        choices = c("Barplot" = "bar",
                                    "Word Cloud" = "cloud"), 
                        selected = "bar"),
# widget for bar plot bin width   
           sliderInput(inputId = "bin",
                       label = "Bin Width",
                       min = 0,
                       max = 1,
                       step = 0.1, 
                       value = 0.5)

    )
  ),
  hr(),
  
  tabsetPanel(type = "tabs",
              tabPanel("Word Frequency Analysis",
                       h3("Top Frequent Words Plot"),
                       plotOutput("barplot1"),
                       hr(),
                       dataTableOutput('table1')),
              tabPanel("Sentiment Analysis", 
                       h3("Top Sentiment Words Plot"),
                       plotOutput("histogram"),
                       hr(),
                       dataTableOutput('table2'))
  )
)

# ===============================================
# Define Server "server" logic
# ===============================================

server <- function(input, output) {
  
  # reactive for word frequency analysis tab (FIRST TAB)
  dat_freq <- reactive({
    # for all albums
    if (input$alb == "All Albums"){
      lyrics_token <- unnest_tokens(tbl = select(dat, lyrics), 
                                   output = word, input = lyrics)
      # for only one album selected
    } else{
      lyrics_token <- unnest_tokens(tbl = filter(select(dat, album, lyrics), album == input$alb), 
                                    output = word, input = lyrics)
    }
    
    # if stop words are removed from the data
    if (input$stop == TRUE){
      lyrics_token <- lyrics_token %>% anti_join(stop_words, by = "word") 
    }
    lyrics_token
  })
  
  #reactive for sentiment analysis tab (SECOND TAB)
  sentiment_analysis <- reactive({
    # for all albums
    if (input$alb == "All Albums"){
      lyrics_token <- unnest_tokens(tbl = select(dat, album, lyrics), 
                                    output = word, input = lyrics) 
      # for only one album selected
    } else{
      lyrics_token <- unnest_tokens(tbl = filter(select(dat, album, lyrics), album == input$alb), 
                                    output = word, input = lyrics) 
    }
    
    # if stop words are removed from the data
    if (input$stop == TRUE){
      lyrics_token <- lyrics_token %>% anti_join(stop_words, by = "word") 
    }
    lyrics_token
  })
  
  
  # ===============================================
  # Outputs for the first TAB (i.e. bar chart)
  # ===============================================
  
  # plotting the word frequency analysis plot
  output$barplot1 <- renderPlot({
    count_dat <- dat_freq() %>% count(word)
    
   # code for bar order
    arrang <- 
      # in decreasing frequency order
      if (input$arrange == "decrease"){
        ord <- count_dat %>% arrange(desc(n)) %>% slice_head(n = input$top)
        # in increasing frequency order
      } else if (input$arrange == "increase"){
        ord <- count_dat %>% arrange(n) %>% slice_head(n = input$top)
      } 
    ord
    
    # code for displaying bar plot
    if (input$display == "bar") {
        plot1 <-  ggplot(data = arrang, aes(x = reorder(word, -n), y = n)) +
          geom_col(width = input$bin, fill = "skyblue2") + 
          labs(title = paste("The Top", input$top, "Most Frequent Words In", input$alb)) +
          xlab("Word") + ylab("Frequency") + theme_bw()
        
    # code for displaying word cloud
    } else if (input$display == "cloud") {
      plot1 <-  wordcloud(words = arrang$word, freq = arrang$n, scale = c(2,2),
                        max.words = 50, random.order = FALSE, colors = brewer.pal(8, "RdYlBu"))
    }
    plot1
  })
  
  # code for numeric summaries of frequencies
  output$table1 <- renderDataTable({
    count_dat <- dat_freq() %>% count(word)
    arrang <- 
      # in decreasing frequency order
      if (input$arrange == "decrease"){
        ord <- count_dat %>% arrange(desc(n)) %>% slice_head(n = input$top)
        # in increasing frequency order
      } else if (input$arrange == "increase"){
        ord <- count_dat %>% arrange(n) %>% slice_head(n = input$top)
      } 
    ord
  })
  
  
  # ===============================================
  # Outputs for the second TAB (i.e. histogram)
  # ===============================================
  
  # plotting the sentiment analysis plot
  output$histogram <- renderPlot({
    ord <- dat_freq() %>% inner_join(sentiments, by = "word") %>% 
            count(word, sentiment, sort = TRUE) %>% slice_head(n = input$top)
    
    sent <- sentiment_analysis() %>% count(album, word, sort = TRUE) %>% 
      ungroup() %>% inner_join(sentiments, by = "word") 
    
    # code for bar order
    arrang <- 
      # in decreasing frequency order
      if (input$arrange == "decrease"){
        ord <- sent %>% arrange(desc(n)) %>% slice_head(n = input$top)
        # in increasing frequency order
      } else if (input$arrange == "increase"){
        ord <- sent %>% arrange(n) %>% slice_head(n = input$top)
      } 
    ord
    
    # code for displaying bar plot
    if (input$display == "bar"){
        plot2 <- arrang %>% ggplot() + 
          geom_col(aes(x = reorder(word, n), y = n, fill = sentiment), width = input$bin) +
          scale_fill_manual(values = c("skyblue2", "lightsalmon1")) +
          xlab(NULL) + coord_flip() + theme_bw() + 
          labs(title = paste("The Top Most Frequent Sentiment Words In", input$alb))
        
        # code for displaying word cloud
    } else if (input$display == "cloud") {
      plot2 <- sentiment_analysis() %>% inner_join(get_sentiments("bing")) %>%
        count(word, sentiment, sort = TRUE) %>% acast(word ~ sentiment, value.var = "n", fill = 0.5) %>%
        comparison.cloud(colors = c("skyblue2", "lightsalmon1"), 
                         random.order = FALSE, scale=c(4, 2.5), max.words = input$top)
    }
    plot2
  })
  
  # code for numeric summaries of frequencies
  output$table2 <- renderDataTable({
    if (input$display == "cloud"){
      ord <- dat_freq() %>%
      inner_join(sentiments, by = "word") %>%
      count(word, sentiment, sort = TRUE) %>% slice_head(n = input$top)
    }
    else {
      sent <- sentiment_analysis() %>% count(album, word, sort = TRUE) %>% 
        ungroup() %>% inner_join(sentiments, by = "word") 
  
      # code for bar order
      arrang <- 
        # in decreasing frequency order
        if (input$arrange == "decrease"){
          ord <- sent %>% arrange(desc(n)) %>% slice_head(n = input$top)
          # in increasing frequency order
        } else if (input$arrange == "increase"){
          ord <- sent %>% arrange(n) %>% slice_head(n = input$top)
        } 
      ord
    }
  })
}

# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

