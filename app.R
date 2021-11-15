library(shiny)
library(tidytext)
library(shinythemes)
library(wordcloud)
library(tidyverse)
library(RColorBrewer)
library(ggplot2)
#library(dplyr)

books <- list("A Mid Summer Night's Dream" = "summer",
              "The Merchant of Venice" = "merchant",
              "Romeo and Juliet" = "romeo")
# task4: add in getFreq function for pre-processing

getFreq <- function(book,stopwords = TRUE) {
  #check that only one of the book is selected
  if(!(book %in% books))
    stop("Unknown book")
  text <- tibble(text= readLines(sprintf("./data/%s.txt",book), encoding = "UTF-8"))
  #could also pass column of text/character instead
  text <-text %>%
    unnest_tokens(word,text)%>%
    count(word, sort = TRUE)
  if(stopwords){
    text <- text %>%
      anti_join(stop_words)
  }
  return(text)
}
# task6: add in shinythemes function
ui <- fluidPage(
  titlePanel("Shakespeare's Plays Word Frequencies"), # Application title
  # task1: add in the sidebarLayout with sidebarPanel and mainPanel
  sidebarLayout(
    sidebarPanel(
      # task2: add in the inputs in the sidebarPanel
      selectInput( "Selection",  "Choose a book", choices = books),
      checkboxInput("stopwords",  "Stop Words", value = TRUE),
      actionButton("action", "Rerun"),
      hr(),
      h3("Word Cloud Settings"),
      sliderInput( "maxwords",  "Max # of Words: ", min = 10, max = 200, value = 100, step = 10),
      sliderInput( "largestword", "Size of largest words: ", min = 1, max =8, value =4),
      sliderInput( "smallestword",  "Size of smallest words: ", min = 0.1, max =4, value =0.5),
      hr(),
      h3("Word Count Settings"),
      sliderInput( "minword",  "Minimum words for Counts Chart", min = 10, max =100, value = 25),
      sliderInput( "fontsize",  "Word size for Counts Chart", min = 8, max =30, value =14),
    ),
    mainPanel(
      # task1: within the mainPanel, create two tabs (Word Cloud and Frequency)
      tabsetPanel(
        # task3: add in the outputs in the sidebarPanel
        tabPanel("Word Cloud", plotOutput("cloud",height = "600px")),
        tabPanel("Word Counts", plotOutput("freq",height = "600px" ))
        
      )
    )
  )
)
# task6: and modify your figure heights


server <- function(input, output) {
  # task5: add in reactivity for getFreq function based on inputs
  freq <- eventReactive(input$action, {
    withProgress({
      setProgress(message = "Processing corpus...")
      getFreq(input$Selection,input$stopwords)
    })
  })
  output$cloud <- renderPlot({
    v <- freq()
    pal <- brewer.pal(8,"Dark2")
    v %>%
      with(
        wordcloud(
          word,
          n,
          scale = c(input$largestword, input$smallestword),
          random.order = FALSE,
          max.words = input$maxwords,
          colors=pal))
  })
  output$freq <-renderPlot({
    v <- freq()
    v %>%
      filter(n > input$minword) %>%
      ggplot(aes(x = reorder(word, n), y = n))+
      geom_col()+
      coord_flip()+
      theme(text = element_text(size=input$fontsize)) +
      labs(x = "", y = "", title = " ")
  })
}
shinyApp(ui = ui, server = server)