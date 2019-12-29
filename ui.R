library(shiny)
library(tidyverse)
library(tidytext)
library(ggplot2)
library(wordcloud)

# Defining UI 
ui <- fluidPage(
  
  titlePanel(strong("Car Review Text Analytics")),  # name the shiny app
  
  sidebarLayout(    # creates a sidebar layout to be filled in
    
    sidebarPanel(   # creates a panel in the sidebar layout
      
      # user reads input file into input box here:
      fileInput("file1", 
                "Upload data (csv file with header)"),
      
      # user input the keywords
      
      textInput("keyWords", " Enter Keywords Comma Seperated", "power,mileage,engine,performance")
      
      
    ),   # end of sidebar panel
    
    ## Main Panel area begins.
    mainPanel(
      
      tabsetPanel(type = "tabs",   # builds tab struc
                  
                  tabPanel(strong("Overview"),   # leftmost tab
                           
                           h4(p("Data input")),
                           
                           p("This app supports only comma separated values (.csv) data file. CSV data file should have headers and the first column of the file should have row names.",align="justify"),
                           
                           br(),
                           
                           h4('How to use this App'),
                           
                           p('To use this app, click on', 
                             span(strong("Upload data (csv file with header)")),
                             'and upload the csv data file and provide the Keywords in the Text Input section seperated by commas.')),
                  
                  
                  
                  tabPanel(strong("Documents Review Cars"),
                           tableOutput('review')),
                  tabPanel(strong("All Sentences in the Document"),
                           tableOutput('allSentence')),
                  tabPanel(strong("Sentences Containing the Keywords"),
                           tableOutput('keywordSentence')),
                  tabPanel(strong("Word Cloud"),
                           plotOutput('wordcloud')),
                  tabPanel(strong("Bar Chart"),
                           plotOutput('bar'))
                  
      ) # end of tabsetPanel
    )# end of main panel
  ) # end of sidebarLayout
)
# end if fluidPage