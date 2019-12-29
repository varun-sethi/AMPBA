##############################
#TABA Assignment
#Team Members
#Animesh Mishra (PGID - 11920102)
#Snigdha Prasad (PGID - 11920038)
#Githu Maria John (PGID - 11920016)
#Varun Sethi (PGID - 11920085)
#Sonaldeep Singh (PGID - 11920070)
##############################
try(require("shiny")||install.packages("shiny"))
try(require("tidytext")||install.packages("tidytext"))
try(require("wordcloud")||install.packages("wordcloud"))
try(require("ggplot2")||install.packages("ggplot2"))
try(require("tidyverse")||install.packages("tidyverse"))


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



server <- function(input, output) {
    
    
    Dataset <- reactive({
        
        if (is.null(input$file1)) { return(NULL) } else
        {
            
            Data <- as.data.frame(
                read.csv(input$file1$datapath, 
                         stringsAsFactors = F, encoding = "Latin1"))
            #Data1 = Data[-c(1),2]
            Data1 = Data[,2] #passing all the reviews to Data1
            #print(Data1[1])
            return(Data1)
        }  # else stmt ends
        
    })  # reactive statement ends
    
    
    keywords <- reactive({
        
        keynames <- input$keyWords
        
        return(keynames)
        
    })  # reactive statement ends
    
    
    output$review = renderTable({
        out = data.frame(Dataset())
        out
    })#Output for Documents Review Cars tab
    
    output$allSentence = renderTable({
        
        text = data.frame(Dataset())[[1]]
        print(length(text))
        textdf = tibble(text = text)
        sent_tokenized = textdf %>% unnest_tokens(Sentences, text, token = "sentences")
        #print(length(sent_tokenized))
        
    }) #Output for All Sentences in the Document tab
    
    
    output$keywordSentence = renderTable({
        
        text = data.frame(Dataset())[[1]]
        #print(length(text))
        textdf = tibble(text = text)
        sent_tokenized = textdf %>% unnest_tokens(sentence, text, token = "sentences")
        #print(len(sent_tokenized))
        sentences = unlist(sent_tokenized)
        myList <- c()
        #print(keywords())
        listKeywords = as.list(strsplit(keywords(), ",")[[1]])
        for (data in sentences) {
            for (i in listKeywords ){
                i = tolower(i)
                i = trimws(i)
                #print(str_detect(data,i))
                if(str_detect(data,i)){
                    print(i)
                    print(data)   
                    myList <- c(myList,data)
                }
            }
        } 
        print(myList)
        
    }) #Output for Sentences containing the Keyword tab
    
    output$bar = renderPlot({
        vector <- c()
        listKeywords = as.list(strsplit(keywords(), ",")[[1]])
        for(keys in listKeywords){
            vector <- c(vector, keys)
        }
        vector <- lapply(vector, tolower)
        vector <- lapply(vector,function(x)gsub('\\s+', '',x))
        print(vector)
        d <- tibble(txt = Dataset())
        library(ggplot2)
        df <-  d %>%
            unnest_tokens(word, txt)%>%
            count(word, sort = TRUE) %>%
            rename(count = n) %>%
            anti_join(stop_words)%>%
            filter( word %in% vector) %>%
            mutate(word = reorder(word, count))
        
        df
        
        df %>%
            ggplot(aes(word, count)) +
            geom_bar(stat = "identity", col = "black", fill = "sky blue") +
            xlab('Keywords') + ylab('Frequency of Occurence') +
            theme(text = element_text(size=16))+
                #  axis.text.x  = element_text(size=10))
            coord_flip()
        
    }) #Output for Bar Chart Tab
    
    
    output$wordcloud = renderPlot({
        vector <- c()
        listKeywords = as.list(strsplit(keywords(), ",")[[1]])
        for(keys in listKeywords){
            vector <- c(vector, keys)
        }
        vector <- lapply(vector, tolower)
        vector <- lapply(vector,function(x)gsub('\\s+', '',x))
        d <- tibble(txt = Dataset())
        library(ggplot2)
        df <-  d %>%
            unnest_tokens(word, txt)%>%
            count(word, sort = TRUE) %>%
            rename(count = n) %>%
            anti_join(stop_words)%>%
            filter( word %in% vector) %>%
            mutate(word = reorder(word, count))
        
        df 
        
        pal <- brewer.pal(8,"Dark2")
        df %>% 
            with(wordcloud(word, count, random.order = FALSE, max.words = 50, colors=pal))
        print(df)
        
    })
    
}
# Run the application 
shinyApp(ui = ui, server = server)






