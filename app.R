## Shiny App for Data Science Capstone - Chand Sooran (https://github.com/ChandSooran)
## April 27, 2017

## Load libraries
library(shiny)
library(stringr)

## Load data
load("TrainingTrigramDF.RData")
load("TrainingPKN.RData")
load("TrainingUnigramDF.RData")

## PRELIMINARY MANIPULATION

## Make TrainingTrigramDF Check vector to check against
TrainingTrigramDF$Check <- paste(TrainingTrigramDF$First, TrainingTrigramDF$Second, sep = "_")
punct <- '[]\\?!\"\'#$%&(){}+*/:;,._`|~\\[<=>@\\^-]'

## USER INTERFACE CODE
ui <- fluidPage(
  titlePanel(strong("Next Word Prediction App - Chand Sooran")),
  
  sidebarLayout(
   sidebarPanel(
     p("Enter a phrase in the text box and the predicted next word appears to the right"),
     textInput(inputId = "Sentence", label = "Phrase"),
     submitButton("Submit")
   ),
   mainPanel(br(),
             br(),
             br(),
             h3(strong(textOutput(outputId = "Result")), align = "center"))
  )
)

## SERVER CODE
server <- function(input, output) {
  
  output$Result <- renderText({
    
    Sentence <- gsub(punct, "", input$Sentence)
    
    ## To lower case
    Sentence <- tolower(Sentence)
    
    ## Extract trigram words
    SentenceWords <- unlist(strsplit(Sentence, " "))
    
    ## If there is an available bigram at the end of the fragment, isolate it
    if(length(SentenceWords) >= 2) {
      TrigramSearch <- c(SentenceWords[length(SentenceWords)-1], SentenceWords[length(SentenceWords)])
      TrigramSearch <- paste(TrigramSearch[1], TrigramSearch[2], sep ="_")
    } else {
      TrigramSearch <- NULL
    }
    
    ## Extract last word of final bigram
    if(length(SentenceWords) >= 1) {
      BigramSearch <- SentenceWords[length(SentenceWords)]
    } else {
      BigramSearch <- NULL
    }
    
    TrigramCheck <- subset(TrainingTrigramDF, Check == TrigramSearch)
    
    ## Isolate the highest frequency bigram in the Trigram set, if it exists
    if(nrow(TrigramCheck) > 0) {
      TrigramCheck <- TrigramCheck[TrigramCheck$Frequency == max(TrigramCheck$Frequency),]  
    }
    
    ## Make BigramCheck
    BigramCheck <- subset(TrainingPKN, First == BigramSearch)
    BigramCheck <- BigramCheck[, -(5:8)]
    names(BigramCheck) <- c("First", "Second", "Content", "Frequency", "PKN")
    BigramCheck <- BigramCheck[with(BigramCheck, order(c(Frequency, PKN), decreasing = TRUE)),]
    BigramCheck <- BigramCheck[complete.cases(BigramCheck),]
    
    ## SCENARIO I:
    ##    Trigram search produces results
    ##    There is only one trigram produced with max frequency
    if(nrow(TrigramCheck) > 0) { # Trigram search produces results
      if(length(TrigramCheck$Frequency) == 1) { # Only one trigram produced with max frequency
        Result <- TrigramCheck$Third
        print(paste("There was at least one trigram, leading to the word:", Result))
      }  
    }
    
    ## SCENARIO II:
    ##    Trigram search produces results
    ##    There are multiple trigrams produced with max frequency
    if(nrow(TrigramCheck) > 0) { # Trigram search produces results
      if(length(TrigramCheck$Frequency) > 1) { # More than one trigram produced with max frequency
        BigramLookup <- paste(TrigramCheck$Second, TrigramCheck$Third, sep = "_")
        BigramLookup <- as.data.frame(BigramLookup, stringsAsFactors = FALSE)
        names(BigramLookup) <- "Content"
        BigramLookupTest <- merge(x = BigramCheck, y = BigramLookup, by = "Content")
        BigramLookupTest <- BigramLookupTest[BigramLookupTest$Frequency == max(BigramLookupTest$Frequency),]
        BigramLookupTest <- BigramLookupTest[with(BigramLookupTest, order(PKN, decreasing = TRUE)),]
        Result <- BigramLookupTest$Second[1]
        print(paste("There was at least one trigram, resulting in the word:", Result))
      }
    }
    
    ## SCENARIO III:
    ##    Trigram search does not produce any results
    ##    There is a relevant bigram 
    if(nrow(TrigramCheck) == 0){ # Trigram search produces no results
      if(nrow(BigramCheck) > 0) { # Bigram search produces results
        BigramCheckAdjusted <- BigramCheck[BigramCheck$Frequency == max(BigramCheck$Frequency),]
        BigramCheckAdjusted <- BigramCheckAdjusted[with(BigramCheckAdjusted, order(PKN, decreasing = TRUE)),]
        BigramCheckAdjusted <- BigramCheckAdjusted[complete.cases(BigramCheckAdjusted),]
        Result <- BigramCheckAdjusted$Second[1]
        print(paste("There were no trigrams, but bigrams suggested:", Result))  
      }
    }
    
    ## SCENARIO IV
    ##  Trigram search does not produce any results
    ##  There is no relevant bigram
    if(nrow(TrigramCheck) == 0) { # Trigram search produces no results
      if(nrow(BigramCheck) == 0) { # Bigram search produces no results
        TrainingUnigramDF <- TrainingUnigramDF[with(TrainingUnigramDF, order(Frequency, decreasing = TRUE)),]
        Result <- TrainingUnigramDF$Content[1]
        print(paste("There were no trigrams or bigrams, so we chose the most frequent Unigram:", Result))
      }
    }
    
    ## Final output!
    Result
  })
  
}

## EXECUTION
shinyApp(ui = ui, server = server)