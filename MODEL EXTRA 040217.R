## Model 5.0 appendix - Chand Sooran 040217
## Making 4-grams and 5-grams

## Initialize libraries
library(tm)
library(stringi)
library(stringr)
library(quanteda)
library(tictoc)
library(ggplot2)
library(caret)
library(AppliedPredictiveModeling)
library(data.table)
library(plyr)

## Set working directory to data file
setwd("C:/Chand Sooran/Johns Hopkins/Capstone 5.0/Model 5.0/Data")

## Load corpus
load("TrainingCorpus.RData")

## Suppress warnings
options(warn = -1)

## MAKE THE TRAINING FOURGRAM AND FIVEGRAM DATAFRAMES

## Make the Training fourgram dataframe
tic()
if(!exists("TrainingFourgramDF")){
  TrainingFourgram <- tokenize(TrainingCorpus, ngrams = 4L, skip = 0L,
                              removePunct = TRUE, removeNumbers = TRUE)
  TrainingFourgramDFM <- dfm(TrainingFourgram, stem = FALSE,
                            ignoredFeatures = stopwords("english"))
  TrainingFourgramDF <- data.frame(Content = features(TrainingFourgramDFM), Frequency = colSums(TrainingFourgramDFM),
                                  row.names = NULL, stringsAsFactors = FALSE)
  TrainingFourgramDF <- TrainingFourgramDF[with(TrainingFourgramDF, order(Frequency, decreasing = TRUE)),]
  save(TrainingFourgramDF, file = "TrainingFourgramDF.RData")
}
ExecTime <- toc()
TrainingFourgramTime <- ExecTime$toc - ExecTime$tic
TotalTime <- TrainingFourgramTime

## Make the Training fivegram dataframe
tic()
if(!exists("TrainingFivegramDF")){
  TrainingFivegram <- tokenize(TrainingCorpus, ngrams = 5L, skip = 0L,
                              removePunct = TRUE, removeNumbers = TRUE)
  TrainingFivegramDFM <- dfm(TrainingFivegram, stem = FALSE,
                            ignoredFeatures = stopwords("english"))
  TrainingFivegramDF <- data.frame(Content = features(TrainingFivegramDFM), Frequency = colSums(TrainingFivegramDFM),
                                  row.names = NULL, stringsAsFactors = FALSE)
  TrainingFivegramDF <- TrainingFivegramDF[with(TrainingFivegramDF, order(Frequency, decreasing = TRUE)),]
  save(TrainingFivegramDF, file = "TrainingFivegramDF.RData")
}
ExecTime <- toc()
TrainingFivegramTime <- ExecTime$toc - ExecTime$tic
TotalTime <- TotalTime + TrainingFivegramTime

## SEPARATE THE WORDS OUT IN THE FOURGRAM AND FIVEGRAM DATA FRAMES

## Separate fivegrams into 5 columns, removing concatentor "_"
if(!exists("SeparateFivegramTime")) {
  tic()

  TrainingFivegramDF$First <- sapply(strsplit(TrainingFivegramDF$Content, "\\_"),"[",1)
  TrainingFivegramDF$First <- tolower(TrainingFivegramDF$First)
  TrainingFivegramDF <- TrainingFivegramDF[!TrainingFivegramDF$First == "#",]
  TrainingFivegramDF <- TrainingFivegramDF[!TrainingFivegramDF$First == "@",]

  TrainingFivegramDF$Second <- sapply(strsplit(TrainingFivegramDF$Content, "\\_"), "[", 2)
  TrainingFivegramDF$Second <- tolower(TrainingFivegramDF$Second)
  TrainingFivegramDF <- TrainingFivegramDF[!TrainingFivegramDF$Second == "#",]
  TrainingFivegramDF <- TrainingFivegramDF[!TrainingFivegramDF$Second == "@",]

  TrainingFivegramDF$Third <- sapply(strsplit(TrainingFivegramDF$Content, "\\_"), "[", 3)
  TrainingFivegramDF$Third <- tolower(TrainingFivegramDF$Third)
  TrainingFivegramDF <- TrainingFivegramDF[!TrainingFivegramDF$Third == "#",]
  TrainingFivegramDF <- TrainingFivegramDF[!TrainingFivegramDF$Third == "@",]

  TrainingFivegramDF$Fourth <- sapply(strsplit(TrainingFivegramDF$Content, "\\_"), "[", 4)
  TrainingFivegramDF$Fourth <- tolower(TrainingFivegramDF$Fourth)
  TrainingFivegramDF <- TrainingFivegramDF[!TrainingFivegramDF$Fourth == "#",]
  TrainingFivegramDF <- TrainingFivegramDF[!TrainingFivegramDF$Fourth == "@",]

  TrainingFivegramDF$Fifth <- sapply(strsplit(TrainingFivegramDF$Content, "\\_"), "[", 5)
  TrainingFivegramDF$Fifth <- tolower(TrainingFivegramDF$Fifth)
  TrainingFivegramDF <- TrainingFivegramDF[!TrainingFivegramDF$Fifth == "#",]
  TrainingFivegramDF <- TrainingFivegramDF[!TrainingFivegramDF$Fifth == "@",]

  TrainingFivegramDF$Content <- iconv(TrainingFivegramDF$Content, "latin1", "ASCII", sub = "") # Remove foreign characters
  TrainingFivegramDF$First <- iconv(TrainingFivegramDF$First, "latin1", "ASCII", sub = "")
  TrainingFivegramDF$Second <- iconv(TrainingFivegramDF$Second, "latin1", "ASCII", sub = "")
  TrainingFivegramDF$Third <- iconv(TrainingFivegramDF$Third, "latin1", "ASCII", sub = "")
  TrainingFivegramDF$Fourth <- iconv(TrainingFivegramDF$Fourth, "latin1", "ASCII", sub = "")
  TrainingFivegramDF$Fifth <- iconv(TrainingFivegramDF$Fifth, "latin1", "ASCII", sub = "")

  TrainingFivegramDF[TrainingFivegramDF == ""] <- NA # Remove blanks
  TrainingFivegramDF <- TrainingFivegramDF[complete.cases(TrainingFivegramDF),] # Remove NAs

  TrainingFivegramDF <- TrainingFivegramDF[with(TrainingFivegramDF, order(Fifth, Fourth, Third, Second, First)),] # NEW
  
  save(TrainingFivegramDF, file = "TrainingFivegramDF.RData")
  ExecTime <- toc()
  SeparateFivegramTime <- ExecTime$toc - ExecTime$tic
  TotalTime <- TotalTime + SeparateFivegramTime
}

## Separate fourgrams into 4 columns, removing concatentor "_"
if(!exists("SeparateFourgramTime")) {
  tic()

  TrainingFourgramDF$First <- sapply(strsplit(TrainingFourgramDF$Content, "\\_"),"[",1)
  TrainingFourgramDF$First <- tolower(TrainingFourgramDF$First)
  TrainingFourgramDF <- TrainingFourgramDF[!TrainingFourgramDF$First == "#",]
  TrainingFourgramDF <- TrainingFourgramDF[!TrainingFourgramDF$First == "@",]

  TrainingFourgramDF$Second <- sapply(strsplit(TrainingFourgramDF$Content, "\\_"), "[", 2)
  TrainingFourgramDF$Second <- tolower(TrainingFourgramDF$Second)
  TrainingFourgramDF <- TrainingFourgramDF[!TrainingFourgramDF$Second == "#",]
  TrainingFourgramDF <- TrainingFourgramDF[!TrainingFourgramDF$Second == "@",]

  TrainingFourgramDF$Third <- sapply(strsplit(TrainingFourgramDF$Content, "\\_"), "[", 3)
  TrainingFourgramDF$Third <- tolower(TrainingFourgramDF$Third)
  TrainingFourgramDF <- TrainingFourgramDF[!TrainingFourgramDF$Third == "#",]
  TrainingFourgramDF <- TrainingFourgramDF[!TrainingFourgramDF$Third == "@",]

  TrainingFourgramDF$Fourth <- sapply(strsplit(TrainingFourgramDF$Content, "\\_"), "[", 4)
  TrainingFourgram$Fourth <- tolower(TrainingFourgramDF$Fourth)
  TrainingFourgramDF <- TrainingFourgramDF[!TrainingFourgramDF$Fourth == "#",]
  TrainingFourgramDF <- TrainingFourgramDF[!TrainingFourgramDF$Fourth == "@",]
  
  
  TrainingFourgramDF$Content <- iconv(TrainingFourgramDF$Content, "latin1", "ASCII", sub = "") # Remove foreign characters
  TrainingFourgramDF$First <- iconv(TrainingFourgramDF$First, "latin1", "ASCII", sub = "")
  TrainingFourgramDF$Second <- iconv(TrainingFourgramDF$Second, "latin1", "ASCII", sub = "")
  TrainingFourgramDF$Third <- iconv(TrainingFourgramDF$Third, "latin1", "ASCII", sub = "")
  TrainingFourgramDF$Fourth <- iconv(TrainingFourgramDF$Fourth, "latin1", "ASCII", sub = "")
  
  TrainingFourgramDF[TrainingFourgramDF == ""] <- NA # Remove blanks
  TrainingFourgramDF <- TrainingFourgramDF[complete.cases(TrainingFourgramDF),] # Remove NAs

  TrainingFourgramDF <- TrainingFourgramDF[with(TrainingFourgramDF, order(Fourth, Third, Second, First)),] # NEW
  
  save(TrainingFourgramDF, file = "TrainingFourgramDF.RData")
  ExecTime <- toc()
  SeparateFourgramTime <- ExecTime$toc - ExecTime$tic
  TotalTime <- TotalTime + SeparateFourgramTime
}
