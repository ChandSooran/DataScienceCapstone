## Model 5.0 - REDUX using Modified Kneser-Ney - Starting March 2017

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

## Define directories
zipurl <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
zipfiledirectory <- c("C://Chand Sooran/Johns Hopkins/Capstone 5.0/Course Dataset Zipfile/")
workingdirectory1 <- c("C://Chand Sooran/Johns Hopkins/Capstone 5.0/Week 1/")
workingdirectory2 <- paste(workingdirectory1, "final", "en_US", sep = "/")
workingdirectory3 <- c("C://Chand Sooran/Johns Hopkins/Capstone 5.0/Profanity")
workingdirectory4 <- c("C://Chand Sooran/Johns Hopkins/Capstone 5.0/Model 5.0/Data/")

## Set working directory
setwd(workingdirectory4)

## Initialize time tracking
TotalTime <- 0

## RETRIEVE DATA

## Set working directory
setwd(workingdirectory1) # Week 1

## Set filenames for download
coursedatafile = paste(zipfiledirectory, "Coursera-Swiftkey.zip", sep = "/")
courseunzippedfile = "final"
filenames = c("en_US.blogs.txt", "en_US.twitter.txt", "en_US.news.txt")

## Download zip file
tic()
if(!file.exists(coursedatafile)){
  download.file(zipurl, coursedatafile)
}
ExecTime <- toc()
ZipTime <- ExecTime$toc - ExecTime$tic
TotalTime <- TotalTime + ZipTime

## Create function for obtaining files into the working directory
getfiles <- function(x){
  if(!file.exists(paste(workingdirectory1, x, sep = "/"))){
    if(!file.exists(paste(workingdirectory1, courseunzippedfile, sep ="/"))){
      unzip(zipfile = coursedatafile)
      file.copy(paste(workingdirectory2, x, sep = "/"), workingdirectory1)
    } else {
      file.copy(paste(workingdirectory2, x, sep = "/"), workingdirectory1)
    }
  }
}

## Get files
sapply(filenames, getfiles)

## Get profane words
if(!exists("ProfaneWords")){
  ProfaneWords <- read.csv(paste(workingdirectory3, "Profane Words.txt", sep = "/"))
  ProfaneWords <- as.vector(t(ProfaneWords))
  ProfaneWords <- gsub("\\s","", ProfaneWords)
  setwd(workingdirectory1)
}

## Read the three files using readLines
tic()
if(!exists("Twitter")){
  Twitter <- readLines(con = "en_US.twitter.txt", n = -1L, skipNul = TRUE, encoding = "UTF-8")
}
ExecTime <- toc()
TwitterReadTime <- ExecTime$toc - ExecTime$tic
TotalTime <- TotalTime + TwitterReadTime

tic()
if(!exists("Blogs")){
  Blogs <- readLines(con = "en_US.blogs.txt", n = -1L, skipNul = TRUE, encoding = "UTF-8")
}
ExecTime <- toc()
BlogsReadTime <- ExecTime$toc - ExecTime$tic
TotalTime <- TotalTime + BlogsReadTime

tic()
if(!exists("News")){
  News <- readLines(con = "en_US.news.txt", n = -1L, skipNul = TRUE, warn = FALSE, encoding = "UTF-8")
}
ExecTime <- toc()
NewsReadTime <- ExecTime$toc - ExecTime$tic
TotalTime <- TotalTime + NewsReadTime

## SAMPLE 9% OF THE COMBINED DATASETS

## Make and save a file sampling 1% of all three datasets together
tic()
if(!exists("All")){
  All <- c(Twitter, Blogs, News)
  SampleProportion <- 0.085 # Set the percentage to sample ****
  All <- sample(All, size = round(SampleProportion * length(All)))
  All <- gsub("#\\S+", "", All) # Remove hashtags
  All <- gsub("@\\S+", "", All) # Remove mentions
  All <- gsub("\\S+\\d\\S+", "", All) # Remove digits
  All <- gsub("\\d\\S+", "", All) ## Remove digits
  All <- gsub("\\S+\\d", "", All) ## Remove digits
  All <- gsub("\\rt|\\RT", "", All) ## Remove "rt"
  setwd(workingdirectory4)
  save(All, file = "All.RData")
}
ExecTime <- toc()
SampleTime <- ExecTime$toc - ExecTime$tic
TotalTime <- TotalTime + SampleTime

## MAKE THE TRAINING, VALIDATION, AND TESTING DATA SETS

## Set the fractions of the dataframe you want to split in training
fractionTraining <- 0.6
fractionValidation <- 0.2
fractionTest <- 0.2

## Compute sample sizes, rounded down to next integer with floor()
sampleSizeTraining <- floor(fractionTraining * length(All))
sampleSizeValidation <- floor(fractionValidation * length(All))
sampleSizeTest <- floor(fractionTest * length(All))

## Create the randomly sampled indices, using setdiff() to avoid overlapping subsets of indices
indexTraining <- sort(sample(seq_len(length(All)),size = sampleSizeTraining))
indexNotTraining <- setdiff(seq_len(length(All)), indexTraining) ## Take the dataset All, remove Training items
indexValidation <- sort(sample(indexNotTraining, size = sampleSizeValidation))
indexTest <- setdiff(indexNotTraining, indexValidation)

## Make the three vectors for training, validation, and testing
Training <- All[indexTraining]
Validation <- All[indexValidation]
Test <- All[indexTest]

## Save three files
tic()
setwd(workingdirectory4)
save(Training, file = "Training.RData")
save(Validation, file = "Validation.RData")
save(Test, file = "Test.RData")
ExecTime <- toc()
SaveFoldsTime <- ExecTime$toc - ExecTime$tic
TotalTime <- TotalTime + SaveFoldsTime

## MAKE THE CORPUSES

## Make the training corpus
tic()
if(!exists("TrainingCorpus")){
  TrainingCorpus <- corpus(Training,
                           docvars = data.frame(party = names(All)))
  save(TrainingCorpus, file = "TrainingCorpus.RData")
}
ExecTime <- toc()
TrainingCorpusTime <- ExecTime$toc - ExecTime$tic
TotalTime <- TotalTime + TrainingCorpusTime

## Make the validation corpus
tic()
if(!exists("ValidationCorpus")){
  ValidationCorpus <- corpus(Validation,
                             docvars = data.frame(party = names(All)))
  save(ValidationCorpus, file = "ValidationCorpus.RData")
}
ExecTime <- toc()
ValidationCorpusTime <- ExecTime$toc - ExecTime$tic
TotalTime <- TotalTime + ValidationCorpusTime

## Make the testing corpus
tic()
if(!exists("TestCorpus")){
  TestCorpus <- corpus(Test,
                       docvars = data.frame(party = names(All)))
  save(TestCorpus, file = "TestCorpus.RData")
}
ExecTime <- toc()
TestCorpusTime <- ExecTime$toc - ExecTime$tic
TotalTime <- TotalTime + TestCorpusTime

## MAKE THE DOCUMENT FREQUENCY MATRICES, INCLUDING TOKENIZATION

## Make the Training dfm
tic()
if(!exists("TrainingDFM")){
  TrainingDFM <- dfm(TrainingCorpus, stem = FALSE, ignoredFeatures = stopwords("english"))
  save(TrainingDFM, file = "TrainingDFM.RData")
}
ExecTime <- toc()
TrainingDFMTime <- ExecTime$toc - ExecTime$tic
TotalTime <- TotalTime + TrainingDFMTime

## Make the Validation dfm
tic()
if(!exists("ValidationDFM")){
  ValidationDFM <- dfm(ValidationCorpus, stem = FALSE, ignoredFeatures = stopwords("english"))
  save(ValidationDFM, file = "ValidationDFM.RData")
}
ExecTime <- toc()
ValidationDFMTime <- ExecTime$toc - ExecTime$tic
TotalTime <- TotalTime + ValidationDFMTime

## Make the Testing dfm
tic()
if(!exists("TestDFM")){
  TestDFM <- dfm(TestCorpus, stem = FALSE, ignoredFeatures = stopwords("english"))
  save(TestDFM, file = "TestDFM.RData")
}
ExecTime <- toc()
TestDFMTime <- ExecTime$toc - ExecTime$tic
TotalTime <- TotalTime + TestDFMTime

## MAKE UNIGRAM DATAFRAMES

## Make the Training unigram dataframe
tic()
if(!exists("TrainingUnigramDF")){
  TrainingUnigram <- tokenize(TrainingCorpus, ngrams = 1L, skip = 0L,
                              removePunct = TRUE, removeNumbers = TRUE)
  TrainingUnigramDFM <- dfm(TrainingUnigram, stem = FALSE,
                            ignoredFeatures = stopwords("english"))
  TrainingUnigramDF <- data.frame(Content = features(TrainingUnigramDFM), Frequency = colSums(TrainingUnigramDFM),
                                  row.names = NULL, stringsAsFactors = FALSE)
  TrainingUnigramDF <- TrainingUnigramDF[with(TrainingUnigramDF, order(Frequency, decreasing = TRUE)),]
  save(TrainingUnigramDF, file = "TrainingUnigramDF.RData")
}
ExecTime <- toc()
TrainingUnigramTime <- ExecTime$toc - ExecTime$tic
TotalTime <- TotalTime + TrainingUnigramTime

## Make the Validation unigram dataframe
tic()
if(!exists("ValidationUnigramDF")){
  ValidationUnigram <- tokenize(ValidationCorpus, ngrams = 1L, skip = 0L,
                                removePunct = TRUE, removeNumbers = TRUE)
  ValidationUnigramDFM <- dfm(ValidationUnigram, stem = FALSE,
                              ignoredFeatures = stopwords("english"))
  ValidationUnigramDF <- data.frame(Content = features(ValidationUnigramDFM), Frequency = colSums(ValidationUnigramDFM),
                                    row.names = NULL, stringsAsFactors = FALSE)
  save(ValidationUnigramDF, file = "ValidationUnigramDF.RData")
}
ExecTime <- toc()
ValidationUnigramTime <- ExecTime$toc - ExecTime$tic
TotalTime <- TotalTime + ValidationUnigramTime

## Make the Testing unigram dataframe
tic()
if(!exists("TestUnigramDF")){
  TestUnigram <- tokenize(TestCorpus, ngrams = 1L, skip = 0L,
                          removePunct = TRUE, removeNumbers = TRUE)
  TestUnigramDFM <- dfm(TestUnigram, stem = FALSE,
                        ignoredFeatures = stopwords("english"))
  TestUnigramDF <- data.frame(Content = features(TestUnigramDFM), Frequency = colSums(TestUnigramDFM),
                              row.names = NULL, stringsAsFactors = FALSE)
  save(TestUnigramDF, file = "TestUnigramDF.RData")
}
ExecTime <- toc()
TestUnigramTime <- ExecTime$toc - ExecTime$tic
TotalTime <- TotalTime + TestUnigramTime

## MAKE BIGRAM DATAFRAMES

## Make Training bigram dataframe
tic()
if(!exists("TrainingBigramDF")){
  TrainingBigram <- tokenize(TrainingCorpus, ngrams = 2L, skip = 0L,
                             removePunct = TRUE, removeNumbers = TRUE)
  TrainingBigramDFM <- dfm(TrainingBigram,
                           stem = FALSE, ignoredFeatures = stopwords("english"))
  TrainingBigramDF <- data.frame(Content = features(TrainingBigramDFM), Frequency = colSums(TrainingBigramDFM),
                                 row.names = NULL, stringsAsFactors = FALSE)
  save(TrainingBigramDF, file = "TrainingBigramDF.RData")
}
ExecTime <- toc()
TrainingBigramTime <- ExecTime$toc - ExecTime$tic
TotalTime <- TotalTime + TrainingBigramTime

## Make Validation bigram dataframe
tic()
if(!exists("ValidationBigramDF")){
  ValidationBigram <- tokenize(ValidationCorpus, ngrams = 2L, skip = 0L,
                               removePunct = TRUE, removeNumbers = TRUE)
  ValidationBigramDFM <- dfm(ValidationBigram, stem = FALSE,
                             ignoredFeatures = stopwords("english"))
  ValidationBigramDF <- data.frame(Content = features(ValidationBigramDFM), Frequency = colSums(ValidationBigramDFM),
                                   row.names = NULL, stringsAsFactors = FALSE)
  save(ValidationBigramDF, file = "ValidationBigramDF.RData")
}
ExecTime <- toc()
ValidationBigramTime <- ExecTime$toc - ExecTime$tic
TotalTime <- TotalTime + ValidationBigramTime

## Make Testing bigram dataframe
tic()
if(!exists("TestBigramDF")){
  TestBigram <- tokenize(TestCorpus, ngrams = 2L, skip = 0L,
                         removePunct = TRUE, removeNumbers = TRUE)
  TestBigramDFM <- dfm(TestBigram, stem = FALSE,
                       ignoredFeatures = stopwords("english"))
  TestBigramDF <- data.frame(Content = features(TestBigramDFM), Frequency = colSums(TestBigramDFM),
                             row.names = NULL, stringsAsFactors = FALSE)
  save(TestBigramDF, file = "TestBigramDF.RData")
}
ExecTime <- toc()
TestBigramTime <- ExecTime$toc - ExecTime$tic
TotalTime <- TotalTime + TestBigramTime

## MAKE TRIGRAM DATAFRAMES

## Make Training trigram dataframe
tic()
if(!exists("TrainingTrigramDF")){
  TrainingTrigram <- tokenize(TrainingCorpus, ngrams = 3L, skip = 0L,
                              removePunct = TRUE, removeNumbers = TRUE)
  TrainingTrigramDFM <- dfm(TrainingTrigram, stem = FALSE,
                            ignoredFeatures = stopwords("english"))
  TrainingTrigramDF <- data.frame(Content = features(TrainingTrigramDFM), Frequency = colSums(TrainingTrigramDFM),
                                  row.names = NULL, stringsAsFactors = FALSE)
  save(TrainingTrigramDF, file = "TrainingTrigramDF.RData")
}
ExecTime <- toc()
TrainingTrigramTime <- ExecTime$toc - ExecTime$tic
TotalTime <- TotalTime + TrainingTrigramTime

## Make Validation trigram dataframe
tic()
if(!exists("ValidationTrigramDF")){
  ValidationTrigram <- tokenize(ValidationCorpus, ngrams = 3L, skip = 0L,
                                removePunct = TRUE, removeNumbers = TRUE)
  ValidationTrigramDFM <- dfm(ValidationTrigram, stem = FALSE,
                              ignoredFeatures = stopwords("english"))
  ValidationTrigramDF <- data.frame(Content = features(ValidationTrigramDFM), Frequency = colSums(ValidationTrigramDFM),
                                    row.names = NULL, stringsAsFactors = FALSE)
  save(ValidationTrigramDF, file = "ValidationTrigramDF.RData")
}
ExecTime <- toc()
ValidationTrigramTime <- ExecTime$toc - ExecTime$tic
TotalTime <- TotalTime + ValidationTrigramTime

## Make Testing trigram dataframe
tic()
if(!exists("TestTrigramDF")){
  TestTrigram <- tokenize(TestCorpus, ngrams = 3L, skip = 0L,
                          removePunct = TRUE, removeNumbers = TRUE)
  TestTrigramDFM <- dfm(TestTrigram, stem = FALSE,
                        ignoredFeatures = stopwords("english"))
  TestTrigramDF <- data.frame(Content = features(TestTrigramDFM), Frequency = colSums(TestTrigramDFM),
                              row.names = NULL, stringsAsFactors = FALSE)
  save(TestTrigramDF, file = "TestTrigramDF.RData")
}
ExecTime <- toc()
TestTrigramTime <- ExecTime$toc - ExecTime$tic
TotalTime <- TotalTime + TestTrigramTime

## CALCULATE THE KNESER-NEY BIGRAM PROBABILITIES FOR THE TRAINING DATA SET WITH ASSUMED VALUE OF d

## Need to calculate five inputs:
## 1. # of words occurring before word w in a bigram (i.e. number of bigram types, word w completes)
##        "TrainingBigramEndCount"
## 2. total # of bigram types
## 3. counts observed for each bigram type
## 4. counts observed for each unigram type
## 5. # of word types that can follow word w(i-1) in a bigram, i.e. # of bigrams that w(i-1) begins

## Remove numbers from alpha-numeric characters
TrainingBigramDF$Content <- gsub("[0-9]+", "", TrainingBigramDF$Content)

## Remove all rows in Unigram where Content is either "#" or "@"
TrainingUnigramDF <- TrainingUnigramDF[!TrainingUnigramDF$Content == "#",]
TrainingUnigramDF <- TrainingUnigramDF[!TrainingUnigramDF$Content == "@",]

## *** SEPARATE BIGRAMS AND TRIGRAMS INTO SEPARATE WORDS ***

## Separate bigrams into separate words by removing concatenator "_", sorted by second word
## Calculate the # of words occurring before each word w completing a bigram
if(!exists("SeparateBigramTime")) {
  tic()
  
  TrainingBigramDF$First <- sapply(strsplit(TrainingBigramDF$Content, "\\_"),"[",1) # Strip first word
  TrainingBigramDF$First <- tolower(TrainingBigramDF$First) # Make first word lower case
  TrainingBigramDF <- TrainingBigramDF[!TrainingBigramDF$First == "#",]
  TrainingBigramDF <- TrainingBigramDF[!TrainingBigramDF$First == "@",]
  
  TrainingBigramDF$Second <- sapply(strsplit(TrainingBigramDF$Content, "\\_"), "[", 2) # Strip second word
  TrainingBigramDF$Second <- tolower(TrainingBigramDF$Second) # Make second word lower case
  TrainingBigramDF <- TrainingBigramDF[!TrainingBigramDF$Second == "#",]
  TrainingBigramDF <- TrainingBigramDF[!TrainingBigramDF$Second == "@",]
  
  TrainingBigramDF$Content <- iconv(TrainingBigramDF$Content, "latin1", "ASCII", sub = "") # Remove foreign characters
  TrainingBigramDF$First <- iconv(TrainingBigramDF$First, "latin1", "ASCII", sub = "")
  TrainingBigramDF$Second <- iconv(TrainingBigramDF$Second, "latin1", "ASCII", sub = "")
  
  TrainingBigramDF[TrainingBigramDF == ""] <- NA # Remove blanks
  TrainingBigramDF <- TrainingBigramDF[complete.cases(TrainingBigramDF),] # Remove NAs

  TrainingBigramDF <- TrainingBigramDF[with(TrainingBigramDF, order(Second, First)),] # NEW Sort alphabetically

  NumberWordsBeforeTrainingBigramDF <- aggregate(TrainingBigramDF["Content"], by = TrainingBigramDF[c("First","Second")], FUN = length) # Aggregate by number for number of unique bigrams with common second word
  NumberWordsBeforeTrainingBigramDF <- aggregate(NumberWordsBeforeTrainingBigramDF["First"], by = NumberWordsBeforeTrainingBigramDF["Second"], FUN = length) # Aggregate by number for number of bigrams by second word
  names(NumberWordsBeforeTrainingBigramDF) <- c("Second", "Before")

  save(TrainingBigramDF, file = "TrainingBigramDF.RData")
  save(NumberWordsBeforeTrainingBigramDF, file = "NumberWordsBeforeTrainingBigramDF.RData")
  ExecTime <- toc()
  SeparateBigramTime <- ExecTime$toc - ExecTime$tic
  TotalTime <- TotalTime + SeparateBigramTime
}

## Separate trigrams into 3 columns, removing concatentor "_"
## Calculate the number of words occurring before each word w completing a trigram
if(!exists("SeparateTrigramTime")) {
  tic()

  TrainingTrigramDF$First <- sapply(strsplit(TrainingTrigramDF$Content, "\\_"),"[",1)
  TrainingTrigramDF$First <- tolower(TrainingTrigramDF$First)
  TrainingTrigramDF <- TrainingTrigramDF[!TrainingTrigramDF$First == "#",]
  TrainingTrigramDF <- TrainingTrigramDF[!TrainingTrigramDF$First == "@",]

  TrainingTrigramDF$Second <- sapply(strsplit(TrainingTrigramDF$Content, "\\_"), "[", 2)
  TrainingTrigramDF$Second <- tolower(TrainingTrigramDF$Second)
  TrainingTrigramDF <- TrainingTrigramDF[!TrainingTrigramDF$Second == "#",]
  TrainingTrigramDF <- TrainingTrigramDF[!TrainingTrigramDF$Second == "@",]
  
  TrainingTrigramDF$Third <- sapply(strsplit(TrainingTrigramDF$Content, "\\_"), "[", 3)
  TrainingTrigramDF$Third <- tolower(TrainingTrigramDF$Third)
  TrainingTrigramDF <- TrainingTrigramDF[!TrainingTrigramDF$Third == "#",]
  TrainingTrigramDF <- TrainingTrigramDF[!TrainingTrigramDF$Third == "@",]

  TrainingTrigramDF$Content <- iconv(TrainingTrigramDF$Content, "latin1", "ASCII", sub = "") # Remove foreign characters
  TrainingTrigramDF$First <- iconv(TrainingTrigramDF$First, "latin1", "ASCII", sub = "")
  TrainingTrigramDF$Second <- iconv(TrainingTrigramDF$Second, "latin1", "ASCII", sub = "")
  TrainingTrigramDF$Third <- iconv(TrainingTrigramDF$Third, "latin1", "ASCII", sub = "")

  TrainingTrigramDF[TrainingTrigramDF == ""] <- NA # Remove blanks
  TrainingTrigramDF <- TrainingTrigramDF[complete.cases(TrainingTrigramDF),] # Remove NAs

  TrainingTrigramDF <- TrainingTrigramDF[with(TrainingTrigramDF, order(Third, Second, First)),] # NEW
  TrainingTrigramDF <- TrainingTrigramDF[with(TrainingTrigramDF, order(Third, Second, First)),]
  
  save(TrainingTrigramDF, file = "TrainingTrigramDF.RData")
  ExecTime <- toc()
  SeparateTrigramTime <- ExecTime$toc - ExecTime$tic
  TotalTime <- TotalTime + SeparateTrigramTime
}

## Calculate the total number of bigram types
NumberBigramTypes <- length(unique(TrainingBigramDF$Content))

## ** CALCULATE CONTINUATION PROBABILITIES ***
if(!exists("ContProbTime")){
  tic()
  NumberWordsBeforeTrainingBigramDF$ContProb <- NumberWordsBeforeTrainingBigramDF$Before / sum(NumberWordsBeforeTrainingBigramDF$Before)
  NumberWordsBeforeTrainingBigramDF <- NumberWordsBeforeTrainingBigramDF[ ,-2]
  save(NumberWordsBeforeTrainingBigramDF, file = "NumberWordsBeforeTrainingBigramDF.RData")
  ExecTime <- toc()
  ContProbTime <- ExecTime$toc - ExecTime$tic
  TotalTime <- TotalTime + ContProbTime
}

## ** CALCULATE LAMBDAS ***

## Calculate the number of word types that can follow an individual word w
if(!exists("CompletedTime")){
  tic()
  TrainingBigramDF <- TrainingBigramDF[with(TrainingBigramDF, order(First,Second)),] # Re-order bigrams by first word

  NumberWordsAfterTrainingBigramDF <- aggregate(TrainingBigramDF["Content"],
                                                by = TrainingBigramDF[c("First","Second")], FUN = length)

  NumberWordsAfterTrainingBigramDF <- aggregate(NumberWordsAfterTrainingBigramDF["Second"],
                                                by = NumberWordsAfterTrainingBigramDF["First"],
                                                FUN = length)

  save(NumberWordsAfterTrainingBigramDF, file = "NumberWordsAfterTrainingBigramDF.RData")
  ExecTime <- toc()
  CompletedTime <- ExecTime$toc - ExecTime$tic
  TotalTime <- TotalTime + CompletedTime
}

## Move the unigram dataframe to all lower case
tolower(TrainingUnigramDF$Content)

## Clean up Unigram
TrainingUnigramDF$Content <- iconv(TrainingUnigramDF$Content, "latin1", "ASCII", sub = "")

## Get rid of blank content in Unigram
TrainingUnigramDF$Content[TrainingUnigramDF$Content == ""] <- NA
TrainingUnigramDF <- TrainingUnigramDF[complete.cases(TrainingUnigramDF),]
TrainingUnigramDF <- TrainingUnigramDF[with(TrainingUnigramDF, order(Content)),]
save(TrainingUnigramDF, file = "TrainingUnigramDF.RData")

## Make new Unigram variable
NumberWordsTrainingUnigramDF <- aggregate(TrainingUnigramDF["Frequency"],
                                          by = TrainingUnigramDF["Content"],
                                          FUN = sum)
save(NumberWordsTrainingUnigramDF, file = "NumberWordsTrainingUnigramDF.RData")

# Make new dataframe for merge activity
NewTrainingUnigramDF <- data.frame(NumberWordsTrainingUnigramDF$Content, NumberWordsTrainingUnigramDF$Frequency)
names(NewTrainingUnigramDF) <- c("First", "Unigram")
save(NewTrainingUnigramDF, file = "NewTrainingUnigramDF.RData")

## Add unigram count for words that begin bigrams
NewAfterTrainingBigramDF <- merge (x = NumberWordsAfterTrainingBigramDF,
                                           y = NewTrainingUnigramDF,
                                           by = "First")

names(NewAfterTrainingBigramDF) <- c("First", "Bigram", "Unigram")

## Initialize d
d <- 0.88

## Calculate lambdas
NewAfterTrainingBigramDF$Lambda <- d * NewAfterTrainingBigramDF$Bigram / NewAfterTrainingBigramDF$Unigram
NewAfterTrainingBigramDF <- NewAfterTrainingBigramDF[ ,-(2:3)]
save(NewAfterTrainingBigramDF, file = "NewAfterTrainingBigramDF.RData")

## CALCULATE KNESER-NEY PROBABILITIES
if(!exists("PKNTime")){
  tic()
  TrainingPKN <- TrainingBigramDF
  TrainingPKN <- merge(x = TrainingPKN, y = NewAfterTrainingBigramDF, by = "First")
  names(TrainingPKN) <- c("First", "Content", "Bigram Frequency", "Second", "Lambda")
  TrainingPKN <- merge(x = TrainingPKN, y = NumberWordsBeforeTrainingBigramDF, by = "Second")
  TrainingPKN <- merge(x = TrainingPKN, y = NewTrainingUnigramDF, by = "First")
  names(TrainingPKN) <- c("First", "Second", "Content", "Bigram Frequency", "Lambda", "ContProb", "Unigram Frequency")
  TrainingPKN$MinMax <- TrainingPKN$`Bigram Frequency` - d
  TrainingPKN$MinMax <- TrainingPKN$MinMax / TrainingPKN$`Unigram Frequency`
  TrainingPKN$PKN <- TrainingPKN$MinMax + (TrainingPKN$Lambda * TrainingPKN$ContProb)
  save(TrainingPKN, file = "TrainingPKN.RData")
  ExecTime <- toc()
  PKNTime <- ExecTime$toc - ExecTime$tic
  TotalTime <- TotalTime + PKNTime
}

## CALCULATE PROBABILITIES FOR VALIDATION SET

## Remove numbers from alpha-numeric content
ValidationBigramDF$Content <- gsub("[0-9]+", " ",ValidationBigramDF$Content)

## Remove all rows in Unigram where content is either "@" or "#"
ValidationUnigramDF <- ValidationUnigramDF[!ValidationUnigramDF$Content == "#",]
ValidationUnigramDF <- ValidationUnigramDF[!ValidationUnigramDF$Content == "@",]

## *** SEPARATE BIGRAMS AND TRIGRAMS INTO SEPARATE WORDS ***

## Separate bigrams into separate words by removing concatenator "_", sorted by second word
## Calculate the # of words occurring before each word w completing a bigram
if(!exists("ValSeparateBigramTime")) {
  tic()
  
  ValidationBigramDF$First <- sapply(strsplit(ValidationBigramDF$Content, "\\_"),"[",1) # Strip first word
  ValidationBigramDF$First <- tolower(ValidationBigramDF$First) # Make first word lower case
  ValidationBigramDF <- ValidationBigramDF[!ValidationBigramDF$First == "#",]
  ValidationBigramDF <- ValidationBigramDF[!ValidationBigramDF$First == "@",]
  
  ValidationBigramDF$Second <- sapply(strsplit(ValidationBigramDF$Content, "\\_"), "[", 2) # Strip second word
  ValidationBigramDF$Second <- tolower(ValidationBigramDF$Second) # Make second word lower case
  ValidationBigramDF <- ValidationBigramDF[!ValidationBigramDF$Second == "#",]
  ValidationBigramDF <- ValidationBigramDF[!ValidationBigramDF$Second == "@",]
  
  ValidationBigramDF$Content <- iconv(ValidationBigramDF$Content, "latin1", "ASCII", sub = "") # Remove foreign characters
  ValidationBigramDF$First <- iconv(ValidationBigramDF$First, "latin1", "ASCII", sub = "")
  ValidationBigramDF$Second <- iconv(ValidationBigramDF$Second, "latin1", "ASCII", sub = "")
  
  ValidationBigramDF[ValidationBigramDF == ""] <- NA # Remove blanks
  ValidationBigramDF <- ValidationBigramDF[complete.cases(ValidationBigramDF),] # Remove NAs
  
  ValidationBigramDF <- ValidationBigramDF[with(ValidationBigramDF, order(Second, First)),] # NEW Sort alphabetically
  
  NumberWordsBeforeValidationBigramDF <- aggregate(ValidationBigramDF["Content"], by = ValidationBigramDF[c("First","Second")], FUN = length) # Aggregate by number for number of unique bigrams with common second word
  NumberWordsBeforeValidationBigramDF <- aggregate(NumberWordsBeforeValidationBigramDF["First"], by = NumberWordsBeforeValidationBigramDF["Second"], FUN = length) # Aggregate by number for number of bigrams by second word
  names(NumberWordsBeforeValidationBigramDF) <- c("Second", "Before")
  
  save(ValidationBigramDF, file = "ValidationBigramDF.RData")
  save(NumberWordsBeforeValidationBigramDF, file = "NumberWordsBeforeValidationBigramDF.RData")
  ExecTime <- toc()
  ValSeparateBigramTime <- ExecTime$toc - ExecTime$tic
  TotalTime <- TotalTime + ValSeparateBigramTime
}

## Separate trigrams into 3 columns, removing concatentor "_"
## Calculate the number of words occurring before each word w completing a trigram
if(!exists("ValSeparateTrigramTime")) {
  tic()
  
  ValidationTrigramDF$First <- sapply(strsplit(ValidationTrigramDF$Content, "\\_"),"[",1)
  ValidationTrigramDF$First <- tolower(ValidationTrigramDF$First)
  ValidationTrigramDF <- ValidationTrigramDF[!ValidationTrigramDF$First == "#",]
  ValidationTrigramDF <- ValidationTrigramDF[!ValidationTrigramDF$First == "@",]
  
  ValidationTrigramDF$Second <- sapply(strsplit(ValidationTrigramDF$Content, "\\_"), "[", 2)
  ValidationTrigramDF$Second <- tolower(ValidationTrigramDF$Second)
  ValidationTrigramDF <- ValidationTrigramDF[!ValidationTrigramDF$Second == "#",]
  ValidationTrigramDF <- ValidationTrigramDF[!ValidationTrigramDF$Second == "@",]
  
  ValidationTrigramDF$Third <- sapply(strsplit(ValidationTrigramDF$Content, "\\_"), "[", 3)
  ValidationTrigramDF$Third <- tolower(ValidationTrigramDF$Third)
  ValidationTrigramDF <- ValidationTrigramDF[!ValidationTrigramDF$Third == "#",]
  ValidationTrigramDF <- ValidationTrigramDF[!ValidationTrigramDF$Third == "@",]
  
  ValidationTrigramDF$Content <- iconv(ValidationTrigramDF$Content, "latin1", "ASCII", sub = "") # Remove foreign characters
  ValidationTrigramDF$First <- iconv(ValidationTrigramDF$First, "latin1", "ASCII", sub = "")
  ValidationTrigramDF$Second <- iconv(ValidationTrigramDF$Second, "latin1", "ASCII", sub = "")
  ValidationTrigramDF$Third <- iconv(ValidationTrigramDF$Third, "latin1", "ASCII", sub = "")
  
  ValidationTrigramDF[ValidationTrigramDF == ""] <- NA # Remove blanks
  ValidationTrigramDF <- ValidationTrigramDF[complete.cases(ValidationTrigramDF),] # Remove NAs
  
  ValidationTrigramDF <- ValidationTrigramDF[with(ValidationTrigramDF, order(Third, Second, First)),] # NEW
  ValidationTrigramDF <- ValidationTrigramDF[with(ValidationTrigramDF, order(Third, Second, First)),]
  
  save(ValidationTrigramDF, file = "ValidationTrigramDF.RData")
  ExecTime <- toc()
  ValSeparateTrigramTime <- ExecTime$toc - ExecTime$tic
  TotalTime <- TotalTime + ValSeparateTrigramTime
}

## Sort Validation bigram
ValidationBigramDF <- ValidationBigramDF[with(ValidationBigramDF, order(First,Second)),] # Re-order bigrams by first word

## Move the unigram dataframe to all lower case
tolower(ValidationUnigramDF$Content)

## Clean up Unigram
ValidationUnigramDF$Content <- iconv(ValidationUnigramDF$Content, "latin1", "ASCII", sub = "")

## Get rid of blank content in Unigram
ValidationUnigramDF$Content[ValidationUnigramDF$Content == ""] <- NA
ValidationUnigramDF <- ValidationUnigramDF[complete.cases(ValidationUnigramDF),]
ValidationUnigramDF <- ValidationUnigramDF[with(ValidationUnigramDF, order(Content)),]
save(ValidationUnigramDF, file = "ValidationUnigramDF.RData")

## Make new Unigram variable
NumberWordsValidationUnigramDF <- aggregate(ValidationUnigramDF["Frequency"],
                                          by = ValidationUnigramDF["Content"],
                                          FUN = sum)
save(NumberWordsValidationUnigramDF, file = "NumberWordsValidationUnigramDF.RData")

# Make new dataframe for merge activity
if(!exists("NewValidationUnigramDF")) {
  tic()
  
  NewValidationUnigramDF <- data.frame(NumberWordsValidationUnigramDF$Content, NumberWordsValidationUnigramDF$Frequency)
  names(NewValidationUnigramDF) <- c("First", "Unigram")
  save(NewValidationUnigramDF, file = "NewValidationUnigramDF.RData")
  ExecTime <- toc()
  NewValUniTime <- ExecTime$toc - ExecTime$tic
  TotalTime <- TotalTime + NewValUniTime
}

## Calculate KN probabilities for validation bigrams that exist in the training set
CommonValidationPKN <- merge(x = TrainingPKN, y = ValidationBigramDF, by = c("First", "Second"))
CommonValidationPKN <- CommonValidationPKN[c(-4,-7,-8,-10,-11)]
CommonValidationPKN <- CommonValidationPKN[c(3,1,2,4,5,6)]
names(CommonValidationPKN) <- c("Content", "First", "Second", "Lambda", "ContProb", "PKN")

## Isolate validation bigrams not in the training set
if(!exists("ValidationResidual")) {
  tic()
  
  ValidationCheck <- CommonValidationPKN
  
  ValidationResidual <- setdiff(ValidationBigramDF$Content, ValidationCheck$Content)
  ValidationResidual <- as.data.frame(ValidationResidual, stringsAsFactors = FALSE)
  names(ValidationResidual) <- "Content"
  ValidationResidual <- merge(x = ValidationResidual, y = ValidationBigramDF, by = "Content")
  ValidationResidual <- ValidationResidual[-2]
  save(ValidationResidual, file = "ValidationResidual.RData")
  ExecTime <- toc()
  ValResidualTime <- ExecTime$toc - ExecTime$tic
  TotalTime <- TotalTime + ValResidualTime
}

## Calculate ValidationResidual lambdas for those first words existing in Training set
if(!exists("ValidationLambdaTrue")) {
  tic()
  ValidationLambdaCheck <- ValidationResidual$First
  ValidationLambdaTrue <- ValidationLambdaCheck[ValidationLambdaCheck %in% NewAfterTrainingBigramDF$First]
  ValidationLambdaTrue <- as.data.frame(ValidationLambdaTrue, stringsAsFactors = FALSE)
  names(ValidationLambdaTrue) <- "First"
  ValidationLambdaTrue <- merge(x = ValidationLambdaTrue, y = NewAfterTrainingBigramDF, by = "First")
  ValidationLambdaTrue <- ValidationLambdaTrue[!duplicated(ValidationLambdaTrue),]
  save(ValidationLambdaTrue, file = "ValidationLambdaTrue.RData")
  ExecTime <- toc()
  TrueLambdaTime <- ExecTime$toc - ExecTime$tic
  TotalTime <- TotalTime + TrueLambdaTime
}

## Calculate ValidationResidual lambdas for first words not existing in Training set
if(!exists("ValidationResidualLambda")) {
  tic()
  ValidationLambdaFalse <- ValidationLambdaCheck[- which(ValidationLambdaCheck %in% ValidationLambdaTrue$First)]
  ValidationLambdaFalse <- as.data.frame(ValidationLambdaFalse, stringsAsFactors = FALSE)
  names(ValidationLambdaFalse) <- "First"
  ValidationLambdaFalseZero <- as.vector(matrix(2.422684e-02, nrow = length(ValidationLambdaFalse$First)))
  ValidationLambdaFalse <- cbind(ValidationLambdaFalse, ValidationLambdaFalseZero)
  names(ValidationLambdaFalse) <- c("First", "Lambda")
  ValidationResidualLambda <- rbind.data.frame(ValidationLambdaTrue, ValidationLambdaFalse)
  ValidationResidualLambda <- ValidationResidualLambda[!duplicated(ValidationResidualLambda),]
  save(ValidationResidualLambda, file = "ValidationResidualLambda.RData")
  ExecTime <- toc()
  ValLambdaTime <- ExecTime$toc - ExecTime$tic
  TotalTime <- TotalTime + ValLambdaTime
}

## Calculate the continuation probabilities for second words existing in the training set
if(!exists("ValidationContTrue")){
  tic()
  ValidationContCheck <- ValidationResidual$Second
  ValidationContTrue <- ValidationContCheck[ValidationContCheck %in% NumberWordsBeforeTrainingBigramDF$Second]
  ValidationContTrue <- as.data.frame(ValidationContTrue, stringsAsFactors = FALSE)
  names(ValidationContTrue) <- "Second"
  ValidationContTrue <- merge(x = ValidationContTrue, y = NumberWordsBeforeTrainingBigramDF, by = "Second")
  ValidationContTrue <- ValidationContTrue[!duplicated(ValidationContTrue),]
  save(ValidationContTrue, file = "ValidationContTrue.RData")
  ExecTime <- toc()
  ContTrueTime <- ExecTime$toc - ExecTime$tic
  TotalTime <- TotalTime + ContTrueTime
}

## Calculate the continuation probabilities for second words not in the training set
if(!exists("ValidationResidualContProb")){
  tic()
  ValidationContFalse <- ValidationContCheck[- which(ValidationContCheck %in% ValidationContTrue$Second)]
  ValidationContFalse <- as.data.frame(ValidationContFalse, stringsAsFactors = FALSE)
  names(ValidationContFalse) <- "Second"
  ValidationContFalseZero <- as.vector(matrix(4.915e-06, nrow = length(ValidationContFalse$Second)))
  ValidationContFalse <- cbind(ValidationContFalse, ValidationContFalseZero)
  names(ValidationContFalse) <- c("Second", "ContProb")
  ValidationContFalse <- ValidationContFalse[!duplicated(ValidationContFalse),]
  save(ValidationContFalse, file = "ValidationContFalse.RData")
  
  ## Make continuation probabilities for all second words in Validation set
  ValidationResidualContProb <- rbind.data.frame(ValidationContTrue, ValidationContFalse)
  ValidationResidualContProb <- ValidationResidualContProb[!duplicated(ValidationResidualContProb),]
  save(ValidationResidualContProb, file = "ValidationResidualContProb.RData")
  ExecTime <- toc()
  ValContFalseTime <- ExecTime$toc - ExecTime$tic
  TotalTime <- TotalTime + ValContFalseTime
}

## Calculate KN probabilities for bigrams not in the training set
if(!exists("ValidationResidualPKN")){
  tic()
  ValidationResidualPKN <- merge(x = ValidationResidual, y = ValidationResidualLambda, by = "First")
  ValidationResidualPKN <- merge(x = ValidationResidualPKN, y = ValidationResidualContProb, by = "Second")
  ValidationResidualPKN <- ValidationResidualPKN[c("Content","First","Second","Lambda","ContProb")]
  ValidationPKNProduct <- ValidationResidualPKN$Lambda * ValidationResidualPKN$ContProb
  ValidationResidualPKN <- cbind(ValidationResidualPKN, ValidationPKNProduct)
  ValidationResidualPKN <- ValidationResidualPKN[with(ValidationResidualPKN, order(First, Second)),]
  names(ValidationResidualPKN) <- c("Content","First","Second","Lambda","ContProb", "PKN")
  save(ValidationResidualPKN, file = "ValidationResidualPKN.RData")
  ExecTime <- toc()
  ValidationPKNTime <- ExecTime$toc - ExecTime$tic
  TotalTime <- TotalTime + ValidationPKNTime
}

## Make full PKN matrix for Validation set
if(!exists("ValidationPKN")){
  tic()
  ValidationPKN <- rbind(CommonValidationPKN, ValidationResidualPKN)
  save(ValidationPKN, file = "ValidationPKN.RData")
  ExecTime <- toc()
  ValPKNTime <- ExecTime$toc - ExecTime$tic
  TotalTime <- TotalTime + ValPKNTime
}

## CALCULATE PERPLEXITY FOR VALIDATION SET, OPTIMIZING FOR d
ValidationPKN$LogPKN <- -log2(ValidationPKN$PKN)
ValidationEntropy <- sum(ValidationPKN$LogPKN) / length(ValidationPKN$LogPKN)
ValidationPerplexity <- 2^ValidationEntropy

## MAKE BACKOFF MODEL STARTING WITH TRIGRAM, THEN USING KNESER-NEY

## Make TrainingTrigramDF Check vector to check against
TrainingTrigramDF$Check <- paste(TrainingTrigramDF$First, TrainingTrigramDF$Second, sep = "_")

Sentence <- "Research into effects of poverty"

## Remove punctuation
punct <- '[]\\?!\"\'#$%&(){}+*/:;,._`|~\\[<=>@\\^-]'
Sentence <- gsub(punct, "", Sentence)

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

## Check to see if the bigram from the sentence exists in any trigrams in the Training set
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
