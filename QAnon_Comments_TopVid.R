
.libPaths("C:/Users/taninecz/Documents/R/win-library/3.5")
setwd("C:/Users/taninecz/Desktop/DI 2019/")

library(quanteda)
#devtools::install_github("kbenoit/quanteda.dictionaries") 
library(quanteda.dictionaries)
library(stringi)
library(readtext)
#setwd("C:/Users/dell/R/win-library/3.4/readtext/extdata/txt/")
#this wont work anymore bc it was deleted in update.
library(magrittr)
library(dplyr)
library("hexbin") 
library(topicmodels)
library(stm)
library(lda)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(tidytext)
library(broom)
library(stringr)
library(tcltk)
library(progress)
library(lasso2)
library(ldatuning)
library(lattice)
library(tm)
library(githubinstall)
library(stminsights)
library(lda)
library(doParallel)
library(devtools)
library(readr) #need for time formatting

QAnon_Comments <- read.csv("QAnon YouTube Full Table - Sheet1.csv", stringsAsFactors = FALSE)

QAnon_Comments$Time <- as.Date(QAnon_Comments$publishedAt)

QCorpus <- corpus(QAnon_Comments)


#find topic numbers
usableText <- str_replace_all(QAnon_Comments$text, "[^[:graph:]]", " ") #removes non graphical chars
QAnon_Comments$text <- usableText       #add usableText back into comments
QCorpus <- corpus(QAnon_Comments)
FindK_pre_corpus <- stri_trans_general(QCorpus, "UTF-8", "ASCII")
FindK_Corpus <- corpus(FindK_pre_corpus, metacorpus = QAnon_Comments)
FindK_Corpus <- tm_map(FindK_Corpus, PlainTextDocument)
processed_texts <- textProcessor(FindK_Corpus$metadata$text, 
                                 metadata = FindK_Corpus$metadata,
                                 onlycharacter = TRUE) #need doc/vocab/metadata
prepped_text_data <- prepDocuments(processed_texts$documents, 
                                   processed_texts$vocab, 
                                   processed_texts$meta)
docs <- processed_texts$documents
vocab <- processed_texts$vocab
meta <- processed_texts$meta                      
findingk <- searchK(docs, vocab, K = c(12:28), init.type = "Spectral",  #~21
                    data = meta, verbose=TRUE)
plot(findingk) 


#get document frequency matrix and run models
Q_DFM <- dfm(QCorpus, 
                 remove = c("^.{0,3}$",#could also remove 3 letter words? orig: "^.{0,3}$"
                            "https://", 
                            "https:â",
                            "httpsâ",
                            "t.coâ",
                            "t.câ",
                            "httpâ",
                            "httâ",
                            "t.co",
                            "https",
                            "http","&amp","&lt","&gt","RT","rt","t.co",
                            "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https",
                            stopwords("english")), 
                 tolower = TRUE,
                 remove_punct=TRUE,
                 remove_numbers=TRUE,
                 remove_symbols=TRUE,
                 stem = FALSE)

Q_DFM_Sparse <-  Q_DFM %>% 
  dfm_select(min_nchar = 3)

Q_DFM_Sparse <- dfm_trim(Q_DFM_Sparse, min_termfreq = 4,
                             termfreq_type = "count", verbose = T)

Q_DFM_Sparse_2stm <- convert(Q_DFM_Sparse, to = "stm")

Sparse_STM <- stm(Q_DFM_Sparse, K = 0,#56, 
                     verbose = TRUE, init.type = "Spectral")

Sparse_STM_20 <- stm(Q_DFM_Sparse, K = 20,  
                  verbose = TRUE, init.type = "Spectral")

plot.STM(Sparse_STM_20, type = c("labels"), labeltype = c("score"), width = 90, 
         text.cex = .7)
plot.STM(Sparse_STM_20, type = c("summary"), labeltype = c("score"),
         main="", 
         width = 10,
         text.cex = 1)



#database
NYPD.csv <- read.csv("NYPD_Motor_Vehicle_Collisions.csv", stringsAsFactors = FALSE)

parse_datetime(NYPD.csv$DATE, "%m/%d/%Y") #"%y"=2-digit years, You want "%Y". 

NYPD.csv$DateTime <- NYPD.csv$DATE
NYPD.csv$DateTime <- parse_datetime(NYPD.csv$DateTime, "%m/%d/%Y") #"%y"=2-digit years, You want "%Y". 
NYPD.csv$DateTime <- as.Date(NYPD.csv$DateTime) #dont really need after all

NYPD.csv$Year <- as.integer(substring(NYPD.csv$DateTime, 1, 4))
NYPD.csv$YearLogical <- grepl("2013|2014|2015|2016|2017|2018", #subset by year
                        NYPD.csv$Year)
NYPD_2013_2018 <- subset(NYPD.csv, `YearLogical`==TRUE) #new object w only 2013-18
NYPD_2013_2018$Incident <- rownames(NYPD_2013_2018) #make variable out of rowname
NYPD_2013_2018$Incident <- as.numeric(NYPD_2013_2018$Incident) 

#s.2013 <- sum(NYPD_2013_2018$Incident[NYPD_2013_2018$Year == 2013])
#s.2014 <- sum(NYPD_2013_2018$Incident[NYPD_2013_2018$Year == 2014])
#s.2015 <- sum(NYPD_2013_2018$Incident[NYPD_2013_2018$Year == 2015])
#s.2016 <- sum(NYPD_2013_2018$Incident[NYPD_2013_2018$Year == 2016])
#s.2017 <- sum(NYPD_2013_2018$Incident[NYPD_2013_2018$Year == 2017])
#s.2018 <- sum(NYPD_2013_2018$Incident[NYPD_2013_2018$Year == 2018])

regress1 <- lm(Year ~ Incident, data = NYPD_2013_2018)
summary(regress1) #-4.393e-06


NYPD.csv$Just17 <- grepl("2017", #subset by year
                              NYPD.csv$Year)
NYPD_2017 <- subset(NYPD.csv, `Just17`==TRUE)

NYPD_2017_ThreeCarOrMore <- subset(NYPD_2017, 
                  NYPD_2017$CONTRIBUTING.FACTOR.VEHICLE.1 != "" &
                  NYPD_2017$CONTRIBUTING.FACTOR.VEHICLE.2 != "" &
                  NYPD_2017$CONTRIBUTING.FACTOR.VEHICLE.3 != "")

NYPD_2017_ThreeCarOrMore$Jan <- grepl("2017-01",
                                      NYPD_2017_ThreeCarOrMore$DateTime)
NYPD_2017_ThreeCarOrMore$May <- grepl("2017-05",
                                      NYPD_2017_ThreeCarOrMore$DateTime)
JanAndMayTable <- table(NYPD_2017_ThreeCarOrMore$Jan, 
                        NYPD_2017_ThreeCarOrMore$May) 
chi2 <- chisq.test(JanAndMayTable)

#question 5
NYPD.csv$Just16 <- grepl("2016", #subset by year
                         NYPD.csv$Year)
NYPD_2016 <- subset(NYPD.csv, `Just16`==TRUE)


