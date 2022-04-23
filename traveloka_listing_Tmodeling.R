# INSTAL PACKAGE
install.packages("topicmodels")
install.packages("corpus")
install.packages("pdftools")
install.packages("quanteda")
#install.packages("xxx")

# LOAD PACKAGES
#library(gutenberg)
library(readr)
library(dplyr)
library(tm)
library(topicmodels)
library(tidytext)
library(corpus)
library(pdftools)
library(tidyverse)
library(NLP)
library(quanteda)

##### Title: TOPIC MODELING tidy data principles ######
# LOAD DATA
travelokabdg <- read.csv(file.choose())

# REMOVE MISSING VALUE ROW AT THE COLUMN
#print(travelokabdg)
#complete.cases(travelokabdg)
#!complete.cases(travelokabdg)
#travelokabdg[complete.cases(travelokabdg), ]
#travelokabdg[!complete.cases(travelokabdg), ]
#travelokabdg <- travelokabdg[complete.cases(travelokabdg), ]
#print(travelokabdg)

# CHECK STRUCTURE
str(travelokabdg)
View(travelokabdg)

# CREATE CORPUS DOC
travelokacorpus <- iconv(travelokabdg$textreviewuser)

corpustb <- Corpus(VectorSource(travelokacorpus))
corpustb <- tm_map(corpustb, content_transformer(tolower))
corpustb <- tm_map(corpustb, removeNumbers)
corpustb <- tm_map(corpustb, removeWords, stopwords("english"))
corpustb <- tm_map(corpustb, removePunctuation, preserve_intra_word_dashes = T)
corpustb <- tm_map(corpustb, stripWhitespace)
corpustb <- tm_map(corpustb, removeWords, c( "n/a"))

# CREATE MATRIX via function
TDM <- TermDocumentMatrix(corpustb)
TDM <- as.matrix(TDM)
TDM [1:10,1:5]

# CREATE MATRIX via function DTM
#DTM <- DocumentTermMatrix(corpustb)
#DTM <- as.matrix((DTM))

# CREATING MODELS
LDA_Mod <- LDA (DTM, k = 4, control = list(seed = 1234))
LDA_Mod

# CREATE MODEL WDTM# CREATE MODEL WITH 6 TOPICS
beta_mod <- tidy(LDA_Mod, matrix = "beta")
beta_mod

# GROUPING THE TERMS BY TOPIC
beta_top_terms <- beta_mod %>% 
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

# DISPLAY THE GROUP TERMS ON THE CHARTS
beta_top_terms %>% 
  mutate (term = reorder_within(term, beta, topic)) %>%
  ggplot (aes(beta, term, fill = factor(topic))) +
            geom_col(show.legend = F) + 
            facet_wrap(~ topic, scales = "free") +
            scale_y_reordered()

# FILTER TERMS BY TOPICS
#tidy(TDM) %>% 
 # filter(corpustb == 3) %>%
 # arrange(desc(count))

# EXAMINING PERDOCUMENT PERTOPIC PROBABILITY
#gamma_mod <- tidy(LDA_Mod, matrix = "gamma")
#gamma_mod 

# GROUPING THE TERMS BY TOPIC
#gamma_top_terms <- gamma_mod %>% 
  #group_by(topic) %>%
  #slice_max(gamma, n = 10) %>% 
  #ungroup() %>%
  #arrange(topic, -gamma)

# DISPLAY THE GROUP TERMS ON THE CHARTS
# gamma_top_terms %>% 
  #mutate (term = reorder_within(term, gamma, topic)) %>%
  #ggplot (aes(gamma, term, fill = factor(topic))) +
  #geom_col(show.legend = F) + 
  #facet_wrap(~ topic, scales = "free") +
  #scale_y_reordered()

#--------------------------------------------------------------------------#

##### Title: TOPIC MODELING fitting models in R #####
# LOAD DATA

