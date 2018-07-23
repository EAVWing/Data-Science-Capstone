setwd("C:/Users/Steve/Data-Science-Toolbox/Capstone")
load("./bigram.Rda")
load("./bigram_Stopped.Rda")
load("./trigram.Rda")
load("./trigram_Stopped.Rda")
load("./fourgram.Rda")
load("./fourgram_Stopped.Rda")
load("./unigram.Rda")

library(stringr)
library(dplyr)
library(NLP)
library(tm)

sentence <- "Go on a romantic date at the"

nextword <- function(sentence){
        ngrams <- data.frame(frequency="", start="", last="")
        sen <- tolower(sentence)
        sen = removePunctuation(sen)
        sen = stripWhitespace(sen)
        sen = removeNumbers(sen)
        sen = str_trim(sen)
        sen = unlist(strsplit(sen,' '))
        senS = removeWords(sen, stopwords("en"))
        senS = senS[senS != ""]
        
        for(i in 2:0){
                gS = senS[(length(senS)-i):length(senS)]
                gS = paste(gS,collapse = ' ')
                ngrams = rbind(ngrams, bigList[which(bigList$start == gS)])
                g = sen[(length(sen)-i):length(sen)]
                g = paste(g,collapse = ' ')
                ngrams = rbind(ngrams, bigList[which(bigList$start == g)])
        }
        head(ngrams, 20)
}

nextword(sentence)        