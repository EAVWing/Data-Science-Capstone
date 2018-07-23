setwd("C:/Users/Steve/Data-Science-Toolbox/Capstone")

library(tm)
library(LaF)
library(RWeka)
library(NLP)
bigList <- data.frame(ngram=as.factor(""), frequency=as.numeric(""))
bigListStopped <- data.frame(ngram=as.factor(""), frequency=as.numeric(""))
sampleSize = 5000

for(i in c(1:2)){

        set.seed(Sys.time())
        ## Make the samples for the unStopped files
        writeLines(sample_lines("./clean corpus/1.txt", sampleSize, determine_nlines("./clean corpus/1.txt")), "1Sample.txt")
        file.rename(from="./1Sample.txt", to="./clean corpus/Samples/1Sample.txt")
        writeLines(sample_lines("./clean corpus/2.txt", sampleSize, determine_nlines("./clean corpus/2.txt")), "2Sample.txt")
        file.rename(from="./2Sample.txt", to="./clean corpus/Samples/2Sample.txt")
        writeLines(sample_lines("./clean corpus/3.txt", sampleSize, determine_nlines("./clean corpus/3.txt")), "3Sample.txt")
        file.rename(from="./3Sample.txt", to="./clean corpus/Samples/3Sample.txt")
        
        ## Make the samples for the Stopped files
        writeLines(sample_lines("./clean corpus stopped/1s.txt", sampleSize, determine_nlines("./clean corpus stopped/1s.txt")), "1sSample.txt")
        file.rename(from="./1sSample.txt", to="./clean corpus stopped/Samples/1sSample.txt")
        writeLines(sample_lines("./clean corpus stopped/2s.txt", sampleSize, determine_nlines("./clean corpus stopped/2s.txt")), "2sSample.txt")
        file.rename(from="./2sSample.txt", to="./clean corpus stopped/Samples/2sSample.txt")
        writeLines(sample_lines("./clean corpus stopped/3s.txt", sampleSize, determine_nlines("./clean corpus stopped/3s.txt")), "3sSample.txt")
        file.rename(from="./3sSample.txt", to="./clean corpus stopped/Samples/3sSample.txt")
        
        ## Create the corpus
        corp <- Corpus(DirSource("./clean corpus/Samples/"))
        corpStopped <- Corpus(DirSource("./clean corpus stopped/Samples/"))
        
        ## Create the TDMs
        toke <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 8))
        tdm <- TermDocumentMatrix(corp, control = list(tokenize = toke))
        tdmS <- TermDocumentMatrix(corpStopped, control = list(tokenize = toke))
        
        ## Pull out any terms that occur at least twice
        freqTerms <- findFreqTerms(tdm, lowfreq = 2)
        m <- rowSums(as.matrix(tdm[freqTerms,]))
        data <- data.frame(ngram = as.factor(names(m)),frequency = as.numeric(m))
        data <- data[order(-data$frequency),]
        
        freqTermsS <- findFreqTerms(tdmS, lowfreq = 2)
        mS <- rowSums(as.matrix(tdmS[freqTermsS,]))
        dataS <- data.frame(ngram = as.factor(names(mS)),frequency = as.numeric(mS))
        dataS <- dataS[order(-dataS$frequency),]
        
        ## Bind onto the big list
        bigList <- rbind(bigList, data)
        bigListStopped <- rbind(bigListStopped, dataS)
        
        ## tapply over the big list to sum frequencies
        bigTap <- tapply(bigList$frequency, bigList$ngram, sum)
        bigList <- data.frame(ngram=as.factor(names(bigTap)), frequency=as.numeric(bigTap))
        bigTapS <- tapply(bigListStopped$frequency, bigListStopped$ngram, sum)
        bigListStopped <- data.frame(ngram=as.factor(names(bigTapS)), frequency=as.numeric(bigTapS))
}