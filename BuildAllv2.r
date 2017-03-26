library(ggplot2)
library(dplyr)
library(tidyr)
library(ngram)
library(data.table)
setwd("~/Documents/Capstone/final/en_US/")

set.seed(1)
ListOfFileNames <- list.files(pattern = "en_US")
corpus <- list()
for (i in 1:length(ListOfFileNames)){ 
    con  <- file(ListOfFileNames[i], open = "r")
    corpus[[i]] <- readLines(con)
    close(con)
} 
corpus.length <- sapply(corpus, length)
corpus.train.index <- sapply(corpus.length, rbinom, size = 1, prob = 0.1)
corpus.train.length <- sapply(corpus.train.index, sum)

corpus.test.index <- sapply(corpus.length - corpus.train.length, rbinom, size = 1, prob = 0.01)
corpus.test.length <- sapply(corpus.test.index, sum)

str <- unlist(corpus)
corpus.train.index <- unlist(corpus.train.index)
corpus.test.index  <- unlist(corpus.test.index)

train <- paste(str[corpus.train.index == 1], collapse = ' ')
remainStr <- str[corpus.train.index == 0]
test <- paste(remainStr[corpus.test.index == 1], collapse = ' ')

train   <- preprocess(train, case = "lower", remove.punct = TRUE,
                      remove.numbers = TRUE, fix.spacing = TRUE)
write(train, "training.txt")

test   <- preprocess(test, case = "lower", remove.punct = TRUE,
                      remove.numbers = TRUE, fix.spacing = TRUE)
write(test, "test.txt")


cutoff <- function(freqarray, cut){
    i <- 1
    while(freqarray[i] > cut){
        i <- i + 1
    }
    i
}


train.ng4   <- ngram(train, n = 4)
train.ng3   <- ngram(train, n = 3)
train.ng2   <- ngram(train, n = 2)
train.ng1   <- ngram(train, n = 1)
phrases4 <- get.phrasetable(train.ng4)
phrases3 <- get.phrasetable(train.ng3)
phrases2 <- get.phrasetable(train.ng2)
phrases1 <- get.phrasetable(train.ng1)

freq.cutoff4 <- cutoff(phrases4$freq, 1)
freq.cutoff3 <- cutoff(phrases3$freq, 1)
freq.cutoff2 <- cutoff(phrases2$freq, 1)
freq.cutoff1 <- cutoff(phrases1$freq, 1)

phrases4 <- phrases4[1:freq.cutoff4, 1:2]
phrases3 <- phrases3[1:freq.cutoff3, 1:2]
phrases2 <- phrases2[1:freq.cutoff2, 1:2]
phrases1 <- phrases1[1:freq.cutoff1, 1:2]

write.csv(phrases1, "ngram1.csv", row.names = FALSE)
write.csv(phrases2, "ngram2.csv", row.names = FALSE)
write.csv(phrases3, "ngram3.csv", row.names = FALSE)
write.csv(phrases4, "ngram4.csv", row.names = FALSE)
write(c(freq.cutoff4, 
        freq.cutoff3,
        freq.cutoff2,
        freq.cutoff1), "cutoff.txt")


phrases4 <- phrases4[1:cutoff(phrases4$freq, 2), 1:2]
phrases3 <- phrases3[1:cutoff(phrases3$freq, 2), 1:2]
phrases2 <- phrases2[1:cutoff(phrases2$freq, 2), 1:2]
phrases1 <- phrases1[1:cutoff(phrases1$freq, 2), 1:2]



split.n.gram <- function(ng){
    temp <- matrix(data = NA, nrow = 0, ncol = 2)
    for(i in 1:length(ng)){
        temp <- rbind(temp, 
                      strsplit(trimws(ng[i], which = "both"), ' (?=[^ ]+$)', perl=TRUE)[[1]])
    }
    temp
}


n1 <- trimws(phrases1$ngrams, which = "both")
voc <- length(n1)

DT <- data.table(split.n.gram(phrases2$ngrams))
n2 <- DT[!duplicated(DT$V1),]

DT <- data.table(split.n.gram(phrases3$ngrams))
n3 <- DT[!duplicated(DT$V1),]

DT <- data.table(split.n.gram(phrases4$ngrams))
n4 <- DT[!duplicated(DT$V1),]


write.csv(n1, "nh1.csv")
write.csv(n2, "nh2.csv")
write.csv(n3, "nh3.csv")
write.csv(n4, "nh4.csv")

n1 <- data.table(fread("nh1.csv"))[,2][[1]]
n2 <- data.table(fread("nh2.csv"))[,2:3]
n3 <- data.table(fread("nh3.csv"))[,2:3]
n4 <- data.table(fread("nh4.csv"))[,2:3]

voc <- length(n1)
hash <- function(str){
    words <- strsplit(str, " ")[[1]]
    temp <- rep(NA, length(words))
    for(i in 1:length(temp)){
        t <- which(n1 == words[i])
        if(length(t) == 0){
            return(0)
        }
        temp[i] <- t
    }
    Hindex <- 0
    for(i in length(temp):1){
        Hindex <- Hindex + temp[i]*voc^(i-1)
    }
    Hindex
}

hashV <- function(strv){
    ind <- rep(NA, length(strv))
    for(i in 1:length(ind)){
        ind[i] <- hash(strv[i])
    }
    ind
}

SwiftKey <- function(str){
    words <- strsplit(str, " ")[[1]]
    if(length(words) < 3){
        if(length(words) == 0){
            return(n1[1])
        }else if(length(words) == 1){
            n3words <- c(" ", " ", words)
            method <- 2
        }else{
            n3words <- c(" ", words)
            method <- 3
        }
    }else{
        n3words <- tail(words, 3)
        h3 <- hashV(n3words)
        if(h3[3] == 0){
            return(n1[1])
        }else if(h3[2] == 0){
            method <-  2
        }else if(h3[1] == 0){
            method = 3
        }else{
            method = 4
        }
    }
    
    
    if(method == 4){
        temp <- paste(n3words, collapse = " ")
        temp <- trimws(temp, which = "both")
        t <- which(n4$V1 == temp)
        if(length(t) == 0){
            method = 3
        }else{
            return(n4$V2[t])
        }
    }
    
    if(method == 3){
        temp <- paste(n3words[2:3], collapse = " ")
        temp <- trimws(temp, which = "both")
        t <- which(n3$V1 == temp)
        if(length(t) == 0){
            method = 2
        }else{
            return(n3$V2[t])
        }
    }
    
    if(method == 2){
        temp <- paste(n3words[3], collapse = " ")
        temp <- trimws(temp, which = "both")
        t <- which(n2$V1 == temp)
        if(length(t) == 0){
            return(n1[1])
        }else{
            return(n2$V2[t])
        }
    }
}

con  <- file("test.txt", open = "r")
test <- readLines(con)
close(con)

test.ng4   <- ngram(test, n = 4)
test.phrases <- get.phrasetable(test.ng4)
test.phrases.highf <- test.phrases[1:cutoff(test.phrases$freq, 1), ]
test.phrases.lowf <- test.phrases[-c(1:cutoff(test.phrases$freq, 1)),]
lowf.sample.index <- rbinom(nrow(test.phrases.lowf), 1, 0.05)
test.phrases.lowf.sample <- test.phrases.lowf[lowf.sample.index == 1, ]


temp <- split.n.gram(test.phrases.highf$ngrams)
predictRight <- 0
for(i in 1:nrow(temp)){
    if(SwiftKey(temp[i,1]) == temp[i, 2]){
        predictRight <- predictRight + test.phrases.highf$freq[i]
    }
}
SwiftKeyaccuracy.high <- predictRight/sum(test.phrases.highf$freq)


temp2 <- split.n.gram(test.phrases.lowf.sample$ngrams)
predictRight.low <- 0
for(i in 1:nrow(temp2)){
    if(SwiftKey(temp2[i,1]) == temp2[i, 2]){
        predictRight.low <- predictRight.low + test.phrases.lowf.sample$freq[i]
        print(i)
    }
}
SwiftKeyaccuracy.low <- predictRight.low/sum(test.phrases.lowf.sample$freq)
Accuracy.extr <- (predictRight + predictRight.low * 20) / sum(test.phrases$freq)
