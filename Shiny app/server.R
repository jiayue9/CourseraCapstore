library(shiny)

library(data.table)
n1 <- data.table(fread("Data/nh1.csv"))[,2][[1]]
n2 <- data.table(fread("Data/nh2.csv"))[,2:3]
n3 <- data.table(fread("Data/nh3.csv"))[,2:3]
n4 <- data.table(fread("Data/nh4.csv"))[,2:3]

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

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    output$predictStr <- renderText( SwiftKey(input$inputStr) )
})