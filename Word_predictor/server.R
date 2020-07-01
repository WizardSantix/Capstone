library(shiny)
library(data.table)
library(dplyr)
library(quanteda)
library(feather)
library(ggplot2)

##########################################
#### PREPARE THE DATA (NOT REACTIVE) #####
##########################################

#Load ngrams

unigram<-as.data.frame(read_feather("./unigram.feather"))
bigram<-as.data.frame(read_feather("./bigram.feather"))
trigram<-as.data.frame(read_feather("./trigram.feather"))
fourgram<-as.data.frame(read_feather("./fourgram.feather"))

#Calculate total n-grams
totalunigrams<-sum(unigram[,2])
totalbigrams<-sum(bigram[,3])
totaltrigrams<-sum(trigram[,4])
totalfourgrams<-sum(fourgram[,5])

#Calculate probability of each n-gram
unigram<-unigram%>%mutate(prob=freq/totalunigrams)
bigram<-bigram%>%mutate(prob=freq/totalbigrams)
trigram<-trigram%>%mutate(prob=freq/totaltrigrams)
fourgram<-fourgram%>%mutate(prob=freq/totalfourgrams)

#Read Bad Words
badwords <- readLines("./badwords.txt")

##############################################################
############# DEFINE PREDICTION FUNCTIONS ####################
##############################################################

#Generate and clean input tokens
process_tokens <- function(string="No string provided"){
    input_tokens<- tokens(string, remove_numbers=TRUE, remove_punct=TRUE, remove_symbols=TRUE, split_hyphens=FALSE)
    input_tokens<-tokens_tolower(input_tokens)
    input_tokens <- tokens_remove(input_tokens, pattern="^[^a-zA-Z]|[^a-zA-Z]$", valuetype="regex")
    input_tokens <- tokens_remove(input_tokens, badwords, padding = TRUE)
    input_tokens<- tokens_remove(input_tokens, stopwords("english"))
    token_vector<-as.character(input_tokens)
    return(token_vector)
}

#Subset the ngrams for coincidences
#Gather maximum number of usable (last n-1 grams) and subset working data containing last string
  
get_wdata <- function(token_vector){
  size<-length(token_vector)
  if (size>=3){
    wdata<-fourgram
    last_string <- token_vector[(size-2):size]
    wdata1 <- wdata %>% filter(word_1 == last_string[1] & word_2 == last_string[2] & word_3 == last_string[3])
    if (nrow(wdata1)==0){
      wdata1 <- wdata %>% filter(
        (word_1 == last_string[1] & word_2 == last_string[2])|
          (word_1 == last_string[1] & word_3 == last_string[3])|
          (word_2 == last_string[2] & word_3 == last_string[3]))
    }
  } else {
    if(size==2){
      wdata<-trigram
      last_string <- token_vector
      wdata1 <- wdata %>% filter(word_1 == last_string[1] & word_2 == last_string[2])
      if (nrow(wdata1)==0){
        wdata1 <- wdata %>% filter(
          (word_1 == last_string[1])|
            (word_2 == last_string[2]))
      }
    } else {
      wdata<-bigram
      last_string <- token_vector
      wdata1 <- wdata %>% filter(word_1 == last_string)
    }
  }
  
  #Work with the last unigram if none of the conditions are met.
  if (nrow(wdata1)==0){
    wdata<-bigram
    last_string <- token_vector[size]
    wdata1 <- wdata %>% filter(word_1 == last_string)
  }
  
  #Work with the -1 unigram if none of the conditions are met.
  if (nrow(wdata1)==0){
    wdata<-bigram
    last_string <- token_vector[size-1]
    wdata1 <- wdata %>% filter(word_1 == last_string)
  }
  
  
  #Work with the -2 unigram if none of the conditions are met.
  if (nrow(wdata1)==0){
    wdata<-bigram
    last_string <- token_vector[size-2]
    wdata1 <- wdata %>% filter(word_1 == last_string)
  }
  return(wdata1)
}

# Generate the table for probable_words and calculate the probabilities

get_pwords<-function(wdata1){
  #Generate table of probable next words and their conditional probability
  probable_words<-data.table(word=as.character(wdata1[,ncol(wdata1)-2]),
                             cprob = wdata1[,ncol(wdata1)],
                             tprob = 0)
  #Look for overall probability in 1-grams and add it to the df
  for (word in as.character(probable_words$word)){
    probable_words$tprob[probable_words$word==word] = unigram$prob[unigram$word_1==word]
  }
  
  #Normalize conditional probability
  
  total_cprob <- sum(probable_words$cprob)
  probable_words$cprob <- probable_words$cprob/total_cprob
  
  #Calculate probability and sort the df with that criterion
  probable_words<-probable_words %>% mutate(prob=cprob*tprob) %>% arrange(desc(prob))
  return(probable_words)
}

########################################################################
############## USE THE PREDICTION FUNCTIONS REACTIVELY #################
########################################################################

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
probable_words<-eventReactive(input$predict,{
    # Gather the string input from the box
    string_input<- input$string_input
    
    # Generate a token vector
    token_vector<- process_tokens(string_input)
    
    # Subset n-grams for usable data
    wdata <- get_wdata(token_vector)
    
    # Get the list of probable words and their probabilities (Conditional_probability*Overall_probability)
    probable_words <- get_pwords(wdata)
    
    names(probable_words)<- c("Word", "Conditional probability", "Overall probability", "Probability")
    
    probable_words2 <- as.data.frame(head(probable_words[-(2:3)],5))
    
    probable_words2$Probability <- probable_words2$Probability*100
    
    return(probable_words2)
    })
  

#Generate the messages to print
output$table1<-renderTable(probable_words())

output$plot1<- renderPlot(plot1<-ggplot(probable_words(),aes(x=Word,y=Probability,fill=Probability))+geom_col())
})
