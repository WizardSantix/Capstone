#####Prediction Algorithm#####

#Require Packages

library(data.table)
library(dplyr)
library(quanteda)

#Load ngrams

unigram<-read.csv("./unigram.csv")
bigram<-read.csv("./bigram.csv")
trigram<-read.csv("./trigram.csv")
fourgram<-read.csv("./fourgram.csv")

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

#Input string
input<-"Very early observations on the Bills game: Offense still struggling but the"

#Generate and clean input tokens
input_tokens<- tokens(input, remove_numbers=TRUE, remove_punct=TRUE, remove_symbols=TRUE, split_hyphens=FALSE)
input_tokens<-tokens_tolower(input_tokens)
input_tokens <- tokens_remove(input_tokens, pattern="^[^a-zA-Z]|[^a-zA-Z]$", valuetype="regex")
#input_tokens <- tokens_remove(input_tokens, pattern="\\b[a-zA-Z]\\b", valuetype="regex")
badwords <- readLines("./final/en_US/badwords.txt")
input_tokens <- tokens_remove(input_tokens, badwords, padding = TRUE)
input_tokens<- tokens_remove(input_tokens, stopwords("english"))

token_vector<-as.character(input_tokens)
size<-length(token_vector)

#Gather maximum number of usable (last n-1 grams) and subset working data containing last string

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

message1 <- "The next word is probably"

print(paste(message1,probable_words$word[1]))

print(paste("Ex.",input,probable_words$word[1]))
