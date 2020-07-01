library(dplyr)
library(quanteda)
library(ggplot2)
library(ngram)
library(data.table)

#Create paths
blogs_path   <- "./final/en_US/en_US.blogs.txt"
news_path    <- "./final/en_US/en_US.news.txt"
twitter_path <- "./final/en_US/en_US.twitter.txt" 

#Get data from the files
blogs   <- readLines(blogs_path, skipNul = TRUE)
news    <- readLines(news_path,  skipNul = TRUE)
twitter <- readLines(twitter_path, skipNul = TRUE)

#Number of lines per file
blogs_lines   <- length(blogs)
news_lines    <- length(news)
twitter_lines <- length(twitter)
total_lines   <- blogs_lines + news_lines + twitter_lines

#Sampling and Tidying
set.seed(666)
samp_pct<-0.25

#Convert data to dataframes to use dplyr
blogs   <- data_frame(text = blogs)
news    <- data_frame(text = news)
twitter <- data_frame(text = twitter)

#Randomly sample 20% of the data
blogs_sample<-sample_n(blogs,blogs_lines*samp_pct)
blogs_sample<-blogs_sample %>% mutate(file="blogs")
news_sample<-sample_n(news,news_lines*samp_pct)
news_sample<-news_sample %>% mutate(file="news")
twitter_sample<-sample_n(twitter,twitter_lines*samp_pct)
twitter_sample<-twitter_sample %>% mutate(file="twitter")

#Create a combined tidy dataset
sampledata<- rbind(blogs_sample,news_sample,twitter_sample)

#Create Corpus with quanteda
samplecorpus<-corpus(sampledata,text_field="text")

#Tokenize the corpus
Tokens <- tokens(samplecorpus, remove_numbers=TRUE, remove_punct=TRUE, remove_symbols=TRUE, split_hyphens=FALSE)
Tokens <- tokens_tolower(Tokens)
#Tokens <- tokens_remove(Tokens, stopwords("english"), padding = TRUE)

#Remove words that start or end with a non a-z character, and single character words.
Tokens <- tokens_remove(Tokens, pattern="^[^a-zA-Z]|[^a-zA-Z]$", valuetype="regex", padding=TRUE)
#Tokens <- tokens_remove(Tokens, pattern="\\b[a-zA-Z]\\b", valuetype="regex")

#Remove bad words 
badwords <- readLines("./final/en_US/badwords.txt")
Tokens <- tokens_remove(Tokens, badwords, padding = TRUE)

#Create feature matrix for tokens
sampledfm <- dfm(Tokens)

#Create wordcloud of most common Tokens
textplot_wordcloud(sampledfm,max_words = 100, min_size = 1, max_size=30,color = RColorBrewer::brewer.pal(8, "Dark2"))

#Analyze n-grams of 2, 3 and 4 tokens
bigram <- tokens_ngrams(Tokens, n = 2, concatenator = " ")
trigram <- tokens_ngrams(Tokens, n = 3, concatenator = " ")
fourgram <- tokens_ngrams(Tokens, n=4, concatenator = " ")

#Create feature matrix for the n-grams
bigramdfm <- dfm(bigram, verbose = FALSE)
trigramdfm <- dfm(trigram, verbose = FALSE)
fourgramdfm <- dfm(fourgram, verbose=FALSE)

#Generate named vectors with ngram counts
sums_1 <- colSums(sampledfm)
sums_2 <- colSums(bigramdfm)
sums_3 <- colSums(trigramdfm)
sums_4 <- colSums(fourgramdfm)

# Create data tables with separate words in individual columns
unigram_table <- data.table(
  word_1 = names(sums_1),
  freq = sums_1)
unigram_table<-unigram_table[-1,]

bigram_table <- data.table(
  word_1 = sapply(strsplit(names(sums_2), " ", fixed = TRUE),"[[",1),
  word_2 = sapply(strsplit(names(sums_2), " ", fixed = TRUE),"[[",2),
  freq = sums_2)

trigram_table <- data.table(
  word_1 = sapply(strsplit(names(sums_3), " ", fixed = TRUE),"[[",1),
  word_2 = sapply(strsplit(names(sums_3), " ", fixed = TRUE),"[[",2),
  word_3 = sapply(strsplit(names(sums_3), " ", fixed = TRUE),"[[",3),
  freq = sums_3)

fourgram_table <- data.table(
  word_1 = sapply(strsplit(names(sums_4), " ", fixed = TRUE),"[[",1),
  word_2 = sapply(strsplit(names(sums_4), " ", fixed = TRUE),"[[",2),
  word_3 = sapply(strsplit(names(sums_4), " ", fixed = TRUE),"[[",3),
  word_4 = sapply(strsplit(names(sums_4), " ", fixed = TRUE),"[[",4),
  freq = sums_4)

#Order by frequency
unigram_table<-arrange(unigram_table,desc(freq))
bigram_table<-arrange(bigram_table,desc(freq))
trigram_table<-arrange(trigram_table,desc(freq))
fourgram_table<-arrange(fourgram_table,desc(freq))

write.csv(unigram_table,file="./unigram.csv",row.names=FALSE)
write.csv(bigram_table,file="./bigram.csv",row.names=FALSE)
write.csv(trigram_table,file="./trigram.csv",row.names=FALSE)
write.csv(fourgram_table,file="./fourgram.csv",row.names=FALSE)