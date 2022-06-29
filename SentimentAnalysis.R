library(tm)
library(wordcloud)

#Read the data file
reviews <- read.csv("Reviews_of_Amazon_Products.csv", header = TRUE)


#Inspecting the dataset
names(reviews)
head(reviews)

summary(reviews)
str(reviews)
dim(reviews)
# 
# > dim(reviews)
# [1] 1123    8


###Filter the products from the main dataset and create 5 separate datasets using subset()

R_16GB_Blue<-subset(reviews, name=="Fire HD 8 Tablet, Wi-Fi, 16 GB-Blue")
R_32GB_Blue<-subset(reviews, name=="Fire HD 8 Tablet, Wi-Fi, 32 GB-Blue")
R_16GB_Magenta<-subset(reviews, name=="Fire HD 8 Tablet,  Wi-Fi, 16 GB-Magenta")
R_32GB_Magenta<-subset(reviews, name=="Fire HD 8 Tablet,  Wi-Fi, 32 GB-Magenta")
R_32GB_Black<-subset(reviews, name=="Fire HD 8 Tablet,  Wi-Fi, 32 GB-Black")


#inspect the review columns of each
head(R_16GB_Blue$reviews.text)
head(R_16GB_Magenta$reviews.text)
head(R_32GB_Blue$reviews.text)
head(R_32GB_Magenta$reviews.text)
head(R_32GB_Black$reviews.text)


#create text vectors
review_16GB_Blue<-R_16GB_Blue$reviews.text
review_16GB_Magenta<-R_16GB_Magenta$reviews.text
review_32GB_Blue<-R_32GB_Blue$reviews.text
review_32GB_Magenta<-R_32GB_Magenta$reviews.text
review_32GB_Black<-R_32GB_Black$reviews.text

#Convert all text to lower case
review_16GB_Blue<-tolower(review_16GB_Blue)
review_32GB_Blue<-tolower(review_32GB_Blue)
review_16GB_Magenta<-tolower(review_16GB_Magenta)
review_32GB_Magenta<-tolower(review_32GB_Magenta)
review_32GB_Black<-tolower(review_32GB_Black)



#gsub() function replaces all matches of a string, 
# if the parameter is a string vector, 
# returns a string vector of the same length 
# and with the same attributes (after possible coercion to character). 
# Elements of string vectors which are not substituted 
# will be returned unchanged (including any declared encoding).

###Remove links from the reviews

review_16GB_Blue<-gsub("http\\S+\\s*", "", review_16GB_Blue)
review_32GB_Blue<-gsub("http\\S+\\s*", "", review_32GB_Blue)
review_16GB_Magenta<-gsub("http\\S+\\s*", "", review_16GB_Magenta)
review_32GB_Magenta<-gsub("http\\S+\\s*", "", review_32GB_Magenta)
review_32GB_Black<-gsub("http\\S+\\s*", "", review_32GB_Black)


#Remove punctuation marks
review_16GB_Blue<-gsub("[[:punct:]]", "", review_16GB_Blue)
review_32GB_Blue<-gsub("[[:punct:]]", "", review_32GB_Blue)
review_16GB_Magenta<-gsub("[[:punct:]]", "", review_16GB_Magenta)
review_32GB_Magenta<-gsub("[[:punct:]]", "", review_32GB_Magenta)
review_32GB_Black<-gsub("[[:punct:]]", "", review_32GB_Black)

#remove digits
review_16GB_Blue<-gsub("[[:digit:]]", "", review_16GB_Blue)
review_32GB_Blue<-gsub("[[:digit:]]", "", review_32GB_Blue)
review_16GB_Magenta<-gsub("[[:digit:]]", "", review_16GB_Magenta)
review_32GB_Magenta<-gsub("[[:digit:]]", "", review_32GB_Magenta)
review_32GB_Black<-gsub("[[:digit:]]", "", review_32GB_Black)


#remove leading blank spaces at the beginning from the reviews
review_16GB_Blue<-gsub("^ ", "", review_16GB_Blue)
review_32GB_Blue<-gsub("^ ", "", review_32GB_Blue)
review_16GB_Magenta<-gsub("^ ", "", review_16GB_Magenta)
review_32GB_Magenta<-gsub("^ ", "", review_32GB_Magenta)
review_32GB_Black<-gsub("^ ", "", review_32GB_Black)


#Remove blank spaces at the end from the reviews
review_16GB_Blue<-gsub(" $", "", review_16GB_Blue)
review_32GB_Blue<-gsub(" $", "", review_32GB_Blue)
review_16GB_Magenta<-gsub(" $", "", review_16GB_Magenta)
review_32GB_Magenta<-gsub(" $", "", review_32GB_Magenta)
review_32GB_Black<-gsub(" $", "", review_32GB_Black)



#Remove "tablet" word from the reviews
review_16GB_Blue<-gsub("tablet", "", review_16GB_Blue)
review_32GB_Blue<-gsub("tablet", "", review_32GB_Blue)
review_16GB_Magenta<-gsub("tablet", "", review_16GB_Magenta)
review_32GB_Magenta<-gsub("tablet", "", review_32GB_Magenta)
review_32GB_Black<-gsub("tablet", "", review_32GB_Black)

#Inspect the vectors after cleaning
head(review_16GB_Blue)
head(review_32GB_Blue)
head(review_16GB_Magenta)
head(review_32GB_Magenta)
head(review_32GB_Black)



#Converting the text vectors to corpus
corpus_16GB_Blue <- Corpus(VectorSource(review_16GB_Blue))
corpus_32GB_Blue <- Corpus(VectorSource(review_32GB_Blue))
corpus_16GB_Magenta <- Corpus(VectorSource((review_16GB_Magenta)))
corpus_32GB_Magenta <- Corpus(VectorSource((review_32GB_Magenta)))
corpus_32GB_Black <- Corpus(VectorSource(review_32GB_Black))


#Inspecting the corpus
corpus_16GB_Blue
corpus_16GB_Magenta
corpus_32GB_Blue
corpus_32GB_Blue
corpus_32GB_Black


#Clean up corpus by removing stop words and Whitespace
corpus_16GB_Blue <- tm_map(corpus_16GB_Blue, removeWords,stopwords("english"))
corpus_16GB_Blue <- tm_map(corpus_16GB_Blue, stripWhitespace)

inspect(corpus_16GB_Blue)

corpus_16GB_Magenta<-tm_map(corpus_16GB_Magenta, removeWords,stopwords("english"))
corpus_16GB_Magenta <- tm_map(corpus_16GB_Magenta, stripWhitespace)
inspect(corpus_16GB_Magenta)


corpus_32GB_Blue <- tm_map(corpus_32GB_Blue, removeWords,stopwords("english"))
corpus_32GB_Blue <- tm_map(corpus_32GB_Blue, stripWhitespace)
inspect(corpus_32GB_Blue)

corpus_32GB_Magenta<-tm_map(corpus_32GB_Magenta, removeWords,stopwords("english"))
corpus_32GB_Magenta <- tm_map(corpus_32GB_Magenta, stripWhitespace)
inspect(corpus_32GB_Magenta)

corpus_32GB_Black <- tm_map(corpus_32GB_Black, removeWords,stopwords("english"))
corpus_32GB_Black <- tm_map(corpus_32GB_Black, stripWhitespace)
inspect(corpus_32GB_Black)

install.packages("SnowballC")
library(SnowballC)

#Stem the words to their root of all reviews present in the corpus
stem_corpus_16GB_Blue <- tm_map(corpus_16GB_Blue, stemDocument)
stem_corpus_16GB_Magenta <- tm_map(corpus_16GB_Magenta, stemDocument)
stem_corpus_32GB_Blue <- tm_map(corpus_32GB_Blue, stemDocument)
stem_corpus_32GB_Magenta <- tm_map(corpus_32GB_Magenta, stemDocument)
stem_corpus_32GB_Black <- tm_map(corpus_32GB_Black, stemDocument)

#####
#Load the positive and negative lexicon data
positive_lexicon <- read.csv("positive-lexicon.txt")
negative_lexicon <- read.csv("negative-lexicon.txt")

#inspect the lexicons
head(positive_lexicon)
tail(positive_lexicon)

head(negative_lexicon)
tail(negative_lexicon)


#Creating a function for sentimental analysis

sentiment <- function(stem_corpus)
{
  #generate wordclouds
  wordcloud(stem_corpus,
            min.freq = 3,
            colors=brewer.pal(8, "Dark2"),
            random.color = TRUE,
            max.words = 100)
  #Calculating the count of total positive and negative words in each review
  #Create variables and vectors
  total_pos_count <- 0
  total_neg_count <- 0
  pos_count_vector <- c()
  neg_count_vector <- c()
  #Calculate the size of the corpus
  size <- length(stem_corpus)
  for(i in 1:size)
  {
    #All the words in current review
    corpus_words<- list(strsplit(stem_corpus[[i]]$content, split = " "))
    #positive words in current review
    
    pos_count <-length(intersect(unlist(corpus_words), unlist(positive_lexicon)))
    
    #negative words in current review
    neg_count <- length(intersect(unlist(corpus_words), unlist(negative_lexicon)))
    total_pos_count <- total_pos_count + pos_count ## overall positive count
    total_neg_count <- total_neg_count + neg_count ## overall negative count
  }
  #Calculating overall percentage of positive and negative words of all the reviews
  total_pos_count ## overall positive count
  total_neg_count ## overall negative count
  total_count <- total_pos_count + total_neg_count
  overall_positive_percentage <- (total_pos_count*100)/total_count
  overall_negative_percentage <- (total_neg_count*100)/total_count
  overall_positive_percentage ## overall positive percentage
  
  #Create a dataframe with all the positive and negative reviews
  df<-data.frame(Review_Type=c("Positive","Negative"),
                 Count=c(total_pos_count ,total_neg_count ))
  print(df) #Print
  overall_positive_percentage<-paste("Percentage of Positive Reviews:",
                                     round(overall_positive_percentage,2),"%")
  return(overall_positive_percentage)
}




sentiment(stem_corpus_16GB_Blue)
sentiment(stem_corpus_16GB_Magenta)
sentiment(stem_corpus_32GB_Blue)
sentiment(stem_corpus_32GB_Magenta)
sentiment(stem_corpus_32GB_Black)


# ##Output
# > Sentiment(stem_corpus_16GB_Blue)
# Review_Type Count
# 1    Positive   117
# 2    Negative    13
# [1] "Percentage of Positive Reviews: 90 %"
# There were 12 warnings (use warnings() to see them)
# > sentiment(stem_corpus_16GB_Magenta)
# Review_Type Count
# 1    Positive  1439
# 2    Negative   201
# [1] "Percentage of Positive Reviews: 87.74 %"
# There were 47 warnings (use warnings() to see them)
# > sentiment(stem_corpus_32GB_Blue)
# Review_Type Count
# 1    Positive   136
# 2    Negative    39
# [1] "Percentage of Positive Reviews: 77.71 %"
# There were 50 or more warnings (use warnings() to see the first 50)
# > sentiment(stem_corpus_32GB_Magenta)
# Review_Type Count
# 1    Positive   101
# 2    Negative    30
# [1] "Percentage of Positive Reviews: 77.1 %"
# There were 48 warnings (use warnings() to see them)
# > sentiment(stem_corpus_32GB_Black)
# Review_Type Count
# 1    Positive   104
# 2    Negative    21
# [1] "Percentage of Positive Reviews: 83.2 %"
# There were 49 warnings (use warnings() to see them)
