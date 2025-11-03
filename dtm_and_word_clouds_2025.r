### This R script uses the TM package for producing a document term matrix (DTM) 
### and the wordcloud package to produce a wordcloud from the TM package.
### The policy statements and Q&A transcripts are to be found in the files statements.csv and QandAs.csv
### to be downloaded from the Canvas website

### Install the packages below by temporarily removing the hashtag. You need to do this only once to install the
### packages on your computer. 

 #install.packages("tm")
 #install.packages("NLP")
 #install.packages("Rcpp")
 #install.packages("xml2")
 #install.packages("slam")
 #install.packages("wordcloud")
 #install.packages("RColorBrewer")
 #install.packages("SnowballC")


### The library commands below load the packages installed above.

library(NLP)
library(Rcpp)
library(tm)
library(RColorBrewer)
library(wordcloud)
library(SnowballC)


### Read in the file statements.csv from the directory where you store this file.
### You need to change the path below to adjust it for your computer

statements <- read.csv2("/Users/gerhardlaimer/Desktop/mcf-seminar/statements.csv", header=TRUE, sep=";", 
stringsAsFactors=FALSE, colClasses=c("integer","integer","character","character"))
str(statements)
#edit(statements)

### Read in the file QandAs.csv from the directory where you store this file.
### You need to change the path below to adjust it for your computer

QandAs<- read.csv2("/Users/gerhardlaimer/Desktop/mcf-seminar/QandAs.csv", header=TRUE, sep=";", 
stringsAsFactors=FALSE, colClasses=c("integer","integer","character","character"))
str(QandAs)

# edit(QandAs)


### Set here whether statements or QandAs are to be analyzed by switching the comment sign.

to_corpus<-data.frame(cbind(statements$unidate, statements$modtxt))
# to_corpus<-data.frame(cbind(QandAs$unidate, QandAs$modtxt))
str(to_corpus)

names(to_corpus)<-c("doc_id","text")
to_corpus$doc_id<-as.character(to_corpus$doc_id)
to_corpus$text<-as.character(to_corpus$text)

to_corpus<-DataframeSource(to_corpus)


### Use TM to create a corpus which is the key r object containing all texts, text identifiers and meta data
### If the input data is an R data frame, it must have two columns labeled "doc_id" and "text".
### In our case "doc_id" is the date of the press conferences and "text" is the policy statements or the
### transcripts from the Q&A sessions. Both variables need to be in character format.

corpus<-VCorpus(to_corpus, readerControl=list(language="english"))

### Technically speaking in R terms, a corpus is a complex list object as can be seen from below:

str(corpus)
length(corpus)
corpus[[1]][1]


### Tokenizer for only n-grams: the parameters 1:2 below produce one-grams (i.e. single words) and bi-grams
### Adjust the numbers below to produce other n-grams. E.g., 4:4 would produce only 4-grams

NLP_tokenizer <- function(x) {
  unlist(lapply(ngrams(words(x), 1:2), paste, collapse = "_"), use.names = FALSE)
}

### The command below produces the central document term matrix, containing all words/tokens in the columns
### and the word counts in the documents in the rows
### Note the options for the control list including stopwords, switching to lower case letters, stemming,
### removing punctuation etc. Change these option to obtain alternative results.
### Your system may take several seconds to produce the DTM.

dtm<-DocumentTermMatrix(corpus,
control = list(removeNumbers = TRUE,
# stopwords = c("FALSE", "false", "fals"),
tokenize = NLP_tokenizer, #
stopwords = TRUE,
tolower = TRUE,
stemming = TRUE,
removePunctuation =TRUE))

### Explore the DTM
dim(dtm)
str(dtm)
inspect(dtm)

### Remove terms that appear in only two percent of the documents
dtm<-removeSparseTerms(dtm, 0.98)

### Inspect the DTM -  if you want to use the edit() command, you have to remove the hashtag
# edit(as.matrix(dtm))


### Show the word frequencies in the DTM

term.frequencies<-colSums(as.matrix(dtm))
order.frequencies<-term.frequencies[order(term.frequencies, decreasing=TRUE)]
str(order.frequencies)
# edit(as.matrix(order.frequencies))



### Produce a word cloud with the code below. Again, options can be adjusted to modify the design of the word clouds

dtm_matrix <- as.matrix(dtm) 
dimnames(dtm_matrix)=="false"
dtm_words <- sort(colSums(dtm_matrix),decreasing=TRUE) 
cloud_frame <- data.frame(word = names(dtm_words),freq=dtm_words)
cloud_frame<-cloud_frame[cloud_frame$word!="fals",]
# edit(cloud_frame)

set.seed(12)
wordcloud(words = cloud_frame$word, freq = cloud_frame$freq, min.freq = 5, 
max.words=400, random.order=FALSE, rot.per=0.35,
colors=brewer.pal(8, "Dark2"))


