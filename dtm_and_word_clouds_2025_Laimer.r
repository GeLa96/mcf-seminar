library(NLP)
library(Rcpp)
library(tm)
library(RColorBrewer)
library(wordcloud)
library(SnowballC)

# statements.csv
statements <- read.csv2("/Users/gerhardlaimer/Desktop/mcf-seminar/statements.csv",
    header = TRUE, sep = ";",
    stringsAsFactors = FALSE, colClasses = c("integer", "integer", "character", "character")
)
# edit(statements) X

QandAs <- read.csv2("/Users/gerhardlaimer/Desktop/mcf-seminar/QandAs.csv",
    header = TRUE, sep = ";",
    stringsAsFactors = FALSE, colClasses = c("integer", "integer", "character", "character")
)
# edit(QandAs)

# set if statements or QandAs should be analyzed
to_corpus <- data.frame(cbind(statements$unidate, statements$modtxt)) # column binds by unidate and modtxt
# to_corpus<-data.frame(cbind(QandAs$unidate, QandAs$modtxt))

names(to_corpus) <- c("doc_id", "text")
to_corpus$doc_id <- as.character(to_corpus$doc_id)
to_corpus$text <- as.character(to_corpus$text)

to_corpus <- tm::DataframeSource(to_corpus)

corpus <- VCorpus(to_corpus, readerControl = list(language = "english"))
View(corpus)

NLP_tokenizer <- function(x) {
    unlist(lapply(ngrams(words(x), 1:2), paste, collapse = "_"), use.names = FALSE)
}

dtm <- DocumentTermMatrix(corpus,
    control = list(
        removeNumbers = TRUE,
        # stopwords = c("FALSE", "false", "fals"),
        tokenize = NLP_tokenizer, #
        stopwords = TRUE,
        tolower = TRUE,
        stemming = TRUE,
        removePunctuation = TRUE
    )
)

dtm <- removeSparseTerms(dtm, 0.98)

dtm_matrix <- as.matrix(dtm)
dimnames(dtm_matrix) == "false"
dtm_words <- sort(colSums(dtm_matrix), decreasing = TRUE)
cloud_frame <- data.frame(word = names(dtm_words), freq = dtm_words)
cloud_frame <- cloud_frame[cloud_frame$word != "fals", ]
# edit(cloud_frame)

set.seed(12)
wordcloud(
    words = cloud_frame$word, freq = cloud_frame$freq, min.freq = 5,
    max.words = 400, random.order = FALSE, rot.per = 0.35,
    colors = brewer.pal(8, "Dark2")
)


###### Creatin own word clouds
## 1) use statemnt.csv and do not filter for fals

# comment out : cloud_frame <- data.frame(word = names(dtm_words), freq = dtm_words)
