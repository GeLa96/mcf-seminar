### This R script uses the TM package for producing a document term matrix (DTM) 
### and the wordcloud package to produce a wordcloud from the TM package.
### The policy statements and Q&A transcripts are to be found in the files statements.csv and QandAs.csv
### to be downloaded from the Canvas website

### Install the packages below by temporarily removing the hashtag. You need to do this only once to install the
### packages on your computer. 

#.libPaths()
#installed.packages(lib.loc ="C:/Program Files/R/R-4.4.1/library")
#installed.packages(lib.loc ="C:/SpecProF/R-Libs")


# install.packages("tm")
# install.packages("NLP")
# install.packages("Rcpp")
# install.packages("xml2")
# install.packages("slam")
# install.packages("wordcloud")
# install.packages("RColorBrewer")
# install.packages("SnowballC")

# install.packages("Matrix")
# install.packages("gamlr")


### The library commands below load the packages installed above.

library(NLP)
library(Rcpp)
library(tm)
library(RColorBrewer)
library(wordcloud)
library(SnowballC)

library(Matrix)
library(gamlr)



### Read in the file statements.csv from the directory where you store this file.
### You need to change the path below to adjust it for your computer

statements <- read.csv2("/Users/gerhardlaimer/Desktop/mcf-seminar/statements.csv", header=TRUE, sep=";", 
stringsAsFactors=FALSE, colClasses=c("integer","integer","character","character"))
str(statements)

# edit(statements)


### Read in the file QandAs.csv from the directory where you store this file.
### You need to change the path below to adjust it for your computer

QandAs<- read.csv2("/Users/gerhardlaimer/Desktop/mcf-seminar/QandAs.csv", header=TRUE, sep=";", 
stringsAsFactors=FALSE, colClasses=c("character","character","character"))
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
# weighting = function(x) weightTfIdf(x, normalize = FALSE), # activate this option for TF-IDF
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





#############################################
### II. Prepate Data for LASSO Regression ###
#############################################

### Load and match covariates for dtm with dates
### The next step is necessary to add dependent LHS variables for the LASSO regressions below
#########################################################################

### Load the MRO_rate data series as dependent variable from the ECB data warehouse
### Adjust date formats to make them compatible with the formats used in the DTM

### Transform date in dtm to ECB data warehouse format DD.MM.YYYY ######===== this is specific for the data source/website  
dtm_df<-data.frame(dtm_matrix)
dtm_df$date1<-row.names(dtm_df)
#dtm_df$date1<-ifelse(substr(dtm_df$date1,1,2)=="99",
#                     paste("19",dtm_df$date1,sep=""),paste("20",dtm_df$date1,sep=""))

dtm_df$date1<-ifelse(nchar(dtm_df$date1)==5,paste("0",dtm_df$date1, sep=""),dtm_df$date1)
dtm_df$date1<-ifelse(nchar(dtm_df$date1)==4,paste("00",dtm_df$date1, sep=""),dtm_df$date1)
dtm_df$date1<-ifelse(nchar(dtm_df$date1)==3,paste("000",dtm_df$date1, sep=""),dtm_df$date1)

dtm_df$date1<-paste("20",dtm_df$date1, sep="")


dtm_df$date2<-paste(substr(dtm_df$date1,7,8),substr(dtm_df$date1,5,6),
                     substr(dtm_df$date1,1,4), sep=".")

dtm_df$date3<-as.numeric(paste(substr(dtm_df$date1,1,4),substr(dtm_df$date1,5,6),sep=""))
# edit(dtm_df)


### read in data from ECB data warehouse
############################################
### You need to change the path below to adjust it for your computer when reading in the data

### Rate on main refinancing operation: MRO_rate! central ECB policy rate
### change the 
mro_ecb<- read.csv2("/Users/gerhardlaimer/Desktop/mcf-seminar/MRO_ECB Data Portal_2025.csv", header=TRUE, sep=";", 
stringsAsFactors=FALSE)
str(mro_ecb)
# edit(mro_ecb)

### extract from mro_ecb the rows corresponding to 7 days after the policy decision
### when the policy gets actually implemented
relevant_rows<-as.numeric(rownames(mro_ecb))[mro_ecb$DATE %in% dtm_df$date2]+7
relevant_mro<-mro_ecb[relevant_rows,]

mro_dat<-data.frame(
         cbind(mro_ecb$DATE[as.numeric(rownames(mro_ecb))[mro_ecb$DATE %in% dtm_df$date2]],
               mro_ecb$MRO_rate[as.numeric(rownames(mro_ecb))[mro_ecb$DATE %in% dtm_df$date2]+7])
               )
names(mro_dat)<-c("date2","MRO_rate")
mro_dat$date2<-as.character(mro_dat$date2)
mro_dat$MRO_rate<-as.numeric(as.character(mro_dat$MRO_rate))
str(mro_dat)


dim(dtm_df)
dtm_df<-merge(dtm_df,mro_dat,by="date2", all=TRUE)
dim(dtm_df)

dtm_df<-dtm_df[order(dtm_df$date3),]

# edit(dtm_df)




##############################################
### III. Text Regression: LASSO Regression ###
##############################################

### Load R packages for running LASSO regressions
# install.packages("Matrix")
library(Matrix)
# install.packages("gamlr")
library(gamlr)

# edit(names(dtm_df))
dim(dtm_df)

### Set estimation window with condition on dtm_df$date 3, can be between
### 199901 and 202309 (Format YYYYMM corresponding to the month of the 
### statement/Q&A session.
#   Duisenberg: June 1 1998 - Oct. 31 2003
#   Trichet:    Nov. 1 2003 - Oct. 31 2011
#   Draghi:     Nov. 1 2011 - Oct. 31 2019
#   Lagarde:    Nov. 1 2019 - now

est_window<-dtm_df$date3>=200001 & dtm_df$date3<=202509    ### set here the first and the last dates; format: YYYYMM

### extract rhs variables from rest of dtm_df
rhs_vars<-names(dtm_df) %in% names(data.frame(as.matrix(dtm)))


### Main LASSO command!!!
############################
fitlin<- gamlr(dtm_df[est_window,rhs_vars], dtm_df$MRO_rate[est_window], lmr=0.01) # lmr=0.01 is the step-size for which wee look for differenet lambda parameters
# fitlin<- gamlr(dtm_df>0, dtm_df$MRO_rate, lmr=0.001)

### show lambda, par, df, r2, aicc
summary(fitlin)


### lowest corrected Akaike score
min(summary(fitlin)[5])
### number of optimal model
grep(as.matrix(min(summary(fitlin)[5])), as.matrix(summary(fitlin)[5]))

### print Lasso path
plot(fitlin)

### Coefficients of optimal model
show_coef<-data.frame(as.matrix(coef(fitlin)))
# str(show_coef)
coef_tab<-cbind(show_coef[show_coef!=0],row.names(show_coef)[show_coef!=0])
# str(coef_tab)
coef_tab[order(coef_tab[,1], decreasing=TRUE),]

# edit(as.matrix(coef(fitlin)))


### Forecast with optimal model
predict(fitlin,dtm_df[,rhs_vars])

### Plot forecasted against actual values
plot(x=predict(fitlin,dtm_df[,rhs_vars]), y=dtm_df$MRO_rate,
xlab='Predicted Values',
ylab='Actual Values',
main='Predicted vs. Actual Values')
abline(a=0, b=1)

### Create time series plot of forecasted versus actual values 
plot(predict(fitlin,dtm_df[,rhs_vars]),type="l",col="red",
     main="Actual MRO rate versus text-based prediction", xlab="Policy Meetings", 
     ylab="MRO Rate in %", xaxt = "n")
lines(dtm_df$MRO_rate,col="green")
axis(side=1,at=c(0,50,100,150,200,250),
     labels=substr(dtm_df$date2[c(1,50,100,150,200,250)],7,10))


### Mean absolute error of forecast
print("Mean Absolute Forecast Error:")
mean(abs(dtm_df$MRO_rate-predict(fitlin,dtm_df[,rhs_vars])))

### Mean absolute percentage error of forecast
print("Mean Absolute Perecentage Forecast Error:")
mean(abs((dtm_df$MRO_rate-predict(fitlin,dtm_df[,rhs_vars]))/dtm_df$MRO_rate))*100




### END OF CODE 
##################################################################



