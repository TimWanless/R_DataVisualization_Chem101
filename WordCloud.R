#This is a word cloud example
dir.create("/file_path/wordcloud")
download.file("https://ibm.box.com/shared/static/cmid70rpa7xe4ocitcga1bv37r0kqnia.txt",
              destfile = "/file_path/wordcloud/churchill_speeches.txt", quiet = TRUE)
#
install.packages("tm")
install.packages("wordcloud")
library(tm)
library(wordcloud)
#
dirPath <- "/files_path/wordcloud/"
#Load the data as corpus
speech <- Corpus(DirSource(dirPath))

inspect(speech)

speech <- tm_map(speech, content_transformer(tolower))

speech <- tm_map(speech, removeNumbers)

speech <- tm_map(speech, removeWords,
                 stopwords("english"))

speech <- tm_map(speech, removeWords,
                 c("jdjfaslk;jdf","dlsjkfkldja"))

speech <- tm_map(speech, removePunctuation)

speech <- tm_map(speech, stripWhitespace)

#creates term document matrix
dtm <- TermDocumentMatrix(speech)

#Matrix transformation
m <- as.matrix(dtm)

#Sort it to show the most frequent words
v <- sort(rowSums(m), decreasing = TRUE)

#transform to a data frame
d <- data.frame(word = names(v), freq=v)
head(d,10)

wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words = 250,
          colors = brewer.pal(8, "Dark2"),
          random.order = FALSE)