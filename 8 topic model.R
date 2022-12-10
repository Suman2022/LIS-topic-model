#Suman Das  7-12-2022# 
#we collected data from 22 central Universities
# Processed datasets documents are used to analyze the text

require(topicmodels)
require(pdftools)
require(tm)
require(tidytext)
require(ggplot2)
require(dplyr)
library(keras)
library(wesanderson)

#load all PDF files
All_Files<-list.files(pattern = "pdf$")
All_opinions <-lapply(All_Files, pdf_text)

#create the corpus that containing words
document<-Corpus(VectorSource(All_opinions))
# cleaning data
document <- tm_map(document, content_transformer(tolower)) # convert all text to lower case
document <- tm_map(document, removeNumbers) # remove numbers from document
document <- tm_map(document, removeWords, stopwords("english")) # remove stopwords in English
document <- tm_map(document, removePunctuation, preserve_intra_word_dashes = TRUE)
document <- tm_map(document, stripWhitespace) # removewhite space

# Create document-term matrix
dtm <- DocumentTermMatrix(document)


# Create Model with 4 Topics
model_lda <- LDA(dtm, k = 8, control = list(seed = 1234))
model_lda

# Shows the probability of a word being associated to a topic
beta_topics <- tidy(model_lda, matrix = "beta") # create the beta model
beta_topics # shows all the information in the beta_topics

# Grouping the terms by topic
beta_top_terms <- beta_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>%
  ungroup() %>%
  arrange(topic, -beta)


# Display the grouped terms on the charts
beta_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) + 
  geom_col(show.legend = FALSE) + 
  facet_wrap(~ topic, scales = "free") + 
  scale_y_reordered()

# topic 1 is about Library, Knowledge management, digital research
# topic 2 is about Information management sources classification services.
# topic 3 is about tWeb development information system software 
# topic 4 is about Information System science classification.
# topic 5 is about Data Types, Knowledge system and social tools
# topic 6 is about library source & development, data literacies.
# topic 7 is about Digital Science, Information source and concept
# topic 8 is about Indexing, information, data, software

# filters terms by topics

# Examining per document per topic probability
# gamme indicates how documents affects the topics
# For example, document 1, 6.4 gamma on topic 1, also document 4 is 9.99 on topic 1, document 4 is the highest gamma
gamma_documents <- tidy(model_lda, matrix = "gamma")
# create a dataframe with gamme results
doc_gamma.df <- data.frame(gamma_documents)
doc_gamma.df$chapter <- rep(1:dim(dtm)[1],4)


# plot gamma results
ggplot(data = doc_gamma.df, aes(x = chapter, y = gamma,
                                group = factor(topic), color = factor(topic))) + geom_line() + facet_wrap(~factor(topic), ncol = 1)

##Not ne
<-terms(model_lda, 5)
head(model_lda@gamma)
model_lda@beta[,1]
sum(model_lda@beta[1,])
sum(exp(model_lda@beta[1,]))
##topic <- 1
##df <- data.frame(term = model_lda@terms, p = exp(model_lda@beta[topic,]))
##head(df[order(-df$p),])


#making word clouds
wordcloud(document, 
          scale=c(4.5,0.5), 
          max.words=8, 
          min.freq=100, 
          random.order=FALSE, 
          rot.per=0.40)

gColors <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}
gg.cols <- ggColors(8)
bp.cols<- c("light blue","cornflowerblue", "coral2", brewer.pal(8,"Dark2"))

wordcloud(document, 
          scale=c(5.5,0.5), 
          max.words=200, 
          min.freq=50, 
          random.order=FALSE, 
          rot.per=0.40, 
          use.r.layout=FALSE, 
          random.color=TRUE, 
          colors = bp.cols
          #colors=brewer.pal(8, "Dark2")
)

