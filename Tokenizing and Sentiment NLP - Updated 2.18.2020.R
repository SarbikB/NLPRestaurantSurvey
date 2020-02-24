my_packages <- c("dplyr","stringr","topicmodels","tidyverse","textdata","wordcloud","reshape2","RColorBrewer","tidytext","tidyr","twitteR", "textreadr","tm","ggplot2","igraph","readxl")
lapply(my_packages, library, character.only = TRUE)
# Use 1 of them
nlp<- read_document(file="/New Survey_1.docx")
nlp_df <- as.data.frame(nlp)
#MIB <- read_document(file="xxxxxxxxxxx")
#class_combo <- c(MBA, MIB)

a <- 40 #how many observations to you have
b <- 6 #how many variables do you have
my_df <- as.data.frame(matrix(nrow=a, ncol=b))

for(z in 1:b){
  for(i in 1:a){
    my_df[i,z]<- nlp[i*b+z-b]
  }#closing z loop
}#closing i loop

colnames(my_df)

##########################################
#Renaming Columns#
#my_df %>%
  #rename(
   # V1 = "Q1",
    #V2 = "Q2",
    #V3 = "Q3",
    #V4 = "Q4",
    #V5 = "Q5",
    #V6 = "Q6"
  #)

names(my_df)[names(my_df) == "V1"] <- "Age"
names(my_df)[names(my_df) == "V2"] <- "Favourite Cusines"
names(my_df)[names(my_df) == "V3"] <- "Making a decision"
names(my_df)[names(my_df) == "V4"] <- "Dining Experience"
names(my_df)[names(my_df) == "V5"] <- "Dessert"
names(my_df)[names(my_df) == "V6"] <- "Sit down (Y/N)"

##########################################



my_txt_1 <- my_df$Age
my_txt_2 <- my_df$`Favourite Cusines`
my_txt_3 <- my_df$`Making a decision`
my_txt_4 <- my_df$`Dining Experience`
my_txt_5 <- my_df$Dessert
my_txt_6 <- my_df$`Sit down (Y/N)`



mydf1 <- data_frame(line=1:a, text=my_txt_1)
mydf2 <- data_frame(line=1:a, text=my_txt_2)
mydf3 <- data_frame(line=1:a, text=my_txt_3)
mydf4 <- data_frame(line=1:a, text=my_txt_4)
mydf5 <- data_frame(line=1:a, text=my_txt_5)
mydf6 <- data_frame(line=1:a, text=my_txt_6)

data(stop_words)
cust_stop<- data.frame(word = c("country", "dishes", "home", "um", "dish", "food", "cuisine", "love", "favorite", "dining", "experience","old","years"),
                       lexicon = c("cust", "cust","cust","cust","cust","cust","cust","cust","cust","cust","cust","cust","cust"))
################################ ####################################################

tidy_1 <- mydf1 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  anti_join(cust_stop)

tidy_2 <- mydf2 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  anti_join(cust_stop)

tidy_3 <- mydf3 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  anti_join(cust_stop)

tidy_4 <- mydf4 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  anti_join(cust_stop)

tidy_5 <- mydf5 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  anti_join(cust_stop)

tidy_6 <- mydf6 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  anti_join(cust_stop) 
##################### tf_idf #############################
my_df2 <- bind_rows(
  mutate(mydf1, question = 'Age'),
  mutate(mydf2, question = 'Favourite Cusines'),
  mutate(mydf3, question = 'Making a decision'),
  mutate(mydf4, question = 'Dining Experience'),
  mutate(mydf5, question = 'Dessert'),
  mutate(mydf6, question = 'Sit down (Y/N)')
)

questions_modif <- my_df2 %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>%
  anti_join(cust_stop) %>%
  count(question, word, sort = TRUE) %>% 
  ungroup()

questions_modif2 <- questions_modif %>% 
  group_by(question) %>% 
  summarise(total = sum(n))

questions_leftjoined <- left_join(questions_modif,questions_modif2)

question_tfidf <- questions_leftjoined %>%
  bind_tf_idf(word, question, n)

question_tfidf %>%
  arrange(desc(tf_idf))

question_tfidf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(question) %>%
  top_n(5) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=question))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~question, ncol=2, scales="free")+
  coord_flip()


#################Sentiments with Bing######################

#Question 2 Bing 
tidy_2_bing <- mydf2 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  anti_join(cust_stop) %>%
  inner_join(get_sentiments('bing')) %>%
  count(word, sentiment, sort=TRUE)

dest_bing <- tidy_2_bing %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

#Question 3 Bing
tidy_3_bing <- mydf3 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  anti_join(cust_stop) %>%
  inner_join(get_sentiments('bing')) %>%
  count(word, sentiment, sort=TRUE)

choice_bing <- tidy_3_bing %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

#Question 4 Bing 
tidy_4_bing <- mydf4 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  anti_join(cust_stop) %>%
  inner_join(get_sentiments('bing')) %>%
  count(word, sentiment, sort=TRUE)

Fav_cus <- tidy_4_bing %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()


# Question 5 Bing 
tidy_5_bing <- mydf5 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  anti_join(cust_stop) %>%
  inner_join(get_sentiments('bing')) %>%
  count(word, sentiment, sort=TRUE)

decision <- tidy_5_bing %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

#Question 6 Bing
tidy_6_bing <- mydf6 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% #here's where we remove tokens
  anti_join(cust_stop) %>%
  inner_join(get_sentiments('bing')) %>%
  count(word, sentiment, sort=TRUE)

tidy_6_bing %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

tidy_5_bing %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100,
                   scale=c(1, 1), 
                   fixed.asp=TRUE,
                   title.size=1)

############################# PLOT AND WORDCLOUD WITH NRC #########################################

nrc_colors <- brewer.pal(10, "Paired")

tidy_1_nrc <- mydf1 %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  anti_join(cust_stop) %>% 
  inner_join(get_sentiments('nrc')) %>% 
  count(word, sentiment, sort = TRUE)

#Tidy_1 - nrc, plot and wordcloud 
age_nrc <- tidy_1_nrc %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

## tide_1 - Wordcloud
tidy_1_nrc %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100,
                   scale=c(2, 2), 
                   fixed.asp=TRUE,
                   title.size=1)

tidy_2_nrc <- mydf2 %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  anti_join(cust_stop) %>% 
  inner_join(get_sentiments('nrc')) %>% 
  count(word, sentiment, sort = TRUE)

#Tidy_2 - nrc, plot and wordcloud 
tidy_2_nrc %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

## tide_2 - Wordcloud
tidy_2_nrc %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100,
                   scale=c(1, 1), 
                   fixed.asp=TRUE,
                   title.size=1)



tidy_3_nrc <- mydf3 %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  anti_join(cust_stop) %>% 
  inner_join(get_sentiments('nrc')) %>% 
  count(word, sentiment, sort = TRUE)
#Tidy_3 - nrc, plot and wordcloud 
decide_nrc <- tidy_3_nrc %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

## tidy_3 - Wordcloud
tidy_3_nrc %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100,
                   scale=c(0.5, 0.5), 
                   fixed.asp=TRUE,
                   title.size=1)


tidy_4_nrc <- mydf4 %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  anti_join(cust_stop) %>% 
  inner_join(get_sentiments('nrc')) %>% 
  count(word, sentiment, sort = TRUE)

#Tidy_4 - nrc, plot and wordcloud 
exp_nrc <- tidy_4_nrc %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

## tide_4 - Wordcloud
ex_wc_nrc <- tidy_4_nrc %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c(nrc_colors),
                   max.words=100,
                   scale=c(0.5, 0.5), 
                   fixed.asp=TRUE,
                   title.size=1)


tidy_5_nrc <- mydf5 %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  anti_join(cust_stop) %>% 
  inner_join(get_sentiments('nrc')) %>% 
  count(word, sentiment, sort = TRUE)
#Tidy_5 - nrc, plot and wordcloud 
dessert_nrc <- tidy_5_nrc %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

## tide_5 - Wordcloud 
wc_q5_nrc <- tidy_5_nrc %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c(nrc_colors),
                   max.words=100,
                   scale=c(1.5, 1), 
                   fixed.asp=TRUE,
                   title.size=1)


tidy_6_nrc <- mydf6 %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  anti_join(cust_stop) %>% 
  inner_join(get_sentiments('nrc')) %>% 
  count(word, sentiment, sort = TRUE)
#tidy_6 with nrc (plot and wordcloud)

tidy_6_nrc %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

tidy_6_nrc %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100,
                   scale=c(0.5, 0.5), 
                   fixed.asp=TRUE,
                   title.size=1)
#####################################Frequency###########################################
age <- tidy_1 %>% 
  count(word, sort = TRUE)

freq_age <- age %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(show.legend = FALSE, fill = "dodgerblue2") +
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

cuisines <- tidy_2 %>%
  count(word, sort = TRUE)

freq_cus<- cuisines %>% 
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col(show.legend = FALSE, fill = "springgreen3") +
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

sweet_savory <- tidy_6 %>% 
  count(word, sort = TRUE)

take_fre <- sweet_savory %>% 
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sweet_savory$word)) +
  geom_col(show.legend = FALSE) +
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

################################### Birgrams ###############################################

library(ggplot2)
questions_combined <- bind_rows(
  mutate(mydf1, question = 'Q1'),
  mutate(mydf2, question = 'Q2'),
  mutate(mydf3, question = 'Q3'),
  mutate(mydf4, question = 'Q4'),
  mutate(mydf5, question = 'Q5'),
  mutate(mydf6, question = 'Q6')
)

question_bigrams <- questions_combined %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)

question_bigrams #We want to see the bigrams (words that appear together, "pairs")

question_bigrams %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 

#to remove stop words from the bigram data, we need to use the separate function:
library(tidyr)
question_bigrams_separated <- question_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

#same as anti-join
question_bigrams_filtered <- question_bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
 # anti_join(stop_words, by = c(word1 = "word")) %>%   #by word if have same column name
 # anti_join(stop_words, by = c(word1 = "word"))
  
  
#creating the new bigram, "no-stop-words":
question_bigram_counts <- question_bigrams_filtered %>%
  count(word1, word2, sort = TRUE) %>%
  mutate(proportion = n/sum(n))
#want to see the new bigrams
question_bigram_counts

######################################################
####### VISUALISING A BIGRAM NETWORK #################
######################################################

#for nlp use n > 2 or 3
#install.packages("igraph")
library(igraph)
question_bigram_graph <- question_bigram_counts %>%
  filter(n>1) %>%
  graph_from_data_frame()

question_bigram_graph


#Here we can see the netword between the people
#install.packages("ggraph")
library(ggraph)
bi_gram <- ggraph(question_bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

#################################################Trigrams#######################################
question_trigrams <- questions_combined %>%
  unnest_tokens(trigram, text, token = "ngrams", n=3) 


  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>% 
  count(word1, word2, word3, sort = TRUE)

question_trigram_counts <- question_trigrams %>%
  count(word1, word2, word3, sort = TRUE)

question_trigram_graph <- question_trigram_counts %>%
  filter(n>0) %>%
  graph_from_data_frame()
question_trigram_graph

graph(question_trigram_graph, layout = "free") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

#################################### Sentiments comparing to everything ###################################################


my_df3 <- bind_rows(
  mutate(tidy_1, question = 'Q1'),
  mutate(tidy_2, question = 'Q2'),
  mutate(tidy_3, question = 'Q3'),
  mutate(tidy_4, question = 'Q4'),
  mutate(tidy_5, question = 'Q5'),
  mutate(tidy_6, question = 'Q6')
)

afinn <- my_df3 %>%
  inner_join(get_sentiments("afinn"))%>%
  group_by(question) %>% #using integer division to define larger sections of text
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc <- bind_rows(
  my_df3%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  my_df3 %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method, question, sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn, bing_and_nrc) %>%
  ggplot(aes(question, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")


#################### Casting DTM ###################################3
#For DTM you have to have unnest and Count
question_dtm <- my_df3  %>%
  count(question, word) %>%
  cast_dtm(question, word, n)

question_dtm


################## Gamma - LDA ########################################3
#install.packages("lda")
#install.packages("topicmodels")
library(topicmodels)
question_lda <- LDA(question_dtm, k=2, control=list(seed=1234)) #Classifying data into 2 different topics
question_lda

#now we are looking for the per topic per word probabilities aka. beta
#beta - what is the probability that "this term" will be generated by "this topic"
library(tidytext)
question_topics <- tidy(question_lda, matrix="beta")
question_topics
library(ggplot2)
library(dplyr)

top_terms <-question_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms

#lets plot the term frequencies by topic
top_terms %>%
  mutate(term=reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend=FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip()

#lets calculate the relative difference between the betas for words in topic 1
#and words in topic 2

beta_spread <- question_topics %>%
  mutate(topic=paste0("topic", topic)) %>%
  spread(topic, beta) %>%
  filter(topic1>.001 | topic2 >0.001) %>%
  mutate(log_rate = log2(topic2/topic1)) %>% 
  arrange(desc(log_rate))  #Positive log rate is the highest potential in topic 2
  #arrange(log_rate) # Negative Log rate is the highest potential that it is in topic 1 

beta_spread
#install.packages("quanteda")
#########################################NB Model
library(quanteda)
library(RColorBrewer)
library(ggplot2)

#loading the pdf files:
library(pdftools) # we need this library to use pdf_text
library(tm)
setwd("C:/Users/sarbi/Desktop/Hult/MBAN/Spring Semester/Text Analytics/Group/Dashboard/PDFs_new")
nm <- list.files(path="C:/Users/sarbi/Desktop/Hult/MBAN/Spring Semester/Text Analytics/Group/Dashboard/PDFs_new")

# the readPDF function doesn't actually read the PDF files, 
#the read PDF creates a function to read in all the PDF files
Rpdf <- readPDF(control = list(text = "-layout"))
opinions <- Corpus(URISource(nm), 
                   readerControl = list(reader = Rpdf))
#we need to convert the VCorpus from the previous point to
#a regular corpus using the corpus() function.
msg.dfm <- dfm(corpus(opinions), tolower = TRUE) #generating document 
msg.dfm <- dfm_trim(msg.dfm, min_termfreq = 2, min_docfreq = 1)
msg.dfm <- dfm_weight(msg.dfm)

head(msg.dfm)
#let's split the docs into training and testing data
msg.dfm.train<-msg.dfm[1:20,]
msg.dfm.test<-msg.dfm[20:40,]

#building the Naive Bayes model:
NB_classifier <- textmodel_nb(msg.dfm.train, c(0,0,1,1,1,0,0,0,0,0,1,1,1,0,0,1,0,0,1,1))
NB_classifier
NB_sum <- summary(NB_classifier)

?textmodel_nb
#These are the the estimates that figure out whether it is success or failure. Check the probability for higher one
#You can average them out if you use different words but should weight them as words like "the" are stop words and lower weighted
# predicting the testing data
pred <- predict(NB_classifier, msg.dfm.test)
pred

#install.packages("imager")
library(png)
library(knitr)
library(func)
library(imager)
im <- load.image("C:/Users/sarbi/Desktop/Hult/MBAN/Spring Semester/Text Analytics/Group/Dashboard/NLP_Overview.png")
plot(im)
