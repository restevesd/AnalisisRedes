library(rtweet)
library(tidyverse)
library(httpuv)
library(lubridate)
library(igraph)
require(gridExtra)
library(tm)
library(SnowballC)
library(wordcloud)
library(ggplot2)
library(dplyr)
library(readr)
library(cluster)
library(ggraph)

## authenticate via access token
token <- create_token(
  app = "DatosTW",
  consumer_key = '',
  consumer_secret = '',
  access_token = '',
  access_secret = '')


#Hashtags
ht <- c("#CoronaVirusecuador","#CODVID19","#Covid_19","#pandemia",
        "#EmergenciaSanitaria","#COVID2019","#QuedateEnCasa",
        "#ToqueDeQuedaEcuador","#CoronaVirus")

coronaGYE <- search_tweets2(ht,n=100000000,include_rts=FALSE, lang = "es",
                          geocode = lookup_coords("Guayaquil,Ecuador"),
                          retryonratelimit = TRUE)
coronaUIO <- search_tweets2(ht,n=100000000,include_rts=FALSE, lang = "es",
                          geocode = lookup_coords("Quito,Ecuador"),
                          retryonratelimit = FALSE)
coronaEC <- search_tweets2(ht,n=100000000,include_rts=FALSE, lang = "es",
                           geocode = lookup_coords("Ecuador"),
                           retryonratelimit = TRUE)

coronaEC1<- search_tweets2("#Covid_19",n=100000000,include_rts=FALSE, lang = "es",
                           geocode = lookup_coords("Ecuador"),
                           retryonratelimit = FALSE)
coronaEC2<- search_tweets2("#CoronaVirusecuador",n=100000000,include_rts=FALSE, lang = "es",
                           geocode = lookup_coords("Ecuador"),
                           retryonratelimit = TRUE)
coronaEC3<- search_tweets2("#COVID2019",n=100000000,include_rts=FALSE, lang = "es",
                           geocode = lookup_coords("Ecuador"),
                           retryonratelimit = TRUE)
coronaEC4<- search_tweets2("#ToqueDeQuedaEcuador",n=100000000,include_rts=FALSE, lang = "es",
                           geocode = lookup_coords("Ecuador"),
                           retryonratelimit = TRUE)
coronaEC5<- search_tweets2("#QuedateEnCasa",n=100000000,include_rts=FALSE, lang = "es",
                           geocode = lookup_coords("Ecuador"),
                           retryonratelimit = TRUE)
coronaEC6<- search_tweets2("#CoronaVirus",n=100000000,include_rts=FALSE, lang = "es",
                           geocode = lookup_coords("Ecuador"),
                           retryonratelimit = TRUE)
coronaEC7<- search_tweets2("#CoronaVirusec",n=100000000,include_rts=FALSE, lang = "es",
                           geocode = lookup_coords("Ecuador"),
                           retryonratelimit = TRUE)

coronaEC <- rbind(coronaEC1,coronaEC2,coronaEC3,coronaEC4,
                  coronaEC5,coronaEC6,coronaEC7)

coronaGYE <- coronaGYE %>% mutate(created_at = with_tz(created_at, tz = "America/Bogota") )
coronaUIO <- coronaUIO %>% mutate(created_at = with_tz(created_at, tz = "America/Bogota") )
coronaEC <- coronaEC %>% mutate(created_at = with_tz(created_at, tz = "America/Bogota") )

coronaEC_save <- unique(rbind(coronaEC,htEC))
coronaGYE_save <- unique(rbind(coronaGYE,htGYE))
coronaUIO_save <- unique(rbind(coronaUIO,htUIO))


saveRDS(coronaGYE_save,file = "htGYE.rds")
saveRDS(coronaUIO_save,file = "htUIO.rds")
saveRDS(coronaEC_save,file = "htEC.rds")

rm(coronaEC1,coronaEC2,coronaEC3,coronaEC4,
      coronaEC5,coronaEC6,coronaEC7)
rm(htEC,htGYE,htUIO)
rm(coronaEC_save,coronaGYE_save,coronaUIO_save)
rm(coronaEC,coronaGYE,coronaUIO)

htGYE <- unique(htGYE)

tweets_tidy <- htGYE %>% unnest(hashtags)

htGYE %>% group_by(status_id) %>% summarise(total = n()) %>% 
  arrange(desc(total))

gye <- htGYE %>% group_by(screen_name) %>%
  filter(created_at > '2020/02/01' ) %>% summarise(numero_tweets = n()) %>% 
  arrange(desc(numero_tweets)) %>% top_n(10,numero_tweets) %>% 
  ggplot(aes(reorder(screen_name, -numero_tweets),
             numero_tweets,label = numero_tweets))+
  geom_bar(stat="identity") +
  geom_label()+
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 13)) +
  theme(axis.text = element_text(size=9))+
  theme(axis.text.x = element_text(angle = 90))+
  labs(
    x = NULL, y = NULL,
    title = "Cantidad de posteos mes Marzo Guayaquil",
    subtitle = "Hashtags relacionados al #CoronaVirus",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

uio <- htUIO %>%  group_by(screen_name) %>%
  filter(created_at > '2020/02/01' ) %>% summarise(numero_tweets = n()) %>% 
  arrange(desc(numero_tweets)) %>% top_n(10,numero_tweets) %>% 
  ggplot(aes(reorder(screen_name, -numero_tweets),
             numero_tweets,label = numero_tweets))+
  geom_bar(stat="identity") +
  geom_label()+
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 13)) +
  theme(axis.text = element_text(size=9))+
  theme(axis.text.x = element_text(angle = 90))+
  labs(
    x = NULL, y = NULL,
    title = "Cantidad de posteos mes Marzo UIO",
    subtitle = "Hashtags relacionados al #CoronaVirus",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

ec <- htEC %>%  group_by(screen_name) %>%
  filter(created_at > '2020/02/01' ) %>% summarise(numero_tweets = n()) %>% 
  arrange(desc(numero_tweets)) %>% top_n(10,numero_tweets) %>% 
  ggplot(aes(reorder(screen_name, -numero_tweets),
             numero_tweets,label = numero_tweets))+
  geom_bar(stat="identity") +
  geom_label()+
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 13)) +
  theme(axis.text = element_text(size=9))+
  theme(axis.text.x = element_text(angle = 90))+
  labs(
    x = NULL, y = NULL,
    title = "Cantidad de posteos mes Marzo ECUADOR",
    subtitle = "Hashtags relacionados al #CoronaVirus",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

grid.arrange(gye, uio, ec, ncol=2)


# HT usados desde Febrero
tweets_tidy %>% filter(!is.na(hashtags)) %>% 
  filter(created_at > '2020/02/01' ) %>% 
  group_by(hashtags) %>% summarise(n=n()) %>% arrange(desc(n)) %>% 
  head(n=20L) %>% 
  ggplot(aes(x = reorder(hashtags,n), y = n)) +
  geom_col() +
  theme_bw() +
  labs(x = "Hashtags", y = "Frecuencia") +
  theme(legend.position = "none") +
  coord_flip()

#Para crear nubes de palabras 
df_grouped <- tweets_tidy %>% filter(!is.na(hashtags)) %>%
  filter(created_at >= '2020/02/01' ) %>% 
  group_by(hashtags) %>% 
  summarise(n=n()) %>% 
  mutate(frecuencia = n / n()) %>%
  arrange(desc(frecuencia))

df_grouped %>% filter(n>1) %>% wordcloud2::wordcloud2()


# Serie de tiempo evolución de tweets
tsgye <- htGYE %>% filter(created_at > '2020/02/01' ) %>%
  ts_plot("24 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frecuencia de posteos mes Marzo Guayaquil",
    subtitle = "HT relacionados #CoronaVirus - cortes cada 24 horas",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

tsUIO <- htUIO %>% filter(created_at > '2020/02/01' ) %>%
  ts_plot("24 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frecuencia de posteos mes Marzo UIO",
    subtitle = "HT relacionados #CoronaVirus - cortes cada 24 horas",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

tsEC<- htEC %>% filter(created_at > '2020/02/01' ) %>%
  ts_plot("24 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frecuencia de posteos mes Marzo ECUADOR",
    subtitle = "HT relacionados #CoronaVirus - cortes cada 24 horas",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

grid.arrange(tsgye, tsUIO, tsEC, ncol=2)

# Grafo de Relaciones
from_to <- htUEC %>% filter(created_at >= '2020-03-01 00:00:00') %>%
  mutate(menciones=purrr::map(.x=text,
                              pattern='@\\w+',
                              .f=str_extract_all)) %>% 
  select(screen_name,menciones,created_at) %>% 
  mutate(nombre_usuario=str_to_lower(paste0('@',screen_name))) %>% 
  unnest(menciones) %>% 
  unnest(menciones) %>% 
  mutate(menciones=str_to_lower(menciones))


### Agrupo y cuento cuantas interaciones tienen cada par de usuarios ####
grafo <- from_to %>% select(nombre_usuario,menciones) %>% 
  group_by(nombre_usuario,menciones) %>% 
  summarise(n=n()) %>% arrange(desc(n))

### Aqui selecciono las veces que ha interactuado las cuentas
grafo <- grafo %>% filter(n>6)

#### Creo el grafo ######
BP_graph <- 
  graph_from_data_frame(d = grafo,directed = TRUE)

## Gráfico Final
BP_graph %>%
  ggraph() +
  geom_edge_link(arrow = arrow(type = "closed", length = unit(1.5, "mm")),
                 aes(end_cap = label_rect(node2.name))) +
  geom_node_label(aes(label = name)) +
  theme_graph()

#Cuentas con más seguidores
htEC  %>% group_by(screen_name) %>% 
  summarise(seguidores = max(followers_count)) %>% 
  arrange(desc(seguidores)) %>% 
  select(screen_name, seguidores)%>% 
  distinct(screen_name,seguidores) %>% 
  top_n(15) %>% 
  ggplot(aes(reorder(screen_name, -seguidores),
             seguidores,label = seguidores))+
  geom_bar(stat="identity") +
  geom_label()+
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 13)) +
  theme(axis.text = element_text(size=9))+
  theme(axis.text.x = element_text(angle = 90))+
  labs(
    x = NULL, y = NULL,
    title = "Cuentas con más seguidores mes Marzo QUITO",
    subtitle = "Hashtags relacionados al #CoronaVirus",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# tweet más RT 
htUIO %>% 
  filter(!is_retweet) %>% 
  filter(retweet_count == max(retweet_count)) %>% 
  select(status_id,created_at,screen_name, retweet_count, followers_count, location, text) %>% View()

htEC %>% group_by(location) %>% dplyr::summarise(n=n()) %>% 
  dplyr::arrange(desc(n)) %>% top_n(20) %>% 
  ggplot(aes(location,n)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### TOPICOS #####

tweets <- htGYE$text

writeLines(as.character(htGYE$text[[1500]]))

tweets <- chartr('áéíóúñ','aeioun',tweets) # Quitar las tildes
tweets <- iconv(tweets, to = "ASCII", sub = "")  

tweets <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets)  # Remove the "RT" (retweet) and usernames 
tweets = gsub("http.+ |http.+$", " ", tweets)  # Remove html links
tweets = gsub("http[[:alnum:]]*", "", tweets)
tweets = gsub("[[:punct:]]", " ", tweets)  # Remove punctuation
tweets = gsub("[ |\t]{2,}", " ", tweets)  # Remove tabs
tweets = gsub("^ ", "", tweets)  # Leading blanks
tweets = gsub(" $", "", tweets)  # Lagging blanks
tweets = gsub(" +", " ", tweets) # General spaces 
tweets = gsub("[[:cntrl:]]", " ", tweets) # saltos de linea y tabulaciones
tweets = tolower(tweets) #convertimos todo a minúsculas
tweets = removeWords(tweets, words = stopwords("spanish"))
tweets = removePunctuation(tweets)
tweets = removeNumbers(tweets)
tweets = stripWhitespace(tweets)

writeLines(as.character(tweets[[1500]]))

tweets = unique(tweets)

corpus <- Corpus(VectorSource(tweets))

corpus <- tm_map(corpus, removeWords, stopwords("spanish"))  
corpus <- tm_map(corpus, removeNumbers)



corpus <- tm_map(corpus, stemDocument)
corpus = tm_map(corpus, removeWords, c("coronavirus","covid", "will",
                                       "coronavirusecuador","codvid19",
                                       "pandemia","emergenciasanitaria",
                                       "quedateencasa","toquedequedaecuador",
                                       "quedateenlacasa"))

writeLines(as.character(corpus[[1]]))
writeLines(as.character(htGYE$text[[1]]))


set.seed(1234)
palet  = brewer.pal(8, 'Dark2')
wordcloud(corpus, min.freq = 100, scale = c(4, 0.2) , 
          random.order = TRUE, col = palet)

dtm = DocumentTermMatrix(corpus)
dtm

doc.length = apply(dtm, 1, sum)
dtm = dtm[doc.length > 0,]
dtm

freq = colSums(as.matrix(dtm))
length(freq)

ord = order(freq, decreasing = TRUE)
(freq[head(ord, n = 20)])

findAssocs(dtm, "corona",0.2)

plot = data.frame(words = names(freq), count = freq)
plot = subset(plot, plot$count > 550) #creating a subset of words having more than 100 frequency
str(plot)
ggplot(data = plot, aes(reorder(words, -count),
                        count,label = count)) + 
  geom_bar(stat = 'identity') +coord_flip() +
  geom_label()+
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 13)) +
  theme(axis.text = element_text(size=9))+
  theme(axis.text.x = element_text(angle = 90))+
  labs(
    x = NULL, y = NULL,
    title = "Palabras más usadas Marzo GUAYAQUIL >550 veces",
    subtitle = "Hashtags relacionados al #CoronaVirus",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )



library(topicmodels)
#LDA model with 5 topics selected
lda_5 = LDA(dtm, k = 5, method = 'Gibbs', 
            control = list(nstart = 5, seed = list(1505,99,36,56,88), best = TRUE, 
                           thin = 500, burnin = 4000, iter = 2000))

#LDA model with 2 topics selected
lda_2 = LDA(dtm, k = 2, method = 'Gibbs', 
            control = list(nstart = 5, seed = list(1505,99,36,56,88), best = TRUE, 
                           thin = 500, burnin = 4000, iter = 2000))

#LDA model with 10 topics selected
lda_10 = LDA(dtm, k = 10, method = 'Gibbs', 
             control = list(nstart = 5, seed = list(1505,99,36,56,88), best = TRUE, 
                            thin = 500, burnin = 4000, iter = 2000))

#Top 10 terms or words under each topic
top10terms_5 = as.matrix(terms(lda_5,10))
top10terms_2 = as.matrix(terms(lda_2,10))
top10terms_10 = as.matrix(terms(lda_10,10))

top10terms_5

top10terms_2

top10terms_10 %>% view()

lda.topics_5 = as.matrix(topics(lda_5))
lda.topics_2 = as.matrix(topics(lda_2))
lda.topics_10 = as.matrix(topics(lda_10))
#write.csv(lda.topics_5,file = paste('LDAGibbs',5,'DocsToTopics.csv'))
#write.csv(lda.topics_2,file = paste('LDAGibbs',2,'DocsToTopics.csv'))
#write.csv(lda.topics_10,file = paste('LDAGibbs',10,'DocsToTopics.csv'))

summary(as.factor(lda.topics_5[,1]))

topicprob_5 = as.matrix(lda_5@gamma)
topicprob_2 = as.matrix(lda_2@gamma)
topicprob_10 = as.matrix(lda_10@gamma)

#write.csv(topicprob_5, file = paste('LDAGibbs', 5, 'DoctToTopicProb.csv'))
#write.csv(topicprob_2, file = paste('LDAGibbs', 2, 'DoctToTopicProb.csv'))
#write.csv(topicprob_10, file = paste('LDAGibbs', 10, 'DoctToTopicProb.csv'))

head(topicprob_10,10)
