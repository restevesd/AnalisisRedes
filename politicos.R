library(rtweet)
library(tidyverse)
library(lubridate)
library(igraph)
library(ggraph)

## authenticate via access token
token <- create_token(
  app = "DatosTW",
  consumer_key = '',
  consumer_secret = '',
  access_token = '',
  access_secret = '')

#Gobierno
users <- c("Salud_Ec","Lenin","mariapaularomo","monserratcream1","CataAndramuno",
           "ottosonnenh","DrJuanCZevallos","ComunicacionEc","alexocles")
usersTL <- get_timeline(users,n=3200)

usersTL <- usersTL %>% mutate(created_at = with_tz(created_at, tz = "America/Bogota") )

saveRDS(usersTL,file = "usersTL.rds")

usersTL %>% group_by(screen_name) %>%
  filter(created_at > '2020/02/01' ) %>% summarise(numero_tweets = n()) %>% 
  arrange(desc(numero_tweets)) %>% 
  ggplot(aes(reorder(screen_name, -numero_tweets),
             numero_tweets,label = numero_tweets)) +
  geom_bar(stat="identity") +
  geom_label()+
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 13)) +
  theme(axis.text = element_text(size=10))+
  labs(
    x = NULL, y = NULL,
    title = "Cantidad de posteos mes Febrero - Marzo",
    subtitle = "Gobierno ecuatoriano - Conteo por usuario",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# HT usados desde Febrero
tweets_tidyGobierno <- usersTL %>% unnest(hashtags)

tweets_tidyGobierno %>% filter(!is.na(hashtags)) %>% 
  filter(created_at > '2020/02/01' ) %>% 
  group_by(hashtags) %>% summarise(n=n()) %>% arrange(desc(n)) %>% 
  head(n=20L) %>% 
  ggplot(aes(x = reorder(hashtags,n), y = n)) +
  geom_col() +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 13)) +
  theme(axis.text = element_text(size=10))+
  labs(
    x = NULL, y = NULL,
    title = "Hashtag más utilizados mes Febrero - Marzo",
    subtitle = "Gobierno ecuatoriano",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  ) +
  coord_flip()

#Para crear nubes de palabras 
df_grouped <- tweets_tidyGobierno %>% filter(!is.na(hashtags)) %>%
  filter(created_at >= '2020/02/01' ) %>% 
  group_by(hashtags) %>% 
  summarise(total=n()) %>% 
  mutate(frecuencia = (total / n()) ) %>%
  arrange(desc(frecuencia))

df_grouped %>% filter(total > 10) %>% wordcloud2::wordcloud2()

#Distribución de Tweets por usuario
usersTL %>% filter(created_at > '2020/02/01' ) %>% 
  ggplot(aes(x =created_at, fill = screen_name)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
  facet_wrap(~screen_name, ncol = 2,scales="free") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Distribución de posteos mes Febrero - Marzo",
    subtitle = "Gobierno ecuatoriano - Distribución por usuario",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )


# Serie de tiempo evolución de tweets
usersTL %>% filter(created_at > '2020/02/01' ) %>%
  ts_plot("24 hours") +
  ggplot2::theme_minimal() +
  ggplot2::theme(plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frecuencia de posteos mes Febrero - Marzo",
    subtitle = "Gobierno ecuatoriano - cortes cada 24 horas",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

# Grafo de Relaciones
from_to_gobierno <- usersTL %>% filter(created_at >= '2020-02-01 00:00:00') %>%
  mutate(menciones=purrr::map(.x=text,
                              pattern='@\\w+',
                              .f=str_extract_all)) %>% 
  select(screen_name,menciones,created_at) %>% 
  mutate(nombre_usuario=str_to_lower(paste0('@',screen_name))) %>% 
  unnest(menciones) %>% 
  unnest(menciones) %>% 
  mutate(menciones=str_to_lower(menciones))


### Agrupo y cuento cuantas interaciones tienen cada par de usuarios ####
grafo <- from_to_gobierno %>% select(nombre_usuario,menciones) %>% 
  group_by(nombre_usuario,menciones) %>% 
  summarise(n=n()) %>% arrange(desc(n))

### Aqui selecciono las veces que ha interactuado las cuentas
grafo <- grafo %>% filter(n>9)

#### Creo el grafo ######
BP_graph <- 
  graph_from_data_frame(d = grafo,directed = TRUE)

plot(BP_graph)

plot(BP_graph, edge.color = "#3366ff", edge.arrow.size = 0.15, 
     vertex.color = "white", vertex.label.color = "black",
     vertex.size = 11)

plot(BP_graph, edge.color = "#3366ff", edge.arrow.size = 0.25, 
     vertex.color = "white", vertex.label.color = "black",
     vertex.size = 25)

plot(BP_graph, layout = layout.circle(BP_graph))

plot(BP_graph, layout = layout.grid(BP_graph))

BP_graph %>%
  ggraph() +
  geom_node_point() +
  theme_graph()

BP_graph %>%
  ggraph() +
  geom_edge_link() +
  theme_graph()

BP_graph %>%
  ggraph() +
  geom_edge_link() +
  geom_node_point() +
  theme_graph()

BP_graph %>%
  ggraph() +
  geom_edge_link() +
  geom_node_label(aes(label = name)) +
  theme_graph()

BP_graph %>%
  ggraph(layout = "linear") +
  geom_edge_arc() +
  geom_node_label(aes(label = name)) +
  theme_graph()

BP_graph %>%
  ggraph(layout = "circle") +
  geom_edge_link() +
  geom_node_label(aes(label = name)) +
  theme_graph()

## Gráfico Final
BP_graph %>%
  ggraph() +
  geom_edge_link(arrow = arrow(type = "closed", length = unit(1.5, "mm")),
                 aes(end_cap = label_rect(node2.name))) +
  geom_node_label(aes(label = name)) +
  theme_graph()

