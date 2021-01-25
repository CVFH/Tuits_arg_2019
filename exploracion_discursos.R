#exploracion_discursos.R

# en este archivo me voy quedando con lo que me sirve de cada tema

#####
# APERTURA DE LIBRERIAS #####

#paquetes

library(lubridate)
library(readxl)
library(ggplot2)
library(patchwork) # para unir graficos
require(tidytext) # para manipular texto
require(ggthemes)
library(wordcloud2) # para nubes de palabras
library(reshape2)
library(tm) # para DocumentTermMatrixs
library(topicmodels) # para modelado de topicos
library(ggbiplot) # para graficar PCA
library(igraph) # para grafos
library(pander)
library(tidyverse)
library(ggwordcloud)

#propias

source("Modules/tuitsCandidatos.R", encoding = "UTF-8")
source("Modules/funcionesGraficos.R", encoding = "UTF-8")




#####
# IMPORTACION DE DATOS #####

# ids

datos_base <- read_xlsx("Data/datos_base.xlsx")

# Datos tuiter 
# nos importan dataframes con tuits de candidatos: joined_gobernadores y joined_presid 

source("preparacion_datos_tuits.R", encoding = "UTF-8")
joined_presid <- traerDatos("presid")
joined_gobernadores <- traerDatos("gob")
joined_candidatos <- traerDatos("tot")


# TRANSFORMACIONES DE LOS DATOS 


# Tokenizando ######

# tokenizando por palabra

joined_presid_tokenizadas <- joined_presid %>% tokenizarTextoTuits() 
joined_gobernadores_tokenizadas <- joined_gobernadores %>% tokenizarTextoTuits() 

# tokenizando por bigramas
joined_presid_tokentweets <- joined_presid %>% tokenizarTextoTuits(tipo_token = "tweets") 
joined_gobernadores_tokentweets <- joined_gobernadores %>% tokenizarTextoTuits(tipo_token = "tweets")

# tokenizado especial para tuits
joined_presid_bigramas <- joined_presid %>% tokenizarTextoTuits(tipo_token = "ngrams") 
joined_gobernadores_bigramas <- joined_gobernadores %>% tokenizarTextoTuits(tipo_token = "ngrams")

# agregando bases ####

joined_candidatos <- joined_candidatos %>% 
  left_join(datos_base)

candidatos_tokenizadas <- rbind(joined_gobernadores_tokenizadas,
                                joined_presid_tokenizadas) %>% 
  left_join(datos_base)


candidatos_tokentweets <- rbind(joined_presid_tokentweets,
                                joined_gobernadores_tokentweets)%>% 
  left_join(datos_base) 



#####
# EXPLORANDO DATOS + PLOTEANDO DATOS
#####
# Lineas de tiempo ######

# este grafico es hermoso
linea_fecha <- ggplot(joined_candidatos %>%  
                         filter(year(created_at) == 2019 ) %>%  
                         arrange(tipo_fecha), 
                       aes(x = date(created_at), fill = tipo_fecha)) +
  geom_histogram(position = "identity", bins = 24, alpha = 0.5)  +
  facet_wrap(~Cargo, ncol = 2)

# pendiente: ver si la baja pos elecciones corresponde al perdedor (ranking. buscar segundos)

source("preparacion_datos_electorales.R", encoding = "UTF-8")
#gobernadores
linea_ranking_gobernadores <- joined_candidatos %>% 
  left_join(votos_totales) %>% 
  mutate(ganador = ifelse(Ranking==1, "ganador", "perdedor")) %>% 
  filter(year(created_at) == 2019 & Cargo == "Gobernador") %>%
  ggplot(aes(x = date(created_at), fill = as.factor(ganador))) +
  geom_histogram(position = "identity", bins = 24, alpha = 0.5)  +
  facet_wrap(~tipo_fecha, ncol = 2)

#presid
linea_ranking_presid <- joined_candidatos %>% 
  left_join(votos_totales) %>% 
  mutate(ganador = ifelse(Ranking==1, "ganador", "perdedor")) %>% 
  filter(year(created_at) == 2019 & Cargo == "Presidente") %>%
  ggplot(aes(x = date(created_at), fill = as.factor(ganador))) +
  geom_histogram(position = "identity", bins = 24, alpha = 0.5) 

# patchwork
patchowrk_ranking <- linea_ranking_gobernadores / linea_ranking_presid

# podriamos sumarle rts?

rts_dia <- joined_candidatos %>%  
  filter(year(created_at) == 2019 ) %>%  
  mutate(dia= date(created_at)) %>% 
  mutate(dia= as.factor(dia)) %>% 
  group_by(dia, tipo_fecha, Cargo) %>% 
  dplyr::summarise(rts_dia = sum(rts))


linea_rts <- ggplot(rts_dia, 
                       aes(x = dia, y = rts_dia, fill = tipo_fecha)) +
  geom_col(alpha = 0.5)  +
  facet_wrap(~Cargo, ncol = 2)


#####
# Cantidad de palabras ######

# Gobernadores

gobernadores_sintesis1 <- joined_gobernadores_tokenizadas %>% 
  group_by(screen_name, tweet_id) %>% 
  dplyr::mutate(cantidad_palabras_tuit =  dplyr::n()) %>% 
  ungroup() %>% 
  group_by(screen_name) %>% 
  dplyr::summarise(palabras_promedio_tuit = mean(cantidad_palabras_tuit),
            palabras_totales_emitidas = dplyr::n())

gobernadores_sintesis2 <-joined_gobernadores %>% 
  subset(Campaña ==1) %>% 
  group_by(screen_name) %>% 
  dplyr::summarise(cantidad_tuits_emitidos = n())

gobernadores_sintesis <- left_join(gobernadores_sintesis1, gobernadores_sintesis2)

gobernadores_promediopalabras_cantidadtuits <- plotPointText(gobernadores_sintesis, 
                                                             aes(cantidad_tuits_emitidos,
                                                                                   palabras_promedio_tuit,
                                                                                   colour=screen_name,
                                                                 size= palabras_totales_emitidas), 
                                                             aes(label= screen_name) ) +
  geom_point()


gobernadores_promediopalabras_cantidadtuits_formateado <- formatPlot(gobernadores_promediopalabras_cantidadtuits,
                                                                     "Elocuencia de los candiatos a gobernador",       
                                                                     "Fuente: elaboración propia")  

# Presid

presid_sintesis1 <- joined_presid_tokenizadas %>% 
  group_by(screen_name, tweet_id) %>% 
  dplyr::mutate(cantidad_palabras_tuit =  n()) %>% 
  ungroup() %>% 
  group_by(screen_name) %>% 
  dplyr::summarise(palabras_promedio_tuit = mean(cantidad_palabras_tuit),
            palabras_totales_emitidas = n())

presid_sintesis2 <-joined_presid %>% 
  subset(Campaña ==1) %>% 
  group_by(screen_name) %>% 
  dplyr::summarise(cantidad_tuits_emitidos = n())

presid_sintesis <- left_join(presid_sintesis1, presid_sintesis2)
 
presid_promediopalabras_cantidadtuits <-plotPointText(presid_sintesis, 
                                                      aes(cantidad_tuits_emitidos,
                                                          palabras_promedio_tuit,
                                                          colour= screen_name),
                                                      aes(label=screen_name)) 





#####
# Nubes de palabras #####

# comparando por distrito

matriz_cargos <- candidatos_tokenizadas %>%
  limpiarTokens(palabras_web = TRUE, hashtags = TRUE, mentions = TRUE) %>%  
  group_by(Cargo, tokens) %>% 
  dplyr::mutate(n =  dplyr::n()) %>% 
  acast(tokens ~ Cargo, value.var = "n", fill = 0)

wordcloud::comparison.cloud(matriz_cargos,
                            colors = c("lightblue", "blue"),
                            max.words = 100,
                            title.size=NULL)

#obvio que el debate pq participaron todxs
# territorialidad de los provincianos. barrio vecino. encuentro


#####
# tf-idf   ######

# todo esto me gusta. presentar ordenadamente
#The idea of tf-idf is 
# to find the important words 
# for the content of each document 
# by decreasing the weight for commonly used words 
# and increasing the weight for words
# that are not used very much in a collection or corpus of documents,
# 
# Calculating tf-idf attempts to find the words that are important (i.e., common) in a text, 
# but not too common. Let’s do that now.

# PENDIENTE: LIMPIAR BIGRAMAS ANTES DEL ANALISIS

# tdf_idf / gobernadores / token = words

joined_gobernadores_tokenizadas_tfidf <- joined_gobernadores_tokenizadas %>%
  dplyr::count(screen_name, tokens, sort = TRUE) %>% 
  limpiarTokens(palabras_web = TRUE) %>%  
  bind_tf_idf(tokens, screen_name, n)

sliced_gobs_tfidf <- joined_gobernadores_tokenizadas_tfidf %>%
  group_by(screen_name) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup()  %>% 
  mutate(tokens = reorder(tokens, tf_idf)) %>% 
  left_join(datos_base) %>% 
  group_by(screen_name) %>% 
  slice_head(n=15) %>% 
  ungroup()

plot_gobs_tfidf <- ggplot(sliced_gobs_tfidf,
       aes(tokens, tf_idf, fill = Distrito)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~screen_name, ncol = 5, scales = "free") +
  coord_flip() +
  scale_x_reordered() + 
  theme(strip.text = element_text(size=8),
        axis.text.y = element_text(size = 6, hjust = 0.8))


plot_gobs_tfidf_formateado <- formatPlot(plot_gobs_tfidf, 
                                         "Palabras con mayor frecuencia")

# tdf_idf / gobernadores /  tokens = bigramas

joined_gobernadores_bigramas_tfidf <- joined_gobernadores_bigramas %>%
  limpiarTokens(bigramas = TRUE)  %>% 
  dplyr::count(screen_name, tokens, sort = TRUE) %>% 
  bind_tf_idf(tokens, screen_name, n)

sliced_gobs_tfidf_bigramas <- joined_gobernadores_bigramas_tfidf %>%
  subset( !(str_detect(tokens, "enlacetuit" ))) %>%
  mutate(tokens = str_replace_all(tokens, "mention", "@")) %>% 
  mutate(tokens = str_replace_all(tokens, "hashtag", "#")) %>% 
  group_by(screen_name) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup()  %>% 
  mutate(tokens = reorder(tokens, tf_idf)) %>% 
  left_join(datos_base) %>% 
  group_by(screen_name) %>% 
  slice_head(n=15) %>% 
  ungroup()

plot_gobs_tfidf_bigramas <- ggplot(sliced_gobs_tfidf_bigramas,
                          aes(tokens, tf_idf, fill = Distrito)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~screen_name, ncol = 5, scales = "free") +
  coord_flip() +
  scale_x_reordered() + 
  theme(strip.text = element_text(size=8),
        axis.text.y = element_text(size = 6, hjust = 0.8))

# tdf_idf / gobernadores / tokens = tweets


joined_gobernadores_tokentweets_tfidf <- joined_gobernadores_tokentweets %>%
 limpiarTokens(palabras_web = TRUE) %>%  
  dplyr::count(screen_name, tokens, sort = TRUE) %>% 
  bind_tf_idf(tokens, screen_name, n)

sliced_gobs_tfidf_tokentweet <- joined_gobernadores_tokentweets_tfidf %>%
  subset( !(str_detect(tokens, "enlacetuit" ))) %>% 
  group_by(screen_name) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup()  %>% 
  mutate(tokens = reorder(tokens, tf_idf)) %>% 
  left_join(datos_base) %>% 
  group_by(screen_name) %>% 
  slice_head(n=15) %>% 
  ungroup()

plot_gobs_tfidf_tokentweet <- ggplot(sliced_gobs_tfidf_tokentweet ,
                                   aes(tokens, tf_idf, fill = Distrito)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~screen_name, ncol = 5, scales = "free") +
  coord_flip() +
  scale_x_reordered() + 
  theme(strip.text = element_text(size=8),
        axis.text.y = element_text(size = 6, hjust = 0.8))

##
# presidente
##

# tdf_idf / presidente / tokens = words

joined_presid_tokenizadas_tfidf <- joined_presid_tokenizadas %>%
  limpiarTokens(palabras_web = TRUE) %>% 
  dplyr::count(screen_name, tokens, sort = TRUE) %>% 
  bind_tf_idf(tokens, screen_name, n) 

sliced_presid_tfidf <- joined_presid_tokenizadas_tfidf %>% 
  group_by(screen_name) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>% 
  mutate(tokens = reorder(tokens, tf_idf)) #ordenamos 

plot_presid_tfidf <- ggplot(sliced_presid_tfidf,
                          aes(tokens, tf_idf, fill = screen_name)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~screen_name, ncol = 2, scales = "free") +
  coord_flip() +
  scale_x_reordered()+ 
  theme(strip.text = element_text(size=8),
        axis.text.y = element_text(size = 7, hjust = 0.8))


# tdf_idf / presidente / tokens = tweets

joined_presid_tokentweets_tfidf <- joined_presid_tokentweets %>%
  limpiarTokens(palabras_web = TRUE) %>% 
  dplyr::count(screen_name, tokens, sort = TRUE) %>% 
  bind_tf_idf(tokens, screen_name, n)

sliced_presid_tfidf_tokentweet <- joined_presid_tokentweets_tfidf %>%
  group_by(screen_name) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup()  %>% 
  mutate(tokens= reorder(tokens, tf_idf)) %>% 
  left_join(datos_base) %>% 
  group_by(screen_name) %>% 
  slice_head(n=15) %>% 
  ungroup()

plot_presid_tfidf_tokentweet <- ggplot(sliced_presid_tfidf_tokentweet ,
                                     aes(tokens, tf_idf, fill = screen_name)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~screen_name, ncol = 2, scales = "free") +
  coord_flip() +
  scale_x_reordered() + 
  theme(strip.text = element_text(size=8),
        axis.text.y = element_text(size = 7, hjust = 0.8))


# tdf_idf / presidente /tokens = bigramas

joined_presid_bigramas_tfidf <- joined_presid_bigramas %>%
  limpiarTokens(bigramas = TRUE)  %>% 
  dplyr::count(screen_name, tokens, sort = TRUE) %>% 
  bind_tf_idf(tokens, screen_name, n)

sliced_presid_tfidf_bigramas <- joined_presid_bigramas_tfidf %>%
  subset( !(str_detect(tokens, "enlacetuit" ))) %>% 
  mutate(tokens = str_replace_all(tokens, "mention", "@")) %>% 
  mutate(tokens = str_replace_all(tokens, "hashtag", "#")) %>% 
  group_by(screen_name) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup()  %>% 
  mutate(tokens= reorder(tokens, tf_idf)) %>% 
  left_join(datos_base) %>% 
  group_by(screen_name) %>% 
  slice_head(n=15) %>% 
  ungroup()

plot_presid_tfidf_bigramas <- ggplot(sliced_presid_tfidf_bigramas,
                                   aes(tokens, tf_idf, fill = screen_name)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~screen_name, ncol = 2, scales = "free") +
  coord_flip() +
  scale_x_reordered() + 
  theme(strip.text = element_text(size=8),
        axis.text.y = element_text(size = 7, hjust = 0.8)) 

# y una de los candidatos para ver

joined_candidatos_tokenizadas_tfidf <- candidatos_tokenizadas %>%
  limpiarTokens(palabras_web = TRUE) %>% 
  dplyr::count(screen_name, tokens, sort = TRUE) %>% 
  bind_tf_idf(tokens, screen_name, n) 

sliced_candidatos_tfidf <- joined_candidatos_tokenizadas_tfidf %>% 
  group_by(screen_name) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>% 
  mutate(tokens = reorder(tokens, tf_idf)) %>%  #ordenamos 
  left_join(datos_base)

plot_candidatos_tfidf <- ggplot(sliced_candidatos_tfidf,
                            aes(tokens, tf_idf, fill = Cargo)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~screen_name, ncol = 3, scales = "free") +
  coord_flip() +
  scale_x_reordered()+ 
  theme(strip.text = element_text(size=8),
        axis.text.y = element_text(size = 7, hjust = 0.8))



#####
# Menciones 
# PREPARACION DE DATOS. #####
candidatos_menciones <- joined_candidatos %>% 
  subset(Campaña==1) %>% 
  mutate(mention_screen_names = as.character(mention_screen_names)) %>% 
  unnest_tokens(tokens, mention_screen_names) %>% 
  subset(!is.na(tokens)) 

cuenta_menciones <- candidatos_menciones %>% 
  dplyr::count(screen_name, tokens)

cuenta_menciones_mutuas <- candidatos_menciones %>% 
  subset(tokens %in% datos_base$screen_name) %>%
  dplyr::count(screen_name, tokens)

cuenta_mencionados <- cuenta_menciones %>% 
  dplyr::group_by(tokens) %>% 
  dplyr::summarise(veces_totales_mencionado= sum(n)) 

cuenta_menciones <- cuenta_menciones %>% 
  left_join(cuenta_mencionados)

cuenta_menciones_filtrado <- cuenta_menciones %>% 
  filter(veces_totales_mencionado > 1)

# GRAFOS
## cualquier mencion #####

grafo_menciones_dirigido_df <- graph.data.frame(d = cuenta_menciones_filtrado,directed = T)

V(grafo_menciones_dirigido_df)$candidato <- as.factor(ifelse(V(grafo_menciones_dirigido_df)$name %in% datos_base$screen_name, "candidato", "otrx"))


colrs <- c("cornflowerblue", "darkgray" )

V(grafo_menciones_dirigido_df)$color <- colrs[V(grafo_menciones_dirigido_df)$candidato]

coords <- layout_in_circle(grafo_menciones_dirigido_df, order =
                             order(V(grafo_menciones_dirigido_df)))

plot.igraph(grafo_menciones_dirigido_df,
            #vertex.size=degree(grafo_menciones_dirigido_df,mode = "in")*2, # Tamaño de nodo
            edge.arrow.size=0.2, # tamaño de flecha de la arista
            edge.arrow.width=0.8, # ancho de flecha de la arista
            edge.color= "azure1" ,# color de arista
            # edge.curved = T, # arista curva,
            vertex.label.dist=2,
            vertex.label.cex=degree(grafo_menciones_dirigido_df,mode = "in")*0.3,
            #vertex.label.cex=0.4, # tamaño de las etiquetas de los nodos
            #main="Cuentas que recibieron más menciones", 
            vertex.label.color = V(grafo_menciones_dirigido_df)$color , # color de etiquetas de nodos
            #vertex.label.family="arial",
            vertex.shape="none", 
            vertex.label=V(grafo_menciones_dirigido_df)$name,
            #layout = coords
            rescale = FALSE, ylim=c(-5,12),xlim=c(-5,11)#, asp = 0
)


# hubs y authorities 

hs <- hub_score(grafo_menciones_dirigido_df, weights=NA)$vector

as <- authority_score(grafo_menciones_dirigido_df, weights=NA)$vector

par(mfrow=c(1,2))

plot(grafo_menciones_dirigido_df, 
    # vertex.size=hs*50, 
     vertex.label.cex=degree(grafo_menciones_dirigido_df,mode = "in")*hs*0.2,
    vertex.shape="none",
    main="Hubs")

plot(grafo_menciones_dirigido_df, 
     vertex.label.cex=degree(grafo_menciones_dirigido_df,mode = "in")*as*0.2,
     vertex.shape="none",
     #vertex.size=as*30, 
     main="Authorities")


## menciones mutuas ######

# grafo dirigido

grafo_mutuas_dirigido_df <- graph.data.frame(d = cuenta_menciones_mutuas,directed = T)

plot.igraph(grafo_mutuas_dirigido_df,
            #vertex.size=degree(gd,mode = "in")*2, # Tamaño de nodo
            edge.arrow.size=0.2, # tamaño de flecha de la arista
            edge.arrow.width=0.8, # ancho de flecha de la arista
            edge.color="grey50", # color de arista
            #edge.curved = T, # arista curva
            vertex.label.cex=0.8, # tamaño de las etiquetas de los nodos
            #main="Candidatos que recibieron más menciones", 
            vertex.label.color="navy", # color de las etiquetas de los nodos
            vertex.size=degree(grafo_mutuas_dirigido_df,mode = "in")*5,
            #vertex.shape = "none",
            vertex.color="azure1", # color de nodos
            vertex.label.family="arial",
            vertex.frame.color= "grey80"
)


#####
# Hashtags #####

candidatos_hashtags <- joined_candidatos %>% 
  tokenizarTextoTuits() %>% 
  subset(str_detect(tokens, "(hashtag)")) %>% 
  mutate(hashtags = str_replace(tokens, "(hashtag)", "#")) %>% 
  select(-c(tokens)) %>% 
  dplyr::count(screen_name, hashtags) %>% 
  left_join(datos_base) %>% 
  group_by(screen_name) %>%
  slice_max(n, n = 5) 

# plot por separado para claridad visual

# de gobernadores

plot_gobernador_hashtags <- ggplot(candidatos_hashtags %>% filter(Cargo=="Gobernador"), 
                                   aes(label = hashtags, 
                                       size = n,
                                       colour= Distrito)) +
  geom_text_wordcloud() +
  facet_wrap(~screen_name, ncol = 3) +
  scale_size_area(max_size = 4) + 
  theme_minimal()
#quizas hacer tablita o shiny para complementar nube de gobernadores

# de presid

plot_presid_hashtags <- ggplot(candidatos_hashtags %>% filter(Cargo=="Presidente"), 
                                   aes(label = hashtags, 
                                       size = n,
                                       colour=screen_name)) +
  geom_text_wordcloud() +
  facet_wrap(~screen_name, ncol = 3) +
  scale_size_area(max_size = 5) + 
  theme_minimal()

#####
# Temas por palabras ########

temas_palabras <- read_xlsx("Data/temas_palabras.xlsx")

temas_palabras_match_tokens <- candidatos_tokenizadas %>% 
  limpiarTokens(palabras_web = TRUE, hashtags = TRUE, mentions = TRUE) %>% 
  select(screen_name, tweet_id, tokens)

# calculando coincidencias

for (columna in 1:ncol(temas_palabras)) {
  
  testear_coincidencias <- na.omit(as.data.frame(temas_palabras[columna])) %>% 
    rename( palabras = colnames(temas_palabras[columna]) )
  
  new <- ifelse( temas_palabras_match_tokens$tokens %in% testear_coincidencias$palabras, 
                 "1", 
                 "0")
  
  temas_palabras_match_tokens[ , ncol(temas_palabras_match_tokens) + 1] <- new                  # Append new column
  
  colnames(temas_palabras_match_tokens)[ncol(temas_palabras_match_tokens)] <- colnames(temas_palabras[columna])  # Rename column name
}

temas_palabras_match_tokens_long <- temas_palabras_match_tokens %>%
  pivot_longer(!c(screen_name, tweet_id, tokens), # una fila para cada combinación tuit/token/tema
               names_to = "temas", values_to = "count") %>%  # con una columna (count) que indica si está presente el tema en ese token-tuit
  filter(count==1) # nos quedamos sólo con los tokens asignados a un tema

# inspeccionando resultados / primera aproximacion

# cantidad de coincidencias por tuit
coincidencias_tweets <- temas_palabras_match_tokens_long %>% 
  group_by(tweet_id) %>% 
  dplyr::summarise(cantidad_coincidencias = sum(as.integer(count))) %>% 
  left_join(joined_candidatos)

# cantidad de coincidencias por tema por tuit
ncoincidencias_tema_tweets <- left_join(joined_candidatos %>% 
                                          filter(Campaña == 1 ),
                                        temas_palabras_match_tokens_long %>% 
                                          dplyr::count(tweet_id, temas) 
                                        ) %>% 
  select(tweet_id, screen_name, text, temas, n) %>% 
  dplyr::rename(coincidencias_tema_tuit = "n")

# ejemplo: tuit con muchas coincidencias sobre un mismo tema
ejemplo1 <- ncoincidencias_tema_tweets %>%  
  arrange(desc(coincidencias_tema_tuit)) %>% 
  head(1)

print(ejemplo1)

# cantidad de temas por tuit
ntemas_tweets <- temas_palabras_match_tokens_long %>% 
  dplyr::count(tweet_id, temas) %>% 
  dplyr::mutate( tweet_id = as.factor(tweet_id))

# unimos con base de datos
cantidad_temas_tuit <- left_join(joined_candidatos %>% 
                                   filter(Campaña == 1 ),
                                 fct_count(ntemas_tweets$tweet_id) %>% 
                                   dplyr::rename(tweet_id = "f")) %>% 
  select(tweet_id, screen_name, text, n) %>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .))) %>% 
  dplyr::rename(cantidad_temas_tuit = "n")


# ejemplo tuit con muchos temas

ejemplo2 <- cantidad_temas_tuit  %>%  
  arrange(desc(cantidad_temas_tuit)) %>% 
  head(1)

print(ejemplo2)

# histograma descriptivo / cantidad de temas por tuit

plot_cantidad_temas_tuit <- cantidad_temas_tuit %>% 
  ggplot(aes(cantidad_temas_tuit)) +
  geom_bar()

# tuits con dos temas

tuits_con_dos_temas <- ncoincidencias_tema_tweets %>% 
  left_join(cantidad_temas_tuit) %>% 
  filter(cantidad_temas_tuit == 2 ) %>% 
  group_by(tweet_id) %>% 
  summarise( tema1 = max(temas),
             tema2 = min(temas)) 

temas_juntos <- tuits_con_dos_temas %>%  
  dplyr::count(tema1, tema2) %>% 
  arrange(desc(n))

head(temas_juntos, 10)

tuits_con_unico_tema <- ncoincidencias_tema_tweets %>% 
  left_join(cantidad_temas_tuit) %>% 
  filter(cantidad_temas_tuit == 1 )

temas_en_tuits_con_unico_tema <- fct_count(tuits_con_unico_tema$temas)


# temas mas populares

temas_populares <- fct_count(ncoincidencias_tema_tweets$temas)
# hacer tabla
# se ve tambien tuits sin clasificar

# temas preferidos por candidato

temas_candidatos <- ncoincidencias_tema_tweets %>% 
  dplyr::count(screen_name, temas) %>% 
  na.omit(n)

plot_temas_candidatos <- temas_candidatos %>% 
  ggplot(aes(screen_name, temas, size= n, colour = screen_name)) +
           geom_count() 


# probar con cosito con flujos 
# hacer un facet wrap por distrito / cargo etc 

#####
# Topic modeling ######
# PROBANDO CON 10 TEMAS #####
# cuarto intento. con bigramas
# EL MAS INTERESANTE HASTA AHORA. PARECE HABER UN TOPICO 1 MAS PERONISTA/IZQ, Y OTRO MAS MACRISTA/DERECHA
# SEGUIR PROBANDO
# POR ALGUN MOTIVO MUCHOS NA/NA

#PROBANDO: CON 10 TEMAS
# preparando datos
candidatos_tokens_dtm <- joined_candidatos %>% 
  tokenizarTextoTuits(tipo_token = "ngrams") %>% 
  limpiarTokens(bigramas = TRUE, palabras_web = TRUE, hashtags = TRUE, mentions = TRUE) %>% 
  dplyr::count(tweet_id, tokens) %>%
  cast_dtm(tweet_id, tokens, n)

candidatos_tokens_LDA <- LDA(candidatos_tokens_dtm, k = 10, control = list(seed = 1234))

#per-topic-per-word probabilities

candidatos_tokens_topics <- tidy(candidatos_tokens_LDA, matrix = "beta")

candidatos_tokens_topics_top_terms <- candidatos_tokens_topics %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

candidatos_tokens_topics_top_terms_grafico <- candidatos_tokens_topics_top_terms  %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() 

# per-document-per-topic probabilities,  γ (“gamma”).


candidatos_tuits_topics <- tidy(candidatos_tokens_LDA, matrix = "gamma") %>% 
  dplyr::rename(tweet_id = "document") %>% 
  left_join(joined_candidatos) %>% 
  select(tweet_id, topic, gamma, screen_name, fav_count, rts, created_at)

candidatos_tuits_topics_top <- candidatos_tuits_topics %>% 
  group_by(tweet_id) %>% 
  dplyr::summarise(gamma = max(gamma)) %>% 
  left_join(candidatos_tuits_topics) %>% 
  left_join(datos_base)

#graficando.

#agrupados por  screen name # hay diferencias, dificil sistematizarlas
candidatos_tuits_topics_top_grouped <- candidatos_tuits_topics_top %>% 
  dplyr::count(screen_name, topic)

candidatos_tuits_topics_top_grouped %>% 
  ggplot(aes(topic, n, colour= screen_name)) +
  geom_point()  +
  facet_wrap(~ screen_name) 

#agrupados por cargo # V INTERESTING!!!!!!!!!
candidatos_tuits_topics_top_grouped <- candidatos_tuits_topics_top %>% 
  dplyr::count(Cargo, topic)

candidatos_tuits_topics_top_grouped %>% 
  ggplot(aes(topic, n, colour= Cargo)) +
  geom_point() 


#agrupados por distrito # hay diferencias, dificil sistematizarlas
candidatos_tuits_topics_top_grouped <- candidatos_tuits_topics_top %>% 
  dplyr::count(Distrito, topic)

candidatos_tuits_topics_top_grouped %>% 
  ggplot(aes(topic, n, colour= Distrito)) +
  geom_point() +
  facet_wrap(~ Distrito)

#agrupados por tipo_fecha # de nueboo v interesting

candidatos_tuits_topics_top_grouped <- candidatos_tuits_topics_top %>% 
  dplyr::count(tipo_fecha, topic)

candidatos_tuits_topics_top_grouped %>% 
  ggplot(aes(topic, n, colour= tipo_fecha)) +
  geom_point() 


#graficando por candidato
candidatos_tuits_topics_top %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ screen_name) +
  labs(x = "topic", y = expression(gamma))


#graficando por distrito
candidatos_tuits_topics_top %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ Distrito) +
  labs(x = "topic", y = expression(gamma))

#graficando por Cargo
candidatos_tuits_topics_top %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ Cargo) +
  labs(x = "topic", y = expression(gamma))

#graficando por tipo de eleccion
candidatos_tuits_topics_top %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ tipo_fecha) +
  labs(x = "topic", y = expression(gamma))

# PROBANDO: CON 3 TEMAS ####
# preparando datos
candidatos_tokens_dtm3 <- joined_candidatos %>% 
  tokenizarTextoTuits(tipo_token = "ngrams") %>% 
  limpiarTokens(bigramas = TRUE, palabras_web = TRUE, hashtags = TRUE, mentions = TRUE) %>% 
  dplyr::count(tweet_id, tokens) %>%
  cast_dtm(tweet_id, tokens, n)

candidatos_tokens_LDA3 <- LDA(candidatos_tokens_dtm3, k = 3, control = list(seed = 1234))

#per-topic-per-word probabilities

candidatos_tokens_topics3 <- tidy(candidatos_tokens_LDA3, matrix = "beta")

candidatos_tokens_topics_top_terms3 <- candidatos_tokens_topics3 %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

candidatos_tokens_topics_top_terms_grafico3 <- candidatos_tokens_topics_top_terms3  %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() 

# per-document-per-topic probabilities,  γ (“gamma”).


candidatos_tuits_topics3 <- tidy(candidatos_tokens_LDA3, matrix = "gamma") %>% 
  dplyr::rename(tweet_id = "document") %>% 
  left_join(joined_candidatos) %>% 
  select(tweet_id, topic, gamma, screen_name, fav_count, rts, created_at)

candidatos_tuits_topics_top3 <- candidatos_tuits_topics3 %>% 
  group_by(tweet_id) %>% 
  dplyr::summarise(gamma = max(gamma)) %>% 
  left_join(candidatos_tuits_topics3 ) %>% 
  left_join(datos_base)

candidatos_tuits_topics_top3 %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ screen_name) +
  labs(x = "topic", y = expression(gamma))


#####
# PCA 
## primer intento. una porqueria ####
# PRIMER INTENTO. MAS O MENOS. A MEJORAR: 
# PROBAR CON PALABRAS EN LUGAR DE TOKENS
# VER SI HAY QUE HACER ALGO MAS CON EL INPUT DE DATOS
#primero paso a document term matrix
#CLARAMENTE C COMPONENTE ES UN SCREEN NAME. 
# ES DECIR LAS PALABRAS DE C U SON MUY UNICAS:
#   PUEDE SER POR LOS HASHTAGS: 
#   PROBAR FILTRANDO ESO; 
# LAS ARROBAS; 
# Y USANDO PALABRAS EN LUGAR DE TWW; 
# FILTRAR LOS ENLACES
# TB PROBAR C TF IDF

candidatos_tokentweet_dtm <- candidatos_tokentweets %>% 
  limpiarTokens() %>% 
 # group_by(screen_name, tokens) %>% 
  count(screen_name, tokens) %>% 
 # ungroup() %>% 
  cast_dtm(screen_name, tokens, n)

# siguiendo los pasos de:
# https://cmdlinetips.com/2019/04/introduction-to-pca-with-r-using-prcomp/
pca_res <- prcomp(candidatos_tokentweet_dtm, scale=TRUE)
summary(pca_res)
pca_res$x[1:5,1:3]
pca_res$center[1:5]
head(pca_res$scale^2, n=5)
var_explained <- pca_res$sdev^2/sum(pca_res$sdev^2)
var_explained[1:5]
pca_res$x %>% 
  as.data.frame %>%
  rownames_to_column("screen_name") %>%
  ggplot(aes(x=PC1,y=PC2)) + 
  geom_point(aes(color=screen_name),size=4) +
  geom_text(aes(label=screen_name)) +
 # geom_label(aes(fill = continent), colour = "white", fontface = "bold") +
 # theme_bw(base_size=32) + 
  labs(x=paste0("PC1: ",round(var_explained[1]*100,1),"%"),
       y=paste0("PC2: ",round(var_explained[2]*100,1),"%")) +
  theme(legend.position="left")


# PROBAR TUNEANDO GRAFICOS TUQ

pca_rep <- data_frame(txt = discursos_texto$txt,
                      discurso = discursos_texto$discursos,
                      presidente = discursos_texto$presidente,
                      pc1 = pca_res$x[,1],
                      pc2 = pca_res$x[,2],
                      clust_id = as.factor(km_clust$cluster))
library(ggrepel)
ggplot(data = pca_rep, mapping = aes(x = pc1, y = pc2, color = clust_id)) +
  #scale_color_manual(values = menta) +
  geom_text_repel(mapping = aes(label = discurso), size = 2, fontface = 'bold') +
  labs(title = 'K-Means Cluster: 5 clusters on PCA Features',
       x = '',
       y = '') +
  theme_minimal()+
  theme(legend.position = 'none',
        axis.text.x = element_blank(),
        axis.text.y=element_blank())f


## segundo  . MUERTE #######

candidatos_tokentweet_dtmv2 <- candidatos_tokentweets %>% 
  limpiarTokens() %>% 
 # group_by(tweet_id, tokens) %>% 
  count(tweet_id, tokens) %>% 
 # ungroup() %>% 
  cast_dtm(tweet_id, tokens, n)

# siguiendo los pasos de:
# https://cmdlinetips.com/2019/04/introduction-to-pca-with-r-using-prcomp/
pca_resv2 <- prcomp(candidatos_tokentweet_dtmv2, scale=TRUE)
summary(pca_resv2)
pca_resv2$x[1:5,1:3]
pca_resv2$center[1:5]
head(pca_resv2$scale^2, n=5)
var_explained <- pca_resv2$sdev^2/sum(pca_resv2$sdev^2)
var_explained[1:5]

aver <- pca_resv2$x %>% 
  as.data.frame %>%
  rownames_to_column("tweet_id") %>%
  left_join(joined_candidatos) # %>% 
  ggplot(aes(x=PC1,y=PC2)) #+ 
  geom_point(aes(color=screen_name),size=4) +
  #geom_text(aes(label=screen_name)) +
  # geom_label(aes(fill = continent), colour = "white", fontface = "bold") +
  # theme_bw(base_size=32) + 
  labs(x=paste0("PC1: ",round(var_explained[1]*100,1),"%"),
       y=paste0("PC2: ",round(var_explained[2]*100,1),"%")) +
  theme(legend.position="left")
  
  
## Tercer intento. haciendo mitad tm y mitad tidytext no funca, una porqueria ####
  
candidatos_dtmv3 <- candidatos_tokenizadas %>% 
    limpiarTokens(palabras_web = TRUE) %>% 
    select(tweet_id, tokens) %>% 
    subset(str_length(tokens) > 2 & !(tokens == "no")) 
  
candidatos_dtmv3 <- candidatos_dtmv3 %>% 
  group_by(tweet_id, tokens) %>% 
  count() %>% 
    cast_dtm(tweet_id, tokens, freq)

candidatos_sparse_dtmv3 <- removeSparseTerms(candidatos_dtmv3, 0.99)

candidatos_sparse_dtmv3

  # siguiendo los pasos de:
  # https://cmdlinetips.com/2019/04/introduction-to-pca-with-r-using-prcomp/
pca_resv3 <- prcomp(candidatos_sparse_dtmv3, scale=TRUE)

#una poronga. me da como 400 componentes de resultado

  summary(pca_resv3)
  pca_resv3$x[1:5,1:3]
  pca_resv3$center[1:5]
  head(pca_resv3$scale^2, n=5)
  var_explained <- pca_resv3$sdev^2/sum(pca_resv3$sdev^2)
  var_explained[1:5]
  
  aver <- pca_resv3$x %>% 
    as.data.frame %>%
    rownames_to_column("tweet_id") %>%
    left_join(joined_candidatos) # %>% 
  ggplot(aes(x=PC1,y=PC2)) #+ 
  geom_point(aes(color=screen_name),size=4) +
    #geom_text(aes(label=screen_name)) +
    # geom_label(aes(fill = continent), colour = "white", fontface = "bold") +
    # theme_bw(base_size=32) + 
    labs(x=paste0("PC1: ",round(var_explained[1]*100,1),"%"),
         y=paste0("PC2: ",round(var_explained[2]*100,1),"%")) +
    theme(legend.position="left")

## Cuarto intento #####
  
  candidatos_dtmv4 <- candidatos_tokenizadas %>% 
    limpiarTokens(palabras_web = TRUE, hashtags= TRUE) %>% 
    # group_by(tweet_id, tokens) %>% 
    count(screen_name, tokens) %>% 
    # ungroup() %>% 
    cast_dtm(screen_name, tokens, n)
  
  # siguiendo los pasos de:
  # https://cmdlinetips.com/2019/04/introduction-to-pca-with-r-using-prcomp/
  pca_resv4 <- prcomp(candidatos_dtmv4, scale=TRUE)
  summary(pca_resv4)
  pca_resv4$x[1:5,1:3]
  pca_resv4$center[1:5]
  head(pca_resv4$scale^2, n=5)
  var_explained <- pca_resv4$sdev^2/sum(pca_resv4$sdev^2)
  var_explained[1:5]
  
  pca_resv4$x %>% 
    as.data.frame %>%
    rownames_to_column("screen_name") %>%
    ggplot(aes(x=PC1,y=PC2)) + 
    geom_point(aes(color=screen_name),size=4) +
    geom_text(aes(label=screen_name)) +
    # geom_label(aes(fill = continent), colour = "white", fontface = "bold") +
    # theme_bw(base_size=32) + 
    labs(x=paste0("PC1: ",round(var_explained[1]*100,1),"%"),
         y=paste0("PC2: ",round(var_explained[2]*100,1),"%")) +
    theme(legend.position="left")
  
ggbiplot(pca_resv4)

## lo de tuq ########
# TuQmano  18:03
# para la representación visual de eso nos quedamos con las primeras 2 componentes.
# Algo así era el workflow con tf_idf:
#   #OPCION 1 
# preparando base

candidatos_filtered <- joined_candidatos %>%  
  subset(Campaña == 1) %>% 
  subset( !str_detect(text, "^RT") )

candidatos_filtered_text <- candidatos_filtered$text

# creando corpus

candidatos_corpusv2 <- Corpus(VectorSource(candidatos_filtered_text)) %>% #limpio la data con la librería tm
  tm_map(x = ., FUN = PlainTextDocument) %>% # NO FUNCA ESTE PASO 
  tm_map(x = ., FUN = removePunctuation) %>%
  tm_map(x = ., FUN = removeNumbers) %>%
  tm_map(x = ., FUN = removeWords, stopwords("spanish")) %>%
  tm_map(x = ., FUN = stripWhitespace)

#NO FUNCA EL PASO QUE SIGUE Y NO SE POR QUE 
candidatos_dtmv5 <- DocumentTermMatrix(candidatos_corpusv2) #Creo una matrix (rows = documents / columns = words / values = freq count)

candidatos_dtmv5$dimnames$Docs <- candidatos_filtered$tweet_id #le ponemos los nombres de los discursos 

# Creo la matriz de tf - idf
candidatos_dtmv5_tf_idf <- weightTfIdf(m = candidatos_dtmv5, normalize = TRUE)
candidatos_dtmv5_tf_idf_mat <- as.matrix(candidatos_dtmv5_tf_idf) 

# K-Means Cluster
#Normalizamos la matrix  porque se basa en Euclidean Distance (y no en Cossine Dissimilarity)
candidatos_dtmv5_tf_idf_mat_norm <- candidatos_dtmv5_tf_idf_mat / apply(candidatos_dtmv5_tf_idf_mat, MARGIN = 1, FUN = function(x) sum(x^2)^0.5)
km_clust <- kmeans(x = candidatos_dtmv5_tf_idf_mat_norm, centers = 5, iter.max = 25)
#PCA ANALISIS
pca_comp <- prcomp(candidatos_dtmv5_tf_idf_mat_norm)

pca_rep <- data_frame(txt = discursos_texto$txt,
                      discurso = discursos_texto$discursos,
                      presidente = discursos_texto$presidente,
                      pc1 = pca_comp$x[,1],
                      pc2 = pca_comp$x[,2],
                      clust_id = as.factor(km_clust$cluster))
library(ggrepel)
ggplot(data = pca_rep, mapping = aes(x = pc1, y = pc2, color = clust_id)) +
  #scale_color_manual(values = menta) +
  geom_text_repel(mapping = aes(label = discurso), size = 2, fontface = 'bold') +
  labs(title = 'K-Means Cluster: 5 clusters on PCA Features',
       x = '',
       y = '') +
  theme_minimal()+
  theme(legend.position = 'none',
        axis.text.x = element_blank(),
        axis.text.y=element_blank())f

# EXPLORANDO MAS COSITASS 
## intento con tdf_idf ####

candidatos_dtmv6 <- candidatos_tokenizadas %>% 
  limpiarTokens(palabras_web = TRUE) %>% 
  select(tweet_id, tokens) %>% 
  subset(str_length(tokens) > 2 & !(tokens == "no")) 

# remover finales de palabras empeora los resultados:
#candidatos_dtmv6$tokens <- stemDocument(candidatos_dtmv6$tokens, language = "es")

candidatos_dtmv6 <- candidatos_dtmv6 %>% 
  group_by(tweet_id, tokens) %>% 
  count()

candidatos_dtmv6 <- candidatos_dtmv6  %>% 
  bind_tf_idf(tokens, tweet_id, freq)

candidatos_dtmv6 <- candidatos_dtmv6 %>% 
  cast_dtm(tweet_id, tokens, tf_idf)

candidatos_sparse_dtmv6 <- removeSparseTerms(candidatos_dtmv6, 0.99)

pca_resv6 <- prcomp(candidatos_sparse_dtmv6, scale=TRUE)

summary(pca_resv6)
pca_resv6$x[1:5,1:3]
pca_resv6$center[1:5]
head(pca_resv6$scale^2, n=5)
var_explained <- pca_resv6$sdev^2/sum(pca_resv63$sdev^2)
var_explained[1:5]



## tdf idf por candidato ####

candidatos_dtmv7 <- candidatos_tokenizadas %>% 
  limpiarTokens() %>%  
  group_by(screen_name, tokens) %>% 
  add_tally() %>% 
  bind_tf_idf(tokens, screen_name, n)

candidatos_dtmv7 <- candidatos_dtmv7 %>% 
  cast_dtm(tweet_id, tokens, tf_idf)

candidatos_sparse_dtmv7 <- removeSparseTerms(candidatos_dtmv7, 0.99)

pca_resv6 <- prcomp(candidatos_sparse_dtmv7, scale=TRUE)

summary(pca_resv6)


## intento con tm. revisar encoding, no funco todavia #####

# preparando base

candidatos_filtered <- joined_candidatos %>%  
  subset(Campaña == 1) %>% 
  subset( !str_detect(text, "^RT") )

candidatos_filtered_text <- candidatos_filtered$text

# creando corpus

candidatos_corpus <- Corpus(VectorSource(candidatos_filtered_text))
#no logre mantener ids:
# candidatos_corpus1 <- Corpus(joined_candidatos$text, docnames = joined_candidatos$tweet_id)
# candidatos_corpus2 <- Corpus(joined_candidatos$text)

# limpiando corpus

candidatos_corpus <- tm_map(candidatos_corpus, tolower)
#candidatos_corpus <- tm_map(candidatos_corpus, PlainTextDocument)
candidatos_corpus <- tm_map(candidatos_corpus, removePunctuation)
candidatos_corpus <- tm_map(candidatos_corpus, removeWords, c(stopwords("spanish")))
candidatos_corpus <- tm_map(candidatos_corpus, stemDocument)

# creo document term matrix

candidatos_DTM <- DocumentTermMatrix(candidatos_corpus)

# chequeando frecuencias de las palabras
frequent_ge_20 <- findFreqTerms(candidatos_DTM, lowfreq = 20)

# removemos sparse terms # esto se podria aplicar a la otra DTM a la que arribamos de otra forma no?
candidatos_sparse_DTM <- removeSparseTerms(candidatos_DTM, 0.995)

# convertimos a data frame # esto no se si es necesario para mi
candidatos_sparse_df <- as.data.frame(as.matrix(candidatos_sparse_DTM))
colnames(tweetsSparse) <- make.names(colnames(tweetsSparse))

# ahora si a PCA

# siguiendo los pasos de:
# https://cmdlinetips.com/2019/04/introduction-to-pca-with-r-using-prcomp/
pca_resv2 <- prcomp(candidatos_sparse_DTM, scale=TRUE)
summary(pca_resv2)
pca_resv2$x[1:5,1:3]
pca_resv2$center[1:5]
head(pca_resv2$scale^2, n=5)
var_explained <- pca_resv2$sdev^2/sum(pca_resv2$sdev^2)
var_explained[1:5]

aver <- pca_resv2$x %>% 
  as.data.frame %>%
  rownames_to_column("tweet_id") %>%
  left_join(joined_candidatos) # %>% 
ggplot(aes(x=PC1,y=PC2)) #+ 
geom_point(aes(color=screen_name),size=4) +
  #geom_text(aes(label=screen_name)) +
  # geom_label(aes(fill = continent), colour = "white", fontface = "bold") +
  # theme_bw(base_size=32) + 
  labs(x=paste0("PC1: ",round(var_explained[1]*100,1),"%"),
       y=paste0("PC2: ",round(var_explained[2]*100,1),"%")) +
  theme(legend.position="left")


"no" %in% stopwords("spanish")
joined_candidatos$text
candidatos_corpus
candidatos_corpus[[4]]

# a minusculas
candidatos_corpus <- tm_map(candidatos_corpus, tolower)




