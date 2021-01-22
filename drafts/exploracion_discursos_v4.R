#exploracion_discursos.R

# APERTURA DE LIBRERIAS #####

#paquetes

library(lubridate)
library(tidyverse)
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


#propias

source("Modules/tuitsCandidatos.R", encoding = "UTF-8")
source("Modules/funcionesGraficos.R", encoding = "UTF-8")



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

candidatos_tokenizadas <- rbind(joined_gobernadores_tokenizadas,
                                joined_presid_tokenizadas) %>% 
  left_join(datos_base)


candidatos_tokentweets <- rbind(joined_presid_tokentweets,
                                joined_gobernadores_tokentweets)

joined_candidatos <- joined_candidatos %>% 
  left_join(datos_base) 


# EXPLORANDO DATOS + PLOTEANDO DATOS 

# Cantidad de palabras ######

# Gobernadores

gobernadores_sintesis1 <- joined_gobernadores_tokenizadas %>% 
  group_by(screen_name, tweet_id) %>% 
  mutate(cantidad_palabras_tuit =  n()) %>% 
  ungroup() %>% 
  group_by(screen_name) %>% 
  summarise(palabras_promedio_tuit = mean(cantidad_palabras_tuit),
            palabras_totales_emitidas = n())

gobernadores_sintesis2 <-joined_gobernadores %>% 
  subset(Campaña ==1) %>% 
  group_by(screen_name) %>% 
  summarise(cantidad_tuits_emitidos = n())

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
  mutate(cantidad_palabras_tuit =  n()) %>% 
  ungroup() %>% 
  group_by(screen_name) %>% 
  summarise(palabras_promedio_tuit = mean(cantidad_palabras_tuit),
            palabras_totales_emitidas = n())

presid_sintesis2 <-joined_presid %>% 
  subset(Campaña ==1) %>% 
  group_by(screen_name) %>% 
  summarise(cantidad_tuits_emitidos = n())

presid_sintesis <- left_join(presid_sintesis1, presid_sintesis2)
 
presid_promediopalabras_cantidadtuits <-plotPointText(presid_sintesis, 
                                                      aes(cantidad_tuits_emitidos,
                                                          palabras_promedio_tuit,
                                                          colour= screen_name),
                                                      aes(label=screen_name)) 



# Frecuencias de palabras ####
# no se para que puede servirme - nada jajaj
frecuencias_candidatos_tokentweets <- candidatos_tokentweets %>% 
  limpiarTokens() %>% 
  group_by(screen_name) %>% 
  count(tokens, sort = TRUE) %>% 
  left_join(candidatos_tokentweets %>% 
              group_by(screen_name) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)


frecuencias_candidatos_tokentweets <- frecuencias_candidatos_tokentweets  %>% 
  select(screen_name, tokens, freq) %>% 
  spread(screen_name, freq)


# words_by_time uso de palabras en el tiempo #####
# TODAVIA NO SE PARA QUE PUEDE SERVIR ESTO. REVISAR
# PUEDE SER INTERESANTE: COMPARAR CON TOTAL DE PALABRS USADAS EN EL TIEMPO X CANDIDATO (EV DE MAS USADAS). 
# O COMPARAR C . algo mas. se me fue lpm
# de los graficos: borrar refs, agregar refs en texto 
words_by_time <- candidatos_tokentweets %>% 
  limpiarTokens() %>% 
  filter(!str_detect(tokens, "^@")) %>%
  mutate(mes = month(created_at)) %>%
  count(mes, screen_name, tokens) %>%
  group_by(screen_name, mes) %>%
  mutate(time_total = sum(n)) %>%
  group_by(screen_name, tokens) %>%
  mutate(tokens_total = sum(n)) %>%
  ungroup() %>%
  rename(count = n) %>%
  filter(tokens_total > 30)

nested_data_words_by_time <- words_by_time %>%
  nest(-tokens, -screen_name) 

nested_models_words_by_time <- nested_data_words_by_time %>%
  mutate(models = map(data, ~ glm(cbind(count, time_total) ~ mes, ., 
                                  family = "binomial")))

slopes_words_by_time <- nested_models_words_by_time %>%
  mutate(models = map(models, tidy)) %>%
  unnest(cols = c(models)) %>%
 # filter(term == "mes") %>%
  mutate(adjusted.p.value = p.adjust(p.value))

top_slopes_words_by_time <- slopes_words_by_time %>% 
  filter(adjusted.p.value < 0.05)

grafico_words_by_time <- words_by_time %>%
  inner_join(top_slopes_words_by_time, by = c("tokens", "screen_name")) %>%
  ggplot(aes(mes, count/time_total, color = tokens)) +
  facet_wrap(~screen_name, ncol = 5, scales = "free") +
  geom_line(size = 1.3) +
  labs(x = NULL, y = "Word frequency")



# Nubes de palabras #####

# simple. no es muy informativa
word_cloud_simple <- candidatos_tokenizadas %>%
  limpiarTokens() %>% 
  count(tokens) 

plot_word_cloud_simple <- 
  wordcloud::wordcloud(word_cloud_simple$tokens, 
                       word_cloud_simple$n, 
                       max.words = 100)

# comparando por distrito

matriz_cargos <- candidatos_tokenizadas %>%
  limpiarTokens() %>%  
  mutate( Cargo = ifelse(Distrito== "Nación", "Presidente", "Gobernador")) %>%
  count(tokens, Cargo, sort = TRUE) %>% 
  acast(tokens ~ Cargo, value.var = "n", fill = 0)

plot_word_cloud_cargos <- wordcloud::comparison.cloud(matriz_cargos,
                                                      colors = c("lightblue", "blue"),
                                                      max.words = 100,
                                                      title.size=NULL)

#obvio que el debate pq participaron todxs
# territorialidad de los provincianos. barrio vecino. encuentro




# tf-idf   ######


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
  count(screen_name, tokens, sort = TRUE) %>% 
  limpiarTokens() %>%  
  bind_tf_idf(tokens, screen_name, n)

sliced_gobs_tfidf <- joined_gobernadores_tokenizadas_tfidf %>%
  subset( !(str_detect(tokens, "enlacetuit" ))) %>% 
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
  count(screen_name, tokens, sort = TRUE) %>% 
  bind_tf_idf(tokens, screen_name, n)

sliced_gobs_tfidf_bigramas <- joined_gobernadores_bigramas_tfidf %>%
  subset( !(str_detect(tokens, "enlacetuit" ))) %>% 
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
 # limpiarTokens(columna= "tokens") %>%  
  count(screen_name, tokens, sort = TRUE) %>% 
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
        axis.text.y = element_text(size = 3, hjust = 0.8))

##
# presidente
##

# tdf_idf / presidente / tokens = words

joined_presid_tokenizadas_tfidf <- joined_presid_tokenizadas %>%
  limpiarTokens() %>% 
  count(screen_name, tokens, sort = TRUE) %>% 
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
        axis.text.y = element_text(size = 5, hjust = 0.8))


# tdf_idf / presidente / tokens = tweets

joined_presid_tokentweets_tfidf <- joined_presid_tokentweets %>%
  limpiarTokens(columna = "tokens") %>% 
  count(screen_name, tokens, sort = TRUE) %>% 
  bind_tf_idf(tokens, screen_name, n)

sliced_presid_tfidf_tokentweet <- joined_presid_tokentweets_tfidf %>%
  subset( !(str_detect(tokens, "enlacetuit" ))) %>% 
  group_by(screen_name) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup()  %>% 
  mutate(tokens= reorder(tokens, tf_idf)) %>% 
  left_join(datos_base) %>% 
  group_by(screen_name) %>% 
  slice_head(n=15) %>% 
  ungroup()

plot_presid_tfidf_tokentweet <- ggplot(sliced_presid_tfidf_tokentweet ,
                                     aes(tokens, tf_idf, fill = Distrito)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~screen_name, ncol = 5, scales = "free") +
  coord_flip() +
  scale_x_reordered() + 
  theme(strip.text = element_text(size=8),
        axis.text.y = element_text(size = 3, hjust = 0.8))


# tdf_idf / presidente /tokens = bigramas

joined_presid_bigramas_tfidf <- joined_presid_bigramas %>%
  limpiarTokens(bigramas = TRUE)  %>% 
  count(screen_name, tokens, sort = TRUE) %>% 
  bind_tf_idf(tokens, screen_name, n)

sliced_presid_tfidf_bigramas <- joined_presid_bigramas_tfidf %>%
  subset( !(str_detect(tokens, "enlacetuit" ))) %>% 
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
  facet_wrap(~screen_name, ncol = 3, scales = "free") +
  coord_flip() +
  scale_x_reordered() + 
  theme(strip.text = element_text(size=8),
        axis.text.y = element_text(size = 6, hjust = 0.8)) 




# Topic modeling ######

# preparando datos

candidatos_dtm <- candidatos_tokenizadas %>% 
  limpiarTokens() %>% 
  count(tweet_id, tokens) %>%
  cast_dtm(tweet_id, tokens, n)

# primera prueba. NO SIRVE. 
#vemos que los  hashtags son transversales a los topicos
candidatos_LDA <- LDA(candidatos_dtm, k = 12, control = list(seed = 1234))

candidatos_topics <- tidy(candidatos_LDA, matrix = "beta")

candidatos_topics_top_terms <- candidatos_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

candidatos_topics_top_terms_grafico <- candidatos_topics_top_terms  %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() 


# segundo intento # no sirve 


candidatos_tokens_dtm <- candidatos_tokentweets %>% 
  rename(tokens = "tokens") %>% 
  limpiarTokens() %>% 
  subset(!str_detect(tokens, "(#)")) %>% 
  count(tweet_id, tokens) %>%
  cast_dtm(tweet_id, tokens, n)

candidatos_tokens_LDA <- LDA(candidatos_tokens_dtm, k = 12, control = list(seed = 1234))

candidatos_tokens_topics <- tidy(candidatos_tokens_LDA, matrix = "beta")

candidatos_tokens_topics_top_terms <- candidatos_tokens_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

candidatos_tokens_topics_top_terms_grafico <- candidatos_tokens_topics_top_terms  %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() 


# tercer intento # no sirve

candidatos_tokens_dtmv3 <- candidatos_tokentweets %>% 
  limpiarTokens() %>% 
  subset(!str_detect(tokens, "(#)")) %>% 
  count(screen_name, tokens) %>%
  cast_dtm(screen_name, tokens, n)

candidatos_tokens_LDAv3 <- LDA(candidatos_tokens_dtmv3, k = 2, control = list(seed = 1234))

candidatos_tokens_topicsv3 <- tidy(candidatos_tokens_LDAv3, matrix = "beta")

candidatos_tokens_topics_top_termsv3 <- candidatos_tokens_topicsv3 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

candidatos_tokens_topics_top_terms_graficov3 <- candidatos_tokens_topics_top_termsv3  %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() 

# cuarto intento. con bigramas
# EL MAS INTERESANTE HASTA AHORA. PARECE HABER UN TOPICO 1 MAS PERONISTA/IZQ, Y OTRO MAS MACRISTA/DERECHA
# SEGUIR PROBANDO
# POR ALGUN MOTIVO MUCHOS NA/NA

candidatos_tokens_dtmv4 <- joined_candidatos %>% 
  subset(!str_detect(text, "(#)")) %>% 
  tokenizarTextoTuits(tipo_token = "ngrams") %>% 
  limpiarTokens(bigramas = TRUE) %>% 
  count(screen_name, tokens) %>%
  cast_dtm(screen_name, tokens, n)

candidatos_tokens_LDAv4 <- LDA(candidatos_tokens_dtmv4, k = 2, control = list(seed = 1234))

candidatos_tokens_topicsv4 <- tidy(candidatos_tokens_LDAv4, matrix = "beta")

candidatos_tokens_topics_top_termsv4 <- candidatos_tokens_topicsv4 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

candidatos_tokens_topics_top_terms_graficov4 <- candidatos_tokens_topics_top_termsv4  %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered() 


# Lineas de tiempo ######

linea_simple <- ggplot(joined_candidatos %>%  
                         filter(year(created_at) == 2019 ) %>%  
                                  arrange(tipo_fecha), 
                       aes(x = date(created_at), fill = tipo_fecha)) +
  geom_histogram(position = "identity", bins = 20, alpha = 0.5)  +
  facet_wrap(~Cargo, ncol = 2)


 

# PCA ####



#primer intento. una porqueria ####
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


# segundo  . MUERTE #######

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
  
  
# Tercer intento. haciendo mitad tm y mitad tidytext no funca, una porqueria ####
  
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

# Cuarto intento #####
  
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

#lo de tuq ########
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
# intento con tdf_idf ####

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



# tdf idf por candidato ####

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


# intento con tm. revisar encoding, no funco todavia #####

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



# Menciones #####

# cualquier mencion #####

candidatos_campaña <- joined_candidatos %>% 
  subset(Campaña==1) 

candidatos_menciones <- candidatos_campaña %>% 
  mutate(mention_screen_names = as.character(mention_screen_names)) %>% 
  unnest_tokens(tokens, mention_screen_names) %>% 
  subset(!is.na(tokens)) 

cuenta_menciones <- candidatos_menciones %>% 
  group_by(screen_name, tokens) %>% 
  dplyr::summarise(n = n())


grafo_menciones_dirigido_df <- graph.data.frame(d = cuenta_menciones,directed = T)
#grafo_dirigido_new_df <- delete.vertices(graph = gd,v = V(graph = gd)[degree(graph = gd,mode = "all")<=2])

# V(grafo_menciones_dirigido_df)$label.cex <- seq(0.5,5,length.out=6)         ## text size
# V(grafo_menciones_dirigido_df)$size      <- seq(10,60,length.out=6)         ## circle size proportional to text size

#layout1 <- layout_in_circle(grafo_menciones_dirigido_df)
edge.start <- ends(grafo_menciones_dirigido_df, es=E(grafo_menciones_dirigido_df), names=F)[,1]
edge.col <- V(grafo_menciones_dirigido_df)$color[edge.start]
  
grafo_menciones_dirigido <- plot.igraph(grafo_menciones_dirigido_df,
                              vertex.size=degree(grafo_menciones_dirigido_df,mode = "in")*2, # Tamaño de nodo
                              edge.arrow.size=0.2, # tamaño de flecha de la arista
                              edge.arrow.width=0.8, # ancho de flecha de la arista
                              edge.color=edge.col,# color de arista
                              # edge.curved = T, # arista curva,
                              vertex.label.dist=2,
                              vertex.label.cex=degree(grafo_menciones_dirigido_df,mode = "in")*0.01,
                              #vertex.label.cex=0.4, # tamaño de las etiquetas de los nodos
                              main="Cuentas que recibieron más menciones", 
                              vertex.label.color="black", # color de las etiquetas de los nodos
                              #vertex.color="blue", # color de nodos
                              #vertex.label.family="arial",
                              vertex.shape="none", 
                              vertex.label=V(grafo_menciones_dirigido_df)$name,
                              #layout1
)

#para editar interactivamente
tkid <- tkplot(grafo_menciones_dirigido_df) #tkid is the id of the tkplot that will open
l <- tkplot.getcoords(tkid) # grab the coordinates from tkplot
tk_close(tkid, window.close = T)
plot(grafo_menciones_dirigido_d, layout=l)

#medidas del grafo
V(grafo_menciones_dirigido_df)
V(grafo_menciones_dirigido_df)$name
vertex_attr(grafo_menciones_dirigido_df)
edge_attr(grafo_menciones_dirigido_df)
V(grafo_menciones_dirigido_df)$label_size

edge.attr.comb=c(weight="sum", type="ignore") 

# hubs y authorities 

hs <- hub_score(grafo_menciones_dirigido_df, weights=NA)$vector

as <- authority_score(grafo_menciones_dirigido_df, weights=NA)$vector

par(mfrow=c(1,2))

plot(grafo_menciones_dirigido_df, vertex.size=hs*50, main="Hubs")

plot(grafo_menciones_dirigido_df, vertex.size=as*30, main="Authorities")

oranges <- colorRampPalette(c("dark red", "gold"))

# para colorear c escalaa
#
#  col <- oranges(max(dist.from.NYT)+1)
# col <- col[dist.from.NYT+1]

# p diferenciar colores 
# 
# Identify the edges going into or out of a vertex, for instance the WSJ. For a single node, use incident(), for multiple nodes use incident_edges()
# inc.edges <- incident(net,  V(net)[media=="Wall Street Journal"], mode="all")
# 
# # Set colors to plot the selected edges.
# ecol <- rep("gray80", ecount(net))
# ecol[inc.edges] <- "orange"
# vcol <- rep("grey40", vcount(net))
# vcol[V(net)$media=="Wall Street Journal"] <- "gold"
# plot(net, vertex.color=vcol, edge.color=ecol)

#Community detection based on edge betweenness (Newman-Girvan)
#High-betweenness edges are removed sequentially (recalculating at each step) and the best partitioning of the network is selected.

ceb <- cluster_edge_betweenness(grafo_menciones_dirigido_df) 
#dendPlot(ceb, mode="hclust") 
plot(ceb, grafo_menciones_dirigido_df)



# menciones mutuas ######


candidatos_menciones_mutuas <- candidatos_menciones %>% 
  subset(tokens %in% datos_base$screen_name) %>%
  select(screen_name, tokens) 

cuenta_menciones_mutuas <- candidatos_menciones_mutuas %>% 
  group_by(screen_name, tokens) %>% 
  dplyr::summarise(n = n())

# grafo dirigido

grafo_dirigido_df <- graph.data.frame(d = cuenta_menciones_mutuas,directed = T)
#grafo_dirigido_new_df <- delete.vertices(graph = gd,v = V(graph = gd)[degree(graph = gd,mode = "all")<=2])

grafo_dirigido <- plot.igraph(grafo_dirigido_df,
            vertex.size=degree(gd,mode = "in")*2, # Tamaño de nodo
            edge.arrow.size=0.2, # tamaño de flecha de la arista
            edge.arrow.width=0.8, # ancho de flecha de la arista
            edge.color="black", # color de arista
            #edge.curved = T, # arista curva
            vertex.label.cex=0.8, # tamaño de las etiquetas de los nodos
            main="Candidatos que recibieron más menciones", 
            vertex.label.color="grey50", # color de las etiquetas de los nodos
            vertex.color="blue", # color de nodos
            vertex.label.family="arial"
)



#grafo no dirigido
# me gusta mas el anterior

grafo_nodirigido_df <- graph.data.frame(cuenta_menciones_mutuas,directed = F)

plot.igraph(grafo_nodirigido_df,
            edge.curved =T, # Aristas curvas
            edge.color = "black", # Color de aristas
            edge.width = (E(grafo_nodirigido_df)-min(E(grafo_nodirigido_df)))/12, # Ancho de aristas
            edge.color="black", # Color de aristas
            vertex.color="azure3",# Color de nodos o vertices
            layout=layout.auto, # layout es la forma del grafo
            vertex.label.color="black") # color de las etiquetas de los nodos


# Hashtags #####

candidatos_hashtags <- joined_candidatos %>% 
  tokenizarTextoTuits() %>% 
  subset(str_detect(tokens, "(hashtag)")) %>% 
  mutate(hashtags = str_replace(tokens, "(hashtag)", "#")) %>% 
  select(-c(tokens)) %>% 
  dplyr::count(screen_name, hashtags) %>% 
  group_by(screen_name) %>%
  slice_max(n, n = 4) %>% 
  left_join(datos_base)

hashtags_matrix <- candidatos_hashtags %>%  
  acast(hashtags ~ screen_name, value.var = "n", fill = 0)

wordcloud::comparison.cloud(matriz_cargos,
                            colors = c("lightblue", "blue"),
                            max.words = 100,
                            title.size=NULL)

hashtags_matrix <- candidatos_hashtags %>% 
  select(screen_name, hashtags, n) %>% 
  as.matrix()

plot__hashtags <- ggplot(candidatos_hashtags,
                         aes(hashtags, n, fill = Distrito)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~screen_name, ncol = 3, scales = "free") +
  coord_flip() +
  scale_x_reordered() + 
  theme(strip.text = element_text(size=8),
        axis.text.y = element_text(size = 6, hjust = 0.8))


plot_gobs_tfidf_formateado <- formatPlot(plot_gobs_tfidf, 
                                         "Palabras con mayor frecuencia")


# Temas por palabras ########