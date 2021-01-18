#exploracion_discursos.R

#####
# APERTURA DE LIBRERIAS
#####
#paquetes

require(tidyverse)
require(readxl)
require(ggplot2)
library(patchwork) # para unir graficos
require(tidytext)
require(ggthemes)

#propias

source("Modules/tuitsCandidatos.R", encoding = "UTF-8")
source("Modules/funcionesGraficos.R", encoding = "UTF-8")

#####
#IMPORTACION DE DATOS
#####

# ids

datos_base <- read_xlsx("Data/datos_base.xlsx")

# Datos tuiter 
# nos importan dataframes con tuits de candidatos: joined_gobernadores y joined_presid 

source("preparacion_datos_tuits.R", encoding = "UTF-8")

#####
# TRANSFORMACIONES DE LOS DATOS
#####
# Tokenizando
######

joined_presid_tokenizadas <- joined_presid %>% tokenizarTextoTuits() 

joined_gobernadores_tokenizadas <- joined_gobernadores %>% tokenizarTextoTuits() 


######
# EXPLORANDO DATOS + PLOTEANDO DATOS
#####
#Cantidad de palabras
######

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
                                                                                   colour=screen_name), 
                                                             aes(label= screen_name) )
gobernadores_promediopalabras_cantidadtuits_formateado <- formatPlot(gobernadores_promediopalabras_cantidadtuits,
                                                                     "Elocuencia de los candiatos a gobernador",
                                                                     "Cantidad de tuits emitidos",
                                                                     "Promedio de palabras por tuit",
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


#####
# tf-idf  
######

#The idea of tf-idf is 
# to find the important words 
# for the content of each document 
# by decreasing the weight for commonly used words 
# and increasing the weight for words
# that are not used very much in a collection or corpus of documents,
# 
# Calculating tf-idf attempts to find the words that are important (i.e., common) in a text, 
# but not too common. Let’s do that now.

joined_gobernadores_tokenizadas_tfidf <- joined_gobernadores_tokenizadas %>%
  count(screen_name, words, sort = TRUE) %>% 
  bind_tf_idf(words, screen_name, n)

joined_presid_tokenizadas_tfidf <- joined_presid_tokenizadas %>%
  count(screen_name, words, sort = TRUE) %>% 
  bind_tf_idf(words, screen_name, n) 

sliced_gobs_tfidf <- joined_gobernadores_tokenizadas_tfidf %>%
  subset( !(str_detect(words, "enlacetuit" ))) %>% 
  group_by(screen_name) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup()  %>% 
  mutate(words = reorder(words, tf_idf)) %>% 
  left_join(datos_base) %>% 
  group_by(screen_name) %>% 
  slice_head(n=15) %>% 
  ungroup()
  


# SERIA MEJOR COLOREAR ESTO POR DISTRITO
plot_gobs_tfidf <- ggplot(sliced_gobs_tfidf,
       aes(words, tf_idf, fill = Distrito)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~screen_name, ncol = 5, scales = "free") +
  coord_flip() +
  scale_x_reordered() + 
  theme(strip.text = element_text(size=8),
        axis.text.y = element_text(size = 3, hjust = 0.8))


plot_gobs_tfidf_formateado <- formatPlot(plot_gobs_tfidf, 
                                         "Palabras con mayor frecuencia")

sliced_presid_tfidf <- joined_presid_tokenizadas_tfidf %>% 
  group_by(screen_name) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>% 
  mutate(words = reorder(words, tf_idf)) #ordenamos 

plot_presid_tfidf <- ggplot(sliced_presid_tfidf,
                          aes(words, tf_idf, fill = screen_name)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~screen_name, ncol = 2, scales = "free") +
  coord_flip() +
  scale_x_reordered()+ 
  theme(strip.text = element_text(size=8),
        axis.text.y = element_text(size = 3, hjust = 0.8))



