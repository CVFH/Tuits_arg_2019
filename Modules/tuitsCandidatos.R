# tuitsCandidatos

# modulo para trabajar con bases de datos de tuits 
# emitidos por políticos


#apertura de liberarias

library(tidyverse)
library(tidytext) # para manipular texto
library(stopwords)

# FUNCIONES 

# de base

determinarTuitsCampaña <- function(df_tuits, fecha_inicio_campaña, fecha_elecciones){
  
  # recibe un dataframe con tuits. se espera que haya una columna
  # "created_at" que incluya la fecha de emisión del tuit
  # además recibe dos parámetros tipo Date: fecha de inicio de la campaña
  # y fecha de las elecciones
  # determina si un tuit fue emitido en campaña o no.
  # crea una columna "Campaña" donde 1 equivale a que el tuit fue emitido durante la campaña
  # 0 en caso contrario
  
  df_tuits_clasif_campaña <- df_tuits %>%  
    mutate( Campaña  = 
              ifelse(as.Date(created_at) < fecha_elecciones & as.Date(created_at) > fecha_inicio_campaña, 
                     1,
                     0) )
  
  return(df_tuits_clasif_campaña)
  
}

seleccionarTextoTuits <- function(df_tuits, colums = c("text", "screen_name", "tweet_id", "created_at", "rts", "fav_count")){
  
  # recibe un dataframe con tuits emitidos
  # se espera que contenga variable "text" con el texto de los tuits emitidos
  # la devuelve transformada en caracteres
  # junto a todas las variables que se hayan indicado en "colums". 
  # por default, estas son: text, screen_name, tweet_id, created_at, rts, fav_count
  
  seleccion_text <- df_tuits %>% 
    select(colums) %>% 
    mutate(text = as.character(text)) 
  
  return(seleccion_text)
  
}

transformarEnlacesTwitter <- function(df_tuits){
  
  # funcion auxiliar que detecta y transforma enlaces de tuiter
  # a fines de descartarlos o eventualmente trabajarlos en analisis posteriores
  # recibe un df de tuits que se espera estén contenidos en una variable "text"
  # devuelve un df en el que los enlaces de tuits han sido identificados como "enlacetuit" en el texto
  # reemplazando al estándar "https://t.co/"
  
  df_tuits_enlaces_resaltados <- df_tuits %>% 
    mutate(text = str_replace_all(text,"https://t.co/", "enlacetuit")) %>% 
    mutate(text = str_replace_all(text,"#", "hashtag")) %>% 
    mutate(text = str_replace_all(text,"@", "mention"))
  
  return(df_tuits_enlaces_resaltados)
  
}


# funciones agregadas 

tokenizarTextoTuits <- function(df_tuits, filtrar_campaña = TRUE, deshacerse_RT = TRUE, tipo_token = "words", grams = 2){
  
  # recibe un df con tuits
  # se espera que el texto de los mismos esté contenido en una variable "text"
  # devuelve su texto tokenizado
  # optativo: no filtrar tuits de campaña (por variable campaña)
  
  if (isTRUE(filtrar_campaña)) { df_tuits <- df_tuits %>% subset( Campaña ==1 ) }
  if (isTRUE(deshacerse_RT)) { df_tuits <- df_tuits %>% subset( !str_detect(text, "^RT") )}
  
  seleccion_text <- df_tuits %>%  
    seleccionarTextoTuits() 
  
  if (tipo_token == "words") {
  seleccion_id_enlaces <- seleccion_text %>% 
    transformarEnlacesTwitter()
  
  seleccion_tokenizada <- seleccion_id_enlaces %>% 
    unnest_tokens(tokens, text) 
  }
  
  else if (tipo_token == "ngrams") {
    seleccion_id_enlaces <- seleccion_text %>% 
      transformarEnlacesTwitter() 
    
    seleccion_tokenizada <- seleccion_id_enlaces %>% 
      unnest_tokens(tokens, text, token = "ngrams", n = grams) 
    }
  
  else { 
    seleccion_tokenizada <- seleccion_text %>% 
      unnest_tokens(tokens, text, token = tipo_token) 
    } 
  
  return(seleccion_tokenizada)
}

limpiarTokens <- function(seleccion_tokenizada, bigramas = FALSE, lista_estandar= TRUE, largo = FALSE, palabras_web = FALSE, mentions = FALSE, hashtags = FALSE){
  
  #recibe una lista de tokens, en una variable tokens 
  # retira aquellos que consideramos innecesarios para el analisis:
  # en principio la lista de palabras que provee la funcion get_stopwords del paquete stopwords
  # optativo: borrar las palabras cortas (menos de tres caracteres)
  # tambien optativo: borrar enlaces de tuiter 
  # finalmente se ofrece la opción de limpiar bigramas (solo palabras stopwords)
  
  palabras_a_borrar <- stopwords("spanish")  # de tm. funciona mejor
  # palabras_a_borrar <- get_stopwords("es") %>%
  #   rename(tokens = "word")
  
  tokens_limpios <- seleccion_tokenizada
  
  if (isTRUE(bigramas)) {
    
    lista_estandar <- FALSE 
    
    bigrams_separated <- tokens_limpios %>%
      separate(tokens, c("word1", "word2"), sep = " ")
    
    bigrams_filtered <- bigrams_separated %>%
      filter(!word1 %in% palabras_a_borrar) %>%
      filter(!word2 %in% palabras_a_borrar)
    
    tokens_limpios <- bigrams_filtered %>%
      unite(tokens, word1, word2, sep = " ")
  }
  
  if (isTRUE(lista_estandar)){
    tokens_limpios <- tokens_limpios %>%
      filter(!(tokens %in% palabras_a_borrar))
  }
  
  if (isTRUE(largo)) {
    tokens_limpios <- tokens_limpios %>% 
    subset(str_length(tokens) > 3 & !(tokens == "no"))
  }
  
  if (isTRUE(palabras_web)) {
    tokens_limpios <- tokens_limpios %>% 
    subset(!str_detect(tokens, "(http)|(t.co)|(enlacetuit)"))  
  }
  if (isTRUE(mentions)) {
    tokens_limpios <- tokens_limpios %>% 
      subset(!str_detect(tokens, "(mention)|(@)"))  
  }
  if (isTRUE(hashtags)) {
    tokens_limpios <- tokens_limpios %>% 
      subset(!str_detect(tokens, "(hashtag)|(#)"))  
  }
  
  return(tokens_limpios)
  
}



