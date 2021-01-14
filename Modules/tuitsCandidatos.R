# tuitsCandidatos

# modulo para trabajar con bases de datos de tuits 
# emitidos por políticos


#apertura de liberarias

require(tidyverse)

# FUNCIONES 

# de base

determinarTuitsCampaña <- function(df_tuits, fecha_inicio_campaña, fecha_elecciones){
  
  #recibe un dataframe con tuits. se espera que haya una columna
  #"created_at" que incluya la fecha de emisión del tuit
  #además recibe dos parámetros tipo Date: fecha de inicio de la campaña
  #y fecha de las elecciones
  #determina si un tuit fue emitido en campaña o no.
  #crea una columna "Campaña" donde 1 equivale a que el tuit fue emitido durante la campaña
  # 0 en caso contrario
  
  df_tuits_clasif_campaña <- df_tuits %>%  
    mutate( Campaña  = 
              ifelse(as.Date(created_at) < fecha_elecciones & as.Date(created_at) > fecha_inicio_campaña, 
                     1,
                     0) )
  return(df_tuits_clasif_campaña)
}

seleccionarTextoTuits <- function(df_tuits, colums = c("text", "screen_name", "tweet_id", "created_at", "rts", "fav_count")){
  
  ### recibe un dataframe con tuits emitidos
  ## se espera que contenga variable "text" con el texto de los tuits emitidos
  #### la devuelve transformada en caracteres
  ## junto a todas las variables que se hayan indicado en "colums". 
  ## por default, estas son: text, screen_name, tweet_id, created_at, rts, fav_count
  seleccion_text <- df_tuits %>% 
    select(colums) %>% 
    mutate(text = as.character(text)) 
  
  return(seleccion_text)
}

transformarEnlacesTwitter <- function(df_tuits){
  
  #funcion auxiliar que detecta y transforma enlaces de tuiter
  #a fines de descartarlos o eventualmente trabajarlos en analisis posteriores
  #recibe un df de tuits que se espera estén contenidos en una variable "text"
  #devuelve un df en el que los enlaces de tuits han sido identificados como "enlacetuit" en el texto
  #reemplazando al estándar "https://t.co/"
  
  df_tuits_enlaces_resaltados <- df_tuits %>% 
    mutate(text = str_replace_all(text,"https://t.co/", "enlacetuit"))
  
  return(df_tuits_enlaces_resaltados)
}


# funciones agregadas 

tokenizarTextoTuits <- function(df_tuits, filtrar_campaña = TRUE){
  
  ## recibe un df con tuits
  # se espera que el texto de los mismos esté contenido en una variable "text"
  ## devuelve su texto tokenizado
  ## optativo: no filtrar tuits de campaña (por variable campaña)
  if (isTRUE(filtrar_campaña)) { df_tuits <- df_tuits %>% subset( Campaña ==1 ) }
  
  seleccion_text <- df_tuits %>%  
    seleccionarTextoTuits() 
  
  seleccion_id_enlaces <- seleccion_text %>% 
    transformarEnlacesTwitter()
  
  seleccion_tokenizada <- seleccion_id_enlaces %>% 
    unnest_tokens(words, text)
  
  return(seleccion_tokenizada)
}


limpiarTokens <- function(seleccion_tokenizada){
  
  #recibe una lista de tokens, en una variable words
  # retira aquellos que consideramos innecesarios para el analisis
  
  tokens_limpios <- seleccion_tokenizada %>% 
    subset(str_length(words) > 3 & !(words == "no"))  %>%  
    subset(!str_detect(words, "(http)|(t.co)")) 

  return(tokens_limpios)
  
}
