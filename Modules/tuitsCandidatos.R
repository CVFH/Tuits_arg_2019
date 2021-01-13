# tuitsCandidatos

# modulo para trabajar con bases de datos de tuits 
# emitidos por políticos


#apertura de liberarias

require(tidyverse)

# FUNCIONES 

# de base

determinarTuitsCampana <- function(df_tuits, fecha_inicio_campana, fecha_elecciones){
  
  #recibe un dataframe con tuits. se espera que haya una columna
  #"created_at" que incluya la fecha de emisión del tuit
  #además recibe dos parámetros tipo Date: fecha de inicio de la campaña
  #y fecha de las elecciones
  #determina si un tuit fue emitido en campaña o no.
  #crea una columna "Campaña" donde 1 equivale a que el tuit fue emitido durante la campaña
  # 0 en caso contrario
  
  df_tuits_clasif_campana <- df_tuits %>%  
    mutate( Campana  = 
              ifelse(as.Date(created_at) < fecha_elecciones & as.Date(created_at) > fecha_inicio_campana, 
                     1,
                     0) )
  return(df_tuits_clasif_campana)
}

seleccionarTextoTuits <- function(df_tuits){
  
  ### recibe un dataframe con tuits emitidos
  ## se espera que contenga variable "text" con el texto de los tuits emitidos
  #### la devuelve transformada en caracteres
  seleccion_text <- df_tuits %>% 
    select(text) %>% 
    mutate(text = as.character(text)) 
  
  return(seleccion_text)
}

# funciones agregadas 

tokenizarTextoTuits <- function(df_tuits, filtrar_campana = TRUE){
  
  ## recibe un df con tuits
  # se espera que el texto de los mismos esté contenido en una variable "text"
  ## devuelve su texto tokenizado
  ## optativo: no filtrar tuits de campana (por variable campana)
  if (isTRUE(filtrar_campana)) { df_tuits <- df_tuits %>% subset( Campana ==1 ) }
  
  seleccion_text <- df_tuits %>%  
    seleccionarTextoTuits() 
  
  seleccion_tokenizada <- seleccion_text %>% unnest_tokens(words, text)
  
  return(seleccion_tokenizada)
}


limpiarTokens <- function(seleccion_tokenizada){
  
  #recibe una lista de tokens, en una variable words
  # retira aquellos que consideramos innecesarios para el analisis
  
  tokens_limpios <- seleccion_tokenizada %>% 
    subset(str_length(words) > 3 & !(words == "no"))  %>%  
    subset(!str_detect(seleccion_tokenizada$words, "(http)|(t.co)")) 

  return(tokens_limpios)
  
}
