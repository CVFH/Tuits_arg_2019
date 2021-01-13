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
