# tablasElectorales

## modulo con funciones
##para scrappear tablas web
## y extraer datos de resultados electorales
## las funciones globales están inspiradas en el reporte de datos provinciales de wikipedia
# pero las funciones de base pueden ser usadas en otros casos

#apertura de liberarias
##### 

library(tidyverse)
library(rvest) # extraer datos de html
library(readxl)

# FUNCIONES

#PARA SCRAPPEAR TABLAS ELECTORALES


#de base

arbolTablas <- function(url){
  
  # recibe un url, 
  # lee el código html 
  # y se queda con los nodos "tabla"
  
  df_url_tables <-read_html(url) %>% 
    html_nodes("table")
  return(df_url_tables)
}

extraerTabla <- function(df_url_tables, nodo){
  
  # extrae la tabla de interés
  # y hace los primeros pasos de limpieza
  
  extracto_tabla <- html_table(df_url_tables[[nodo]], 
                               fill=TRUE, 
                               header=TRUE)  
  return(extracto_tabla)
}

borrarPrimeraFila <- function(extracto_tabla){
  
  extracto_tabla <- extracto_tabla[-c(1, nrow(extracto_tabla)), ]
  return(extracto_tabla)
}

reducirAnchoTabla <- function(extracto_tabla){
  
  ## función destinada a la limpieza de algunos atributos
  ## de las tablas obtenidas de wikipedia con datos sobre la elección a gobernador
  #en este caso: borramos columnas innecesarias
  
  columnas_a_borrar <- c(3) # en todos los casos la columna tres es innecesaria
  columna_a_borrar <- 0   # vamos a buscar otras columnas a borrar
  # en particular, 
  # hay algunas tablas que tienen columnas con nombres vacios, pero no todas. 
  # las limpiamos      
  for (i in colnames(extracto_tabla)) {      
    columna_a_borrar <- columna_a_borrar + 1 
    if (is.na(i)) { columnas_a_borrar <- append(columnas_a_borrar, columna_a_borrar)} 
  }
  
  extracto_tabla_corta <-  extracto_tabla[, -(columnas_a_borrar) ]
  return(extracto_tabla_corta)
}

omitNaTabla <- function(extracto_tabla_corta) {
  ## función destinada a la limpieza de algunos atributos
  ## de las tablas obtenidas de wikipedia con datos sobre la elección a gobernador
  #en este caso: omitimos filas con valores vacios 
  extracto_tabla_sin_nas <- na.omit(extracto_tabla_corta)
  return(extracto_tabla_sin_nas)
}

renombrarTabla <- function(extracto_tabla_sin_nas){
  
  ## función destinada a la limpieza de algunos atributos
  ## de las tablas obtenidas de wikipedia con datos sobre la elección a gobernador
  #en este caso: cambiamos nombres de las columnas
  extracto_tabla_renombrada <- extracto_tabla_sin_nas %>% 
    dplyr::rename(
      Candidato = 1,
      Vicecandidato = 2,
      Porcentaje = "%") 
  return(extracto_tabla_renombrada)
}

limpiezaTabla <- function(extracto_tabla_renombrada){
  
  ## función destinada a la limpieza de algunos atributos
  ## de las tablas obtenidas de wikipedia con datos sobre la elección a gobernador
  #en este caso:  formateamos variables de interés: votos y porcentaje
  
  extracto_tabla_limpia <- extracto_tabla_renombrada %>%  
    dplyr::mutate(Porcentaje = str_replace(Porcentaje,"\\%", "")) %>% 
    dplyr::mutate(Porcentaje = str_trim(
      str_replace(Porcentaje,"\\,", ".")
    ) ) %>% 
    dplyr::mutate(Porcentaje = as.numeric(Porcentaje)) %>% 
    dplyr::mutate(Votos = str_trim(
      str_replace_all(Votos,"\\.", "")
    ) ) %>% 
    dplyr::mutate(Votos = as.numeric(Votos))
  
  
  return(extracto_tabla_limpia)
  
}

agregarVotosGobernador <- function(tabla_limpia){ 
  
  #calcula los votos agregados de la fórmula
  #para tablas que reportan de manera separada cada lista
  
  votos_afirmativos <- sum(tabla_limpia$Votos)
  
  tabla_summarized <- tabla_limpia %>% 
    group_by(Candidato) %>% 
    dplyr::summarise(Votos = sum(Votos),
              Porcentaje = sum(Votos)/votos_afirmativos*100)
  
  return(tabla_summarized)
}

reducirLargoTabla <- function(tabla_limpia_agregada){
  
  #se queda solamente 
  # con los votos obtenidos por cada candidato
  
  tabla_reducida <- tabla_limpia_agregada %>% 
    subset(!str_detect(
      tabla_limpia_agregada$Candidato, "(Vot)|(Elect)|(Particip)|(Tot)|(Absten)")) 
  
  return(tabla_reducida)
}

agregarColumnas <- function(tabla_reducida, nombre_distrito = ""){
  
  #recibe una tabla limpia. la transforma: 
  #agrega ranking y nombre de distrito, si es que se provee uno (chr)
  
  #ordena por porcentaje y agrega ranking
  
  tabla_reducida <- tabla_reducida %>%arrange(desc(Porcentaje)) 
  tabla_reducida$Ranking <- 1:nrow(tabla_reducida)
  
  # si se ingresó, agrega nombre de distrito
  
  if (nchar(nombre_distrito) > 0) {tabla_reducida$Distrito <- nombre_distrito}
  
  return(tabla_reducida)
}

#funciones agregadas

extraer_datos_wiki <- function(url, nodo){
  #recibe un url de wikipedia y un nodo
  #devuelve la tabla del nodo seleccionado en crudo
  
  arbol_tablas <- arbolTablas(url) 
  tabla_cruda <- extraerTabla(arbol_tablas, nodo)
  return(tabla_cruda)
}

procesar_datos_wiki <- function(tabla_cruda, nombre_distrito = "") {
  
  #función que recibe una tabla cruda
  #con datos de resultados electorales 
  #extraidos de wikipedia
  # y devuelve una tabla limpia y reducida con los datos que necesitamos para trabajar
  # en este caso consisten en solamente las filas con los nombres de los candidatos
  #además, si se desea,
  #agrega una columna con el nombre del distrito ( ingresar una cadena, tipo chr )
  
  tabla_s_primera_fila <- borrarPrimeraFila(tabla_cruda)
  tabla_corta <- reducirAnchoTabla(tabla_s_primera_fila)
  tabla_sin_nas <- omitNaTabla(tabla_corta)
  tabla_renombrada <- renombrarTabla(tabla_sin_nas)
  tabla_limpia <- limpiezaTabla(tabla_renombrada)
  tabla_reducida <- reducirLargoTabla(tabla_limpia)
  tabla_lista <- agregarColumnas(tabla_reducida, nombre_distrito)
  
  return(tabla_lista)
}
