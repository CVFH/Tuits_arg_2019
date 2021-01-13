#####
#apertura de liberarias
#####

require(tidyverse)
require(readxl)
require(rvest) # extraer datos de html
require(ggplot2)
require(polAr)

##### 
#importación de datos
#####
#QUE DESDOBLARON: ELECCIONES GRALES 16/JUNIO. SOLO SANTA FE PASO 28/ABRIL

# Formosa

formosa1 <- insfran_gildo <- read.csv("Data/insfran_gildo.csv")
formosa2 <- adrianbogadoOK <- read.csv("Data/adrianbogadoOK.csv")

# San Luis

sluis1 <- arsaa2019 <- read.csv("Data/arsaa2019.csv")
sluis2 <- claudiojpoggi <- read.csv("Data/claudiojpoggi.csv")

# Santa Fe # perdio oficialismo

sfe1 <- omarperotti <- read.csv("Data/Kicillofok.csv")
sfe2 <- AntonioBonfatti <- read.csv("Data/Kicillofok.csv")

# Tierra del Fuego # perdio oficialismo

tfuego1 <- gustavomelella <- read.csv("Data/gustavomelella.csv")
tfuego2 <- RosanaBertone <- read.csv("Data/RosanaBertone.csv")


#SIMULTANEAS: QUE CELEBRARON ELECCIONES GRALES 27/OCTUBRE, PASO 11/AGOSTO

#PBA

baires1 <- Kicillofok <- read.csv("Data/Kicillofok.csv")
baires2 <- mariuvidal <- read.csv("Data/mariuvidal.csv")

#CABA

caba1 <- horaciorlarreta <-read.csv("Data/horaciorlarreta.csv")
caba2 <- MatiasLammens <- read.csv("Data/MatiasLammens.csv")

#CATAMARCA 

catamarca1 <- RaulJalil_ok <- read.csv("Data/RaulJalil_ok.csv")
#R. Gomez presidente UCR NO POSEE / NO SE ENCUENTRA

#LA RIOJA . Nota: no hubo PASO

lrioja1 <- QuintelaRicardo <- read.csv("Data/QuintelaRicardo.csv")
lrioja2 <- JulioMartinezLR <- read.csv("Data/JulioMartinezLR.csv")


# PRESIDENCIALES 

presid1 <- alferdez <- read_xlsx("Data/alferdez.xlsx")
presid2 <-mauriciomacri <- read.csv("Data/mauriciomacri.csv")
presid3 <-RLavagna <- read.csv("Data/RLavagna.csv")
presid4 <-NicolasdelCano <- read.csv("Data/NicolasdelCano.csv")
presid5 <-juanjomalvinas <- read.csv("Data/juanjomalvinas.csv")
presid6 <-jlespert <- read.csv("Data/jlespert.csv")


# ids

datos_base <- read_xlsx("Data/datos_base.xlsx")

#####
#transformacion de datos base
#####
joined_presid <- rbind(presid1, presid2, presid3, presid4, presid5, presid6)

ver <- show_available_elections()
#####
###Exploración de popularidad de los discursos 
#####

# FUNCIONES

#de base

tablasWiki <- function(url){
  df_url_tables <-read_html(url) %>% 
    html_nodes("table")
 return(df_url_tables)
}

extraerTabla <- function(df_url_tables, nodo){
 
  ##extrae la tabla de interés
 ##y hace los primeros pasos de limpieza
  
   extracto_tabla <- html_table(df_url_tables[[nodo]], 
                              fill=TRUE, 
                              header=TRUE)  
  extracto_tabla <- extracto_tabla[-c(1, nrow(extracto_tabla)), ]
  
    return(extracto_tabla)
}
 
limpiezaTabla <- function(extracto_tabla){
  
  ## función destinada a la limpieza de algunos atributos
  ## de las tablas obtenidas de wikipedia con datos sobre la elección a gobernador
  
  #primero: borramos columnas innecesarias
  
  columnas_a_borrar <- c(3) # en todos los casos la columna tres es innecesaria
  columna_a_borrar <- 0   # vamos a buscar otras columnas a borrar
  # en particular, 
  # hay algunas tablas que tienen columnas con nombres vacios, pero no todas. 
  # las limpiamos      
  for (i in colnames(extracto_tabla)) {      
    columna_a_borrar <- columna_a_borrar + 1 
    if (is.na(i)) { columnas_a_borrar <- append(columnas_a_borrar, columna_a_borrar)} 
  }
  
  extracto_tabla_limpia <-  extracto_tabla[, -(columnas_a_borrar) ]
  
  #segundo: omitimos filas con valores vacios 
  extracto_tabla_limpia <- na.omit(extracto_tabla_limpia)
  
  #tercero: cambiamos nombres de las columnas
  extracto_tabla_limpia <- extracto_tabla_limpia %>% 
        rename(
          Candidato = 1,
            Vicecandidato = 2,
            Porcentaje = "%") 
  
  #cuarto: formateamos variables de interés: votos y porcentaje
  
  extracto_tabla_limpia <- extracto_tabla_limpia %>%  
    mutate(Porcentaje = str_replace(Porcentaje,"\\%", "")) %>% 
    mutate(Porcentaje = str_trim(
      str_replace(Porcentaje,"\\,", ".")
    ) ) %>% 
    mutate(Porcentaje = as.numeric(Porcentaje)) %>% 
    mutate(Votos = str_trim(
      str_replace(Votos,"\\.", "")
    ) ) %>% 
    mutate(Votos = as.numeric(Votos))

  
  return(extracto_tabla_limpia)
  
}

agregarVotosGobernador <- function(tabla_limpia){ 
  #calcula los votos agregados de la fórmula
  #para tablas que reportan de manera separada cada lista
 
  votos_afirmativos <- sum(tabla_limpia$Votos)
 
 tabla_summarized <- tabla_limpia %>% 
   group_by(Candidato) %>% 
   summarize(Votos = sum(Votos),
             Porcentaje = sum(Votos)/votos_afirmativos*100)
 
   return(tabla_summarized)
}

reducirLargoTabla <- function(tabla_limpia_agregada){
  
  #se queda solamente 
  # con los votos obtenidos por cada candidato

  tabla_reducida <- tabla_limpia_agregada %>% 
    subset(!str_detect(
      tabla_limpia_agregada$Candidato, "(Vot)|(Elect)|(Particip)|(Tot)")) 

  return(tabla_reducida)
}

añadirColumnas <- function(tabla_reducida, nombre_distrito = "") {
  
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
  
  arbol_tablas <- tablasWiki(url) 
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
  
  tabla_limpia <- limpiezaTabla(tabla_cruda)
  tabla_reducida <- reducirTabla(tabla_limpia)
  tabla_lista <- añadirColumnas(tabla_reducida, nombre_distrito)
  
  return(tabla_lista)
}


# EXTRACCION DE DATOS 

# provincias y presidenciales juntas


# urls a importar 

url_presid <- "https://es.wikipedia.org/wiki/Elecciones_presidenciales_de_Argentina_de_2019"

# datos provincias 

#urls

url_baires <- "https://es.wikipedia.org/wiki/Elecciones_provinciales_de_Buenos_Aires_de_2019"
url_caba <- "https://es.wikipedia.org/wiki/Elecciones_de_la_Ciudad_Aut%C3%B3noma_de_Buenos_Aires_de_2019"
url_sfe <- "https://es.wikipedia.org/wiki/Elecciones_provinciales_de_Santa_Fe_de_2019"
url_lrioja <- "https://es.wikipedia.org/wiki/Elecciones_provinciales_de_La_Rioja_(Argentina)_de_2019"
url_tfuego <- "https://es.wikipedia.org/wiki/Elecciones_provinciales_de_Tierra_del_Fuego_de_2019"
url_catamarca <- "https://es.wikipedia.org/wiki/Elecciones_provinciales_de_Catamarca_de_2019"
url_sluis <- "https://es.wikipedia.org/wiki/Elecciones_provinciales_de_San_Luis_de_2019"
url_formosa <- "https://es.wikipedia.org/wiki/Elecciones_provinciales_de_Formosa_de_2019"

#nota >(el nùmero de nodo lo identificamos manualmente al visitar cada sitio web
#y/o explorando la red de nodos obtenida en el paso anterior.
#por este motivo es que hemos encontrado convieniente armar dos funciones en pasos separados)

votos_baires_crudo <- extraer_datos_wiki(url_baires, 14)
votos_caba_crudo  <- extraer_datos_wiki(url_caba, 11)
votos_catamarca_crudo  <- extraer_datos_wiki(url_catamarca, 11)
votos_lrioja_crudo  <- extraer_datos_wiki(url_lrioja, 7)
votos_sfe_crudo <- extraer_datos_wiki(url_sfe, 8)
votos_sluis_crudo  <- extraer_datos_wiki(url_sluis, 3)
votos_tfuego_crudo  <- extraer_datos_wiki(url_tfuego, 8)
votos_formosa_crudo  <- extraer_datos_wiki(url_formosa, 2)


#Hacemos datos tidy y nos quedamos con lo que necesitamos

votos_baires <- procesar_datos_wiki(votos_baires_crudo, "Buenos Aires")
votos_caba <- procesar_datos_wiki(votos_caba_crudo, "CABA")
votos_catamarca <- procesar_datos_wiki(votos_catamarca_crudo, "Catamarca")
votos_lrioja <- procesar_datos_wiki(votos_lrioja_crudo, "La Rioja")
votos_sfe <- procesar_datos_wiki(votos_sfe_crudo, "Santa Fe")
votos_sluis <- procesar_datos_wiki(votos_sluis_crudo, "San Luis")

# Tratamiento separado: Formosa y Tierra del Fuego
#en los casos de tierra del fuego y formosa,
#el sistema electoral y la forma de comunicarlos nos exigen hacer transformaciones adicionales

# Tierra del Fuego ## RESOLVER 

limpiando_votos_tfuego <- votos_tfuego_crudo %>%  
  subset(!str_detect(votos_tfuego_crudo$`Partido/Alianza`, "%"))
limpiando_votos_tfuego <- reducirTabla(limpiando_votos_tfuego)  
  
  votos_tfuego_crudo[, -c(3)]
votos_tfuego_limpia <- limpiezaTabla(votos_tfuego_crudo)
votos_tfuego_limpia <- votos_tfuego_limpia[, -c(6)] %>% 
  rename("Partido/Alianza" = 3)
votos_tfuego_limpia_agregada <- agregarVotosGobernador(votos_tfuego_limpia)
votos_tfuego <- reducirTabla(votos_tfuego_limpia)
votos_tfuego$Distrito <- "Tierra del Fuego"

aver <- na.omit( votos_tfuego_crudo  )
# Formosa 

votos_formosa_limpia <- limpiezaTabla(votos_formosa_crudo)

votos_formosa_limpia_reducida <- reducirTabla(votos_formosa_limpia)

votos_formosa_limpia_reducida <- votos_formosa_limpia_reducida %>% 
  subset(!str_detect(votos_formosa_limpia_reducida$`Partido/Alianza`, "Total"))

votos_formosa_agregada <- agregarVotosGobernador(votos_formosa_limpia_reducida)

votos_formosa <- añadirColumnas(votos_formosa_agregada, "Formosa")

#uniremos estos dataframes en uno único a los fines de realizar los cálculos que no interesan



#unimos bases
votos_gobernadores <- rbind(votos_sfe, 
                            votos_baires, 
                            votos_caba, 
                            votos_catamarca, 
                           # votos_tfuego, 
                            votos_sluis, 
                           # votos_formosa, 
                            votos_lrioja)

#añadimos id de la cuenta de tuiter
votos_gobernadores <- left_join(votos_gobernadores, 
                            datos_base)

#GRAIFICANDO RELACIONES 
#####
# datos presidenciales

# importacion 
# wiki_presid <- read_html(
#   "https://es.wikipedia.org/wiki/Elecciones_presidenciales_de_Argentina_de_2019")
# 
# wiki_presid_nodos <- wiki_presid  %>% 
#   html_nodes("table") #%>% 
#   #html_table(fill = TRUE)

votos_presid <- extraerTabla(nodos_presid,28)

# limpieza de datos

# primera limpieza de filas y columnas irrelevantes
votos_presid <- votos_presid  %>% 
  subset(votos_presid$Votos != "NA")

# renombro columnas
colnames(votos_presid) <- as.character(head(votos_presid,1))

## descarto columnas. resultó más sencillo utilizar índices
votos_presid <- votos_presid[ , 1:6] 
votos_presid <- votos_presid[ ,-3]

# vuelvo a limpiar nombres restantes

votos_presid <- votos_presid %>%  
  rename(Vice = "Fórmula", 
         Partido = `Partido o alianza.1`, 
         Porcentaje = `Vicepresidente/a`)

retener <- str_which(votos_presid$Porcentaje,"%")

votos_presid <- votos_presid[retener, ]

# trasnformaciones en las variables

#reservamos una tabla base 
votos_presid_base <- votos_presid

votos_presid <- votos_presid_base %>% 
  mutate(Porcentaje = str_replace(Porcentaje,"\\%", "")) %>% 
  mutate(Porcentaje = str_trim(
      str_replace(Porcentaje,"\\,", ".")
      ) ) %>% 
  mutate(Porcentaje = as.numeric(Porcentaje)) %>% 
  mutate(Votos = str_trim(
    str_replace(Votos,"\\.", "")
  ) ) %>% 
  mutate(Votos = as.numeric(Votos))
    

# transformaciones en datos de base, para combinar
fecha_paso <- as.Date("2019-08-11")
fecha_grales <- as.Date("2019-10-27")



presid_popu <- joined_presid %>%  
  mutate(fecha_tuit = as.Date(created_at)) %>% 
  subset( fecha_tuit < fecha_grales & fecha_tuit > fecha_paso ) %>% 
  select(fecha_tuit, screen_name, rts,fav_count, followers_count, friends_count )

presid_popu_ranking <- presid_popu %>% 
  group_by(screen_name) %>% 
  summarise(rts_obtenidos_totales = sum(rts),
            favs_obtenidos_totales = sum(fav_count),
            cantidad_emitidos_totales = n(),
            rts_obtenidos_promedio = rts_obtenidos_totales/cantidad_emitidos_totales,
            favs_obtenidos_promedio = favs_obtenidos_totales/cantidad_emitidos_totales
            )

# uniendo ambas bases 
screen_name <- presid_popu_ranking$screen_name
votos_presid$screen_name <- c("alferdez", "mauriciomacri", "RLavagna", "juanjomalvinas", "NicolasdelCano", "jlespert" , NA, NA, NA, NA, NA , NA )

presid_popu_ranking <- full_join(presid_popu_ranking, votos_presid, by= "screen_name")


# Graficando
ggplot(presid_popu_ranking, aes(rts_obtenidos_promedio, Porcentaje, colour = screen_name)) +
  geom_point()





# Datos provincias

url_bsas <- "https://es.wikipedia.org/wiki/Elecciones_provinciales_de_Buenos_Aires_de_2019"
votos_bsas <- read_html(url_bsas) %>% 
  html_nodes("table")


#####
###Exploración de clivajes entre los discursos
#####