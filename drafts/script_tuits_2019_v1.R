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

sanluis1 <- arsaa2019 <- read.csv("Data/arsaa2019.csv")
sanluis2 <- claudiojpoggi <- read.csv("Data/claudiojpoggi.csv")

# Santa Fe # perdio oficialismo

santafe1 <- omarperotti <- read.csv("Data/Kicillofok.csv")
santafe2 <- AntonioBonfatti <- read.csv("Data/Kicillofok.csv")

# Tierra del Fuego # perdio oficialismo

tfuego1 <- gustavomelella <- read.csv("Data/gustavomelella.csv")
tfuego2 <- RosanaBertone <- read.csv("Data/RosanaBertone.csv")


#SIMULTANEAS: QUE CELEBRARON ELECCIONES GRALES 27/OCTUBRE, PASO 11/AGOSTO

#PBA

PBA1 <- Kicillofok <- read.csv("Data/Kicillofok.csv")
PBA2 <- mariuvidal <- read.csv("Data/mariuvidal.csv")

#CABA

CABA1 <- horaciorlarreta <-read.csv("Data/horaciorlarreta.csv")
CABA2 <- MatiasLammens <- read.csv("Data/MatiasLammens.csv")

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

#####
#transformacion de datos base
#####
joined_presid <- rbind(presid1, presid2, presid3, presid4, presid5, presid6)

ver <- show_available_elections()
#####
###Exploración de popularidad de los discursos 
#####

# FUNCIONES

# abrir tablas de una pagina de wikipedia
tablasWiki <- function(url){
  df_url_tables <-read_html(url) %>% 
    html_nodes("table")
 return(df_url_tables)
}


# datos presidenciales

# importacion 
wiki_presid <- read_html(
  "https://es.wikipedia.org/wiki/Elecciones_presidenciales_de_Argentina_de_2019")

wiki_presid_nodos <- wiki_presid  %>% 
  html_nodes("table") #%>% 
  #html_table(fill = TRUE)

votos_presid <- html_table(wiki_presid_nodos[[28]], 
                           fill=TRUE, 
                           header=TRUE) 

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