# preparacion_datos_tuits

# en este archivo armamos nuestras bases de datos principales


#####
# APERTURA DE LIBRERIAS
#####

#paquetes

require(tidyverse)
require(janitor)
require(readxl)
require(rvest) # extraer datos de html

#propias

source("Modules/tuitsCandidatos.R", encoding = "UTF-8")

#####
#IMPORTACION DE DATOS
#####

# ids

datos_base <- read_xlsx("Data/datos_base.xlsx")

#QUE DESDOBLARON: ELECCIONES GRALES 16/JUNIO. SOLO SANTA FE PASO 28/ABRIL

fecha_elecciones_desdoblada <-as.Date("2019-06-16")
fecha_campaña_desdoblada <-as.Date("2019-04-28")

formosa1 <- "Data/insfran_gildo.csv"
formosa2 <- "Data/adrianbogadoOK.csv"
sluis1 <-"Data/alberto_rsaa.csv"
sluis2 <-"Data/claudiojpoggi.csv"
sfe1 <- "Data/omarperotti.csv"
sfe2 <-"Data/AntonioBonfatti.csv"
tfuego1 <-"Data/gustavomelella.csv"
tfuego2 <-"Data/RosanaBertone.csv"

desdobladas_filenames <- c(formosa1, formosa2, sluis1, sluis2, sfe1, sfe2, tfuego1, tfuego2)

desdobladas_df <- desdobladas_filenames %>% 
  map_dfr(read.csv, encoding = "UTF-8" ) %>% 
  determinarTuitsCampaña(fecha_campaña_desdoblada, fecha_elecciones_desdoblada)


#SIMULTANEAS: QUE CELEBRARON ELECCIONES GRALES 27/OCTUBRE, PASO 11/AGOSTO

fecha_paso <- as.Date("2019-08-11")
fecha_grales <- as.Date("2019-10-27")

baires1 <- "Data/Kicillofok.csv"
baires2 <- "Data/mariuvidal.csv"
caba1 <- "Data/horaciorlarreta.csv"
caba2 <- "Data/MatiasLammens.csv"
catamarca1 <- "Data/RaulJalil_ok.csv"
lrioja1 <- "Data/QuintelaRicardo.csv"
lrioja2 <- "Data/JulioMartinezLR.csv"

simultaneas_filenames <- c(baires1, baires2, caba1, caba2, catamarca1, lrioja1, lrioja2)

simultaneas_df <- simultaneas_filenames %>% 
  map_dfr(read.csv, encoding = "UTF-8" ) %>% 
  determinarTuitsCampaña(fecha_paso, fecha_grales)

joined_gobernadores <- rbind(simultaneas_df, desdobladas_df)

# PRESIDENCIALES 

# aquí tenemos un archivo -xlsx y los demás .csv. 
# resultó más sencillo descargarlos independientemente y luego unirlos

presid1 <- read_xlsx("Data/alferdez.xlsx")
presid2 <- read.csv("Data/mauriciomacri.csv")
presid3 <- read.csv("Data/RLavagna.csv")
presid4 <- read.csv("Data/NicolasdelCano.csv")
presid5 <- read.csv("Data/juanjomalvinas.csv")
presid6 <- read.csv("Data/jlespert.csv")

joined_presid <- rbind(presid1, presid2, presid3, presid4, presid5, presid6)

joined_presid <- joined_presid %>% 
  determinarTuitsCampaña(fecha_paso, fecha_grales)
