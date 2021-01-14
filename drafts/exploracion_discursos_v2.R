

#####
# APERTURA DE LIBRERIAS
#####
#paquetes

require(tidyverse)
require(readxl)
require(rvest) # extraer datos de html
require(ggplot2)
require(polAr)
library(patchwork) # para unir graficos
require(tidytext)

#propias
source("Modules/tablasElectorales.R", encoding = "UTF-8")
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

desdobladas_files <- c(formosa1, formosa2, sluis1, sluis2, sfe1, sfe2, tfuego1, tfuego2)

desdobladas_df <- desdobladas_files %>%  
  map_dfr( read.csv, encoding = "UTF-8" )

desdobladas_df <- desdobladas_df %>% 
  map_dfr(determinarTuitsCampaña, fecha_campaña_desdoblada,fecha_elecciones_desdoblada)

      #       
#%>% 
  determinarTuitsCampaña(fecha_campaña_desdoblada,fecha_elecciones_desdoblada)
#SIMULTANEAS: QUE CELEBRARON ELECCIONES GRALES 27/OCTUBRE, PASO 11/AGOSTO

fecha_paso <- as.Date("2019-08-11")
fecha_grales <- as.Date("2019-10-27")


baires1 <- read.csv("Data/Kicillofok.csv", encoding = "UTF-8") %>% determinarTuitsCampana(fecha_paso, fecha_grales)
baires2 <- read.csv("Data/mariuvidal.csv", encoding = "UTF-8") %>% determinarTuitsCampana(fecha_paso, fecha_grales)
caba1 <- read.csv("Data/horaciorlarreta.csv", encoding = "UTF-8") %>% determinarTuitsCampana(fecha_paso, fecha_grales)
caba2 <- read.csv("Data/MatiasLammens.csv", encoding = "UTF-8") %>% determinarTuitsCampana(fecha_paso, fecha_grales)
catamarca1 <- read.csv("Data/RaulJalil_ok.csv", encoding = "UTF-8") %>% determinarTuitsCampana(fecha_paso, fecha_grales)
lrioja1 <- read.csv("Data/QuintelaRicardo.csv", encoding = "UTF-8") %>% determinarTuitsCampana(fecha_paso, fecha_grales)
lrioja2 <- read.csv("Data/JulioMartinezLR.csv", encoding = "UTF-8") %>% determinarTuitsCampana(fecha_paso, fecha_grales)


# PRESIDENCIALES 

presid1 <- read_xlsx("Data/alferdez.xlsx") %>% determinarTuitsCampana(fecha_paso, fecha_grales)
presid2 <- read.csv("Data/mauriciomacri.csv", encoding = "UTF-8") %>% determinarTuitsCampana(fecha_paso, fecha_grales)
presid3 <- read.csv("Data/RLavagna.csv", encoding = "UTF-8") %>% determinarTuitsCampana(fecha_paso, fecha_grales)
presid4 <- read.csv("Data/NicolasdelCano.csv", encoding = "UTF-8") %>% determinarTuitsCampana(fecha_paso, fecha_grales)
presid5 <- read.csv("Data/juanjomalvinas.csv", encoding = "UTF-8") %>% determinarTuitsCampana(fecha_paso, fecha_grales)
presid6 <- read.csv("Data/jlespert.csv", encoding = "UTF-8") %>% determinarTuitsCampana(fecha_paso, fecha_grales)


#####
# TRANSFORMACIONES DE LOS DATOS
#####
# Tokenizando
######

formosa1_tokenizada <- formosa1 %>% tokenizarTextoTuits() 
formosa1_tokenizada_limpia <- formosa1_tokenizada %>%  limpiarTokens()

formosa2_tokenizada <- formosa2 %>% tokenizarTextoTuits() 
formosa2_tokenizada_limpia <- formosa2_tokenizada %>%  limpiarTokens()

sluis1_tokenizada <- sluis1 %>% tokenizarTextoTuits() 
sluis1_tokenizada_limpia <- sluis1_tokenizada %>%  limpiarTokens()

sluis2_tokenizada <- sluis2 %>% tokenizarTextoTuits() 
sluis2_tokenizada_limpia <- sluis2_tokenizada %>%  limpiarTokens()

sfe1_tokenizada <- sfe1 %>% tokenizarTextoTuits() 
sfe1_tokenizada_limpia <- sfe1_tokenizada %>%  limpiarTokens()

sfe2_tokenizada <- sfe2 %>% tokenizarTextoTuits() 
sfe2_tokenizada_limpia <- sfe2_tokenizada %>%  limpiarTokens()

tfuego1_tokenizada <- tfuego1 %>% tokenizarTextoTuits() 
tfuego1_tokenizada_limpia <- tfuego1_tokenizada %>%  limpiarTokens()

tfuego2_tokenizada <- tfuego2 %>% tokenizarTextoTuits() 
tfuego2_tokenizada_limpia <- tfuego2_tokenizada %>%  limpiarTokens

baires1_tokenizada <- baires1 %>% tokenizarTextoTuits() 
baires1_tokenizada_limpia <- baires1_tokenizada %>%  limpiarTokens()

baires2_tokenizada <- baires2 %>% tokenizarTextoTuits() 
baires2_tokenizada_limpia <- baires2_tokenizada %>%  limpiarTokens()

caba1_tokenizada <- caba1 %>% tokenizarTextoTuits() 
caba1_tokenizada_limpia <- caba1_tokenizada %>%  limpiarTokens()

caba2_tokenizada <- caba2 %>% tokenizarTextoTuits() 
caba2_tokenizada_limpia <- caba2_tokenizada %>%  limpiarTokens()

catamarca1_tokenizada <- catamarca1 %>% tokenizarTextoTuits() 
catamarca1_tokenizada_limpia <- catamarca1_tokenizada %>%  limpiarTokens()

lrioja1_tokenizada <- lrioja1 %>% tokenizarTextoTuits() 
lrioja1_tokenizada_limpia <- lrioja1_tokenizada %>%  limpiarTokens()

lrioja2_tokenizada <- lrioja2 %>% tokenizarTextoTuits() 
lrioja2_tokenizada_limpia <- lrioja2_tokenizada %>%  limpiarTokens()

presid1_tokenizada <- presid1 %>% tokenizarTextoTuits() 
presid1_tokenizada_limpia <- presid1_tokenizada %>%  limpiarTokens()

presid2_tokenizada <- presid2 %>% tokenizarTextoTuits() 
presid2_tokenizada_limpia <- presid2_tokenizada %>%  limpiarTokens()

presid3_tokenizada <- presid3 %>% tokenizarTextoTuits() 
presid3_tokenizada_limpia <- presid3_tokenizada %>%  limpiarTokens()

presid4_tokenizada <- presid4 %>% tokenizarTextoTuits() 
presid4_tokenizada_limpia <- presid4_tokenizada %>%  limpiarTokens()

presid5_tokenizada <- presid5 %>% tokenizarTextoTuits() 
presid5_tokenizada_limpia <- presid5_tokenizada %>%  limpiarTokens()

presid6_tokenizada <- presid6 %>% tokenizarTextoTuits() 
presid6_tokenizada_limpia <- presid6_tokenizada %>%  limpiarTokens()

######
#Uniendo
#######

joined_presid_tokenizadas <- rbind(presid1_tokenizada, 
                                   presid2_tokenizada, 
                                   presid3_tokenizada, 
                                   presid4_tokenizada, 
                                   presid5_tokenizada, 
                                   presid6_tokenizada)
  
joined_presid_tokenizadas_limpias <- rbind(presid1_tokenizada_limpia, 
                                           presid2_tokenizada_limpia, 
                                           presid3_tokenizada_limpia, 
                                           presid4_tokenizada_limpia, 
                                           presid5_tokenizada_limpia, 
                                           presid6_tokenizada_limpia)
  
  
joined_gobernadores_tokenizadas <- rbind(
  formosa1_tokenizada,
  formosa2_tokenizada,
  sluis1_tokenizada,
  sluis2_tokenizada,
  sfe1_tokenizada,
  sfe2_tokenizada,
  baires1_tokenizada,
  baires2_tokenizada,
  caba1_tokenizada,
  caba2_tokenizada,
  tfuego1_tokenizada,
  tfuego2_tokenizada,
  lrioja1_tokenizada,
  lrioja2_tokenizada,
  catamarca1_tokenizada )
  
  
joined_gobernadores_tokenizadas_limpias <-rbind(
  formosa1_tokenizada_limpia,
  formosa2_tokenizada_limpia,
  sluis1_tokenizada_limpia,
  sluis2_tokenizada_limpia,
  sfe1_tokenizada_limpia,
  sfe2_tokenizada_limpia,
  baires1_tokenizada_limpia,
  baires2_tokenizada_limpia,
  caba1_tokenizada_limpia,
  caba2_tokenizada_limpia,
  tfuego1_tokenizada_limpia,
  tfuego2_tokenizada_limpia,
  lrioja1_tokenizada_limpia,
  lrioja2_tokenizada_limpia,
  catamarca1_tokenizada_limpia )

######
# EXPLORANDO DATOS
#####
#Cantidad de palabras
######


