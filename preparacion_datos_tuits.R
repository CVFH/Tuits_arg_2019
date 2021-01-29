# preparacion_datos_tuits

# en este archivo armamos nuestras bases de datos principales


#####
# APERTURA DE LIBRERIAS
#####

#paquetes

library(tidyverse)
library(janitor)
#library(readxl)

#propias

source("https://raw.githubusercontent.com/CVFH/Tuits_arg_2019/master/Modules/tuitsCandidatos.R", encoding = "UTF-8")

#####
#IMPORTACION DE DATOS #####

traerDatosTuits <- function(tipo_dato){
  
 # función que trae los datos necesarios.
 # opciones: candidatos a presidente, a gobernador, todos juntos, datos de base
 # "presid", "gob", "tot", "base", respectivamente
  
  if(tipo_dato == "base") {
 # ids

    datos_base <- read.csv("https://raw.githubusercontent.com/CVFH/Tuits_arg_2019/master/Data/datos_base.csv", encoding = "UTF-8", stringsAsFactors = FALSE)
    devolver_data <- datos_base
 }

  else if (tipo_dato == "gob") {
    
  #QUE DESDOBLARON: ELECCIONES GRALES 16/JUNIO. SOLO SANTA FE PASO 28/ABRIL
  
  fecha_elecciones_desdoblada <-as.Date("2019-06-16")
  fecha_campaña_desdoblada <-as.Date("2019-04-28")
  
  formosa1 <- "https://raw.githubusercontent.com/CVFH/Tuits_arg_2019/master/Data/insfran_gildo.csv"
  formosa2 <- "https://raw.githubusercontent.com/CVFH/Tuits_arg_2019/master/Data/adrianbogadoOK.csv"
  sluis1 <-"https://raw.githubusercontent.com/CVFH/Tuits_arg_2019/master/Data/alberto_rsaa.csv"
  sluis2 <-"https://raw.githubusercontent.com/CVFH/Tuits_arg_2019/master/Data/claudiojpoggi.csv"
  sfe1 <- "https://raw.githubusercontent.com/CVFH/Tuits_arg_2019/master/Data/omarperotti.csv"
  sfe2 <-"https://raw.githubusercontent.com/CVFH/Tuits_arg_2019/master/Data/AntonioBonfatti.csv"
  tfuego1 <-"https://raw.githubusercontent.com/CVFH/Tuits_arg_2019/master/Data/gustavomelella.csv"
  tfuego2 <-"https://raw.githubusercontent.com/CVFH/Tuits_arg_2019/master/Data/RosanaBertone.csv"
  
  desdobladas_filenames <- c(formosa1, formosa2, sluis1, sluis2, sfe1, sfe2, tfuego1, tfuego2)
  
  desdobladas_df <- desdobladas_filenames %>% 
    map_dfr(read.csv, encoding = "UTF-8", stringsAsFactors = FALSE) %>% 
    determinarTuitsCampaña(fecha_campaña_desdoblada, fecha_elecciones_desdoblada)
  
  
  #SIMULTANEAS: QUE CELEBRARON ELECCIONES GRALES 27/OCTUBRE, PASO 11/AGOSTO
  
  fecha_paso <- as.Date("2019-08-11")
  fecha_grales <- as.Date("2019-10-27")
  
  baires1 <- "https://raw.githubusercontent.com/CVFH/Tuits_arg_2019/master/Data/Kicillofok.csv"
  baires2 <- "https://raw.githubusercontent.com/CVFH/Tuits_arg_2019/master/Data/mariuvidal.csv"
  caba1 <- "https://raw.githubusercontent.com/CVFH/Tuits_arg_2019/master/Data/horaciorlarreta.csv"
  caba2 <- "https://raw.githubusercontent.com/CVFH/Tuits_arg_2019/master/Data/MatiasLammens.csv"
  catamarca1 <- "https://raw.githubusercontent.com/CVFH/Tuits_arg_2019/master/Data/RaulJalil_ok.csv"
  lrioja1 <- "https://raw.githubusercontent.com/CVFH/Tuits_arg_2019/master/Data/QuintelaRicardo.csv"
  lrioja2 <- "https://raw.githubusercontent.com/CVFH/Tuits_arg_2019/master/Data/JulioMartinezLR.csv"
  
  simultaneas_filenames <- c(baires1, baires2, caba1, caba2, catamarca1, lrioja1, lrioja2)
  
  simultaneas_df <- simultaneas_filenames %>% 
    map_dfr(read.csv, encoding = "UTF-8" ) %>% 
    determinarTuitsCampaña(fecha_paso, fecha_grales)
  
  joined_gobernadores <- rbind(simultaneas_df, desdobladas_df)  %>% 
    select("created_at", 
           "text", 
           "rts", "fav_count", 
           "tweet_id", 
           "screen_name", "user_id", "description", 
           "location", 
           "mention_screen_names", 
           "in_reply_to_screen_name")
  
  devolver_data <- joined_gobernadores
 
  }
  
  else if (tipo_dato == "presid") {
    
  # PRESIDENCIALES 
    
    # fechas
    
    fecha_paso <- as.Date("2019-08-11")
    fecha_grales <- as.Date("2019-10-27")

  # enlaces
    
  presid1 <- "https://raw.githubusercontent.com/CVFH/Tuits_arg_2019/master/Data/alferdez.csv"
  presid2 <- "https://raw.githubusercontent.com/CVFH/Tuits_arg_2019/master/Data/mauriciomacri.csv"
  presid3 <- "https://raw.githubusercontent.com/CVFH/Tuits_arg_2019/master/Data/RLavagna.csv"
  presid4 <- "https://raw.githubusercontent.com/CVFH/Tuits_arg_2019/master/Data/NicolasdelCano.csv"
  presid5 <- "https://raw.githubusercontent.com/CVFH/Tuits_arg_2019/master/Data/juanjomalvinas.csv"
  presid6 <- "https://raw.githubusercontent.com/CVFH/Tuits_arg_2019/master/Data/jlespert.csv"
  
  presid_filenames <- c(presid2, presid3, presid4, presid5, presid6)
  
  joined_presid <- presid_filenames %>% 
    map_dfr(read.csv, encoding = "UTF-8", stringsAsFactors = FALSE ) %>% 
    determinarTuitsCampaña(fecha_paso, fecha_grales) %>% 
    select("created_at", 
           "text", 
           "rts", "fav_count", 
           "tweet_id", 
           "screen_name", "user_id", "description", 
           "location", 
           "mention_screen_names", 
           "in_reply_to_screen_name")

  presid1 <- read.csv(presid1, encoding = "UTF-8", stringsAsFactors = FALSE) %>% 
    determinarTuitsCampaña(fecha_paso, fecha_grales) %>% 
    select("created_at", 
           "text", 
           "rts", "fav_count", 
           "tweet_id", 
           "screen_name", "user_id", "description", 
           "location", 
           "mention_screen_names", 
           "in_reply_to_screen_name")
  
  joined_presid <- joined_presid %>% rbind(presid1)
  
  devolver_data <- joined_presid

}

  else if (tipo_dato=="tot") {
    
  # traemos bases separadas
    
     joined_presid <- traerDatosTuits("presid")
     joined_gobernadores <- traerDatosTuits("gob")
     
  # las unimos
      
  joined_candidatos <- rbind(joined_gobernadores,
                             joined_presid) 
  
  devolver_data <- joined_candidatos

  }
  
  else {
    devolver_data <- "datos no disponibles"
    #pendiente: hacer un raise warning
  }
 
  return(devolver_data)

}