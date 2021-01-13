

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
source("Modules/tablasElectorales.R")
source("Modules/tuitsCandidatos.R")

#####
#IMPORTACION DE DATOS
#####

# ids

datos_base <- read_xlsx("Data/datos_base.xlsx")


#QUE DESDOBLARON: ELECCIONES GRALES 16/JUNIO. SOLO SANTA FE PASO 28/ABRIL
as.Character
fecha_elecciones_desdoblada <-as.Date("2019-06-16")
fecha_campaña_desdoblada <-as.Date("2019-04-28")

# Formosa

formosa1 <- read.csv("Data/insfran_gildo.csv", encoding = "UTF-8")
formosa1 <- formosa1 %>% determinarTuitsCampana(fecha_campaña_desdoblada,fecha_elecciones_desdoblada)

formosa1_tokenizada <- formosa1 %>% tokenizarTextoTuits() 
formosa1_tokenizada <- formosa1_tokenizada %>%  limpiarTokens()


