# preparacion_datos_electorales.R

# en este archivo abrimos y preparamos datos electorales 
#####
# APERTURA DE LIBRERIAS
#####
#paquetes

require(tidyverse)
require(rvest) # extraer datos de html

#propias
source("Modules/tablasElectorales.R", encoding = "UTF-8")

#####
# EXTRACCION Y TRANSFORMACION / DATOS ELECTORALES
#####

#####
#PRESIDENCIALES
#####

# ids

datos_base <- read_xlsx("Data/datos_base.xlsx")

# urls a importar 

url_presid <- "https://es.wikipedia.org/wiki/Elecciones_presidenciales_de_Argentina_de_2019"

# Procesamiento

votos_presid_crudo <- extraer_datos_wiki(url_presid, 28)
votos_presid <- procesar_datos_wiki(votos_presid_crudo, "Nación")

# agregamos datos de base

votos_presid <- left_join(votos_presid, 
                          datos_base)
#####
# PROVINCIALES 
#####

#urls

url_baires <- "https://es.wikipedia.org/wiki/Elecciones_provinciales_de_Buenos_Aires_de_2019"
url_caba <- "https://es.wikipedia.org/wiki/Elecciones_de_la_Ciudad_Aut%C3%B3noma_de_Buenos_Aires_de_2019"
url_sfe <- "https://es.wikipedia.org/wiki/Elecciones_provinciales_de_Santa_Fe_de_2019"
url_lrioja <- "https://es.wikipedia.org/wiki/Elecciones_provinciales_de_La_Rioja_(Argentina)_de_2019"
#url_tfuego <- "https://es.wikipedia.org/wiki/Elecciones_provinciales_de_Tierra_del_Fuego_de_2019"
url_catamarca <- "https://es.wikipedia.org/wiki/Elecciones_provinciales_de_Catamarca_de_2019"
url_sluis <- "https://es.wikipedia.org/wiki/Elecciones_provinciales_de_San_Luis_de_2019"
url_formosa <- "https://es.wikipedia.org/wiki/Elecciones_provinciales_de_Formosa_de_2019"

# procesamiento 

#nota >(el nùmero de nodo lo identificamos manualmente al visitar cada sitio web
#y/o explorando la red de nodos obtenida en el paso anterior.
#por este motivo es que hemos encontrado convieniente armar dos funciones en pasos separados)

votos_baires_crudo <- extraer_datos_wiki(url_baires, 14)
votos_caba_crudo  <- extraer_datos_wiki(url_caba, 11)
votos_catamarca_crudo  <- extraer_datos_wiki(url_catamarca, 11)
votos_lrioja_crudo  <- extraer_datos_wiki(url_lrioja, 7)
votos_sfe_crudo <- extraer_datos_wiki(url_sfe, 8)
votos_sluis_crudo  <- extraer_datos_wiki(url_sluis, 3)
#votos_tfuego_crudo  <- extraer_datos_wiki(url_tfuego, 8)
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
#Para Tierra del Fuego además recurrimos a otra fuente

# Tierra del Fuego  

url_tfuego <- "https://www.argentina.gob.ar/analisis-politico-electoral/tierra-del-fuego"
votos_tfuego_crudo <- extraer_datos_wiki(url_tfuego, 1)
votos_tfuego <- votos_tfuego_crudo %>% 
  reducirLargoTabla() %>% 
  agregarColumnas("Tierra del Fuego") %>% 
  rename( 'Partido/Alianza' = "Agrupación")

# Formosa 

votos_formosa_limpia <- votos_formosa_crudo %>% 
  borrarPrimeraFila() %>% 
  reducirAnchoTabla() %>% 
  omitNaTabla() %>% 
  renombrarTabla() %>% 
  limpiezaTabla() %>% 
  reducirLargoTabla()

votos_formosa_limpia <- votos_formosa_limpia %>% 
  subset(!str_detect(votos_formosa_limpia$`Partido/Alianza`, "Total"))

votos_formosa_agregada <- agregarVotosGobernador(votos_formosa_limpia)

votos_formosa <- agregarColumnas(votos_formosa_agregada, "Formosa")

#uniremos estos dataframes en uno único a los fines de realizar los cálculos que no interesan

#unimos bases
votos_gobernadores <- bind_rows(votos_sfe, 
                                votos_baires, 
                                votos_caba, 
                                votos_catamarca, 
                                votos_tfuego, 
                                votos_sluis, 
                                votos_formosa, 
                                votos_lrioja)

#añadimos id de la cuenta de tuiter
votos_gobernadores <- left_join(votos_gobernadores, 
                                datos_base)