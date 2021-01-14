

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


#propias
source("Modules/tablasElectorales.R", encoding = "UTF-8")
source("Modules/tuitsCandidatos.R", encoding = "UTF-8")

#####
# IMPORTACION DE DATOS / TUITS
#####

# ids

datos_base <- read_xlsx("Data/datos_base.xlsx")


#QUE DESDOBLARON: ELECCIONES GRALES 16/JUNIO. SOLO SANTA FE PASO 28/ABRIL

fecha_elecciones_desdoblada <-as.Date("2019-06-16")
fecha_campaña_desdoblada <-as.Date("2019-04-28")

# Formosa

formosa1 <- read.csv("Data/insfran_gildo.csv")
formosa2 <- read.csv("Data/adrianbogadoOK.csv")

# San Luis

sluis1 <- read.csv("Data/alberto_rsaa.csv")
sluis2 <- read.csv("Data/claudiojpoggi.csv")

# Santa Fe # perdio oficialismo

sfe1 <- read.csv("Data/omarperotti.csv")
sfe2 <- read.csv("Data/AntonioBonfatti.csv")

# Tierra del Fuego # perdio oficialismo

tfuego1 <- read.csv("Data/gustavomelella.csv")
tfuego2 <- read.csv("Data/RosanaBertone.csv")


#SIMULTANEAS: QUE CELEBRARON ELECCIONES GRALES 27/OCTUBRE, PASO 11/AGOSTO

fecha_paso <- as.Date("2019-08-11")
fecha_grales <- as.Date("2019-10-27")

#PBA

baires1 <- read.csv("Data/Kicillofok.csv")
baires2 <- read.csv("Data/mariuvidal.csv")

#CABA

caba1 <- read.csv("Data/horaciorlarreta.csv")
caba2 <- read.csv("Data/MatiasLammens.csv")

#CATAMARCA 

catamarca1 <- read.csv("Data/RaulJalil_ok.csv")
#R. Gomez presidente UCR NO POSEE / NO SE ENCUENTRA

#LA RIOJA . Nota: no hubo PASO

lrioja1 <- read.csv("Data/QuintelaRicardo.csv")
lrioja2 <- read.csv("Data/JulioMartinezLR.csv")


# PRESIDENCIALES 

presid1 <- read_xlsx("Data/alferdez.xlsx")
presid2 <- read.csv("Data/mauriciomacri.csv")
presid3 <- read.csv("Data/RLavagna.csv")
presid4 <- read.csv("Data/NicolasdelCano.csv")
presid5 <- read.csv("Data/juanjomalvinas.csv")
presid6 <- read.csv("Data/jlespert.csv")

#####
## TRANSFORMACIONES DE LOS DATOS / TUITS
#####
# determinamos qué tuits fueron emitidos durante la campaña

#para desdobladas

formosa1 <- formosa1 %>% determinarTuitsCampaña(fecha_campaña_desdoblada,fecha_elecciones_desdoblada)
formosa2 <- formosa2 %>% determinarTuitsCampaña(fecha_campaña_desdoblada,fecha_elecciones_desdoblada)
sluis1 <- sluis1 %>% determinarTuitsCampaña(fecha_campaña_desdoblada,fecha_elecciones_desdoblada)
sluis2 <- sluis2 %>% determinarTuitsCampaña(fecha_campaña_desdoblada,fecha_elecciones_desdoblada)
sfe1 <- sfe1 %>% determinarTuitsCampaña(fecha_campaña_desdoblada,fecha_elecciones_desdoblada)
sfe2 <- sfe2 %>% determinarTuitsCampaña(fecha_campaña_desdoblada,fecha_elecciones_desdoblada)
tfuego1 <- tfuego1 %>% determinarTuitsCampaña(fecha_campaña_desdoblada,fecha_elecciones_desdoblada)
tfuego2 <- tfuego2 %>% determinarTuitsCampaña(fecha_campaña_desdoblada,fecha_elecciones_desdoblada)

# para simultaneas

baires1 <- baires1 %>% determinarTuitsCampaña(fecha_paso, fecha_grales)
baires2 <- baires2 %>% determinarTuitsCampaña(fecha_paso, fecha_grales)
caba1 <- caba1 %>% determinarTuitsCampaña(fecha_paso, fecha_grales)
caba2 <- caba2 %>% determinarTuitsCampaña(fecha_paso, fecha_grales)
catamarca1 <- catamarca1 %>% determinarTuitsCampaña(fecha_paso, fecha_grales)
lrioja1 <- lrioja1 %>% determinarTuitsCampaña(fecha_paso, fecha_grales)
lrioja2 <- lrioja2 %>% determinarTuitsCampaña(fecha_paso, fecha_grales)
presid1 <- presid1 %>% determinarTuitsCampaña(fecha_paso, fecha_grales)
presid2 <- presid2 %>% determinarTuitsCampaña(fecha_paso, fecha_grales)
presid3 <- presid3 %>% determinarTuitsCampaña(fecha_paso, fecha_grales)
presid4 <- presid4 %>% determinarTuitsCampaña(fecha_paso, fecha_grales)
presid5 <-presid5 %>% determinarTuitsCampaña(fecha_paso, fecha_grales)
presid6 <- presid6 %>% determinarTuitsCampaña(fecha_paso, fecha_grales)

# unimos bases

joined_presid <- rbind(presid1, presid2, presid3, presid4, presid5, presid6)

joined_gobernadores <- rbind(
  formosa1,
  formosa2,
  sluis1,
  sluis2,
  sfe1,
  sfe2,
  baires1,
  baires2,
  caba1,
  caba2,
  tfuego1,
  tfuego2,
  lrioja1,
  lrioja2,
  catamarca1 )

#####
# EXTRACCION Y TRANSFORMACION / DATOS ELECTORALES
#####
#PRESIDENCIALES
#####
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

# Tierra del Fuego ## RESOLVER 

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

#####
# UNIENDO Y TRABAJANDO DATOS / TUTIS / ELECTORALES
#####

# Provincias

gobernadores_popu <- joined_gobernadores %>%  
  subset( Campaña == 1 ) %>% 
  select(created_at, screen_name, rts,fav_count, followers_count, friends_count ) %>% 
  group_by(screen_name) %>% 
  summarise(rts_obtenidos_totales = sum(rts),
            favs_obtenidos_totales = sum(fav_count),
            cantidad_emitidos_totales = n(),
            rts_obtenidos_promedio = rts_obtenidos_totales/cantidad_emitidos_totales,
            favs_obtenidos_promedio = favs_obtenidos_totales/cantidad_emitidos_totales
  )

# uniendo ambas bases 

gobernadores_popu_ranking <- full_join(gobernadores_popu, 
                                 votos_gobernadores %>% 
                                   subset( Ranking == 1 | Ranking ==2 ) %>% 
                                   subset(!is.na(screen_name)),
                                          "screen_name")

# Presidente

presid_popu <- joined_presid %>%  
  subset( Campaña == 1 ) %>% 
  select(created_at, screen_name, rts,fav_count, followers_count, friends_count ) %>% 
  group_by(screen_name) %>% 
  summarise(rts_obtenidos_totales = sum(rts),
            favs_obtenidos_totales = sum(fav_count),
            cantidad_emitidos_totales = n(),
            rts_obtenidos_promedio = rts_obtenidos_totales/cantidad_emitidos_totales,
            favs_obtenidos_promedio = favs_obtenidos_totales/cantidad_emitidos_totales
  )

# uniendo ambas bases 

presid_popu_ranking <- full_join(presid_popu, 
                                 votos_presid, 
                                 "screen_name")

#####
# GRAFICOS DE RELACIONES
#####
# provincias y nacion por separado
#####
# Provincias

#rts

gobernadores_rtspromedio_porcentaje <- ggplot(gobernadores_popu_ranking, 
       aes(rts_obtenidos_promedio, 
           Porcentaje, 
           colour = Distrito)) +
  geom_point(size = 2,alpha = 0.8) +
  geom_text(aes(label=screen_name),hjust=0, vjust=0) +
  theme_minimal()

#favs

gobernadores_favspromedio_porcentaje <- ggplot(gobernadores_popu_ranking, 
                                              aes(favs_obtenidos_promedio, 
                                                  Porcentaje, 
                                                  colour = Distrito)) +
  geom_point(size = 2,alpha = 0.8)+
  geom_text(aes(label=screen_name),hjust=0, vjust=0) +
  theme_minimal()


# Presidente

# rts

presid_rtspromedio_porcentaje <- ggplot(na.omit(presid_popu_ranking), 
       aes(rts_obtenidos_promedio, 
           Porcentaje, 
           colour = screen_name)) +
  geom_point(size = 2,alpha = 0.8) +
  theme_minimal()

#favs

presid_favspromedio_porcentaje <- ggplot(na.omit(presid_popu_ranking), 
                                        aes(favs_obtenidos_promedio, 
                                            Porcentaje, 
                                            colour = screen_name)) +
  geom_point(size = 2,alpha = 0.8) +
  theme_minimal()

#####
# TODOS LOS CANDIDATOS
#####

presid_popu_ranking <- presid_popu_ranking %>% rename("Partido/Alianza" = "Partido o alianza")
candidatos_popu_ranking <- rbind(gobernadores_popu_ranking, presid_popu_ranking) %>% 
  subset(!is.na(screen_name))

candidatos_rts_votos <- ggplot(candidatos_popu_ranking, 
                              aes(log(rts_obtenidos_totales), 
                                  log(Votos),
                                  colour= Distrito)) +
  geom_point(size = 2,alpha = 0.8) +
  theme_minimal()

candidatos_rtspromedio_votos <- ggplot(candidatos_popu_ranking, 
                               aes(Votos,
                                   rts_obtenidos_promedio, 
                                   colour= Distrito)) +
  geom_point(size = 2,alpha = 0.8) +
  theme_minimal()

candidatos_favs_votos <- ggplot(candidatos_popu_ranking, 
                               aes(log(favs_obtenidos_totales), 
                                   log(Votos),
                                   colour= Distrito)) +
  geom_point(size = 2,alpha = 0.8) +
 # geom_text(aes(label=screen_name),hjust=0, vjust=0) +
  theme_minimal()

candidatos_favs_votos <- ggplot(candidatos_popu_ranking, 
                                aes(favs_obtenidos_promedio, 
                                    Votos,
                                    colour= Distrito)) +
  geom_point(size = 2,alpha = 0.8) +
  # geom_text(aes(label=screen_name),hjust=0, vjust=0) +
  theme_minimal()

candidatos_emitidos_votos <- ggplot(candidatos_popu_ranking, 
                                    aes(cantidad_emitidos_totales, 
                                        Votos,
                                        colour= Distrito)) +
  geom_point(size = 2,alpha = 0.8) +
  # geom_text(aes(label=screen_name),hjust=0, vjust=0) +
  theme_minimal()

candidatos_emitidos_porcentaje <- ggplot(candidatos_popu_ranking, 
                                    aes(cantidad_emitidos_totales, 
                                        Porcentaje,
                                        colour= Distrito)) +
  geom_point(size = 2,alpha = 0.8) +
  # geom_text(aes(label=screen_name),hjust=0, vjust=0) +
  theme_minimal()

candidatos_rts_emitidos <- ggplot(candidatos_popu_ranking, 
                                         aes(cantidad_emitidos_totales, 
                                             rts_obtenidos_totales,
                                             colour= Distrito)) +
  geom_point(size = 2,alpha = 0.8) +
   geom_text(aes(label=screen_name),hjust=0, vjust=0) +
  theme_minimal()


#####
# MAPAS
#####




#####
# Patchworks (pendiente trabajar)
#####

rtspromedio_porcentaje <- gobernadores_rtspromedio_porcentaje | presid_rtspromedio_porcentaje  +
  plot_layout(heights = c(4, 14))

rtspromedio_porcentaje + plot_annotation(
  title = 'Correlación entre cantidad de Rts Promedio y Porcentaje de votos obtenido',
  caption = "Fuente: elaboración propia")


