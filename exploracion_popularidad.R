#exploracion_popularidad.R
#EXPLORACION DE MEDIDAS DE POPULARIDAD DE LOS CANDIDATOS

#####
# APERTURA DE LIBRERIAS
#####
#paquetes

require(tidyverse)
require(readxl)
require(ggplot2) # para graficar
library(patchwork) # para unir graficos


#propias
source("Modules/tablasElectorales.R", encoding = "UTF-8")
source("Modules/tuitsCandidatos.R", encoding = "UTF-8")
source("Modules/funcionesGraficos.R", encoding = "UTF-8")

#####
# IMPORTACION DE DATOS / TUITS
#####

# Datos tuiter 
# nos importan dataframes con tuits de candidatos: joined_gobernadores y joined_presid 

source("preparacion_datos_tuits.R", encoding = "UTF-8")
joined_presid <- traerDatosTuits("presid")
joined_gobernadores <- traerDatosTuits("gob")
joined_candidatos <- traerDatosTuits("tot")

# Datos electorales 
# nos importan tablas con votos obtenidos por cada candidato: votos_gobernadores y votos_presid

source("preparacion_datos_electorales.R", encoding = "UTF-8")
votos_gobernadores <- traerDatosElectorales("gob")
votos_presid <- traerDatosElectorales("presid")

#####
# UNIENDO Y TRABAJANDO DATOS / TUTIS / ELECTORALES
#####

# Provincias

gobernadores_popu <- joined_gobernadores %>%  
  subset( Campaña == 1 ) %>% 
  select(created_at, screen_name, rts,fav_count, followers_count, friends_count ) %>% 
  group_by(screen_name) %>% 
  dplyr::summarise(rts_obtenidos_totales = sum(rts),
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
  dplyr::summarise(rts_obtenidos_totales = sum(rts),
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
#####
# Patchworks (pendiente trabajar)
#####

rtspromedio_porcentaje <- gobernadores_rtspromedio_porcentaje | presid_rtspromedio_porcentaje  +
  plot_layout(heights = c(4, 14))

rtspromedio_porcentaje + plot_annotation(
  title = 'Correlación entre cantidad de Rts Promedio y Porcentaje de votos obtenido',
  caption = "Fuente: elaboración propia")


