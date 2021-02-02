## funcionesGraficos.R
## este modulo contiene funciones que serán de utilidad a la hora de graficar

library(tidyverse)
library(ggplot2)
library(gt)

plotPoint <- function(df, aes){
  
  # recibe un df, y un conjunto de parametros aes
  # devuelve un grafico de puntos dispersos
  
  plotPoint <- ggplot(df, 
                      aes) +
    geom_point(size = 2,alpha = 0.8) +
    theme_minimal()
  
  return(plotPoint)
  
}
  
plotPointText <-function(df, aes1, aes2){
  
  # recibe un df, y un conjunto de parametros aes
  # aes1 = parametros para el point
  # aes2 = parametros para text
  # devuelve un grafico de puntos dispersos + sus etiquetas
  
  plotPointText <- ggplot(df, 
                          aes1) +
    geom_point(size = 2,alpha = 0.8) +
    geom_text(aes2,hjust=0.3, vjust=0.8, size=3, colour="black") 

  return(plotPointText)
}

formatPlot <- function(plot, plottitle="", plotsubtitle="", xlabel="", ylabel="", plotcaption=""){
  
  formatted_plot <- plot +
    theme_minimal() +
    labs(title = plottitle, 
         caption = plotcaption,
         subtitle = plotsubtitle,
         x = xlabel,
        y = ylabel) +
    theme(legend.position = "none") 
  
  return(formatted_plot)
  
}

formatoTabla <- function(tabla, titulo = "", subtitulo = "", back_color = "#0B0B0B") {
  
  # recibe dataframe
  # usa gt para devolverlo con formato estéticamente afín a nuestra web
  
  tabla_formato <- tabla %>%  gt() %>% 
    gt::tab_header(
      title = titulo,
      subtitle = subtitulo) %>% 
    gt::tab_style(
      style=  cell_fill(color = "#00BFFF", alpha = 0.5),
      locations = cells_title(groups = c("title", "subtitle"))) %>% 
    gt::tab_style(
      style= cell_text(
        v_align = "middle",
        align = "center"),
      locations = cells_stubhead()) %>% 
    gt::tab_style(
      style=  cell_fill(color = "#E9EDF1", alpha = 0.5),
      locations = cells_body()) %>% 
    gt::tab_style(
      style= cell_text(
        color = back_color,
        align = "center",
        v_align = "middle",
        weight = "lighter"),
      locations = cells_body())
  
  return(tabla_formato)
}
