## funcionesGraficos.R
## este modulo contiene funciones que serán de utilidad a la hora de graficar

library(tidyverse)
library(ggplot2)
library(ggthemes)

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
    geom_text(aes2,hjust=0, vjust=0.8, size=3, colour="black") 

  return(plotPointText)
}

formatPlot <- function(plot, plottitle="", plotsubtitle="", xlabel="", ylabel="", plotcaption=""){
  
  formatted_plot <- plot +
    theme_clean() +
    labs(title = plottitle, 
         caption = plotcaption,
         subtitle = plotsubtitle,
         x = xlabel,
        y = ylabel) +
    theme(legend.position = "none") 
  
  return(formatted_plot)
  
}

