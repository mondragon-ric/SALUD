#librerias
library(dplyr)
library(readxl)
library(ggplot2)

#Limpiar el ambiente
rm(list = ls())

#Establecer directorio 
setwd("C:/Users/")

#Cargar datos
rh_salud <- read_excel("rh_salud.xlsx", sheet = "Concentrado")

#Seleccionar las columnas que se necesitan
rh_salud <- rh_salud[c("Año", "Entidad Federativa", "TOTAL")]
rh_salud <- subset(rh_salud, `Entidad Federativa` != "Nacional")

#Graficar
for (i in seq(2012, 2021)) {
  rh_salud_for <- subset(rh_salud, Año == i)
  grafica <- ggplot(data = rh_salud_for, aes(x = TOTAL, y = reorder(`Entidad Federativa`, TOTAL))) +
    geom_bar(stat = "identity", fill = "steelblue") +
    geom_text(aes(label = paste0(TOTAL)), hjust = -0.2, color = "black", size=2) +
    labs(title = paste("Total de personal médico", i),
         x = "Personal Médico",
         y = "Entidad federativa") +
    theme_minimal()
  ggsave(filename = paste("rh_salud", i, ".png"), plot = grafica, width = 10, height = 4.5)
}



