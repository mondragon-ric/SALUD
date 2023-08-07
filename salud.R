#librerias
library(dplyr)
library(readxl)
library(ggplot2)

#Limpiar el ambiente
rm(list = ls())

#Establecer directorio 
setwd("C:/Users/")

#Cargar datos
salud_20 <- read_excel("salud_2020.xlsx", sheet = "02", range = cell_rows(10:1989), 
                       col_names = F)
colnames(salud_20) <- c("Entidad", "Sexo", "Grupo_edad", "Pob_total", "Afil_total", 
                        "Afil_IMSS", "Afil_ISSSTE", "Afil_ISSSTE_estatal", 
                        "Afil_PEME_SEDE_MARI", "Afil_INSABI", "Afil_IMSS_BIEN", 
                        "Afil_Privada", "Afil_otra", "No_afiliada", "No_especificado")

salud_10 <- read_excel("salud_2010.xls", range = cell_rows(11:1990), col_names = F)
colnames(salud_10) <- c("Entidad", "Sexo", "Grupo_edad", "Pob_total", "Afil_total", 
                        "Afil_IMSS", "Afil_ISSSTE", "Afil_ISSSTE_estatal", 
                        "Afil_PEME_SEDE_MARI", "Afil_Seg_Popular", 
                        "Afil_Privada", "Afil_otra", "No_afiliada", "No_especificado")

salud_00 <- read_excel("salud_2000.xlsx", sheet = "CPyV2000_Nal_SS1", 
                       range = cell_rows(10:1692), col_names = F)
colnames(salud_00) <- c("País", "Entidad", "Sexo", "Grupo_edad", "Pob_total", 
                        "No_afiliada", "Afil_total", "Afil_IMSS", "Afil_ISSSTE", 
                        "Afil_PEME_SEDE_MARI", "Afil_otra", "No_especificado")

#Obtener las columnas necesarias
salud_20 <- data.frame(salud_20[c("Entidad", "Sexo", "Grupo_edad", "Pob_total", "Afil_total", "No_afiliada")])

salud_10 <- data.frame(salud_10[c("Entidad", "Sexo", "Grupo_edad", "Pob_total", "Afil_total", "No_afiliada")])

salud_00 <- data.frame(salud_00[c("Entidad", "Sexo", "Grupo_edad", "Pob_total", "Afil_total", "No_afiliada")])

#Obtener solo los totales
salud_20 <- filter(salud_20, Sexo == "Total" & Grupo_edad == "Total")
salud_10 <- filter(salud_10, Sexo == "Total" & Grupo_edad == "Total")
salud_00 <- filter(salud_00, Sexo == "Total" & Grupo_edad == "Total")

#Obtener los porcentajes de personas afiliadas
salud_20$Porc_afil <- round((salud_20$Afil_total/salud_20$Pob_total)*100, 2)
salud_10$Porc_afil <- round((salud_10$Afil_total/salud_10$Pob_total)*100, 2)
salud_00$Porc_afil <- round((salud_00$Afil_total/salud_00$Pob_total)*100, 2)

#Obtener los porcentajes de personas no afiliadas
salud_20$Porc_no_afil <- round((salud_20$No_afiliada/salud_20$Pob_total)*100, 2)
salud_10$Porc_no_afil <- round((salud_10$No_afiliada/salud_10$Pob_total)*100, 2)
salud_00$Porc_no_afil <- round((salud_00$No_afiliada/salud_00$Pob_total)*100, 2)

#Corregir nombres de los estados y agregar abreviaturas de los estados
salud_20$Entidad <- c("Estados Unidos Mexicanos", "Aguascalientes", "Baja California", 
                      "Baja California Sur", "Campeche", "Coahuila de Zaragoza", 
                      "Colima", "Chiapas", "Chihuahua", "Ciudad de México", "Durango", 
                      "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "Estado de México", 
                      "Michoacán de Ocampo", "Morelos", "Nayarit", "Nuevo León", 
                      "Oaxaca", "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí", 
                      "Sinaloa","Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", 
                      "Veracruz de Ignacio de la Llave", "Yucatán", "Zacatecas")

salud_10$Entidad <- c("Estados Unidos Mexicanos", "Aguascalientes", "Baja California", 
                      "Baja California Sur", "Campeche", "Coahuila de Zaragoza", 
                      "Colima", "Chiapas", "Chihuahua", "Ciudad de México", "Durango", 
                      "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "Estado de México", 
                      "Michoacán de Ocampo", "Morelos", "Nayarit", "Nuevo León", 
                      "Oaxaca", "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí", 
                      "Sinaloa","Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", 
                      "Veracruz de Ignacio de la Llave", "Yucatán", "Zacatecas")

salud_00$Entidad <- c("Estados Unidos Mexicanos", "Aguascalientes", "Baja California", 
                      "Baja California Sur", "Campeche", "Coahuila de Zaragoza", 
                      "Colima", "Chiapas", "Chihuahua", "Ciudad de México", "Durango", 
                      "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", "Estado de México", 
                      "Michoacán de Ocampo", "Morelos", "Nayarit", "Nuevo León", 
                      "Oaxaca", "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí", 
                      "Sinaloa","Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", 
                      "Veracruz de Ignacio de la Llave", "Yucatán", "Zacatecas")

#Graficar los porcentajes de personas afiliadas
afi_20 <- ggplot(data = salud_20, aes(x = Porc_afil, y = reorder(Entidad, Porc_afil))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(Porc_afil)), hjust = -0.2, color = "black", size=2) +
  labs(title = "Población afiliada a algún servicio de salud 2020",
       x = "Entidad",
       y = "Porcentaje de población afiliada") +
  theme_minimal()

ggsave(filename = paste("afi_20.png"), plot = afi_20, width = 10, height = 4.5)

afi_10 <- ggplot(data = salud_10, aes(x = Porc_afil, y = reorder(Entidad, Porc_afil))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(Porc_afil)), hjust = -0.2, color = "black", size=2) +
  labs(title = "Población afiliada a algún servicio de salud 2010",
       x = "Entidad",
       y = "Porcentaje de población afiliada") +
  theme_minimal()

ggsave(filename = paste("afi_10.png"), plot = afi_10, width = 10, height = 4.5)

afi_00 <- ggplot(data = salud_00, aes(x = Porc_afil, y = reorder(Entidad, Porc_afil))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(Porc_afil)), hjust = -0.2, color = "black", size=2) +
  labs(title = "Población afiliada a algún servicio de salud 2000",
       x = "Entidad",
       y = "Porcentaje de población afiliada") +
  theme_minimal()

ggsave(filename = paste("afi_00.png"), plot = afi_00, width = 10, height = 4.5)

#Graficar los porcentajes de personas no afiliadas
no_afi_00 <- ggplot(data = salud_00, aes(x = Porc_no_afil, y = reorder(Entidad, Porc_no_afil))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(Porc_no_afil)), hjust = -0.2, color = "black", size=2) +
  labs(title = "Población no afiliada a algún servicio de salud 2000",
       x = "Entidad",
       y = "Porcentaje de población no afiliada") +
  theme_minimal()

ggsave(filename = paste("no_afi_00.png"), plot = no_afi_00, width = 10, height = 4.5)

no_afi_10 <- ggplot(data = salud_10, aes(x = Porc_no_afil, y = reorder(Entidad, Porc_no_afil))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(Porc_no_afil)), hjust = -0.2, color = "black", size=2) +
  labs(title = "Población no afiliada a algún servicio de salud 2010",
       x = "Entidad",
       y = "Porcentaje de población no afiliada") +
  theme_minimal()

ggsave(filename = paste("no_afi_10.png"), plot = no_afi_10, width = 10, height = 4.5)

no_afi_20 <- ggplot(data = salud_20, aes(x = Porc_no_afil, y = reorder(Entidad, Porc_no_afil))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(Porc_no_afil)), hjust = -0.2, color = "black", size=2) +
  labs(title = "Población no afiliada a algún servicio de salud 2020",
       x = "Entidad",
       y = "Porcentaje de población no afiliada") +
  theme_minimal()

ggsave(filename = paste("no_afi_20.png"), plot = no_afi_20, width = 10, height = 4.5)





