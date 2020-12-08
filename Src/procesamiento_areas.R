# Librerias
library(ggplot2)
library(dplyr)
library(DataExplorer)
library(plotly)
library(tidyverse)
library(FactoMineR)
library(readxl)

setwd("C:/Users/diant/Documents/deproyectos-Cauca/Bases/Análisis/Src")

# Lectura de los datos

df_hogares <- read_excel('../input/Base_Hogares_101020.xlsx', skip = 1)
df_diccionario <- read_excel('../input/Base_Hogares_101020.xlsx')
df_dicc <- data_frame(preguntas = names(df_diccionario), nombre_base = names(df_hogares))
cobertura <- read.csv("../input/cobertura_final.csv", sep = ";", encoding = "UTF-8")

cobertura[, "NUMERO.DE.FORMULARIO"] <- as.character(cobertura[, "NUMERO.DE.FORMULARIO"])
base_cobertura <- left_join(df_hogares, cobertura,  by =c("A00"= "NUMERO.DE.FORMULARIO"))

rep <- c("10504",  "28011", "9011", "9111")
df_hogares[which(df_hogares$A00 %in% rep),] %>% View()

base_cobertura %>% head()


# Seleccionando las variables que nos darán información de las áreas

base_areas <- select(base_cobertura, c(""))





