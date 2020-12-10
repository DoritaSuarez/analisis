# Librerias
library(ggplot2)
library(dplyr)
library(plotly)
library(tidyverse)
library(FactoMineR)
library(readxl)
library(FactoMineR)
library(ade4)
library(FactoClass)
library(factoextra)
library(reshape2)
library(tidyr)
# Text Mining
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")


setwd("~/analisis/Src")

source("preprocesamiento.R", encoding = "UTF-8")
source("graficos.R", encoding = "UTF-8")



# Nivel educativo de los integrantes del hogar ----------------------------

# Por personas

df_personas %>% select(
    c(
      CONSECUTIVO:A00, 
      starts_with("p1708")
    )
  ) %>% 
  mutate(
    p1708 = replace_na(p1708, 1),  
    p1708 = factor(p1708, labels = c("Ninguno", "Preescolar", "Básica\nPrimaria", "Básica\nSecundaria", "Educación media\n(vocacional\ndiversificada o técnica)", "Tecnológica", "Superior o\nuniversitaria"))
  ) %>% 
  ggplot(aes(as.factor(p1708)))+
    geom_bar(col = "#005117", fill = "#8CBD0E")+
    labs(x="Nivel educativo", y="Número de personas")
  

# Por tramo

df_nivel_educativo_tramos <- df_personas %>% select(c(
      CONSECUTIVO:A00, 
      starts_with("p1708")
  )) %>% 
  mutate(
    CONSECUTIVO = as.character(CONSECUTIVO),
    A00 = as.character(A00),
    p1708 = replace_na(p1708, 1),  
    p1708 = factor(p1708, labels = c("Ninguno", "Preescolar", "Básica\nPrimaria", "Básica\nSecundaria", "Educación media\n(vocacional\ndiversificada o técnica)", "Tecnológica", "Superior o\nuniversitaria"))
  ) %>% 
  inner_join(df_hogares, by = c("CONSECUTIVO","A00")) %>% 
  select(
    CONSECUTIVO:A00, 
    starts_with("p1708"),
    Nombre
  ) %>% 
  rename(tramo = Nombre) 


create_barplot_tramo(df_nivel_educativo_tramos, "p1708", var_cat="tramo", "Nivel educativo", "Número de personas") 
  
table(df_nivel_educativo_tramos$p1708) %>% 
  prop.table()

## El 16% de las personas encuestadas no tiene ningún tipo de educación formal,
## el 45% completó estudios de preescolar y primaria, 35% culminó sus estudios de secundaria,
## o educación media; y un 4% de los encuestados tiene un nivel tecnológico o universitario


# Organización familiar para la producción --------------------------------

df_org_familiar <- df_personas %>% 
  select(c(
    CONSECUTIVO:A00, 
    starts_with("p1709")
  )) %>% 
  mutate(
    p1709 = factor(p1709, labels = c("Trabajador familiar\nsin remuneración", "Trabajador familiar\nremunerado", "Ayudante familiar\nocasional", "Otro"))
  )

ggplot(df_org_familiar,aes(as.factor(p1709)))+
  geom_bar(col = "#005117", fill = "#8CBD0E")+
  labs(x="Tipo de contribución a la producción económica", y="Número de personas")

df_org_familiar$p1709 %>% table() %>% prop.table()

## Aproxiamdamente el 37% de las personas contribuyen en la producción de su familia
## en calidad de trabajador familiar remunerado, 16% contribuye como trabajador familiar
## sin remuneración, 11% son ayudantes familiares ocasionales, y 34% contribuye de otras formas.
## 3% de los encuestados no tienen información relacionada con la contribución

df_org_familiar$p1709other %>% wordcloud_graph()


# Porcentaje del tiempo dedicado a tareas del hogar por persona -----------


df_tiempo_dedicado <- df_personas %>% 
  select(c(
    CONSECUTIVO:A00, 
    starts_with("p1706")
  )) 

plt1 <- df_tiempo_dedicado %>% select(p1706) %>%
  ggplot(aes(x="", y = p1706)) +
  geom_boxplot(col = "#005117", fill = "#8CBD0E") + 
  coord_flip() +
  theme_classic() +
  ylab("Porcentaje de tiempo dedicado a tareas del hogar") +
  xlab("")+
  scale_y_continuous(labels = scales::comma)+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

plt2 <- ggplot(df_tiempo_dedicado, aes(as.numeric(p1706)))+
  geom_histogram(col = "#005117", fill = "#8CBD0E", bins = 10)+
  labs(x="", y="Frecuencia")+
  scale_x_continuous(labels = scales::comma)+
  theme_classic()

egg::ggarrange(plt2, plt1, heights = 2:1)



# Número de personas que componen el hogar --------------------------------

df_hogares %>% 
  ggplot(aes(as.integer(P16)))+
  geom_bar(col = "#005117", fill = "#8CBD0E")+
  scale_x_continuous(breaks = seq(1, 15, 1))+
  labs(x="Número de personas que compone el hogar", y="Frecuencia")
  
df_hogares %>% 
  ggplot(aes(as.integer(P16)))+
  geom_bar(col = "#005117", fill = "#8CBD0E")+
  scale_x_continuous(breaks = seq(1, 15, 1))+
  labs(x="Número de personas que compone el hogar", y="Frecuencia")+
  facet_wrap(vars(P18))

df_hogares %>% 
  ggplot(aes(as.integer(P16), fill = P18))+
  geom_bar(col = "#005117", position="fill")+
  scale_x_continuous(breaks = seq(1, 15, 1))+
  labs(x="Número de personas que compone el hogar", y="Frecuencia")

df_hogares$P18 %>% table()
