# Librerias
library(ggplot2)
library(dplyr)
library(DataExplorer)
library(plotly)
library(tidyverse)
library(FactoMineR)
library(readxl)
library(FactoMineR)
library(ade4)
library(FactoClass)
library(factoextra)
library(reshape2)
# Text Mining
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")


setwd("~/analisis/Src")

source("preprocesamiento.R", encoding = "UTF-8")

# df_hogares %>% View()


# Costos totales --------------------------------------------------------

# estimación del presupuesto mensual de gastos del hogar ----------------------------------------------------------

df_gastos <- df_hogares %>% select(
  c(
    CONSECUTIVO:NOMBRES, 
    starts_with("P59")
    )
  )

df_dicc %>% View()
df_gastos %>% head() %>% View()

df_gastos$P59 <- transformacion_faltantes(df_gastos$P59)

ggplot(df_gastos, aes(as.numeric(P59)))+
  geom_histogram(col = "#005117", fill = "#8CBD0E")+
  labs(title = "Distribución de gastos mensuales del hogar",
       x="Cantidad (millones de pesos)",
       y="Frecuencia")

ggplot(df_gastos, aes(x="P59", y=P59))+geom_boxplot()

plt1 <- df_gastos %>% select(P59) %>%
  ggplot(aes(x="", y = P59)) +
  geom_boxplot(col = "#005117", fill = "#8CBD0E") + 
  coord_flip() +
  theme_classic() +
  xlab("") +
  scale_y_continuous(labels = scales::comma)+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

plt2 <- ggplot(df_gastos, aes(as.numeric(P59)))+
  geom_histogram(col = "#005117", fill = "#8CBD0E")+
  labs(x="Cantidad (millones de pesos)", y="Frecuencia")+
  scale_x_continuous(labels = scales::comma)+
  theme_classic()

egg::ggarrange(plt2, plt1, heights = 2:1)

df_gastos <- df_gastos %>%  mutate(
  gastos_mes_hogar = case_when(
    is.na(P59) ~ NA_real_,
    P59 >= 0 & P59 < 100000 ~ 1,#"Entre 0 y 5mts",
    P59 >= 0 & P59 < 200000 ~ 2,#"Entre 0 y 5mts",
    P59 >= 200000 & P59 < 500000 ~ 3,#"Entre 5mts y 1km",
    P59 >= 500000 & P59 < 1000000 ~ 4,#"Entre 5km y 10km",
    P59 >= 1000000 & P59 < 2000000 ~ 5,#"Entre 10km y 50km",
    P59 >= 2000000 ~ 6,#"más de 50km",
    TRUE ~ NA_real_
  )
)

df_gastos$gastos_mes_hogar <- as.factor(df_gastos$gastos_mes_hogar)
levels(df_gastos$gastos_mes_hogar) <- c("Entre $0 y $100.000",
                                             "Entre $100.000 y $200.000",
                                             "Entre $200.000 y $500.000",
                                             "Entre $500.000 y $1'000.000",
                                             "Entre $1'000.000 y $2'000.000",
                                             "Mas de $2'000.000")

df_gastos %>% 
  dplyr::filter(!is.na(gastos_mes_hogar)) %>% 
  ggplot(aes(gastos_mes_hogar))+
  geom_bar(col = "#005117", fill = "#8CBD0E")+
  geom_text(aes(label = paste0(round(..count..*100/length(df_gastos$gastos_mes_hogar)),"%"),digits=3), stat = "count", vjust = 0.5,hjust = -0.4, colour = "black")+  
  labs(x="Gastos mensuales del hogar", y="Número de hogares")+
  scale_y_continuous(limits = c(0,3000))+
  coord_flip()

df_gastos$tramo <- df_hogares$Nombre

df_gastos %>% 
  dplyr::filter(!is.na(gastos_mes_hogar)) %>% 
  ggplot(aes(gastos_mes_hogar, fill=tramo))+
  geom_bar(col = "#005117", position = "dodge")+
  geom_text( aes(label = paste0(round(..count..*100/length(df_gastos$gastos_mes_hogar)),"%"),digits=3), stat = "count", vjust = 0.5,hjust = -0.4, colour = "black",position =  position_dodge(.9))+  
  labs(x="Gastos mensuales del hogar", y="Número de hogares")+
  scale_y_continuous(limits = c(0,3000))+
  coord_flip()
