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
table(df_hogares$P16) %>% prop.table()

df_hogares %>% 
  ggplot(aes(as.integer(P16)))+
  geom_bar(col = "#005117", fill = "#8CBD0E")+
  scale_x_continuous(breaks = seq(1, 15, 1))+
  labs(x="Número de personas que compone el hogar", y="Frecuencia")
  
## El gráfico y la tabla anterior muestra la distrbución del número de personas en el hogar.
##  Se observa que casi el 15% de los hogares de los encuestados se componen de una o dos
## personas, casi el 60% de los hogares se componen de tres, cuatro o cinco miembros,
## 18% de los hogares están compuestos por seis o siete personas, y el restante 7% lo
## conforman hogares con ocho personas o más

df_hogares %>% 
  ggplot(aes(as.integer(P16)))+
  geom_bar(col = "#005117", fill = "#8CBD0E")+
  scale_x_continuous(breaks = seq(1, 15, 1))+
  labs(x="Número de personas que compone el hogar", y="Frecuencia")+
  facet_wrap(vars(P18))

table(df_hogares$P18) %>% prop.table()

## Del total de hogares, 32.4% de ellos afirman que la totalidad de personas que componen
## el hogar participan en las actividades económicas que realiza el hogar para adquirir
## sus medios de vida. El restante 67.6% de los hogares emplea únicamente a algunos de
## sus miembros para el desarrollo de las actividades.


df_hogares %>% 
  ggplot(aes(as.integer(P16), fill = factor(P18, labels = c('Todos','Algunos'))))+
  geom_bar(col = "#005117", position="fill")+
  geom_text(data = . %>% 
              group_by(P16, P18) %>%
              tally() %>%
              mutate(p = n / sum(n)) %>%
              ungroup(),
            aes(y = p, label = scales::percent(p), colour = P18),
            position = position_stack(vjust = 0.5),
            show.legend = FALSE,
            size=2.7)+
  scale_x_continuous(breaks = seq(1, 15, 1))+
  scale_color_manual(values = c("black", "white"))+
  scale_fill_manual(name = "¿Quiénes participan\nen las actividades\neconómicas del hogar?", values = c("Todos" = "#8CBD0E", "Algunos" = "#005117"))+
  labs(x="Número de personas", y="Frecuencia")

## En el gráfico anterior se observa por número de personas en el hogar, la proporción
## de hogares en los que todos sus miembros participan en las actividades de adquisición
## de medios de vida; y la propoción de los hogares en los que algunos de sus miembros
## participan. Se puede obserevar que por lo
## general, en uno de cada cuatro hogares todos los miembros de la familia participan en
## las actividades de adquisición de medios de vida

df_prop_trabajadores <- df_hogares %>% 
  select(c("P16", "P18", "P18B")) %>% 
  mutate_all(as.integer) %>% 
  mutate(
    prop_trabajadores = ifelse(P18==1,100,round(P18B*100/P16,2))
  ) 

plt1 <- df_prop_trabajadores %>% select(prop_trabajadores) %>%
  ggplot(aes(x="", y = prop_trabajadores)) +
  geom_boxplot(col = "#005117", fill = "#8CBD0E") + 
  coord_flip() +
  theme_classic() +
  xlab("") + ylab("Proporción de personas que trabajan en el hogar") +
  scale_y_continuous(labels = scales::comma)+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

plt2 <- ggplot(df_prop_trabajadores, aes(as.numeric(prop_trabajadores)))+
  geom_histogram(col = "#005117", fill = "#8CBD0E", bins = 15)+
  labs(x="", y="Frecuencia")+
  scale_x_continuous(labels = scales::comma)+
  theme_classic()

egg::ggarrange(plt2, plt1, heights = 2:1)

## En el gráfico anterior se observa la distribución de la proporción de personas que participan en
## las actividades de adquisición de medios de vida. Hay que tener en cuenta que el número
## de hogares con 100% de participación de los miembros en actividades económicas son 1540,
## y el número de hogares donde solo algunos miembros participan es 3202, que son los que se
## pueden observar a la izquierda de la distribución

df_otras_actividades <- df_hogares %>% 
  select(c(
    CONSECUTIVO:A00,  
    starts_with(c("P1801","P181"))
  )) %>% 
  pivot_longer(
    cols = starts_with(c("P1801","P181")),
    names_to = "num_actividad", 
    values_to = "actividad"
  ) %>% 
  filter(!is.na(actividad)) 

(df_otras_actividades$actividad == "NINGUNA") %>% table() %>% prop.table()

## Entre las otras actividades productivas se dedican por fuera de la parcela
## o terreno familiar, destaca la respuesta NINGUNA, la cual aparece en el 55%
## de las respuestas

df_otras_actividades %>% 
  filter(actividad!="NINGUNA") %>% 
  select(actividad) %>% 
  wordcloud_graph()

## Las anteriores son las otras actividades productivas a las que las personas
## que no están participando en las actividades de adquisición de medios de vida
## se dedican por fuera de la parcela o terreno familiar


# Retribución económica de las otras actividades --------------------------

df_hogares %>% 
  select(c(
    CONSECUTIVO:A00,  
    starts_with(c("P1802"))
  )) 


table(df_hogares$P18)[[2]]

## Para los siguientes cálculos, se tiene en cuenta que el numero de personas que ocupa
## parcialmente su tiempo en otras actividades es 3202, y de estos, un 45% hace otra 
## actividad diferente de ninguna. Estas personas son quienes indican una clasificación para 
## la actividad productiva que hacen fuera de la parcela o terreno familiar

df_hogares %>% 
  select(c("P1802A","P1802B","P1802C")) %>% 
  `colnames<-`(c("Dinero", "Productos", "Disminución\nen gastos")) %>% 
  mutate_all(as.integer) %>% 
  summarise_all(sum, na.rm = TRUE) %>% 
  pivot_longer(
    cols = everything(),
    names_to = 'actividad',
    names_prefix = "acti",
    values_to = "num_hogares",
    values_drop_na = TRUE
  ) %>% 
  mutate(
    porcentaje=num_hogares/table(df_hogares$P18)[[2]]
  ) %>% 
  ggplot(aes(x=reorder(actividad, -porcentaje), y=porcentaje))+
  geom_bar( stat = "identity", position = "fill", fill = "#8CBD0E")+
  geom_bar( stat = "identity", fill='#005117')+
  geom_text(aes(label = paste0(round(porcentaje*100,2),'%')), stat = "identity", vjust = 0.5,hjust = 2, colour = "white",position =  position_dodge(.9), size=2.7)+  
  labs(x='Actividad', y='Porcentaje')+
  scale_y_continuous(labels = scales::percent_format())+
  coord_flip()

## Del total de personas que ocupa parcialmente su tiempo en otras actividades,
## casi el 20% es retribuído con dinero por hacer esas actividades, el 7% de personas es retribuído con
## reducción de gastos de alimentación, y un 6% se retribuye con productos


  