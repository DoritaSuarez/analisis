library(ggplot2)
library(dplyr)
library(DataExplorer)
library(plotly)
library(tidyverse)
library(FactoMineR)
library(readxl)
library(corrplot)
library(ggcorrplot)
library(reshape2)
# Text Mining
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

setwd("C:/Users/diant/Documents/deproyectos-Cauca/Bases/Análisis/Src")

source("preprocesamiento.R", encoding = "UTF-8")


# Funciones ---------------------------------------------------------------

calculador_frecuencias <- function(nombre_var){
  df_temp = df_actividades[df_actividades[,nombre_var] == 1, ]
  df_abs <- df_temp %>% apply(., 2, sum)
  df_rel <- round(df_abs/df_abs[nombre_var]*100, 1)
  return(list(abs = df_abs, rel = df_rel))
}


# Actividades principales -------------------------------------------------

df_actividades <- df_hogares %>% select(starts_with(c("P02")))
df_actividades <- apply(df_actividades, 2, transformacion_faltantes) %>% data.frame()  %>% .[, 1:9] %>% data_frame()

names(df_actividades) <- c("Agricultura", "Ganaderia","Especies Menores", "Pesca", "Otras Pecuarias", "Forestal", "Minería", "Comercio", "Otras actividades")

# Distribución de las actividades principales

df_act = melt(df_actividades, id.vars = NULL)

resumen_actividades <- df_act %>% group_by(variable, value) %>% 
  summarise(conteo = n()) %>% 
  mutate(porcentaje = conteo/sum(conteo)) %>% 
  arrange(porcentaje, variable)

resumen_actividades$value = ifelse(resumen_actividades$value == 1, "Si", "No")
names(resumen_actividades)[2] <- "Actividades"

resumen_actividades %>% ggplot(aes(x=reorder(variable, desc(porcentaje)), y = porcentaje, fill = Actividades))+geom_col()


resumen_actividades <- resumen_actividades %>% arrange(porcentaje, Actividades)


resumen_actividades["pos"] <- NA
for(i in 1:nrow(resumen_actividades)){
  x = resumen_actividades[i, "porcentaje"]
  act = resumen_actividades[i, "Actividades"]
  resumen_actividades[i, "pos"] = ifelse(act == "Si", x*100/2, (1-x+x/2)*100)
}


act_g1 <- ggplot(resumen_actividades, aes(x = variable, y = porcentaje*100, fill = Actividades))
act_g1 + geom_bar(stat = "identity") +
  xlab("Actividad") +
  ylab("Porcentaje") +
  scale_fill_manual("Actividad", values = c("No" = "#8CBD0E", "Si" = "#005117")) + coord_flip()
  # geom_text(data=resumen_actividades, aes(y = pos,
                                  # label = paste0(round(porcentaje*100),"%")), size=3)+coord_flip()


# Calculo de correlaciones entre las variables binarias generadas y respectivo gráfico
correlaciones <-  df_actividades[, 1:9] %>% apply(., 2, transformacion_faltantes) %>% data_frame() %>% cor()

corrplot(correlaciones, method = "color")

# Se calculan las frecuencias relativas y absolutas para cada una de las actividades principales que reportan los hogares

df_relativas <- data.frame()
df_absolutas <- data.frame()
for(i in 1:ncol(df_actividades)){
  df_relativas <- rbind(df_relativas, calculador_frecuencias(names(df_actividades)[i])$rel)
  df_absolutas <- rbind(df_absolutas, calculador_frecuencias(names(df_actividades)[i])$abs)
}

names(df_absolutas) <- names(df_actividades)
names(df_relativas) <- names(df_actividades)
row.names(df_absolutas) <- names(df_actividades)
row.names(df_relativas) <- names(df_relativas)
df_relativas[, "Actividad"] <- names(df_relativas)
df_absolutas[, "Actividad"] <- names(df_absolutas)

# Se organizan los datos de forma que puedan ser procesados por ggplot
plotDat_rel <- gather(df_relativas, key = "Actividad1", value = "Porcentaje", -Actividad)
plotDat_abs <- gather(df_absolutas, key = "Actividad1", value = "Conteo", -Actividad)

ggplot(plotDat_rel, aes(Actividad1, Actividad, col = Porcentaje, fill = Porcentaje, label = paste0(Porcentaje, "%"))) +
  geom_tile() +
  geom_text(col = "#073482") +
  theme_minimal() +
  scale_fill_gradient2(low = "#FAFA00", mid = "#E8E6E8", high = "#00CDDB") +
  scale_color_gradient2(low = "#FAFA00", mid = "#E8E6E8", high = "#00CDDB")


ggplot(plotDat_abs, aes(Actividad1, Actividad, col = Conteo, fill = Conteo, label = Conteo)) +
  geom_tile() +
  geom_text(col = "#073482") +
  theme_minimal() +
  scale_fill_gradient2(low = "#FAFA00", mid = "#E8E6E8", high = "#00CDDB") +
  scale_color_gradient2(low = "#FAFA00", mid = "#E8E6E8", high = "#00CDDB")

# ¿ que actividad proviene el sustento del hogar?
docs <- Corpus(VectorSource(c(df_hogares$P02ICUAL, df_hogares$P02JCUAL) %>% na.omit() ))
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("spanish"))

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


numero_actividades <- data_frame(df_actividades, Clase = df_hogares$CLASE, Tramo = df_hogares$Nombre)
numero_actividades$Clase <- factor(numero_actividades$Clase)
levels(numero_actividades$Clase) <- c("Cabecera", "Corregimiento", "Rural")

numero_actividades_grap <- numero_actividades %>% group_by(Clase, Tramo) %>% melt() %>% data_frame()
numero_actividades_grap$value = factor(numero_actividades_grap$value)
levels(numero_actividades_grap$value) = c("No", "Si")

# numero_actividades_grap <- numero_actividades_grap %>% 
#   group_by(Clase, Tramo, variable) %>%
#   summarise(conteo = n())

resumen_actividades <- numero_actividades_grap %>% group_by(Clase, Tramo, variable, value) %>% 
  summarise(conteo = n())

resumen_actividades_grap <- resumen_actividades %>% group_by(Clase, Tramo, variable) %>% mutate(conteoC = sum(conteo)) %>% mutate(porcentaje = conteo/conteoC*100)

# ggplot(resumen_actividades_grap, aes(x = variable, y = porcentaje, fill = Tramo)) + geom_col(stat="identity")
# + facet_grid(cols = vars(Clase)) + coord_flip()

