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



# Descripcion de los ingresos ---------------------------------------------

# Tiene(si/no) alguno de los ingesos siguientes: Agro, jornales, donaciones, remesas: Fuente de ingreso

df_hogares[, "ing_agrop"] <- ifelse(transformacion_faltantes(df_hogares$P53) == 1, 1, 0)
df_hogares[, "ing_jorna"] <- ifelse(transformacion_faltantes(df_hogares$P54) == 1, 1, 0)
df_hogares[, "ing_donac"] <- ifelse(transformacion_faltantes(df_hogares$P56) == 1, 1, 0)
df_hogares[, "ing_remes"] <- ifelse(transformacion_faltantes(df_hogares$P57) == 1, 1, 0)
df_hogares[, "ing_otros"] <- ifelse(transformacion_faltantes(df_hogares$P58) == 1, 1, 0)

df_ing <- data_frame(df_hogares[, "ing_agrop"],
                     df_hogares[, "ing_donac"],
                     df_hogares[, "ing_jorna"],
                     df_hogares[, "ing_otros"],
                     df_hogares[, "ing_remes"])

names(df_ing) <- c("Actividades agropecuarias",
                   "Donaciones",
                   "Jornales",
                   "Otros",
                   "Remesas")

df_ing = melt(df_ing)

resumen_fuentes_ingreso <- df_ing %>% group_by(variable, value) %>% 
  summarise(conteo = n()) %>% 
  mutate(porcentaje = conteo/sum(conteo))

resumen_fuentes_ingreso$value = ifelse(resumen_fuentes_ingreso$value == 1, "Si", "No")
names(resumen_fuentes_ingreso)[2] <- "Ingreso"


ing_g1 <- ggplot(resumen_fuentes_ingreso, aes(x = variable, y = porcentaje*100, fill = Ingreso))
ing_g1 + geom_bar(stat = "identity") +
  xlab("Fuente de ingreso") +
  ylab("Porcentaje") +
  scale_fill_manual("Ingreso", values = c("No" = "#8CBD0E", "Si" = "#005117")) +
  coord_flip()

# Monto Ingresos por fuente -----------------------------------------------
## ===== Ingresos Agropecuarios ======= #

monto_agro <- df_hogares %>% 
  select(P5301
  ) %>% 
  mutate(
    peor_escenario = case_when(
      is.na(P5301) ~ NA_real_,
      P5301 == 1 ~ 0,
      P5301 == 2 ~ 250000,
      P5301 == 3 ~ 500000,
      P5301 == 4 ~ 1000000,
      P5301 == 5 ~ 2000000,
      TRUE ~ NA_real_
    ),
    mejor_escenario = case_when(
      is.na(P5301) ~ NA_real_,
      P5301 == 1 ~ 250000,
      P5301 == 2 ~ 500000,
      P5301 == 3 ~ 1000000,
      P5301 == 4 ~ 2000000,
      P5301 == 5 ~ 4000000,
      TRUE ~ NA_real_
    ),
    medio_escenario = case_when(
      is.na(P5301) ~ NA_real_,
      P5301 == "1" ~ 125000,
      P5301 == "2" ~ 275000,
      P5301 == "3" ~ 750000,
      P5301 == "4" ~ 1500000,
      P5301 == "5" ~ 3000000, # continuando con la progresion de valores
      TRUE ~ NA_real_
    )
  )

base_ingreso_agro <- df_hogares %>% select(P5301, P5302, P5302OTHER) %>%
  data_frame(., monto_agro[, -1])

ingreso_mensual_agro <- base_ingreso_agro %>% 
  mutate(
    ingresos_mensuales_pe = case_when(
      is.na(P5302) ~ NA_real_,
      P5302 == 1 ~ peor_escenario,
      P5302 == 2 ~ peor_escenario/2,
      P5302 == 3 ~ peor_escenario/3,
      P5302 == 4 ~ peor_escenario/6,
      P5302 == 5 ~ peor_escenario/12,
      P5302OTHER == "15 DIAS" ~ peor_escenario*2,
      P5302OTHER == "3 MESES" ~ peor_escenario/3,
      P5302OTHER == "4 MESES" ~ peor_escenario/4,
      P5302OTHER == "5 MESES" ~ peor_escenario/5,
      P5302OTHER == "7 MESES" ~ peor_escenario/7,
      P5302OTHER == "8 MESES" ~ peor_escenario/8,
      P5302OTHER == "ANUAL" ~ peor_escenario/12,
      P5302OTHER == "DIARIO" ~ peor_escenario*30,
      P5302OTHER == "MENSUAL" ~ peor_escenario,
      P5302OTHER == "SEMANAL" ~ peor_escenario*4,
      P5302OTHER == "SEMESTRAL" ~ peor_escenario/6,
      P5302OTHER == "NO APLICA" ~ peor_escenario,
      P5302OTHER == "OCASIONALMENTE" ~ 0,
      P5302OTHER == "NO SABE NO RESPONDE" ~ 0,
      P5302OTHER == "POR COSECHA" ~ peor_escenario/3,
      TRUE ~ NA_real_
    ),
    ingresos_mensuales_me = case_when(
      is.na(P5302) ~ NA_real_,
      P5302 == 1 ~ mejor_escenario,
      P5302 == 2 ~ mejor_escenario/2,
      P5302 == 3 ~ mejor_escenario/3,
      P5302 == 4 ~ mejor_escenario/6,
      P5302 == 5 ~ mejor_escenario/12,
      P5302OTHER == "15 DIAS" ~ mejor_escenario*2,
      P5302OTHER == "3 MESES" ~ mejor_escenario/3,
      P5302OTHER == "4 MESES" ~ mejor_escenario/4,
      P5302OTHER == "5 MESES" ~ mejor_escenario/5,
      P5302OTHER == "7 MESES" ~ mejor_escenario/7,
      P5302OTHER == "8 MESES" ~ mejor_escenario/8,
      P5302OTHER == "ANUAL" ~ mejor_escenario/12,
      P5302OTHER == "DIARIO" ~ mejor_escenario*30,
      P5302OTHER == "MENSUAL" ~ mejor_escenario,
      P5302OTHER == "SEMANAL" ~ mejor_escenario*4,
      P5302OTHER == "SEMESTRAL" ~ mejor_escenario/6,
      P5302OTHER == "NO APLICA" ~ mejor_escenario,
      P5302OTHER == "OCASIONALMENTE" ~ 0,
      P5302OTHER == "NO SABE NO RESPONDE" ~ 0,
      P5302OTHER == "POR COSECHA" ~ mejor_escenario/3,
      TRUE ~ NA_real_
    ),
    ingresos_mensuales_med = case_when(
      is.na(P5302) ~ NA_real_,
      P5302 == 1 ~ medio_escenario,
      P5302 == 2 ~ medio_escenario/2,
      P5302 == 3 ~ medio_escenario/3,
      P5302 == 4 ~ medio_escenario/6,
      P5302 == 5 ~ medio_escenario/12,
      P5302OTHER == "15 DIAS" ~ medio_escenario*2,
      P5302OTHER == "3 MESES" ~ medio_escenario/3,
      P5302OTHER == "4 MESES" ~ medio_escenario/4,
      P5302OTHER == "5 MESES" ~ medio_escenario/5,
      P5302OTHER == "7 MESES" ~ medio_escenario/7,
      P5302OTHER == "8 MESES" ~ medio_escenario/8,
      P5302OTHER == "ANUAL" ~ medio_escenario/12,
      P5302OTHER == "DIARIO" ~ medio_escenario*30,
      P5302OTHER == "MENSUAL" ~ medio_escenario,
      P5302OTHER == "SEMANAL" ~ medio_escenario*4,
      P5302OTHER == "SEMESTRAL" ~ medio_escenario/6,
      P5302OTHER == "NO APLICA" ~ medio_escenario,
      P5302OTHER == "OCASIONALMENTE" ~ 0,
      P5302OTHER == "NO SABE NO RESPONDE" ~ 0,
      P5302OTHER == "POR COSECHA" ~ medio_escenario/3,
      TRUE ~ NA_real_
    )
  )

ingreso_mensual_agro[, "ing_agrop"] <- df_hogares[, "ing_agrop"]

## ===== Ingresos Jornales ======= #

monto_jornales <- df_hogares %>% 
  select(P5401
  ) %>% 
  mutate(
    peor_escenario = case_when(
      is.na(P5401) ~ NA_real_,
      P5401 == 1 ~ 0,
      P5401 == 2 ~ 250000,
      P5401 == 3 ~ 500000,
      P5401 == 4 ~ 1000000,
      P5401 == 5 ~ 2000000,
      TRUE ~ NA_real_
    ),
    mejor_escenario = case_when(
      is.na(P5401) ~ NA_real_,
      P5401 == 1 ~ 250000,
      P5401 == 2 ~ 500000,
      P5401 == 3 ~ 1000000,
      P5401 == 4 ~ 2000000,
      P5401 == 5 ~ 2000000,
      TRUE ~ NA_real_
    ),
    medio_escenario = case_when(
      is.na(P5401) ~ NA_real_,
      P5401 == 1 ~ 125000,
      P5401 == 2 ~ 275000,
      P5401 == 3 ~ 750000,
      P5401 == 4 ~ 1500000,
      P5401 == 5 ~ 5000000,
      TRUE ~ NA_real_
    )
  )

base_ingreso_jornales <- df_hogares %>% select(P5401, P5402,P5402OTHER) %>%
  data_frame(., monto_jornales[, -1])


ingreso_mensual_jornales <- base_ingreso_jornales %>% 
  mutate(
    ingresos_mensuales_pe = case_when(
      is.na(P5402) ~ NA_real_,
      P5402 == 1 ~ peor_escenario,
      P5402 == 2 ~ peor_escenario/2,
      P5402 == 3 ~ peor_escenario/3,
      P5402 == 4 ~ peor_escenario/6,
      P5402 == 5 ~ peor_escenario/12,
      P5402OTHER == "15 DIAS" ~ peor_escenario*2,
      P5402OTHER == "3 MESES" ~ peor_escenario/3,
      P5402OTHER == "4 MESES" ~ peor_escenario/4,
      P5402OTHER == "5 MESES" ~ peor_escenario/5,
      P5402OTHER == "7 MESES" ~ peor_escenario/7,
      P5402OTHER == "8 MESES" ~ peor_escenario/8,
      P5402OTHER == "ANUAL" ~ peor_escenario/12,
      P5402OTHER == "DIARIO" ~ peor_escenario*30,
      P5402OTHER == "MENSUAL" ~ peor_escenario,
      P5402OTHER == "SEMANAL" ~ peor_escenario*4,
      P5402OTHER == "SEMESTRAL" ~ peor_escenario/6,
      P5402OTHER == "2 DIAS" ~ peor_escenario*15,
      P5402OTHER == "NO APLICA" ~ 0,
      P5402OTHER == "OCASIONALMENTE" ~ 0,
      P5402OTHER == "SEMANAL" ~ peor_escenario*4,
      TRUE ~ NA_real_
    ),
    ingresos_mensuales_me = case_when(
      is.na(P5402) ~ NA_real_,
      P5402 == 1 ~ mejor_escenario,
      P5402 == 2 ~ mejor_escenario/2,
      P5402 == 3 ~ mejor_escenario/3,
      P5402 == 4 ~ mejor_escenario/6,
      P5402 == 5 ~ mejor_escenario/12,
      P5402OTHER == "15 DIAS" ~ mejor_escenario*2,
      P5402OTHER == "3 MESES" ~ mejor_escenario/3,
      P5402OTHER == "4 MESES" ~ mejor_escenario/4,
      P5402OTHER == "5 MESES" ~ mejor_escenario/5,
      P5402OTHER == "7 MESES" ~ mejor_escenario/7,
      P5402OTHER == "8 MESES" ~ mejor_escenario/8,
      P5402OTHER == "ANUAL" ~ mejor_escenario/12,
      P5402OTHER == "DIARIO" ~ mejor_escenario*30,
      P5402OTHER == "MENSUAL" ~ mejor_escenario,
      P5402OTHER == "SEMANAL" ~ mejor_escenario*4,
      P5402OTHER == "SEMESTRAL" ~ mejor_escenario/6,
      P5402OTHER == "2 DIAS" ~ mejor_escenario*15,
      P5402OTHER == "NO APLICA" ~ 0,
      P5402OTHER == "OCASIONALMENTE" ~ 0,
      P5402OTHER == "SEMANAL" ~ mejor_escenario*4,
      TRUE ~ NA_real_
    ),
    ingresos_mensuales_me = case_when(
      is.na(P5402) ~ NA_real_,
      P5402 == 1 ~ medio_escenario,
      P5402 == 2 ~ medio_escenario/2,
      P5402 == 3 ~ medio_escenario/3,
      P5402 == 4 ~ medio_escenario/6,
      P5402 == 5 ~ medio_escenario/12,
      P5402OTHER == "15 DIAS" ~ medio_escenario*2,
      P5402OTHER == "3 MESES" ~ medio_escenario/3,
      P5402OTHER == "4 MESES" ~ medio_escenario/4,
      P5402OTHER == "5 MESES" ~ medio_escenario/5,
      P5402OTHER == "7 MESES" ~ medio_escenario/7,
      P5402OTHER == "8 MESES" ~ medio_escenario/8,
      P5402OTHER == "ANUAL" ~ medio_escenario/12,
      P5402OTHER == "DIARIO" ~ medio_escenario*30,
      P5402OTHER == "MENSUAL" ~ medio_escenario,
      P5402OTHER == "SEMANAL" ~ medio_escenario*4,
      P5402OTHER == "SEMESTRAL" ~ medio_escenario/6,
      P5402OTHER == "2 DIAS" ~ medio_escenario*15,
      P5402OTHER == "NO APLICA" ~ 0,
      P5402OTHER == "OCASIONALMENTE" ~ 0,
      P5402OTHER == "SEMANAL" ~ medio_escenario*4,
      TRUE ~ NA_real_
    )
  )

ingreso_mensual_jornales[, "ing_jorna"] <- df_hogares[, "ing_jorna"]

## ===== Ingresos Donaciones ======= #
## Origen de las donaciones

ingreso_donaciones <- df_hogares %>% select(P5601A, P5601B, P5601C, P5601D, P5601OTHER)
names(ingreso_donaciones) <-c("Apoyo familiar","Apoyo a la producción", "Apoyo a la educación", "Apoyo a la salud", "Otro")

donaciones_fuente <- ingreso_donaciones %>% melt(id.vars = NULL) %>% data_frame()
# names(donaciones_fuente) = c("Fuente donación", "Recibe Donacion")
# donaciones_fuente$`Recibe Donacion` <- transformacion_faltantes(donaciones_fuente$`Recibe Donacion`)
donaciones_fuente$value <- transformacion_faltantes(donaciones_fuente$value)
resumen_fuentes_donaciones <- donaciones_fuente %>% group_by(variable, value) %>% 
  summarise(conteo = n()) %>% 
  mutate(porcentaje = conteo/sum(conteo))

resumen_fuentes_donaciones$value = ifelse(resumen_fuentes_donaciones$value == 0, "No", "Si")
names(resumen_fuentes_donaciones) <- c("Fuente","Ingreso", "conteo", "Porcentaje")
resumen_fuentes_donaciones[10,2] <- "Si"

ing_g2 <- ggplot(resumen_fuentes_donaciones, aes(x = Fuente, y = Porcentaje*100, fill = Ingreso))
ing_g2 + geom_bar(stat = "identity") +
  xlab("Origen de las donaciones") +
  ylab("Porcentaje") +
  scale_fill_manual("Ingreso", values = c("No" = "#8CBD0E", "Si" = "#005117")) +
  coord_flip()

# Valor de las donaciones mensuales
ingreso_donaciones["Forma"] <- ifelse(df_hogares$P5605 == 1, "Dinero", "Especies")
monto_temp = ifelse(df_hogares$P5605A == 99, NA, df_hogares$P5605A)
ingreso_donaciones["Monto_mes"] <- as.numeric(monto_temp)/as.numeric(df_hogares$P5606)
ingreso_donaciones["ing_donac"] = df_hogares[, "ing_donac"]

## ===== Ingresos Remesas ======= #
## Origen de las remesas

ingreso_remesas <- df_hogares %>% select(P5701A, P5701B, P5702A, P5702B, P5702_PAIS)
names(ingreso_remesas) <-c("Familia", "Otro", "Exterior", "Dentro", "Pais")

remesas_fuente <- ingreso_remesas %>% melt(id.vars = NULL) %>% data_frame()
# names(remesas_fuente) = c("Fuente remesa", "Recibe Remesa")

remesas_fuente$value <- transformacion_faltantes(remesas_fuente$value)
resumen_fuentes_remesas <- remesas_fuente %>% group_by(variable, value) %>% 
  summarise(conteo = n()) %>% 
  mutate(porcentaje = conteo/sum(conteo))

resumen_fuentes_remesas$value = ifelse(resumen_fuentes_remesas$value == 0, "No", "Si")
names(resumen_fuentes_remesas) <- c("Fuente","Ingreso", "conteo", "Porcentaje")

ing_g2 <- ggplot(resumen_fuentes_remesas[1:8,], aes(x = Fuente, y = Porcentaje*100, fill = factor(Ingreso)))
ing_g2 + geom_bar(stat = "identity") +
  xlab("Origen de las remesas") +
  ylab("Porcentaje") +
  scale_fill_manual("Ingreso", values = c("No" = "#8CBD0E", "Si" = "#005117")) +
  coord_flip()

# Valor de las donaciones mensuales
ingreso_remesas["Monto_mes"] <- ifelse(df_hogares$P5703 == 99, NA, df_hogares$P5703) %>% as.numeric()

ingreso_remesas["ing_remes"] = df_hogares[, "ing_remes"]
ingreso_remesas["monto_mes_estimado"] = as.numeric(df_hogares$P5703)/as.numeric(df_hogares$P5704) ## Preguntar

## ===== Otros Ingresos ======= #
## Hay que calcular los otros ingresos!!

# ¿ que actividad proviene el ingreso?
docs <- Corpus(VectorSource(df_hogares$P5801))
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

# Monto de los ingresos 

base_ingreso_otros <- df_hogares %>% select(P5803, P5802,P5802OTHER)
base_ingreso_otros$P5803 <- ifelse(base_ingreso_otros$P5803 == 99, NA, base_ingreso_otros$P5803) %>% as.numeric()

ingreso_mensual_otros <- base_ingreso_otros %>% 
  mutate(
    ingresos_mensuales_otros = case_when(
      is.na(P5802) ~ NA_real_,
      P5802 == 1 ~ P5803,
      P5802 == 2 ~ P5803/2,
      P5802 == 3 ~ P5803/3,
      P5802 == 4 ~ P5803/6,
      P5802 == 5 ~ P5803/12,
      P5802OTHER == "DIARIO" ~ P5803*30,
      P5802OTHER == "HACE 7 MESES NO RECIBE" ~ P5803/7,
      P5802OTHER == "FAMILIAS EN ACCION" ~ P5803,
      P5802OTHER == "NO SABE" ~ P5803/12,
      P5802OTHER == "OCASIONAL" ~ P5803/12,
      P5802OTHER == "POR TEMPORADA" ~ P5803/3,
      P5802OTHER == "SEMANAL" ~ P5803*4,
      P5802OTHER == "UNA SOLA VEZ" ~ 0,
      P5802OTHER == "TIENDA" ~ 0,
      TRUE ~ NA_real_
    )
  )

ingreso_mensual_otros[, "ing_otros"] <- df_hogares[, "ing_otros"]

## ¿Porque hay registros con other y no con el valor?


# Calculo total de los ingresos -------------------------------------------

calculo_limpio <- function(x){
  k <- which(is.na(x) | x == Inf)
  x[k] = 0
  return(x)
}

ingreso_total <- calculo_limpio(transformacion_faltantes(ingreso_mensual_agro$ingresos_mensuales_med * ingreso_mensual_agro$ing_agrop)) +
  calculo_limpio(transformacion_faltantes(ingreso_mensual_jornales$ingresos_mensuales_me * ingreso_mensual_jornales$ing_jorna)) +
  calculo_limpio(transformacion_faltantes(ingreso_donaciones$Monto_mes * ingreso_donaciones$ing_donac)) +
  calculo_limpio(transformacion_faltantes(ingreso_remesas$monto_mes_estimado * ingreso_remesas$ing_remes)) +
  calculo_limpio(transformacion_faltantes(ingreso_mensual_otros$ingresos_mensuales_otros * ingreso_mensual_otros$ing_otros))


ingresos <-  data_frame(Act_agrop = transformacion_faltantes(ingreso_mensual_agro$ingresos_mensuales_med * ingreso_mensual_agro$ing_agrop) ,
                      Jornales = transformacion_faltantes(ingreso_mensual_jornales$ingresos_mensuales_me * ingreso_mensual_jornales$ing_jorna) ,
                      Donaciones = transformacion_faltantes(ingreso_donaciones$Monto_mes * ingreso_donaciones$ing_donac) ,
                      Remesas = transformacion_faltantes(ingreso_remesas$monto_mes_estimado * ingreso_remesas$ing_remes) ,
                      Otros = transformacion_faltantes(ingreso_mensual_otros$ingresos_mensuales_otros * ingreso_mensual_otros$ing_otros),
                    Total = ingreso_total,
                    Clase = df_hogares$CLASE,
                    Tramo = df_hogares$Nombre)

Ingresos_grap = ingresos[,-8] %>% melt("Clase") %>% subset(value != 0 & value != Inf) %>% data_frame()
names(Ingresos_grap) = c("Clase", "Fuente", "Ingresos")
Ingresos_grap$Clase <- factor(Ingresos_grap$Clase)
levels(Ingresos_grap$Clase) <- c("Cabecera", "Corregimiento", "Rural")
Ingresos_grap <- na.omit(Ingresos_grap)
ingresos_grap = ggplot(Ingresos_grap, aes(Fuente , Ingresos, fill = Clase))
ingresos_grap + geom_boxplot() + ylab("Ingresos Mensuales") + xlab("Fuente de ingreso") + scale_y_continuous(labels = comma) + ylim(0, 800000)


Ingresos_tram = ingresos[,-7] %>% melt("Tramo") %>% subset(value != 0 & value != Inf) %>% data_frame()
names(Ingresos_tram) = c("Tramo", "Fuente", "Ingresos")
Ingresos_tram$Tramo <- factor(Ingresos_tram$Tramo)
Ingresos_tram <- na.omit(Ingresos_tram)
Ingresos_tram = ggplot(Ingresos_tram, aes(Fuente , Ingresos, fill = Tramo))
Ingresos_tram + geom_boxplot() + ylab("Ingresos Mensuales") + xlab("Fuente de ingreso") + scale_y_continuous(labels = comma) + ylim(0, 800000)


calcula_resumen_ingreso <- function(variable){
  conteo <- ifelse(variable == 0, "No Reporta", "Reporta") %>% table()
  porcentaje <- conteo/sum(conteo)
  con_ingreso <- variable[variable!= 0 | variable!= Inf]
  media <- mean(con_ingreso)
  mediana <- median(con_ingreso)
  desviacion <- sd(con_ingreso)
  cuantiles <- quantile(con_ingreso)
  rango <- (max(con_ingreso) - min(con_ingreso))/5
  return(list(conteo = conteo, porcentaje = porcentaje, media = media, mediana= mediana, desviacion = desviacion, cuantiles = cuantiles))
}

calcula_resumen_ingreso(ingresos$Act_agrop)


# Cálculo del Autoconsumo -------------------------------------------------------------

medios_df <- df_hogares %>% select(c("P03A", "P03B", "P03C", "P03D", "P03E", "P03F", "P03G", "P03H", "P03I", "P03J", "Nombre", "CLASE"))

names(medios_df) <- c("Agricultura", "Ganaderia", "EspeciesMenores", "Pesca", "OtrasPecuarias", "Forestal", "Mineria", "Comercio", "OtrasActPec", "OtrasActNoPec", "Tramo", "Clase")

for(i in 1:ncol(medios_df)){
  medios_df[i] <- medios_df[i] %>% unlist() %>%  as.numeric()
}

medios_df_graf <- melt(medios_df, id.vars = c("Tramo", "Clase")) %>% data_frame()
medios_df_graf$Clase <- medios_df_graf$Clase %>% as.factor()
levels(medios_df_graf$Clase) <- c("Cabecera", "Corregimiento", "Rural")

med_grap <- ggplot(medios_df_graf, aes(x = value, color = Clase))
med_grap + geom_histogram(col = "#005117", fill = "#8CBD0E", position = "dodge", binwidth = 10)+ facet_wrap(. ~ variable, ncol = 5) + xlab("Porcentaje de los medios de vida del hogar") + ylab("Actividad")

# Autoconsumo y venta

autoc_df <- df_hogares %>% select(c("P04AA", "P04AB", "P04BA", "P04BB", "P04CA", "P04CB", "P04DA", "P04DB", "P04EA", "P04EB", "P04FA", "P04FB","P04HA", "P04HB", "P04IA", "P04IB", "Nombre", "CLASE"))

for(i in 1:ncol(autoc_df)){
  autoc_df[i] <- autoc_df[i] %>% unlist() %>%  as.numeric()
}

autoc_df_auto <- autoc_df %>% select(ends_with(c("A", "Nombre", "CLASE")))
names(autoc_df_auto) <- c("Agricultura", "Ganadería", "EspeciesMenores", "Pesca", "OtrasPec", "Forestal", "Comercio", "Otro", "Tramo", "Clase")

autoc_df_graf <- melt(autoc_df_auto, id.vars = c("Tramo", "Clase")) %>% data_frame()
autoc_df_graf$Clase <- autoc_df_graf$Clase %>% as.factor()
levels(autoc_df_graf$Clase) <- c("Cabecera", "Corregimiento", "Rural")

med_grap <- ggplot(autoc_df_graf, aes(x = value, color = Clase))
med_grap + geom_histogram(col = "#005117", fill = "#8CBD0E", position = "dodge", binwidth = 10)+ facet_wrap(. ~ variable, ncol = 4) + xlab("Porcentaje de autoconsumo") + ylab("Actividad")

autoc_df_graf_temp <- autoc_df_graf

autoc_df_graf_temp["medios"] <- !is.na(autoc_df_graf_temp$value)

autoc_df_graf_temp <- autoc_df_graf_temp %>% group_by(variable, medios) %>% count() %>% mutate(perc = round(n/nrow(autoc_df) * 100)) %>% arrange(medios)
autoc_df_graf_temp$medios <- as.factor(autoc_df_graf_temp$medios)
levels(autoc_df_graf_temp$medios) <- c("No", "Si")

autoc_df_plot <- ggplot(autoc_df_graf_temp, aes(x = variable, y = perc, fill = medios))
autoc_df_plot + geom_bar(stat = "identity") +
  xlab("Actividad") +
  ylab("Porcentaje") +
  scale_fill_manual("Actividad", values = c("No" = "#8CBD0E", "Si" = "#005117")) + coord_flip()

autoc_df_graf_temp

# Porcentaje de autoconsumo animales y cultivos

for(i in 1:ncol(autoc_df_auto)){
  autoc_df_auto[i][autoc_df_auto[i] %>% is.na()] <- 0
}

# autoc_df_auto_porc <- autoc_df_auto %>% mutate(porc = ))
autoc_df_auto <- data.frame(autoc_df_auto)

autoc_df_auto[, "porc"] <- NA
# autoc_df_auto[, "median"] <- NA

for(i in 1:nrow(autoc_df_auto)){
  autoc_df_auto$porc[i] <-  max(autoc_df_auto[i, c(1:5)])
  # autoc_df_auto$median[i] <-  min(autoc_df_auto[i, c(1:5)])
}


autoconsumo <- autoc_df_auto$porc/100 * ingresos$Act_agrop
boxplot(autoconsumo, ylim = c(0, 400000))

hist(autoconsumo, xlim = c(0, 1000000), bbreaks = 15)

autoconsumo %>% summary()
