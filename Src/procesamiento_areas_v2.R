# Librerias
library(ggplot2)
library(dplyr)
library(DataExplorer)
library(plotly)
library(tidyverse)
library(FactoMineR)
library(readxl)
library(corrplot)
library(ggcorrplot)

# Lectura de los datos

df_hogares_original <- read_excel('../input/Base_Hogares_101020.xlsx', skip = 1)
df_hogares_original["id_unico"] <- 1:nrow(df_hogares_original)
df_diccionario <- read_excel('../input/Base_Hogares_101020.xlsx')
df_dicc <- data_frame(preguntas = names(df_diccionario), nombre_base = names(df_hogares))
cobertura <- read.csv("../input/cobertura_final.csv", sep = ";", encoding = "UTF-8")

cobertura[, "NUMERO.DE.FORMULARIO"] <- as.character(cobertura[, "NUMERO.DE.FORMULARIO"])

# Vamos a dejar los que están duplicados por el momentos por fuera del ejercicio de imputación de la base de datos


dup <- (df_hogares_original$A00 %>% table())[which(df_hogares_original$A00 %>% table() > 1)] %>% names()
df_hogares <- df_hogares_original[-which(df_hogares_original$A00 %in% dup), ]
cobertura <- cobertura[-which(cobertura$NUMERO.DE.FORMULARIO %in% dup), ]

df_hogares <- subset(df_hogares, A00 != "28011")

areas_pre_cruzar <- df_hogares_original[which(df_hogares_original$A00 %in% c(dup, "28011")), c("id_unico", "area_m2")]
areas_pre_cruzar$area_m2 <- as.numeric(areas_pre_cruzar$area_m2)

cobertura[which(cobertura$NUMERO.DE.FORMULARIO %in% df_hogares$A00 == FALSE),]

base_cobertura <- left_join(df_hogares, cobertura,  by =c("A00"= "NUMERO.DE.FORMULARIO"))
df_hogares[, "Nombre_Tramo"] <- df_hogares["Nombre"]

base_cobertura$A00
# Funciones
dim(base_cobertura)
dim(df_hogares)

transformacion_faltantes <- function(x){
  k <- which(is.na(x))
  k1 <- which(x == 99)
  x[k] = 0
  x[k1] = NA
  return(as.numeric(x))
}

# Analisis por actividades economicas

df_actividades <- df_hogares %>% select(starts_with(c("P02")))
df_actividades <- apply(df_actividades, 2, transformacion_faltantes) %>% data.frame()  %>% .[, 1:9]
names(df_actividades) <- c("Agricultura", "Ganaderia","Especies Menores", "Pesca", "Otras Pecuarias", "Forestal", "Minería", "Comercio", "Otras actividades")


correlaciones <-  df_actividades[, 1:9] %>% apply(., 2, transformacion_faltantes) %>% data_frame() %>% cor()

corrplot(correlaciones, method = "color")

calculador_frecuencias <- function(nombre_var){
  df_temp = df_actividades[df_actividades[,nombre_var] == 1, ]
  df_abs <- df_temp %>% apply(., 2, sum)
  df_rel <- round(df_abs/df_abs[nombre_var]*100, 1)
  return(list(abs = df_abs, rel = df_rel))
}

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

# ggcorrplot(df_relativas, hc.order = TRUE, lab = TRUE)


plotDat_rel <- gather(df_relativas, key = "Actividad1", value = "Porcentaje", -Actividad)
plotDat_abs <- gather(df_absolutas, key = "Actividad1", value = "Conteo", -Actividad)


#### % Medios de vida, del 100% de personas que se dedican a la actividad de la respectiva fila, el x % se dedica a... respectiva columna, solo interpretar por filas

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

## Actividad principal

df_act_principal <- df_hogares %>% select(starts_with(c("P03")))
df_act_principal <- apply(df_act_principal, 2, transformacion_faltantes) %>% data.frame()
names(df_act_principal) <- c("Agricultura", "Ganaderia", "Especies Menores", "Pesca", "Otras Pecuarias", "Forestal", "Minería", "Comercio", "Otras actividades")


act_principal <- c()

for(i in 1:nrow(df_act_principal)){
  act_principal[i] <- names(which.max(df_act_principal[i,]))
}

act_principal_hogar <- act_principal

act_principal <- act_principal %>% table() %>% data.frame()
names(act_principal) <- c("Actividad", "Conteo")

ggplot(act_principal, aes(x = reorder(Actividad, -Conteo), y = Conteo)) + geom_bar(stat = "identity")


## Actividad Principal por tramo

df_act_principal <- df_hogares %>% select(starts_with(c("P03", "Nombre_Tramo")))
df_act_principal <- apply(df_act_principal[,c(1:10)], 2, transformacion_faltantes) %>% data.frame(., df_hogares["Nombre_Tramo"])
names(df_act_principal) <- c("Agricultura", "Ganaderia", "Especies Menores", "Pesca", "Otras Pecuarias", "Forestal", "Minería", "Comercio", "Otras campo", "otras_agro", "Nombre_Tramo" )

# No reportado, % restante a la suma de los otros
df_act_principal["Total Reportado"] <- df_act_principal[, 1:10] %>% apply(., 1, sum)
df_act_principal["No Reportado"] <- 100 - df_act_principal["Total Reportado"]

act_principal <- c()


for(i in 1:nrow(df_act_principal)){
  act_principal[i] <- names(which.max(df_act_principal[i, -c(11, 12, 13)]))
}

act_princ_tramo <- data_frame(Actividad = act_principal, Tramo = df_act_principal[, 11]) %>% group_by(Actividad, Tramo) %>% count()
names(act_princ_tramo) <- c("Actividad", "Tramo", "Conteo")

ggplot(act_princ_tramo, aes(x = reorder(Actividad, -Conteo), y = Conteo)) + geom_bar(aes(fill = Tramo), stat="identity", position=position_dodge())

table(df_hogares$Nombre_Tramo)

## Actividad Secundaria

act_secundaria <- c()
porcentaje_sec <- c()

for(i in 1:nrow(df_act_principal)){
  act_secundaria[i] <- names(which.max(df_act_principal[i, -c(10, which.max(df_act_principal[i, -10]))]))
  porcentaje_sec[i] <- df_act_principal[i,(which.max(df_act_principal[i, -c(10, which.max(df_act_principal[i, -10]))]))]
}


act_sec_tramo <- data_frame(Actividad = act_secundaria, Tramo = df_act_principal[, 10]) %>% group_by(Actividad, Tramo) %>% count()
names(act_sec_tramo) <- c("Actividad", "Tramo", "Conteo")

ggplot(act_sec_tramo, aes(x = reorder(Actividad, -Conteo), y = Conteo)) + geom_bar(aes(fill = Tramo), stat="identity", position=position_dodge())

table(df_hogares$Nombre_Tramo)



## Imputacion valores de metros cuadrados

base_areas <- select(base_cobertura, c("n_miembros_hogar", "area_m2", "valor_m2", "P4101", "P4102", "P4104", "P4201", "P420103B", "P43", "P43B", "P41", "P4101", "P53", "P5301", "P5302", "P5302OTHER", "P60", "P6001", "P6002", "CLASE", "A00", "id_unico"))
base_areas <- data_frame(base_areas, act_principal = act_principal)
base_areas$area_m2 <- as.numeric(base_areas$area_m2)

# Clase - act_principal - Area


base_areas$P60 %>% table()

base_areas %>% group_by(act_principal, CLASE) %>% summarise(media = mean(area_m2, na.rm=TRUE), mediana = median(area_m2, na.rm=TRUE))

p <- ggplot(base_areas, aes(act_principal,area_m2, fill = as.factor(CLASE))) + geom_boxplot() + ylim(c(0, 300000))
p + facet_grid(vars(CLASE), scales="free")

p <- ggplot(base_areas, aes(act_principal,area_m2, fill = as.factor(CLASE))) + geom_boxplot()
p + facet_grid(vars(CLASE), scales="free")


# Area - Percibe ingresos - monto 

# Convirtiendo valores usando las frecuencias

base_areas$P5301

base_areas$P5302
base_areas$P5302OTHER

ingreso <- base_areas %>% 
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
      P5301 == 5 ~ 1000000,
      TRUE ~ NA_real_
    ),
    medio_escenario = case_when(
      is.na(P5301) ~ NA_real_,
      P5301 == 1 ~ 125000,
      P5301 == 2 ~ 275000,
      P5301 == 3 ~ 750000,
      P5301 == 4 ~ 1500000,
      P5301 == 5 ~ 5000000,
      TRUE ~ NA_real_
    )
  )


base_areas_ingreso <- cbind(base_areas, ingreso[, -1]) %>% data_frame()



ingreso_mensual <- base_areas_ingreso %>% 
  select(P5301, P5302, 
         P5302OTHER,
         peor_escenario, 
         mejor_escenario,
         medio_escenario
  ) %>% 
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
      TRUE ~ NA_real_
    ),
    ingresos_mensuales_me = case_when(
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
      TRUE ~ NA_real_
    )
  )
#

base_areas_ingreso_escenarios <- cbind(base_areas_ingreso, ingreso_mensual[, -c(1:6)]) %>% data_frame()
base_areas_ingreso_escenarios$act_principal <- as.factor(base_areas_ingreso_escenarios$act_principal)
p2 <- ggplot(base_areas_ingreso_escenarios, aes(x = area_m2, y = ingresos_mensuales_me, colour=factor(CLASE))) + geom_point()
p2 + facet_grid(act_principal ~ CLASE, margins = TRUE) + xlim(c(0, 4000000))
#

## Ajustando un modelo para identificar los valores atípicos
## Variable respuesta : Area en metros cuadrados

fit1 <- lm(log(area_m2) ~ as.factor(act_principal) + log(ingresos_mensuales_me) + as.factor(CLASE)  , data = base_areas_ingreso_escenarios)
summary(fit1)

distancias_cook <- cooks.distance(fit1)

plot(distancias_cook, pch="*", cex=2, main="Influential Obs by Cooks distance", ylim = c(0, 0.04))  # plot cook's distance
abline(h = 4*mean(distancias_cook, na.rm=T), col="red")  # add cutoff line
text(x=1:length(distancias_cook)+1, y=distancias_cook, labels=ifelse(distancias_cook>4*mean(distancias_cook, na.rm=T),names(distancias_cook),""), col="red")
datos_influencia <- as.numeric(names(distancias_cook)[(distancias_cook > 4*mean(distancias_cook, na.rm=T))])

datos_influyentes <- base_areas_ingreso_escenarios[datos_influencia,]


print(paste("Se encontraron en total: ", dim(datos_influyentes_base_completa)[1], "datos influyentes"))
datos_influyentes_base_completa <- df_hogares[datos_influencia, ]
datos_influyentes_base_completa$VEREDA %>% table() %>% max()
datos_influyentes_base_completa$ENCUESTADOR %>% table() %>% max()
datos_influyentes_base_completa$Nombre_Tramo %>% table() %>% max()

## Reemplazando por los valores qué mas sentido tendrían para ese caso especifico
## Ajustando el modelo sin los datos influyentes

mantener <- which(base_areas_ingreso_escenarios$act_principal %in% c("Forestal", "Otras Pecuarias"))
k <- datos_influencia[which(!(datos_influencia %in% mantener))]
base_aie_limpia <- base_areas_ingreso_escenarios[-k, ]
fit2 <- lm(log(area_m2) ~ as.factor(act_principal) + log(ingresos_mensuales_me) + as.factor(CLASE)  , data = base_aie_limpia)
summary(fit2)

k1 <- which(datos_influyentes$act_principal %in% c("Forestal", "Otras Pecuarias"))
datos_influyentes["nuevas_act"] <- datos_influyentes$act_principal
datos_influyentes[k1, "nuevas_act"] <- "Minería"

new_data <- datos_influyentes[, c("act_principal", "ingresos_mensuales_me", "CLASE")]

# Predicciones de las areas para los datos atípicos

valores_areas_ajustadas <- predict(fit2, new_data) %>% exp() %>% round()
base_areas_ajustadas <- base_areas_ingreso_escenarios
base_areas_ajustadas[datos_influencia, "area_m2"] <- valores_areas_ajustadas


base_areas_ajustadas %>% group_by(act_principal, CLASE) %>% summarise(media = mean(area_m2, na.rm=TRUE), mediana = median(area_m2, na.rm=TRUE))

p <- ggplot(base_aie_limpia, aes(act_principal, log(area_m2), fill = as.factor(CLASE))) + geom_boxplot()
p + facet_grid(vars(CLASE), scales="free")

p <- ggplot(base_aie_limpia, aes(act_principal,area_m2, fill = as.factor(CLASE))) + geom_boxplot()
p + facet_grid(vars(CLASE), scales="free") + ylim(c(0, 390000))

base_areas_ajustadas$area_m2 %>% summary()

datos_ajuste2 <- which(base_areas_ajustadas$area_m2 <= 20)

base_datos_ajustar2 <- base_areas_ajustadas[datos_ajuste2, c("CLASE", "act_principal")]

mean_clase_actividad = base_areas_ajustadas %>% group_by(CLASE, act_principal) %>% summarise(media = mean(area_m2, na.rm = T))

cruce_medias <- left_join(base_datos_ajustar2, mean_clase_actividad)

base_areas_ajustadas[datos_ajuste2, "area_m2"] = cruce_medias$media


## Uniendo las dos bases de datos

areas_cruzar <- base_areas_ajustadas[, c("id_unico", "area_m2")]
base_areas_totales <- rbind(areas_pre_cruzar, areas_cruzar)
base_areas_totales <- base_areas_totales[order(base_areas_totales$id_unico), ]

df_hogares_original$area_m2_corregida <- base_areas_totales$area_m2
df_hogares_original %>% dim()
df_hogares_original$area_m2_corregida %>% sort()

df_hogares_original

write_delim(df_hogares_original, "../Output/base_hogares_corregida.csv", na = "")


write.xlsx(
  df_hogares_original,
  "../Output/df_hogares_original.xlsx",
  sheetName = "base_personas",
  col.names = TRUE,
  row.names = FALSE,
  append = FALSE,
  showNA = FALSE,
  password = NULL
)

write_tsv(df_hogares_original, "../Output/base_hogares_corregida.csv", na = "")

# base_areas_ajustadas_v2 <- 

# add labels

## AQUI VOY
base_atipicos_area <- df_hogares[which(df_hogares$valor_m2 <= 20) , ]
base_atipicos_area$P53 %>% table()%>% barplot()
base_atipicos_area$P5301 %>% table() %>%  barplot()


metros <- df_hogares$valor_m2 %>% na.omit() %>% as.numeric()
df_hogares$valor_m2[which(df_hogares$P41 == "99")] <- NA
dos <- df_hogares$valor_m2 %>% na.omit() %>% as.numeric() %>% sort()
dos[2]



# Valores en metros cuadrados

valores_m2_atip <- df_hogares$area_m2 %>% na.omit() %>% as.numeric() %>% boxplot()
valores_m2_atip$out


valores_m2_log <- df_hogares$area_m2 %>% na.omit() %>% as.numeric() %>% log() %>% boxplot()
valores_m2_log$out

df_hogares$`Cobertura campo`


valores_m2_log <- df_hogares$area_m2 %>% na.omit() %>% as.numeric() %>% log() %>% boxplot()
valores_m2_log