# Librerias
library(ggplot2)
library(dplyr)
library("DataExplorer")
library("plotly")
library(tidyverse)
library(readxl)
# library(xlsx)

# library(tidyverse)
setwd("C:/Users/diant/Documents/deproyectos-Cauca/Bases/Análisis/Src/")

# Lectura de los datos
base_hogares <- read.csv("../Input/hogares.csv", sep = ";", encoding = "UTF-8")
base_personas <- read.csv("../Input/personas.csv", sep = ";", encoding = "UTF-8")
diccionario <- read.csv("../Input/diccionario.csv", sep =";", encoding = "UTF-8")
muestra_rural <- read.csv("../Input/muestra_rural.csv", sep = ";", encoding = "UTF-8")
muestra_urbana <- read.csv("../Input/muestra_urbano.csv", sep = ";", encoding = "UTF-8")
cobertura <- read.csv("../Input/cobertura_final.csv", sep = ";", encoding = "UTF-8")
tramos <- read.csv("../Input/Tramos_Municipios.csv", sep=";", encoding = "UTF-8")
niveles <- read.csv("../Input/niveles12.csv", sep = ",")

#


head(base_hogares)
head(base_personas)
head(muestra_rural)
head(muestra_urbana)

rellenar <- function(x){
  x <- as.character(x)
  if(nchar(x) == 1){
    x = paste0("00", x)
  }
  if(nchar(x) == 2){
    x = paste0("0", x)
  }
  return(x)
}

# Uniendo las dos muestras

muestra_rural[, "Tipo"] = "Rural"
muestra_urbana[, "Tipo"] = "Urbana"

muestra1 <- muestra_rural[, c(1:3, 19)]
muestra2 <- muestra_urbana[, c(1:3, 11)]
names(muestra2) <- names(muestra1)

for(i in 1:nrow(muestra1)){
  muestra1[i , "MPIO_CCDGO"] <- paste0(muestra1$DPTO_CCDGO[i], rellenar(muestra1[i, "MPIO_CCDGO"]))
}

muestra <- rbind(muestra1, muestra2)

# Uniendo cobertura con muestra

muestra %>% View()

# Uniendo las muestras con los tramos

tramos$X.U.FEFF.CODIGO_NOM <- as.character(tramos$X.U.FEFF.CODIGO_NOM)

muestra_tramos <- left_join(muestra, tramos, by = c("MPIO_CCDGO" = "X.U.FEFF.CODIGO_NOM"))

# uniendo muestra_tramo con los niveles

niveles_cruce <- niveles %>% group_by(DPTOMPIO, nivel) %>% count()
niveles_cruce$DPTOMPIO <- as.character(niveles_cruce$DPTOMPIO)
names(niveles_cruce)[1] = names(muestra_tramos)[3]

muestra_tramo_niveles <- left_join(muestra_tramos, niveles_cruce)

niveles$DPTOMPIO <- as.character(niveles$DPTOMPIO)
tramos$DPTOMPIO <- as.character(tramos$X.U.FEFF.CODIGO_NOM)

tramo_niveles <- left_join(tramos, niveles, by = "DPTOMPIO")

## Uniendo con la cobertura

cobertura$municipio_nombre <- tolower(cobertura$MUNICIPIO)
tramo_niveles$municipio_nombre <- tolower(tramo_niveles$NOMB_MPIO)



## Base con tramo-niveles
tramo_niveles$mpio_cod <- tramo_niveles$X.U.FEFF.CODIGO_NOM
base_hogares$mpio_cod <- as.character(base_hogares$P01)

hogares_tramo_niveles <- left_join(base_hogares, tramo_niveles, by = "mpio_cod")

niveles_mpio <- niveles %>% group_by(DPTOMPIO, nivel) %>% count()

hogares_tramo <- left_join(base_hogares, tramos, by = c("mpio_cod" = "DPTOMPIO"))

# hogares_nivel <- left_join(base_hogares, niveles_mpio, by = c("mpio_cod" = "DPTOMPIO"))

cobertura_muestra <- left_join(cobertura_muestra)

# A esta altura ya podemos hacer compararaciones por nivelesy tramos

############# Correcciones sobre la base de datos

#Donaciones - P5605A

hogares_tramo_niveles$P5605A %>% hist()

boxplot(hogares_tramo_niveles$P5605A)$out %>% length()

hogares_tramo_niveles$P5605A %>% na.omit() %>% min()

toNA <- which(hogares_tramo_niveles$P5605A %in% c("99", "999", "9999"))
hogares_tramo_niveles$P5605A[toNA] <- NA

hogares_tramo_niveles$P5605A %>% boxplot()

# donaciones_tipo <- function(x, name){
#   par(mfrow = c(1, 2))
#   k <- which(x == 1)
#   atip <- boxplot(hogares_tramo_niveles$P5605A[k])$out %>% min()
#   boxplot(hogares_tramo_niveles$P5605A[k], main = name)
#   hist(hogares_tramo_niveles$P5605A[k], main = name)
# }
# 
# donaciones_tipo(hogares_tramo_niveles$P5601A, "Apoyo familiar")
# donaciones_tipo(hogares_tramo_niveles$P5601B, "Apoyo a la producción")
# donaciones_tipo(hogares_tramo_niveles$P5601C)
# donaciones_tipo(hogares_tramo_niveles$P5601D)

#

# hist(hogares_tramo_niveles$P59)
# min_out <- boxplot(hogares_tramo_niveles$P59)$out %>% min()



### Procesamiento Miguel


# Carga de librerias ------------------------------------------------------



# Carga de datos ----------------------------------------------------------

df_hogar <- read_excel('../Input/HOGAR_20200930.xlsx', range = "A3:AHC4745")
df_personas <- read_excel('../Input/PERSONAS_2020923.xlsx', range = "A2:Q21217")

# df_hogar <- base_hogares
# df_personas <- base_personas

# df_personas$CONSECUTIVO <- df_personas$X.U.FEFF.CONSECUTIVO
# df_hogar$CONSECUTIVO <- df_hogar$X.U.FEFF.CONSECUTIVO
# Jornal medio ------------------------------------------------------------

df_group_personas <- df_personas %>% 
  group_by(CONSECUTIVO, A00) %>% 
  summarise(
    n_miembros_hogar = n(),
    ocupaciones_diferentes = n_distinct(p1705)
  ) 

df_jornal_medio <- df_hogar %>% 
  select(CONSECUTIVO, A00, P5403) %>% 
  full_join(df_group_personas, by = c("CONSECUTIVO","A00")) %>% 
  mutate(jornal_medio_hogar = P5403/n_miembros_hogar)

df_jornal_medio %>% 
  ggplot(aes(jornal_medio_hogar)) +
  geom_boxplot()

df_jornal_medio %>% 
  ggplot(aes(P5403, jornal_medio_hogar)) +
  geom_point()

df_jornal_medio %>% 
  ggplot(aes(n_miembros_hogar,P5403)) +
  geom_point()

df_jornal_medio %>% 
  ggplot(aes(n_miembros_hogar,P5403)) +
  geom_boxplot()

# Areas a metros cuadrados ------------------------------------------------


df_hogar %>% 
  select(P41,
         P41UM,
         P41UMOTHER
  ) %>%
  filter(!is.na(P41UMOTHER)) %>% 
  distinct(P41UMOTHER) %>% 
  View()

df_area_m2 <- df_hogar %>% 
  select(CONSECUTIVO, A00, 
         P41,
         P41UM,
         P41UMOTHER
  ) %>% 
  mutate(
    area_m2 = case_when(
      is.na(P41) ~ NA_real_,
      P41UM == 1 ~ P41,
      P41UM == 2 ~ P41*10000,
      P41UM == 3 ~ P41*6400,
      P41UM == 4 ~ P41*3900,
      P41UMOTHER == "MEDIA HECTAREA" ~ 5000,
      P41UMOTHER == "CUARTERON" ~ 2500,
      P41UMOTHER == "HECTARE Y MEDIA" ~ 15000,
      TRUE ~ NA_real_
    )
  )

# Valor de la vivienda ----------------------------------------------------
hogares_tramo$CONSECUTIVO <- hogares_tramo$X.U.FEFF.CONSECUTIVO

df_vivienda <- df_hogar %>% 
  select(CONSECUTIVO, A00, P6003) %>% 
  full_join(df_area_m2, by = c("CONSECUTIVO","A00")) %>% 
  mutate(valor_m2 = P6003/area_m2)


b1 <- left_join(df_group_personas, df_vivienda, by = c("A00", "CONSECUTIVO"))
base_final <- left_join(hogares_tramo, b1, by = c("CONSECUTIVO","A00"))

write.csv(base_final, "../Output/base_atipicos.csv", sep = "\t")

write.xlsx(
  base_final,
  "../Output/base_final.xlsx",
  sheetName = "base_personas",
  col.names = TRUE,
  row.names = FALSE,
  append = FALSE,
  showNA = FALSE,
  password = NULL
)

write_delim(base_final, "../Output/base_atipicos.csv", na = "")
write_tsv(base_final, "../Output/base_atipicos.csv", na = "")



# Arreglando problemas de nombres

mpios_nombre_cambiar <- c("cáceres ", "ituango ", "cáceres ", "ayapel ", "sucre ", "nechi ", "san jacinto del cauca ", "toledo ", "liborina ", " san jacinto del cauca")
mpios_nombre_correctos <- c("cáceres", "ituango", "ayapel", "sucre", "nechi", "san jacinto del cauca", "toledo", "liborina", "san jacinto del cauca")

for(i in 1:length(mpios_nombre_cambiar)){
  k <- which(cobertura$municipio_nombre == mpios_nombre_cambiar[i])
  cobertura$municipio_nombre[k] <- mpios_nombre_correctos[i]
}


cobertura_tramo_niveles <- left_join(cobertura, tramo_niveles, by = "municipio_nombre")
kn <- which(is.na(cobertura_tramo_niveles$nivel))
kn %>% length()

base_cobertura <- left_join(cobertura, base_hogares, by =c("NUMERO.DE.FORMULARIO" = "A00"))

head(base_cobertura)

base_hogares["VEREDA_NOM"] = gsub(".* - ","",base_hogares$VEREDA)
niveles12["VEREDA_NOM"] = niveles12$NOMBRE_VER
base_hogares_niveles <- left_join(base_hogares, niveles12, by = c("VEREDA_NOM")) # 
















# Niveles12 de limpieza.R

niveles12$NOMB_MPIO
base_hogares$VEREDA

# Funciones

grafico_antes_despues <- function(v1, v2){
  # Servicio de transporte
  transporte <- data.frame(construccion = v1, contingencia = v2, diferencia = v1 - v2)
  
  transporte_grafico <- data_frame(transporte = c(v1, v2),
                                   temporalidad = c(rep("Construccion", length(v1)), rep("Contingencia", length(v2))))
  
  transporte_resumen <- transporte_grafico %>% subset(transporte != 99) %>% group_by(temporalidad, transporte) %>% count()
  transporte_cambio <- transporte %>% subset(construccion != 99) %>% subset(contingencia != 99) %>% group_by(diferencia) %>% count()
  
  p <- ggplot(data = transporte_resumen, aes(x = transporte, y = n, fill = temporalidad)) + geom_bar(stat = "identity", position = position_dodge())
  
  fig1 <- ggplotly(p)

  p2 <- ggplot(data = transporte_cambio, aes(x = diferencia, y = n)) + geom_bar(stat = "identity", position = position_dodge())
  
  fig2 <- ggplotly(p2)
  
  return(list(comparativo = fig1, diferencia = fig2))
  
}


descriptivo_unica_dis <- function(x){
  print(summary(x))
  x_data = data.frame(var = x)
  p <- ggplot(data = x, aes(x = var)) + geom_bar()
  fig <- ggplotly(p)
  return(fig)
}

descriptivo_unica_cont <- function(x){
  print(summary(x))
  x_data = data.frame(var = x)
  p <- ggplot(data = x_data, aes(x = var)) + geom_histogram()
  fig <- ggplotly(p)
  return(fig)
}

descriptivo_unica_cont <- function(x){
  print(summary(x))
  x_out <- boxplot(x)$out
  x_data = data.frame(var = x)
  x_data_out <- subset(x_data, var > min(x_out) & var < min(x_out))
  p <- ggplot(data = x_data, aes(x = var)) + geom_histogram()
  p1 <- ggplot(data = x_data_out, aes(x = var)) + geom_histogram()
  fig <- ggplotly(p)
  fig_out <- ggplotly(p1)
  return(list(normal = fig, w_out = fig_out, normal_l = p, normal_w = p1))
}


descriptivo_unica_cont()

# Variables categoricas



# Dividiendo las bases de datos

capital_fisico <- base_hogares %>% select(starts_with(c("P38","P39","P45", "P46" , "P47", "P48", "P49", "P50", "P51", "P52" ,"P60","P61", "P62", "P64" ,"P67", "P68", "P69")))
# create_report(capital_fisico) # Creando el reporte

# Infraesructura para necesidades básicas
# Servicio de transporte
transporte <- data.frame(construccion = capital_fisico$P6901A, contingencia = capital_fisico$P6902A, diferencia = capital_fisico$P6901A - capital_fisico$P6902A)

transporte_grafico <- data_frame(transporte = c(capital_fisico$P6901A, capital_fisico$P6902A),
                                 temporalidad = c(rep("Construccion", length(capital_fisico$P6901A)), rep("Contingencia", length(capital_fisico$P6902A))))

transporte_resumen <- transporte_grafico %>% subset(transporte != 99) %>% group_by(temporalidad, transporte) %>% count()
transporte_cambio <- transporte %>% subset(construccion != 99) %>% subset(contingencia != 99) %>% group_by(diferencia) %>% count()

p <- ggplot(data = transporte_resumen, aes(x = transporte, y = n, fill = temporalidad)) + geom_bar(stat = "identity", position = position_dodge())

fig <- ggplotly(p)
fig

p2 <- ggplot(data = transporte_cambio, aes(x = diferencia, y = n)) + geom_bar(stat = "identity", position = position_dodge())

fig <- ggplotly(p2)
fig

salida <- grafico_antes_despues(capital_fisico$P6901A, capital_fisico$P6902A)
salida$comparativo

# Hectareas

sale <- descriptivo_unica_cont(base_hogares$P41)
sale$normal_l
sale$normal_w
sale$normal
sale$w_out

hist(base_hogares$P41)

boxplot(base_hogares$P41, ylim= c(0, 40))
boxplot(base_hogares$P41, ylim= c(0, 40))$out


# Tenencia de la tierra
# Numero de hogares con menos de 40 Hectareas
which(base_hogares$P41 < 40) %>% length() # Número de hogares

## ### ### ### 
## Medios de vida

reemplazar <- function(x){
  x[which(is.na(x))] <- 0
  return(as.numeric(x))
}

medios_vida <- base_hogares %>% select(starts_with(c("P02"))) %>% .[, 1:10] # Municipio

medios_vida <- medios_vida

for( i in 1:ncol(medios_vida)){
  medios_vida[, i] <- reemplazar(medios_vida[, i])
}

d <- dist.binary(medios_vida)
hcl <- hclust(d)
plot(hcl)

hcl1 <- hclustvar(medios_vida)
plot(hcl1)


medios_vida_completo <- base_hogares %>% select(starts_with(c("P02")))
medios_vida_completo$P02ICUAL %>% table()
medios_vida_completo$P02JCUAL %>% table()
