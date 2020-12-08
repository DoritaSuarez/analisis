library(dplyr)
library(readxl)

setwd("C:/Users/diant/Documents/deproyectos-Cauca/Bases/Análisis/Src")

# Funciones  --------------------------------------------------------------

# Los valores NA en este caso corresponden a ceros y los 99, corresponden a los NA -___-

transformacion_faltantes <- function(x){
  k <- which(is.na(x))
  k1 <- which(x == 99)
  x[k] = 0
  x[k1] = NA
  return(as.numeric(x))
}
# Lectura de los datos ----------------------------------------------------


df_hogares_orig <- read_excel('../input/Base_Hogares_101020.xlsx', skip = 1)
df_diccionario <- read_excel('../input/Base_Hogares_101020.xlsx')
df_dicc <- data_frame(preguntas = names(df_diccionario), nombre_base = names(df_hogares_orig))
cobertura <- read_excel("../Input/cobertura_final.xlsx")
# cobertura <- read.csv("../input/cobertura_final.csv", sep = ";", encoding = "UTF-8")

cobertura[, "NUMERO DE FORMULARIO"] <- as.character(cobertura$`NUMERO DE FORMULARIO`)

#################################################
# Id´s que no son únicos, corresponden a lugares donde se encuestaron dos hogares en la misma ubicación geográfica

res <- df_hogares_orig$A00 %>% table()
noUnique <- res[res>1] %>% names()
ids <- df_hogares_orig[df_hogares_orig$A00 %in% noUnique, ]$A00
for(i in noUnique){
  ids[ids %in% i] <- paste0(ids[ids %in% i], c("A", "B"))
}

df_hogares_orig[df_hogares_orig$A00 %in% noUnique, ]$A00 <- ids

## Corrección de algunos ids de la base de cobertura
cruce_ids <- cobertura$`NUMERO DE FORMULARIO` %in% df_hogares_orig$A00
cobertura$`NUMERO DE FORMULARIO`[which(!cruce_ids)] %>% table()

cobertura$`NUMERO DE FORMULARIO`[which(cobertura$`NUMERO DE FORMULARIO` == "15006")] <- "150061"
cobertura$`NUMERO DE FORMULARIO`[which(cobertura$`NUMERO DE FORMULARIO` == "2020811")] <- "202081"
cobertura$`NUMERO DE FORMULARIO`[which(cobertura$`NUMERO DE FORMULARIO` == "21214")] <- "212141"
cobertura$`NUMERO DE FORMULARIO`[which(cobertura$`NUMERO DE FORMULARIO` == "25310")] <- "25310A"
cobertura$`NUMERO DE FORMULARIO`[which(cobertura$`NUMERO DE FORMULARIO` == "253102")] <- "25310B"
cobertura$`NUMERO DE FORMULARIO`[which(cobertura$`NUMERO DE FORMULARIO` == "34601")] <- "34601A"
cobertura$`NUMERO DE FORMULARIO`[which(cobertura$`NUMERO DE FORMULARIO` == "346012")] <- "34601B"
cobertura$`NUMERO DE FORMULARIO`[which(cobertura$`NUMERO DE FORMULARIO` == "35106")] <- "351061"
cobertura$`NUMERO DE FORMULARIO`[which(cobertura$`NUMERO DE FORMULARIO` == "351071")] <- "35107"
cobertura$`NUMERO DE FORMULARIO`[which(cobertura$`NUMERO DE FORMULARIO` == "35803")] <- "358031"
cobertura$`NUMERO DE FORMULARIO`[which(cobertura$`NUMERO DE FORMULARIO` == "35806")] <- "358061"
cobertura$`NUMERO DE FORMULARIO`[which(cobertura$`NUMERO DE FORMULARIO` == "38810")] <- "388101"
cobertura$`NUMERO DE FORMULARIO`[which(cobertura$`NUMERO DE FORMULARIO` == "38810")] <- "388101"
cobertura$`NUMERO DE FORMULARIO`[which(cobertura$`NUMERO DE FORMULARIO` == "30201")] <- "30206"
cobertura$`NUMERO DE FORMULARIO`[which(cobertura$`NUMERO DE FORMULARIO` == "28011")][2] <- "28012"
cobertura$`NUMERO DE FORMULARIO`[which(cobertura$`NUMERO DE FORMULARIO` == "24809")] <- "24909"


cruce_ids <- cobertura$`NUMERO DE FORMULARIO` %in% df_hogares_orig$A00
nounicos_cob <- cobertura$`NUMERO DE FORMULARIO`[which(!cruce_ids)] %>% table() %>% names()

ids_cob <- cobertura[cobertura$`NUMERO DE FORMULARIO` %in% nounicos_cob, ]$`NUMERO DE FORMULARIO`
for(i in (ids_cob %>% table() %>% names())){
  ids_cob[ids_cob %in% i] <- paste0(ids_cob[ids_cob %in% i], c("A", "B"))
}


cobertura[cobertura$`NUMERO DE FORMULARIO` %in% nounicos_cob, ]$`NUMERO DE FORMULARIO` <- ids_cob

cruce_ids <- cobertura$`NUMERO DE FORMULARIO` %in% df_hogares_orig$A00
cruce_ids[!cruce_ids]

# corroborando que todos los formularios hayan cruzado correctamente
cobertura %>% 
  group_by(`NUMERO DE FORMULARIO`) %>%
  count() %>% 
  filter(n>1)

df_hogares_orig %>%
  group_by(A00) %>%
  count() %>% 
  filter(!(A00 %in% cobertura$`NUMERO DE FORMULARIO`))

cobertura %>%
  group_by(`NUMERO DE FORMULARIO`) %>%
  count() %>% 
  filter(!(`NUMERO DE FORMULARIO` %in% df_hogares_orig$A00))

distinct(cobertura[, "NUMERO DE FORMULARIO"]) %>% dim()
# dd <- cobertura[, "NUMERO DE FORMULARIO"] %>% unique() %>% table()

# Cruzando las dos bases de datos
base_cobertura <- left_join(df_hogares_orig, cobertura,  by = c("A00"= "NUMERO DE FORMULARIO"))
df_hogares <- base_cobertura
df_hogares %>% dim()

if(dim(df_hogares)[1] == dim(df_hogares_orig)[1]){
  print("La base de datos de coberturas fue cargada correctamente")
}
