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

setwd("C:/Users/diant/Documents/deproyectos-Cauca/Bases/Análisis/Src")

# Lectura de los datos

df_hogares <- read_excel('../input/Base_Hogares_101020.xlsx', skip = 1)
df_diccionario <- read_excel('../input/Base_Hogares_101020.xlsx')
df_dicc <- data_frame(preguntas = names(df_diccionario), nombre_base = names(df_hogares))
cobertura <- read.csv("../input/cobertura_final.csv", sep = ";", encoding = "UTF-8")

cobertura[, "NUMERO.DE.FORMULARIO"] <- as.character(cobertura[, "NUMERO.DE.FORMULARIO"])
base_cobertura <- left_join(df_hogares, cobertura,  by =c("A00"= "NUMERO.DE.FORMULARIO"))

base_cobertura %>% names()

head(df_hogares)

## Calculando los valores de los metros cuadrados corregidos

areas_cabecera <- subset(base_cobertura, CLASE ==  1)
areas_corregimiento <- subset(base_cobertura, CLASE ==  2)
areas_rural <- subset(base_cobertura, CLASE ==  3)


areas_c <- areas_cabecera$area_m2 %>% na.omit() %>% as.numeric()  %>%  boxplot()
areas_cor <- areas_corregimiento$area_m2 %>% na.omit() %>% as.numeric() %>%  boxplot()
areas_rur <- areas_rural$area_m2 %>% na.omit() %>% as.numeric() %>% boxplot(ylim = c(1, 80000))

areas_c$stats %>% exp()
areas_cor$stats %>% exp()
areas_rur$stats %>% exp()

areas_variables <- select(base_cobertura, c(area_m2, CLASE, valor_m2))

p <- ggplot(areas_variables, aes(as.numeric(area_m2), as.numeric(valor_m2), color = as.character(CLASE))) + geom_point()

plot(base_cobertura$area_m2, base_cobertura$valor_m2)


areas_c$out %>% length()
areas_cor$out %>% length()
areas_rur$out %>% length()


base_cobertura %>% left_join(muestra, by = )

cobertura

# Funciones

transformacion_faltantes <- function(x){
  k <- which(is.na(x))
  k1 <- which(x == 99)
  
  # if(class(x) == "character"){
  #   x[k, ] = "0"
  #   x[k1, ] = NA
  # }
  # else{
  x[k] = 0
  x[k1] = NA
  # }
  return(as.numeric(x))
}

# Capitales

capital_fisico <- df_hogares %>% select(starts_with(c("P38","P39","P45", "P46" , "P47", "P48", "P49", "P50", "P51", "P52", "P53", "P54" ,"P60","P61", "P62", "P64" ,"P67", "P68", "P69", "ocupaciones_diferentes", "Tramo", "Nombre", "n_miembros_hogar")))

medios_vida <- df_hogares %>% select(starts_with("P02")) %>% select(-P02ICUAL , -P02JCUAL) %>% data.frame()
for(i in 1:ncol(medios_vida)){
  medios_vida[, i] <- transformacion_faltantes(medios_vida[, i])
}

medios_vida[, "numero_actividades"] <- apply(medios_vida, 1, sum)
medios_vida_pca <- PCA(medios_vida)





medios_vida_cf<- df_hogares %>% select(starts_with((c("P02")))) %>% cbind(., capital_fisico) # %>% data_frame()

capital_financiero_cf<- df_hogares %>% select(starts_with((c("P02")))) %>% cbind(., capital_financiero) # %>% data_frame()



for(i in 1:ncol(medios_vida_cf)){
  medios_vida_cf[, i] <- transformacion_faltantes(medios_vida_cf[, i])
}



medios_vida_cf <- data_frame(medios_vida_cf)

# medios_vida_acp <- select(medios_vida_cf, -P02JCUAL, -P38A, -P38C7...491, -P38D8...503, -P38E1, -P38E2, -P38E1)
medios_vida_acp <- select(medios_vida, P53, P5301, P54, )

pca_cf <- PCA(medios_vida_acp)


# Medio de vida vs variables de interes

to_grap <- medios_vida_cf %>% group_by(P02A, P53) %>% count()
to_grap[, "perc"] = to_grap[, "n"]/sum(to_grap[, "n"])*100
ggplot(data = to_grap, aes(x = P02A, y = P53, fill = n)) + geom_tile()

#

statsBivariate <- function(data, v1, v2){
  to_grap <- medios_vida_cf %>% group_by(.dots = c(v1, v2)) %>% count()
  to_grap[, "perc"] = to_grap[, "n"]/sum(to_grap[, "n"])*100
  grafico <- ggplot(data = to_grap, aes_string(x = v1, y = v2, fill = "n")) + geom_tile()
  return(list(grafico = grafico, resume = to_grap))
}

statsCont <- function(data, v1, v2){
  to_grap <- medios_vida_cf %>% group_by(.dots = c(v1, v2)) %>% count()
  to_grap[, "perc"] = to_grap[, "n"]/sum(to_grap[, "n"])*100
  grafico <- ggplot(data = to_grap, aes_string(x = v1, y = v2, fill = "n")) + geom_tile()
  return(list(grafico = grafico, resume = to_grap))
}


statsBivariate(medios_vida_cf, "P02B", "P53")


subset(df_dicc, nombre_base == "P53")
medios_vida_cf$P


# Analisis financiero

capitales <- data.frame(ing_agro = transformacion_faltantes(as.numeric(capital_financiero$P53)),
                        ing_monto = transformacion_faltantes(as.numeric(capital_financiero$P5301)),
                        ing_frec = transformacion_faltantes(as.numeric(capital_financiero$P5302)),
                        ing_jor = transformacion_faltantes(as.numeric(capital_financiero$P54)),
                        ing_jor_mon = transformacion_faltantes(as.numeric(capital_financiero$P5401)),
                        ing_jor_frec = transformacion_faltantes(as.numeric(capital_financiero$P5402)),
                        credito = transformacion_faltantes(capital_financiero$P55),
                        cred_prod = transformacion_faltantes(capital_financiero$P5501),
                        cred_cubre = transformacion_faltantes(capital_financiero$P550301))


capital_financiero_medios <- cbind(capitales, medios_vida)

for(i in 1:ncol(medios_vida_acp)){
  medios_vida_acp[, i] = transformacion_faltantes(as.numeric(medios_vida_acp[, i]))
}

capital_financiero_medios_pca <- PCA(capital_financiero_medios)


# Relacion monto de los ingresos vs frecuencia del ingreso
#Agricola
capitales %>% subset( ing_agro == 1) %>% nrow()
df_cf_bi <- capitales %>% subset( ing_agro == 1) %>% group_by(ing_monto, ing_frec) %>% count()
grafico <- ggplot(data = df_cf_bi, aes(x = as.character(ing_monto), y = as.character(ing_frec), fill = n)) + geom_tile()
#Jornales
capitales %>% subset( ing_jor == 1) %>% nrow()
df_cf_bi_j <- capitales %>% subset( ing_jor == 1) %>% group_by(ing_jor_mon, ing_jor_frec) %>% count()
grafico <- ggplot(data = df_cf_bi_j, aes(x = as.character(ing_jor_mon), y = as.character(ing_jor_frec), fill = n)) + geom_tile()
# Acceso a créditos
capitales %>% subset( credito == 1) %>% nrow()
df_cf_bi_j <- capitales %>% subset( credito == 1) %>% group_by(cred_prod, cred_cubre) %>% count()
grafico <- ggplot(data = df_cf_bi_j, aes(x = cred_prod, y = cred_cubre, fill = n)) + geom_tile()
# Donaciones


# statsBivariate(df_cf_bi, "ing_monto", "inf_frec")


capital_financiero_acp <- data.frame(ing_agro = abs(as.numeric(capital_financiero$P53) - 2 ), monto_agro = as.numeric(capital_financiero$P5301))


################ Analisis Capital financiero ################

capital_financiero <- df_hogares %>% select(starts_with(c("P53", "P5301", "P54", "P55", "P5501", "P55", "P550301", "P56", "P5601", "P57", "P5701", "P58", "P5804", "P580401", "Tramo", "Nombre")))

names(capital_financiero)
preg_cap_fin <- df_dicc %>% subset(nombre_base %in% names(capital_financiero))

var_financiero <- names(capital_financiero)[c(1, 2, 5, 6, 10, 11, 12, 28, 30, 31:34, 52, 57, 58)]

df_capital_financiero <- capital_financiero[, names(capital_financiero) %in% var_financiero] %>% as.data.frame()

for(i in 1:ncol(df_capital_financiero)){
  df_capital_financiero[, i] <- as.factor(transformacion_faltantes(df_capital_financiero[, i]))
}

acm<-dudi.acm(df_capital_financiero,nf=10,scannf = FALSE)
fviz_mca_var(acm,repel=T)+labs(title ="Nube de puntos de las variables del capital físico")

fviz_contrib(acm,axes=c(1,2),choice = "var" )+labs(title = "Contribución de variables en el primer plano factorial")

acm$rank
acm$co

fviz_cos2(acm,axes=c(1,2),choice = "var" )+labs(title = "Cos2 de las variables  en el primer plano factorial")


df_capital_financiero["Tramo"] = as.factor(df_hogares$Nombre)

acmsup <- MCA(df_capital_financiero,quali.sup = 17,ncp=2,graph = TRUE)

coor_cat<-acmsup$quali.sup$coord
coor_cat

