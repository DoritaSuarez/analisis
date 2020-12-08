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

## Calculo de los ingresos

df_hogares %>% names()
df_hogares$P5301

#ingresos generados en la producción propia Agropecuaria

# Ingresos P5301 ----------------------------------------------------------

df_hogares[, "IngresosPropios"] <- transformacion_faltantes(df_hogares$P53)
df_hogares[, "IngresosPropios"] <- ifelse(df_hogares$IngresosPropios == 1, 1, 0)

p <- ggplot(data = df_hogares, aes(as.factor(IngresosPropios)))+
  geom_bar()+xlab("Ingresos propios por actividades agropecuarias") +  ylab("Frecuencia")
p

table(df_hogares$IngresosPropios)

df_hogares[, "Monto"] <- ifelse(df_hogares$P5301 == 1, 1, 0)

monto_generado <- df_hogares %>% 
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
      P5301 == 5 ~ 3000000,
      TRUE ~ NA_real_
    ),
    medio_escenario = case_when(
      is.na(P5301) ~ NA_real_,
      P5301 == "1" ~ 125000,
      P5301 == "2" ~ 275000,
      P5301 == "3" ~ 750000,
      P5301 == "4" ~ 1500000,
      P5301 == "5" ~ 2000000,
      TRUE ~ NA_real_
    )
  )

base_areas_ingreso <- cbind(df_hogares, monto_generado[, -1]) %>% data_frame()


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
      TRUE ~ NA_real_
    )
  )

ingreso_mensual[, "IngresosPropios"] <- df_hogares[, "IngresosPropios"]

## SOLO DEL GRÁFICO

pe = subset(ingreso_mensual, IngresosPropios == 1)$ingresos_mensuales_pe
me = subset(ingreso_mensual, IngresosPropios == 1)$ingresos_mensuales_me
med = subset(ingreso_mensual, IngresosPropios == 1)$ingresos_mensuales_med

Escenarios_plot <- data.frame(escenario = c(rep("Peor", length(pe)),
                                            rep("Mejor", length(me)),
                                            rep("Medio")),
                              ingresos = c(pe, me))

p <- ggplot(Escenarios_plot, aes(ingresos, fill = escenario))
p + geom_boxplot()

p <- ggplot(ingreso_mensual, aes(ingresos_mensuales_med, fill = escenario))
p + geom_histogram(alpha = 0.6)



p1 <- ggplot(Escenarios_plot, aes(x = escenario, y = ingresos, fill = escenario))
p1 + geom_boxplot(alpha = 0.6)

summary(pe)
summary(me)



# JORNALES ----------------------------------------------------------------

df_hogares[, "IngresosJornales"] <- transformacion_faltantes(df_hogares$P54)
df_hogares[, "IngresosJornales"] <- ifelse(df_hogares$IngresosJornales == 1, 1, 0)

p <- ggplot(data = df_hogares, aes(as.factor(IngresosJornales)))+
  geom_bar()+xlab("Ingresos por jornales") +  ylab("Frecuencia")
p

table(df_hogares$IngresosJornales)


monto_generado_jornales <- df_hogares %>% 
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

base_jornales_ingreso <- cbind(df_hogares, monto_generado[, -1]) %>% data_frame()


ingreso_mensual_jornales <- base_jornales_ingreso %>% 
  select(P5401, P5402, 
         P5402OTHER,
         peor_escenario, 
         mejor_escenario,
         medio_escenario
  ) %>% 
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
      TRUE ~ NA_real_
    )
  )

# SOLO GRAFICO

ingreso_mensual_jornales[, "IngresosJornales"] <- df_hogares[, "IngresosJornales"]

pe = subset(ingreso_mensual_jornales, IngresosJornales == 1)$ingresos_mensuales_pe
me = subset(ingreso_mensual_jornales, IngresosJornales == 1)$ingresos_mensuales_me

Escenarios_plot <- data.frame(escenario = c(rep("Peor", length(pe)), rep("Mejor", length(me))),
                              ingresos = c(pe, me))

p <- ggplot(Escenarios_plot, aes(ingresos, fill = escenario))
p + geom_histogram(alpha = 0.6) + xlim(0, 4000000)


p1 <- ggplot(Escenarios_plot, aes(x = escenario, y = ingresos, fill = escenario))
p1 + geom_boxplot(alpha = 0.6)+xlab("Ingresos por jornales") + ylab("Número de hogares")+ylim(0,1200000)

summary(pe)
summary(me)

length(pe)

# DONACIONES ----------------------------------------------------------------

df_hogares[, "IngresosDonaciones"] <- transformacion_faltantes(df_hogares$P56)
df_hogares[, "IngresosDonaciones"] <- ifelse(df_hogares$IngresosDonaciones == 1, 1, 0)

p <- ggplot(data = df_hogares, aes(as.factor(IngresosDonaciones)))+
  geom_bar()+xlab("Ingresos por donaciones") +  ylab("Frecuencia")
p

table(df_hogares$IngresosDonaciones)

# Origen de las donaciones
n = nrow(df_hogares)
donacionesTipo <- c(rep("Apoyo familiar", n),
                    rep("Apoyo a la producción",  n),
                    rep("Apoyo a la educación", n),
                    rep("Apoyo a la salud", n),
                    rep("Otro", n))
donacionesTiene <- c(transformacion_faltantes(df_hogares$P5601A),
                     transformacion_faltantes(df_hogares$P5601B),
                     transformacion_faltantes(df_hogares$P5601C),
                     transformacion_faltantes(df_hogares$P5601D),
                     transformacion_faltantes(df_hogares$P5601OTHER))


donaciones <- data.frame(tipo_donacion = donacionesTipo,
                         donacion = as.factor(donacionesTiene))

pd <- ggplot(donaciones, aes(tipo_donacion, fill = donacion)) + geom_bar() + xlab("Tipo de donación") + ylab("Proporcion - Absoluto")
pd

montoDonaciones <- as.numeric(df_hogares$P5605A)/as.numeric(df_hogares$P5606)
IngresoDonaciones <- data.frame(monto = ifelse(montoDonaciones == 99, NA, montoDonaciones),Tramo = df_hogares$Nombre)

p <- ggplot(IngresoDonaciones, aes(Tramo, monto, fill = Tramo )) + geom_boxplot() +
  ylim(0, 2000000)
p



# REMESAS -----------------------------------------------------------------


df_hogares[, "IngresosDonaciones"] <- transformacion_faltantes(df_hogares$P57)
df_hogares[, "IngresosDonaciones"] <- ifelse(df_hogares$IngresosDonaciones == 1, 1, 0)

p <- ggplot(data = df_hogares, aes(as.factor(IngresosDonaciones)))+
  geom_bar()+xlab("Ingresos por donaciones") +  ylab("Frecuencia")
p

table(df_hogares$IngresosDonaciones)



# Ingresos Total ---------------------------------------------------------

ingresos = ingreso_mensual$medio_escenario + ingreso_mensual_jornales$medio_escenario + IngresoDonaciones$monto + ingresosRemesas$monto

df_hogares$n_miembros_hogar

educacion = df_hogares$

boxplot(ingresos)


## Relación de nivel educativo (capital humano Pr. 17.8) con área de terreno (capital físico pregunta 41) con ingresos (capital financiero Preguntas 53.1 producción + 54.1 Jornales +56.5 Donaciones +57.3 remesas + porcentaje de monto 53.1 destinado a autoconsumo en la pregunta 4.b) por zonas

