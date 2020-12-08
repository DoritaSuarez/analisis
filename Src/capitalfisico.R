# Analisis del capital físico

# Infraestructura - Necesidades básicas

df_infraes_constr <- df_hogares %>% select(starts_with("P6901"))
df_infraes_contin <- df_hogares %>% select(starts_with("P6902"))

for(i in 1:ncol(df_infraes_constr)){
  df_infraes_constr[, i] <- transformacion_faltantes(df_infraes_constr[, i] %>% unlist() %>% as.numeric())
  df_infraes_contin[, i] <- transformacion_faltantes(df_infraes_contin[, i] %>% unlist() %>% as.numeric())
}

# Etapa de construcción vs estapa de contingencia

names(df_infraes_constr) <- c("Transporte",
                       "Salud",
                       "Educación",
                       "Comercialización",
                       "Seguridad",
                       "Relaciones_comunitarias")

names(df_infraes_contin) <- c("Transporte",
                       "Salud",
                       "Educación",
                       "Comercialización",
                       "Seguridad",
                       "Relaciones_comunitarias")


df_infraes_constr[, "Etapa"] <- "Construcción"

df_infraes_contin[, "Etapa"] <- "Contingencia"

df_infraes <- rbind(df_infraes_constr, df_infraes_contin)

df_infraes_graph <- melt(df_infraes) %>% data_frame()


infraes_grap <- ggplot(df_infraes_graph, aes(value, fill = Etapa))
infraes_grap + geom_bar(position = "dodge")+ facet_wrap(. ~ variable, ncol = 3) + xlab("Infraestructura") + ylab("Número de hogares") +
  scale_fill_manual("Etapa", values = c("Construcción" = "#8CBD0E", "Contingencia" = "#005117"))
autoc_df_graf_temp


# Diferencia entre las puntuaciones

infra_diferencia <- df_infraes_constr[, -7]

for(i in 1:ncol(infra_diferencia)){
  infra_diferencia[, i] <-  df_infraes_constr[, i] - df_infraes_contin[, i]
}

infra_diferencia[, "Clase"] <- df_hogares$CLASE
infra_diferencia_grap <- melt(infra_diferencia, id.vars = "Clase") %>% data_frame()
infra_diferencia_grap$Clase <- as.factor(infra_diferencia_grap$Clase)
levels(infra_diferencia_grap$Clase) <- c("Cabecera", "Corregimiento", "Rural")


dif_grap <- ggplot(infra_diferencia_grap, aes(x = value, fill = Clase))
dif_grap + geom_bar(position = "dodge")+ facet_wrap(. ~ variable, ncol = 3) +
  xlab("Diferencia entre la etapa de construcción y contingencia") + ylab("Número de hogares")

t.test(infra_diferencia_grap$value)

# Infraestructura vial

df_inf_vial <- df_hogares %>% select("P68A", "P68B","P68C", "P68D", "P68E", "P6801", "P6802")


# Distancia a donde se sacan los productos

for(i in 1:ncol(df_inf_vial)){
  df_inf_vial[, i] <- transformacion_faltantes(df_inf_vial[, i] %>% unlist() %>% as.numeric())
}

names(df_inf_vial) <- c("Predio", "Mercados_locales", "Mercados_regionales", "Mercados_nacionales", "Intermediarios", "Distancia", "Costo")

df_inf_donde_venta <- df_inf_vial[, 1:5]
df_inf_donde_venta[, "Tramo"] <- df_hogares$Nombre

df_inf_donde_venta <- melt(df_inf_donde_venta, id.vars = "Tramo") %>% data_frame()
df_inf_donde_venta$value <- as.factor(df_inf_donde_venta$value)
levels(df_inf_donde_venta$value) <- c("No", "Si")


resumen_ventas <- df_inf_donde_venta %>% group_by(variable, value) %>% 
  summarise(conteo = n()) %>% 
  mutate(porcentaje = conteo/sum(conteo)) %>% 
  arrange(porcentaje, variable)

names(resumen_ventas)[2] <- "Ventas"

resumen_ventas %>% ggplot(aes(x=reorder(variable, desc(porcentaje)), y = porcentaje, fill = Ventas))+geom_col()


resumen_ventas <- resumen_ventas %>% arrange(porcentaje, Ventas)


resumen_ventas["pos"] <- NA
for(i in 1:nrow(resumen_ventas)){
  x = resumen_ventas[i, "porcentaje"]
  act = resumen_ventas[i, "Ventas"]
  # resumen_ventas[i, "pos"] = ifelse(act == "Si", x*100/2, (1-x+x/2)*100)
}


act_g1 <- ggplot(resumen_ventas, aes(x = variable, y = porcentaje*100, fill = Ventas))
act_g1 + geom_bar(stat = "identity") +
  xlab("¿Dónde vende los productos?") +
  ylab("Porcentaje") +
  scale_fill_manual("Ventas", values = c("No" = "#8CBD0E", "Si" = "#005117")) + coord_flip()


# Distancia y precio

summary(df_inf_vial$Costo)


boxplot(df_inf_vial$Costo, ylim = c(0, 100000))
boxplot(df_inf_vial$Distancia, ylim = c(0, 8000))
