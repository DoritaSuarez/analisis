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
                                             "Entre $100.000\ny $200.000",
                                             "Entre $200.000\ny $500.000",
                                             "Entre $500.000\ny $1'000.000",
                                             "Entre $1'000.000\ny $2'000.000",
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
  dplyr::filter(!is.na(get("gastos_mes_hogar"))) %>% 
  ggplot(aes(gastos_mes_hogar, fill=tramo))+
  geom_bar(col = "#005117", position = "dodge")+
  geom_text( aes(label = paste0(round(..count..*100/length(df_gastos$gastos_mes_hogar),digits=1),"%")), stat = "count", vjust = 0.5,hjust = -0.4, colour = "black",position =  position_dodge(.9), size=2.7)+  
  labs(x="Gastos mensuales del hogar", y="Número de hogares")+
  scale_y_continuous(limits = c(0,2100))+
  coord_flip()+
  scale_fill_manual(values=c("#005117", "#8CBD0E", "#5ABCB9"), 
                                     name="Tramo")+
  theme(panel.background = element_rect(fill = "gray97",
                                        colour = "gray97",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "white"))

createBarPlot <- function( df_data, var_name)
{
  library(dplyr)
  library(ggplot2)
  plot <- df_data %>% 
    as_tibble() %>% 
    filter_(!is.na(df_data[var_name])) %>% 
    ggplot(aes_(as.name(var_name), fill=as.name("tramo")))+
    geom_bar(col = "#005117", position = "dodge")+
    geom_text( aes(label = paste0(round(..count..*100/length(as.name(var_name)),digits=1),"%")), stat = "count", vjust = 0.5,hjust = -0.4, colour = "black",position =  position_dodge(.9), size=2.7)+  
    labs(x="Gastos mensuales del hogar", y="Número de hogares")+
    scale_y_continuous(limits = c(0,2100))+
    coord_flip()+
    scale_fill_manual(values=c("#005117", "#8CBD0E", "#5ABCB9"), 
                      name="Tramo")+
    theme(panel.background = element_rect(fill = "gray97",
                                          colour = "gray97",
                                          size = 0.5, linetype = "solid"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                          colour = "white"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "white"))
  return(plot)
}

createBarPlot(df_gastos, "gastos_mes_hogar")
  
Cstack_info()

create_barplot_categories <- function(df_datos, var_plot, var_cat, var_cat_name, xlab, ylab) {
  df_datos %>% 
    as_tibble() %>% 
    filter(!is.na(!!sym(var_plot))) %>% 
    ggplot(aes(x=!!sym(var_plot), fill=!!sym(var_cat)))+
      geom_bar(col = "#005117", position = "dodge")+
      geom_text(aes(label = paste0(round(..count..*100/length(df_datos[[var_plot]]),digits=1),"%")), stat = "count", vjust = 0.5,hjust = -0.4, colour = "black",position =  position_dodge(.9), size=2.7)+  
      labs(x=xlab, y=ylab)+
      scale_y_continuous(limits = c(0,2100))+
      coord_flip()+
      scale_fill_manual(values=c("#005117", "#8CBD0E", "#5ABCB9"), 
                      name=var_cat_name)+
      theme(panel.background = element_rect(fill = "gray97",
                                          colour = "gray97",
                                          size = 0.5, linetype = "solid"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                          colour = "white"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "white"))
}

create_barplot_categories(df_gastos, "gastos_mes_hogar", "tramo", "Tramo", xlab = "Gastos mensuales del hogar", ylab = "Número de hogares")



# Perdidas ----------------------------------------------------------------

  
df_perdidas <- df_hogares %>% select(
  c(
    CONSECUTIVO:NOMBRES, 
    starts_with(c("P06","P07"))
  )
)

df_perdidas %>% head() %>% View()


# Cuotas creditos de actividades productivas ------------------------------


df_cuotas <- df_hogares %>% select(
  c(
    CONSECUTIVO:NOMBRES, 
    starts_with(c("P55"))
  )
)

df_cuotas %>% head() %>% View()

df_cuotas %>% 
  mutate(P55 = factor(P55, labels=c("Si",'No'))) %>% 
  ggplot(aes(P55))+
    geom_bar(col = "#005117")

table(df_cuotas$P55) %>% prop.table()

## Interpretación: Aproximadamente el 82% de los hogares encuestados no tienen acceso a créditos.

df_cuotas %>% 
  filter(!is.na(P5501)) %>% 
  mutate(P5501 = factor(P5501, labels=c("Si",'No'))) %>% 
  ggplot(aes(P5501))+
  geom_bar(col = "#005117")

table(df_cuotas$P5501) %>% prop.table()

## Del 18% que sí tiene acceso a créditos, actualmente el 70% hace uso de algún crédito para desempeñar su actividad productiva


graficos_binarios <- function(df_graph, nombres_gra, tramo = T, cat, xlab_grap, ylab_grap, df_orig){
  names(df_graph) <- nombres_gra
  
  for(i in 1:ncol(df_graph)){
    df_graph[, i] <- transformacion_faltantes(df_graph[, i] %>% unlist() %>% as.numeric())
  }
  
  df_graph[, "Tramo"] <- df_orig[, "Tramo"]
  df_graph <- df_graph %>% melt("Tramo") %>% data_frame()
  df_graph$value <- as.factor(df_graph$value)
  levels(df_graph$value) <- c("No", "Si")
  
  resumen_df_graph <- df_graph %>% group_by(variable, value) %>% 
    summarise(conteo = n()) %>% 
    mutate(porcentaje = conteo/sum(conteo)) %>% 
    arrange(porcentaje, variable)
  
  resumen_df_graph <- na.omit(resumen_df_graph)
  
  names(resumen_df_graph)[2] <- cat
  
  
  act_g1 <- ggplot(resumen_df_graph, aes(x = variable, y = porcentaje*100, fill =!!sym(cat)))
  act_g1 + geom_bar(stat = "identity") +
    xlab(xlab_grap) +
    ylab("Porcentaje") +
    scale_fill_manual(cat, values = c("No" = "#8CBD0E", "Si" = "#005117")) + coord_flip()
}


table(df_cuotas$P5501)[[1]]

df_cuotas %>% 
  select(c("P550101A","P550101B","P550101C","P550101D")) %>% 
  `colnames<-`(c("Banco privado", "Estado", "Persona natural", "Almacen")) %>% 
  mutate_all(as.integer) %>% 
  summarise_all(sum, na.rm = TRUE) %>% 
  pivot_longer(
    cols = everything(),
    names_to = 'acreedor',
    names_prefix = "acre",
    values_to = "num_deudores",
    values_drop_na = TRUE
  ) %>% 
  mutate(
    porcentaje=num_deudores/table(df_cuotas$P5501)[[1]]
  ) %>% 
  ggplot(aes(x=reorder(acreedor, -porcentaje), y=porcentaje))+
    geom_bar( stat = "identity", position = "fill", fill = "#8CBD0E")+
    geom_bar( stat = "identity", fill='#005117')+
    geom_text(aes(label = round(porcentaje,2)), stat = "identity", vjust = 0.5,hjust = 2, colour = "white",position =  position_dodge(.9), size=2.7)+  
    labs(x='Acreedor', y='Porcentaje')+
    scale_y_continuous(labels = scales::percent_format())+
    coord_flip()



create_barplot_binaries <- function(df_datos, var_plot, var_cat, var_cat_name, xlab, ylab) {
    
}
