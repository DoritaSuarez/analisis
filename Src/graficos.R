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

# createBarPlot(df_gastos, "gastos_mes_hogar")

create_barplot_tramo <- function(df_datos, var_plot, var_cat="tramo", xlab, ylab) {
  df_datos %>% 
    as_tibble() %>% 
    filter(!is.na(!!sym(var_plot))) %>% 
    ggplot(aes(x=!!sym(var_plot), fill=!!sym(var_cat)))+
    geom_bar(col = "#005117", position = "dodge")+
    geom_text(aes(label = paste0(round(..count..*100/length(df_datos[[var_plot]]),digits=1),"%")), stat = "count", vjust = 0.5,hjust = -0.4, colour = "black",position =  position_dodge(.9), size=2.7)+  
    labs(x=xlab, y=ylab)+
    # scale_y_continuous(limits = c(0,2100))+
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
}


create_barplot_tramo_hor <- function(df_datos, var_plot, var_cat="tramo", xlab, ylab) {
  df_datos %>% 
    as_tibble() %>% 
    filter(!is.na(!!sym(var_plot))) %>% 
    ggplot(aes(x=!!sym(var_plot), fill=!!sym(var_cat)))+
    geom_bar(col = "#005117", position = "dodge")+
    geom_text(aes(label = paste0(round(..count..*100/length(df_datos[[var_plot]]),digits=1),"%")), stat = "count", vjust = -1,hjust = 0.5, colour = "black",position =  position_dodge(.9), size=2.7)+  
    labs(x=xlab, y=ylab)+
    # scale_y_continuous(limits = c(0,2100))+
    # coord_flip()+
    scale_fill_manual(values=c("#005117", "#8CBD0E", "#5ABCB9"), 
                      name="Tramo")+
    theme(panel.background = element_rect(fill = "gray97",
                                          colour = "gray97",
                                          size = 0.5, linetype = "solid"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                          colour = "white"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "white"))
}

# create_barplot_tramo(df_gastos, "gastos_mes_hogar", xlab = "Gastos mensuales del hogar", ylab = "NÃºmero de hogares")


# Nube de palabras

wordcloud_graph <- function(variable, maxWords = 200){
  
  docs <- Corpus(VectorSource(c(variable) %>% na.omit() ))
  docs <- tm_map(docs, content_transformer(tolower))
  docs <- tm_map(docs, removeWords, stopwords("spanish"))
  
  dtm <- TermDocumentMatrix(docs)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  
  d$word <- gsub(",", "", d$word)
  # d$word <- gsub(".", "", d$word)
  
  set.seed(1234)
  wordcloud(words = d$word, freq = d$freq, min.freq = 1,
            max.words=maxWords, random.order=TRUE, rot.per=0.8, 
            colors=brewer.pal(8, "Dark2"))
  
}

# wordcloud_graph(df_inf_prod$Herramientas_dificiles, 200)


# Este es para reemplazar

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
  
  
  act_g1 <- ggplot(resumen_df_graph, aes(x = reorder(variable, -porcentaje), y = porcentaje*100, fill =!!sym(cat)))+
    geom_bar(stat = "identity") +
    xlab(xlab_grap) +
    ylab("Porcentaje") +
    scale_fill_manual(cat, values = c("No" = "#8CBD0E", "Si" = "#005117")) + coord_flip()
  
  return( list(resumen = resumen_df_graph, graph = act_g1))
}



