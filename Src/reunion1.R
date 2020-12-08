# Lectura de los datos
library(ggplot2)
setwd("C:/Users/diant/Documents/deproyectos-Cauca/Bases/Análisis/Src/")
base_personas <- read.csv("../Input/PERSONAS_2020923_V0.csv", sep = ";")
base_hogares <- read.csv("../Input/hogares_prueba.csv", sep = "\t", encoding = "UTF-8")

head(base_personas)
head(base_hogares)

par(mfrow = c(2,1))
# Transporte
plot(as.factor(na.omit(base_hogares$P6901A)), main = "P6901A", ylim = c(0, 2000), col = "red")
# par(new = T)
plot(as.factor(na.omit(base_hogares$P6901A)), main = "P6902A", ylim = c(0, 2000), alpha = 0.3)

# Salud
plot(as.factor(na.omit(base_hogares$P6901A)), main = "P6901B", ylim = c(0, 2000), col = "red")
par(new = T)
plot(as.factor(na.omit(base_hogares$P6901B)), main = "P6902B", ylim = c(0, 2000), alpha = 0.3)

# Educación
plot(as.factor(na.omit(base_hogares$P6901A)), main = "P6901C", ylim = c(0, 2000), col = "red")
# par(new = T)
plot(as.factor(na.omit(base_hogares$P6901B)), main = "P6902C", ylim = c(0, 2000))

# Comercializacion
# Educación
plot(as.factor(na.omit(base_hogares$P6901A)), main = "P6901D", ylim = c(0, 2000), col = "red")
# par(new = T)
plot(as.factor(na.omit(base_hogares$P6901B)), main = "P6902D", ylim = c(0, 2000))

# Seguridad
# Educación
plot(as.factor(na.omit(base_hogares$P6901A)), main = "P6901E", ylim = c(0, 2000), col = "red")
# par(new = T)
plot(as.factor(na.omit(base_hogares$P6901B)), main = "P6902E", ylim = c(0, 2000))

table(as.character(base_hogares$P6801))

# Tipo de destinación vs Distancias

k <-  which(base_hogares$P6801 != 99 & base_hogares$P6801 != 9999)
p <- ggplot(data = base_hogares[k,], aes(x = P6801, fill = as.character(P68A)))
p + geom_histogram(position = "dodge")

