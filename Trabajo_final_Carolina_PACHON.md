---
title: "Trabajo_final_CursoR_2023_I"
author: "Carolina Pachón"
date: "2023-09-29"
output: html_document
---
# Directorio de trabajo

```{r}
setwd("C:/CAROLINA/trabajos CAROLINA/CursoR_2023/Trabajo_final/Trabajo_final_Carolina")
getwd()
```
# Paquetes instalados
```{r}
install.packages("tidyverse", dependencies = TRUE)
library("tidyverse")
dplyr::filter()
install.packages("modeest")
library("modeest")
```

#Cargar los datos (leer la tabla)

```{r}
datos_iris <- read_delim("Tabla_especies_NA.csv",delim = ";", col_types = cols(.default = col_number(),
                                                                  Sepal.Length = col_number(),
                                                                  Sepal.Width = col_number(),
                                                                  Petal.Length = col_number(),
                                                                  Petal.Width = col_number(),
                                                                  Species = col_character()),
                        na = c("","NA"))
datos_iris <- data.frame(datos_iris)
datos_iris
```

##Descripción general de los datos

```{r}
#Descripción general
str(datos_iris)
```

```{r}
#Número de datos
nrow(datos_iris)
count(datos_iris)

```

```{r}
# Nombres de las columnas
names(datos_iris)
```

```{r}
#Tipo de datos por columna
class(datos_iris$Sepal.Length)

```

```{r}
# Tipo de datos
typeof(datos_iris)

```

```{r}
#Tamaño de la lista
length(datos_iris)

```

```{r}
#Parte superior de los datos
head(datos_iris)

```

```{r}
#Parte final de los datos
tail(datos_iris)

```

```{r}
#¿Cuántos individuos de cada especie se midieron?
datos_iris %>%
  count(Species)
```

```{r}
#¿Cuántos individuos tienen un ancho de pétalo superior a 1.0?
datos_iris %>%
  subset(Petal.Width > 1.0) %>%
  count()
```

## Pregunta para analizar los datos

### Modificar la tabla

1. ¿Cuál es la relación entre el ancho y el largo del sépalo?
```{r}
datos_iris$Rel_Sepal <- datos_iris$Sepal.Width / datos_iris$Sepal.Length
datos_iris
```

2. ¿Cuál es la relación entre el ancho y largo de los pétalos?
```{r}
datos_iris$Rel_Petal <- datos_iris$Petal.Width / datos_iris$Petal.Length
datos_iris
```

### Preguntas estadísticas

1. ¿Cuál es el valor medio de la longitud del sépalo para cada una de las 3 especies?
```{r}
datos_iris %>%
  group_by(Species)%>%
  summarise(mean_sepal = mean(Sepal.Length, na.rm = T))

```
1.a. Visualización gráfica de las medias

```{r}
Means <- ggplot(data = datos_iris,
                aes(x = Species, y = Sepal.Length, fill = Species)) + 
  geom_boxplot()
Means + labs (title = "Boxplot",
             x= "Especies",
             y= "Longitud sépalo (cm)",
             col = "Especies")
```

2. ¿setosa y virginica tienen medias iguales para la variable ancho del pétalo?
```{r}
# Primero crear los subgrupos de "setosa" y "virginica" para la variable ancho del pétalo

setosa_petal_w <- na.omit(subset(datos_iris, datos_iris$Species == "setosa")$Petal.Width)
virginica_petal_w <- na.omit(subset(datos_iris, datos_iris$Species == "virginica")$Petal.Width)

#Segundo relizar una prueba T-student de comparación de medias, asumiendo normalidad y homocedasticidad de los datos

ttest_setosa_veir <- t.test(setosa_petal_w,virginica_petal_w,equal=T,conf.level = 1-0.05, alternative = "two.sided")
ttest_setosa_veir

#La medias de las dos especies para la variable ancho del pétalo No son iguales con un valor P < 0.05.
```
2.a. Observación gráfica del t.test

```{r}
ttest_plot <- boxplot(setosa_petal_w, virginica_petal_w, 
                      names = c("I. setosa", "I. virginica"), 
                      main = "T de Student",
                      col = c("yellow", "orange"))
ttest_plot
mtext(paste("p-value: ", signif(ttest_setosa_veir$p.value, digits = 3)), 
      side = 3, line = -1, cex = 0.8)

```

3. ¿Existe una relación lineal entre el ancho y el largo del pétalo?
```{r}
lineal_reg <- lm(datos_iris$Petal.Width~datos_iris$Petal.Length, data = datos_iris)
summary(lineal_reg)

```
3.a. Visulización gráfica de la regresión

```{r}
lineal_reg_petal <- plot(datos_iris$Petal.Length, datos_iris$Petal.Width, 
                         main = "Regresión Lineal", 
                         xlab = "Longitud Petalo (cm)", ylab = "Ancho Petalo (cm)", 
                         col = factor(datos_iris$Species),
                         pch = 15)
abline(lineal_reg, col = "red")
legend("topleft",
       pch = 23,
       legend = levels(factor(datos_iris$Species)),
       col=factor(levels(factor(datos_iris$Species))))

```


