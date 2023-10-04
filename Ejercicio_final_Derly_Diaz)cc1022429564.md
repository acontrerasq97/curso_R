---
title: "Ejercicio_final"
author: "Derly Diaz"
date: "2023-09-28"
output:
  html_document:
    df_print: paged
  pdf_document: default
  word_document: default
---
```{r}
library(tidyverse)
setwd("C:/Users/derly/OneDrive - Universidad Nacional de Colombia/Documents/CursoR_2023")
read.csv2("Ejercicio final/Tabla_especies_NA.csv")

```
```{r}
flores <- read_delim("Tabla_especies_NA.csv",
                     col_types = list(Sepal.Length = col_double(),
                                     Sepal.Width = col_double(),
                                     Petal.Length = col_double(),
                                     Petal.Width = col_double()))
class(flores)
flores
class(flores$Sepal.Length)
class(flores$Petal.Length)
flores
flores %>%
  count(Species) 
  
```
La relación entre el largo y el ancho tanto de flor como de sepalo diferencian la especie?

```{r}
#Cuanto datos faltantes tengo por columna?
missings <- function(x) return(sum(is.na(x)))
apply(flores,2,missings)
#Crear dos columnas nuevas con la relacion largo/ancho tanto de sepalo como de petalo
flores_operacion <- flores %>%
  mutate(rel_sepal_lw = Sepal.Length/Sepal.Width ) %>%
  mutate(rel_petal_lw = Petal.Length/Petal.Width )
flores_operacion
#Los datos faltantes por columna, se ven representados cuando obtengo el resultado de la operación?
missings2 <- function(x) return(sum(is.na(x)))
apply(flores_operacion,2,missings)
#si
```
```{r}
#Grafico de largo vs ancho de sepalo y petalo
grafico_flores_sepalrel <- ggplot(data = flores, 
                aes(x = Sepal.Width,
                    y = Sepal.Length,
                    col = Species)) +
  geom_point(shape = 18, size = 2, alpha = 0.8)+
geom_smooth(method = "lm")
grafico_flores_sepalrel + 
  scale_colour_manual(values = c("darkblue", "skyblue", "purple"))
grafico_flores_petalrel <- ggplot(data = flores, 
                aes(x = Petal.Width,
                    y = Petal.Length,
                    col = Species)) +
  geom_point(shape = 17, size = 2, alpha = 0.8)+
geom_smooth(method = "lm")
grafico_flores_petalrel + 
  scale_colour_manual(values = c("darkblue", "skyblue", "purple"))
```

Se nota una mayor similitud entre la relación largo/ancho de sepalo y petalo entre las especies versicolor y virginica, la diferencia entre las especies se hace mas evidente cuando se observa la relacion largo/ancho del petalo, donde la especie setosa presenta una relacion diferente a la que presentan las otras dos especies


```{r}
library("modeest")
#quiero saber la media de cada una de las relaciones por especie, para saber que tan separadas estan
medias <- flores_operacion %>%
  group_by(Species) %>%
  summarise(mean_relsepalo = mean(rel_sepal_lw, na.rm = T),
            mean_relpetalo = mean(rel_petal_lw, na.rm = T))
medias
#se confirma la similitud entre las especies versicolor y virginica

# Quiero saber la desviación estandar para saber qué tan dispersos estan mis datos
desviacion_est <- flores_operacion %>%
  group_by (Species)%>%
  summarise(desv_relsepalo = sd(rel_sepal_lw, na.rm = T),
            desv_relpetalo = sd(rel_petal_lw, na.rm = T)) 
desviacion_est
#En general los datos de las relaciones en las diferentes especies no muestran una dispersión alta
```
```{r}
#se hace el grafico de box plot para ver graficamente ....
ggplot(data = flores_operacion,
       aes(x = Species,y = rel_petal_lw, fill = Species))+
  geom_boxplot()+
  geom_jitter(shape = 1 ,position = position_jitter(0.2))

ggplot(data = flores_operacion,
       aes(x = Species,y = rel_sepal_lw, fill = Species))+
  geom_boxplot()+
  geom_jitter(shape = 1 ,position = position_jitter(0.2))
```

La relación entre el largo y el ancho tanto de flor como de sepalo diferencian la especie?
Si, esta relación es más evidente para diferenciar la especie setosa de las especies versicolor y virginica, ya que estas tienen bastante similitud en su relacion largo/ancho tanto de petalo como de sepalo
