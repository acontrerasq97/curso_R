---
title: "Ejercicio Final"
author: "Ana María Forero Barrero"
date: "2023-10-01"
output: html_document
---

# Ejercicio final. Curso Introducción al análisis en R para datos biológicos

---

## Descripción general de los datos

Para este trabajo, los datos a analizar son los siguientes: 

```{r}
library(tidyverse)
datos_flores <- read_delim("Tabla_especies_NA.csv", 
                            col_types = list(Sepal.Length = col_double(),
                                             Sepal.Width = col_double(),
                                             Petal.Length = col_double(),
                                             Petal.Width = col_double(),
                                             Species=col_character()),
                            na = c("NA", "na", "", "N/A"))
datos_flores
```

Ahora, se pueden observar las características del conjunto de datos: 

```{r}
str(datos_flores)
```

Datos faltantes:
```{r}
sum(is.na(datos_flores))  # Total de datos faltantes
colSums(is.na(datos_flores))  #Total de datos faltantes por columnas
```

Adicionalmente, las categorías de la variable Species son
```{r}
unique(datos_flores$Species)
```

Así, se tiene entonces una tabla con 150 observaciones de 5 varibles para tres especies de plantas del género *Iris* que tiene además 23 datos faltantes en total.

Para facilitar el trabajo con estos datos se optó por cambiar los nombres de las variables.

```{r}
datos_flores <- datos_flores %>%
  rename(largo_sepalo  = Sepal.Length,
         ancho_sepalo= Sepal.Width,
         largo_petalo = Petal.Length,
         ancho_petalo = Petal.Width,
         especie = Species)
datos_flores
```


## Objetivos

¿Cómo varían las medidas al interior de cada especie?
¿Cómo se relacionan el ancho y largo de pétalo y sépalo en cada especie? 

¿A partir de estos datos es posible determinar una relación entre las medidas tomadas en petalos y sepalos en las flores de *Iris* y cómo son éstas dependiendo de cada especie?


## Análisis estadístico

```{r}
library ("modeest")
summary(datos_flores)
```

Media de las variables por especie
```{r}
datos_medias <- datos_flores %>% 
  group_by(especie) %>% 
  summarise(mean_largo_sepalo = mean(largo_sepalo, na.rm = T),
            mean_ancho_sepalo = mean(ancho_sepalo, na.rm = T),
            mean_largo_petalo = mean(largo_petalo, na.rm = T),
            mean_ancho_petalo = mean(ancho_petalo, na.rm = T))
```

Desviación estándar de las variables por especie
```{r}
datos_desv <- datos_flores %>% 
  group_by(especie) %>% 
  summarise(desv_largo_sepalo = sd(largo_sepalo, na.rm = T),
            desv_ancho_sepalo = sd(ancho_sepalo, na.rm = T),
            desv_largo_petalo = sd(largo_petalo, na.rm = T),
            desv_ancho_petalo = sd(ancho_petalo, na.rm = T))
```

Para establecer una correlación entre las variables por especie se hace una prueba de coeficiente de correlación, para la cual se realiza una prueba de normalidad de los datos.

### Pruebas por especie

i. **Setosa**
```{r}
setosa <- datos_flores %>% 
  filter(especie == "setosa")

setosa
```

- Coeficiente de correlación:

```{r}
#install.packages("nortest")
library(nortest)

#Prueba de normalidad
ad.test(setosa$largo_sepalo)
ad.test(setosa$ancho_sepalo)

ad.test(setosa$largo_petalo)
ad.test(setosa$ancho_petalo)
```

Para la variables de sépalo el p-valor indica probabilidad de normalidad de los datos por lo cual se opta por le coeficiente de correlación de pearson, contrario a lo arrojado para pétalo.

```{r}
cor.test(setosa$largo_sepalo, setosa$ancho_sepalo, method = "pearson")

cor.test(setosa$largo_petalo, setosa$ancho_petalo, method = "spearman")
```


ii. **Virginica**
```{r}
virginica <- datos_flores %>% 
  filter(especie == "virginica")

virginica
```

- Coeficiente de correlación:

```{r}
#Prueba de normalidad
ad.test(virginica$largo_sepalo)
ad.test(virginica$ancho_sepalo)

ad.test(virginica$largo_petalo)
ad.test(virginica$ancho_petalo)
```

Ya que el p-valor para cada variables es menor o relativamente cercano a 0.05, se opta por utilizar el coeficiente de correlación de Spearman.

```{r}
cor.test(virginica$largo_sepalo, virginica$ancho_sepalo, method = "spearman")

cor.test(virginica$largo_petalo, virginica$ancho_petalo, method = "spearman")
```


iii. **Versicolor**
```{r}
versicolor <- datos_flores %>% 
  filter(especie == "versicolor")

versicolor
```

- Coeficiente de correlación:

```{r}
#Prueba de normalidad
ad.test(versicolor$largo_sepalo)
ad.test(versicolor$ancho_sepalo)

ad.test(versicolor$largo_petalo)
ad.test(versicolor$ancho_petalo)
```

Para la variables de sépalo el p-valor indica probabilidad de normalidad de los datos por lo cual se opta por le coeficiente de correlación de pearson, contrario a lo arrojado para pétalo

```{r}
cor.test(versicolor$largo_sepalo, versicolor$ancho_sepalo, method = "pearson")

cor.test(versicolor$largo_petalo, versicolor$ancho_petalo, method = "spearman")
```


Finalmente, las varibles más correlacionadas son las de sépalo de setosa y las de pétalo de versicolor, sin embargo en todas las variables existe correlación positiva. 


## Representación gráfica

```{r}
names(datos_flores)

g_sepalo <- ggplot(datos_flores, 
                aes(x = largo_sepalo,
                    y = ancho_sepalo,
                    shape = especie,
                    col = especie)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

g_sepalo_labs <- g_sepalo +
  theme_light() +
  labs(title = "Relación sépalo",
       x = "Largo del sépalo en cm",
       y = "Ancho del sépalo en cm") +
  scale_color_manual(values = c("#e41a1c", "#2c7bb6", "#e66101"))

g_sepalo_labs

```

```{r}
g_petalo <- ggplot(datos_flores, 
                aes(x = largo_petalo,
                    y = ancho_petalo,
                    shape = especie,
                    col = especie)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

g_petalo_labs <- g_petalo +
  theme_light() +
  labs(title = "Relación pétalo",
       x = "Largo del pétalo en cm",
       y = "Ancho del pétalo en cm") +
  scale_color_brewer(palette="Set1")

g_petalo_labs
```


