---
title: "Script_Trabajo_Final"
author: "Gloria Fernanda Villamizar"
date: "2023-09-27"
output: html_document
---

###Lo primero que se debe hacer es llamar el paquete Tidyverse que contiene las funciones que quiero emplear. Para ello uso únicamente la función library ya que el paquete fue instalado previamente. 
```{r}
library (tidyverse)
```
###Para ejecutar los análisis estadísticos, se debe llamar el paquete Modeest

```{r}

library ("modeest") 
```


###Se deben llamar los datos con los que voy a realizar mi análisis usando la función read.csv2, y asignarlos a una variable. La función read.csv2 es la indicada en este caso debido a que los datos están separados por ;.
```{r}
datos_plantas <- read.csv2("Tabla_especies_NA.csv", header = T, sep = ";")
datos_plantas


```
###Debido a que las variables están como caracteres, se debe proceder a transormarlas en valores numéricos.

```{r}
datos_plantas <- datos_plantas%>% mutate_at(c("Sepal.Length", "Sepal.Width", "Petal.Length","Petal.Width"), as.numeric)
datos_plantas
```


###Antes de analizar estadísticamente los datos, es necesario eliminar los datos faltantes.

```{r}
sum(is.na(datos_plantas))

datos_plantas_sinNA <- drop_na(datos_plantas)
datos_plantas_sinNA
```
```{r}
datos_box <- datos_plantas_sinNA[,3:5]
```




###Para obtener el promedio de los datos, se usa la función mean()
```{r}
mean(datos_plantas_sinNA$Sepal.Length)
mean(datos_plantas_sinNA$Sepal.Width)
mean(datos_plantas_sinNA$Petal.Length)
mean(datos_plantas_sinNA$Petal.Width)
```
###Para obtener la moda de los datos, se usa la función mfv()

```{r}
mfv(datos_plantas_sinNA$Sepal.Length)
mfv(datos_plantas_sinNA$Sepal.Width)
mfv(datos_plantas_sinNA$Petal.Length)
mfv(datos_plantas_sinNA$Petal.Width)
mfv(datos_plantas_sinNA$Species)
```

###Para obtener la mediana de los datos, se usa la función median()
```{r}
median(datos_plantas_sinNA$Sepal.Length)
median(datos_plantas_sinNA$Sepal.Width)
median(datos_plantas_sinNA$Petal.Length)
median(datos_plantas_sinNA$Petal.Width)

```
###Para obtener el rango en el que se encuentran los datos, es decir, el valor nímo y el máximo, se usa la función range()

```{r}
range(datos_plantas_sinNA$Sepal.Length)
range(datos_plantas_sinNA$Sepal.Width)
range(datos_plantas_sinNA$Petal.Length)
range(datos_plantas_sinNA$Petal.Width)
```


###Para obtener la varianza de cada conjunto de datos, se usa la función Var


```{r}
var(datos_plantas_sinNA$Sepal.Length)
var(datos_plantas_sinNA$Sepal.Width)
var(datos_plantas_sinNA$Petal.Length)
var(datos_plantas_sinNA$Petal.Width)
```
###Para obtener la desviación estándar de cada conjunto de datos, y así saber qué tan dispersos están, se usa la función sd()

```{r}
sd(datos_plantas_sinNA$Sepal.Length)
sd(datos_plantas_sinNA$Sepal.Width)
sd(datos_plantas_sinNA$Petal.Length)
sd(datos_plantas_sinNA$Petal.Width)
```
###Para obtener los cuantiles, usamos la siguiente función:
```{r}
quantile(datos_plantas_sinNA$Sepal.Length, probs=c(seq(0,1,0.25)))
quantile(datos_plantas_sinNA$Sepal.Width, probs=c(seq(0,1,0.25)))
quantile(datos_plantas_sinNA$Petal.Length, probs=c(seq(0,1,0.25)))
quantile(datos_plantas_sinNA$Petal.Width, probs=c(seq(0,1,0.25)))

```

##Pregunta: ¿Los pétalos de la especie Setosa tienen una relación de largo y acho mayor a la de la especie Virginica?


###Para responder esa pregunta, es necesario crear un subconjunto que comprenda los datos del largo y ancho de cada especie
```{r}
datos.setosa <- subset(datos_plantas_sinNA,
                       Species=="setosa",
                       select=c("Petal.Length", "Petal.Width"))
datos.setosa

datos.virginica <- subset(datos_plantas_sinNA,
                       Species=="virginica",
                       select=c("Petal.Length", "Petal.Width"))
datos.virginica

```
###
```{r}
datos.setosa$len_wid_set <- datos.setosa$Petal.Length/datos.setosa$Petal.Width
datos.setosa

datos.virginica$len_wid_vir <- datos.virginica$Petal.Length/datos.virginica$Petal.Width
datos.virginica

```


###Teniendo en cuenta que la prueba T únicamente es válida si los datos tienen una distribución normal, se hace necesario construir un histograma para cada subconjunto de datos.

```{r}
ggplot(data=datos.setosa,
       aes(x=len_wid_set))+
geom_histogram(binwidth =0.01)
```
```{r}
ggplot(data=datos.virginica,
       aes(x=len_wid_vir))+
geom_histogram(binwidth =0.01)
```

###Aunque los histogramas de dichas relaciones no tienen una distribución normal, se opta por mantener dicha relación como eje central de la pregunta ante la imposibilidad de encontrar datos con distribución normal. 

###Una vez construído los subconjuntos, se organiza la función de la prueba T, con una hipótesis alternativa que indica que la relación entre largo y ancho de los pétalos de Virginica es mayor a los de Setosa
```{r}
t.test(datos.setosa[,3],datos.virginica[,3], var.equal = F, conf.level=1-0.05, alternative="greater")
```


###Al comparar las medias de ambas variables por medio de la prueba T, podemos identificar que evidentemente la relación entre el largo y ancho de los pétalos de la especie Setosa es mayor al de la especie Virginica.


###Para evidenciar mejor la distribución de los datos previamente analizados, se usa un Box Plot.
```{r}
ggplot(data=datos_plantas_sinNA, 
         aes(x=Petal.Length, y=Petal.Width, fill=Species))+
         geom_boxplot()+
  geom_jitter(shape= 1, position=position_jitter(0.2))
```










