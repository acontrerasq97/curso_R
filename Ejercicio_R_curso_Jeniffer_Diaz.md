---
title: "Ejercicio_R_curso_Jeniffer_Diaz"
author: "Jeniffer Díaz"
date: "2023-09-25"
output: html_document
---

##Cargar los datos
Primero cargamos los datos que vamos a utilizar y en esta parte superior siempre ponemos las librerias que vayamos necesitando.
Al cargar el directorio, se indica que sea leído como un texto delimitado y se le asigna un nombre, como objeto, en este caso 'Ej_plants'.
Con la función str, no permite saber que es un dataframe de 150 filas por 5 columnas. Cuatro de esa columnas: Sepal.Length, Sepal.Width, Petal.Length y Petal.Width tienen datos numericos, y una columna: Species, tiene datos tipo categorico.

```{r}
library(readr)
library(dplyr)
library(nortest)
library(psych)

setwd("D:/JD/Proyectos/Maestría/Materias/CursoR_2023/Datos/Ejercicio_entrega")
Ej_plants <- read_delim("Tabla_especies_NA.csv", show_col_types = F)

#Ej_plants <- read_delim("~/MAESTRIA_EN_ESTADISTICA/25_monitorias/archivos/iris.txt", delim = ";", escape_double = FALSE, trim_ws = TRUE)
```


## Pregunta
¿Qué relación hay entre el largo del petalo y el largo del sepalo para cada una de las especies?

```{r}
Sepal.Length <- Ej_plants$Sepal.Length
mean(Sepal.Length, na.rm = T)
var(Sepal.Length, na.rm = T)
sd(Sepal.Length, na.rm = T)
```
```{r}
Petal.Length <- Ej_plants$Petal.Length
mean(Petal.Length, na.rm = T)
var(Petal.Length, na.rm = T)
sd(Petal.Length, na.rm = T)
```
```{r}
Ej_plants %>% count(Petal.Length, "virginica")
```


```{r}
Ej_plants %>% count(Petal.Length, "setosa")
```


```{r}
Ej_plants %>% count(Petal.Length, "versicolor")
```

##Histogramas
Para visualizar la distribución de los datos realizamos unos histogramas de las variables. Al hacer el de longitud del petalo nos damos cuenta que presenta una distribución bimodal, por lo que es necesario realizar el diagrama por especies.
El histoghrama de longitud del sépalo presenta una distribución más homogénea, para confirmar si tienen distribución normal se realiza la prueba de normalidad.


```{r}
hist(Ej_plants$Petal.Length)
```


```{r}
hist(subset(Ej_plants, Ej_plants$Species == "setosa")$Petal.Length, breaks=10)
```


```{r}
hist(subset(Ej_plants, Ej_plants$Species == "virginica")$Petal.Length, breaks=10)
```


```{r}
hist(subset(Ej_plants, Ej_plants$Species == "versicolor")$Petal.Length, breaks=10)
```


```{r}
hist(Sepal.Length, freq = F)

```
## Prueba de normalidad

A través del test de Kolmogorv vamos a evaluar si las variables tienen una distribución normal.


```{r}
lillie.test(Ej_plants$Sepal.Length) #no se distribuye normalmente
```


```{r}
lillie.test(Ej_plants$Petal.Length)

```
Al realizar la prueba de normalidad nos arroja que los datos no se distribuyen de manera normal, por ende no aplica para la realización de pruebas paramétricas como la T student. Al no distribuirse normalmente realizamos una prueba no paramétrica a las variables de interés, en este caso se realizará la prueba de Kruskal Wallis.


```{r}
kruskal.test(Petal.Length ~ Species, data= Ej_plants)
```


```{r}
kruskal.test(Sepal.Length ~ Species, data= Ej_plants)
```
Ambas dan como resultado un p valor significativo, indicando que la variable dependiente cambia en función de cada especie.

##Correlación

Debemos primero quitar los NA, para que nos arroje la correlación. Una vez retirados los NA vamos a quitar las columnas que no necesitamos, en este caso Petal.Width y Sepal.Width y se crea objeto Ej_plants3
```{r}

Ej_plants2 <- Ej_plants[complete.cases(Ej_plants), ]

Ej_plants3 <-  select(Ej_plants2, -Petal.Width, -Sepal.Width)


```

##Correlación
Con el unevo objeto Ej_plants3 se procedió a realizar la correlación con el tipo de simbolo número 20, se activan los asteriscos de las indicaciones con TRUE y se indica el título que llevará el gráfico.

```{r}
plot(Ej_plants3[, 1:2], main= "Correlation plot")

```

Al realizar el gráfico de correlación no es muy clara que tanta correlación hay, por eso se realiza de una forma más detallada para ver las graficas y los datos de correlación al tiempo.

```{r}
pairs.panels(Ej_plants3["Sepal_length"=setosa$Sepal_Length, "Petal_Length"=setosa$Petal_Length], pch=20, stars=TRUE, main="Correlacion largo del petalo y sepalo")
```


```{r}
pairs.panels(Ej_plants3[1:2],bg=c("red","yellow","blue")[Ej_plants3$Species],
             pch=21,main="Correlacion largo del petalo y sepalo")
```

Al hacer la correlación, obtenemos la grafica de dispersión en puntos, las distribuciones de las variables en barras y los coeficientes de correlación y significancia con los asteriscos. Con estos resultados podemos concluir que la magnitud entre la longitud del sepalo con la longitud del petalo es del 86%, la magnitud de la longitud del sepalo entre las especies es del 77% y la magnitud de la longitud del petalo entre las especies es del 95%. Todas tienen significancia pero la correlación de la longitud del pétalo entre las especies, es la más fuerte.

rmarkdown::render("Ejercicio_R_curso_Jeniffer_Diaz.Rmd")

