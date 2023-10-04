---
title: "Ejercicio Final Introducción a Análisis en R para Datos Biológicos"
author: "Gabriela Alejandra Ramírez Castro"
output: html_document
---
### Organización de los datos y descripción de los datos

Para empezar, se caraga el paquete "Tidyverse":
```{r}
library(tidyverse)
```

Al abrir el archivo de los datos, se identifica como delimitador el punto y coma (;), por lo tanto se utiliza el siguiente comando para la correcta lectura de los datos:

```{r}
iris <- read.csv2("Tabla_especies_NA.csv")
```

Para conocer el tipo de datos con los que se está trabajando se utiliza el comando class(x) o str(X) para conocer toda la información del data frame:

```{r}
str(iris)
```
Al ver que todos los datos aparecen como "character", se realiza el siguiente comando para cambiar las columnas que contengan valores númericos a "numeric":

```{r}
iris$Sepal.Length <- as.numeric(iris$Sepal.Length)
iris$Sepal.Width <- as.numeric(iris$Sepal.Width)
iris$Petal.Length <- as.numeric(iris$Petal.Length)
iris$Petal.Width <- as.numeric(iris$Petal.Width)

str(iris)
```

Para tener una mejor organización de las columnas, se colocará "Species" como primera columna:

```{r}
iris <- iris%>%
  select("Species", everything())
iris <- iris[,-1]
head(iris)
```

Conocer las especies de Iris con las que se trabajará:

```{r}
unique(iris$Species)
```
¿Cuántas observaciones se tiene de cada especie?

```{r}
iris %>% 
  count(Species)
```

¿Cuál es el rango de los datos de cada medición?

```{r}
range(iris$Sepal.Length, na.rm = T)
range(iris$Sepal.Width, na.rm = T)
range(iris$Petal.Length, na.rm = T)
range(iris$Petal.Width, na.rm = T)
```
De esta manera es posible distinguir los datos con los que se va trabajar, dando una vista general de las variables y las especies.

### ¿cómo varian las dimensiones del pétalo y sépalo en las especies _Iris_ _setosa_, _Iris_ _versicolor_ y _Iris_ _virginica_?

Para calcular la media de cada variable en las especies se utilizó el siguiente comando:

```{r}
Media <- aggregate(.~Species, data = iris, FUN = mean)
Media
```

Se realizó el cálculo de la varianza:

```{r}
Varianza <- aggregate(.~Species, data = iris, FUN = var)
Varianza
```

Igualmente su desviación estandar:

```{r}
Desviacion_estandar <- aggregate(.~Species, data = iris, FUN = sd)
Desviacion_estandar
```

### Gráfica de barras de las medias de las dimensiones del pétalo y sépalo entre las especies de Iris

Por medio de esta gráfica se busca el realizar una comparación entre las diferentes especies y sus dimesiones florales, para determinar si son un factor que llegue a distinguirlas entre sí. Para ello se llevo a cabo el siguiente código:

```{r}

df_long <- gather(Media, key = "Variable", value = "Medida", -Species)

require(RColorBrewer)

ggplot(df_long, aes(x = Variable, y = Medida, fill = Species)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Medias de las dimensiones del pétalo y sépalo de las especies de Iris",
       x = "Dimensiones", y = "Media") + scale_fill_brewer(palette = "Set2") +
  theme_minimal()
  
```

Por medio de esta gráfica, se observa que las dimensiones de largo y ancho del pétalo para _Iris_ _setosa_ son de menor tamaño que las demás especies, teniendo una media para el largo del pétalo de 1.451282 y media del ancho de 0.2461538; esto llega a dar a entender, que los pétalos de _Iris_ _setosa_ son más pequeños que en las demás especies. En cuanto a sus sepálos, esta presenta la media de menor valor en cuanto a su largo (4.987179), pero presenta la mayor media del ancho del sepálo (3.446154), indicando un sépalo corto en comparación de las demás Iris, pero mucho más ancho. 

En cuanto a _Iris_ _virginica_, en las medidas de largo y ancho del pétalo, y largo del sépalo, es la que obtuvo el mayor valor de medias, con los valores de 5.515909, 2.0227273 y 6.545455, respectivamente. Se infiere que esta especie es la que presenta flores más grandes.	

Por otro lado, para la especie _Iris_ _versicolor_, se observan valores que se encuentran intermedios entre las especies _Iris_ _setosa_ y _Iris_ _virginica_ (largo pétalo: 4.215556, ancho pétalo: 1.3133333 y largo sepálo: 5.902222).Sin, embargo, presenta una media para ancho de Sepálo con menor valor 2.751111, siendo así la que presenta un sepálo más delgado.	

Es así, como se puede corroborar que las dimensiones de pétalo y sepálo entre las especies de Iris, varían entre cada una de ellas, definiendo de manera clara cada especie, proporcionando diferencias claves entre el tamaño sus cáliz y corola.
   
  
  