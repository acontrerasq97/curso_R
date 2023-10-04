---
title: "Ejercicio_Final_Rbasico"
author: "Fernanda Penagos"
date: "2023-09-27"
output:
  pdf_document: default
  html_document: default
---

# Ejercicio Final - Fernanda Penagos

#### **1. Una descripción general de los datos: utilice funciones que hablamos durante el curso para hacer una caracterización general de los datos.**

Cargar libreria

```{r}
#install.packages("tidyverse", dependecies = TRUE)
library("tidyverse")
library("modeest")

```

Llamado tabla especies de manera explicita

```{r}
flor_iris <- read_delim("/Users/fernanda/Library/CloudStorage/OneDrive-Personal/CursoR_UNAL/Ejercicio_entrega/Tabla_especies_NA.csv", 
                       col_types = list(Sepal.Length = col_double(),
                                             Sepal.Width = col_double(),
                                             Petal.Length = col_double(),
                                             Petal.Width = col_double(),
                                             Species = col_character()),
                            na = c("NA", "na", "", "N/A"))
flor_iris
```

Organización data

```{r}
flor_iris <- flor_iris[, c(5,1:4)]
flor_iris
```

Información general data

-   Longitud data

    ```{r}
    length(flor_iris)
    ```

-   Clase data

    ```{r}
    class(flor_iris)
    ```

-   Numero de filas

    ```{r}
    nrow(flor_iris)
    ```

-   Numero de columnas

    ```{r}
    ncol(flor_iris)
    ```

-   Nombre columnas

    ```{r}
    colnames(flor_iris)
    ```

-   Tipo de datos por columna

    -   Species

        ```{r}
        typeof(flor_iris$Species)
        ```

    -   Sepal length

        ```{r}
        typeof(flor_iris$Sepal.Length)
        ```

    -   Sepal Width

        ```{r}
        typeof(flor_iris$Sepal.Width) 
        ```

    -   Petal Length

        ```{r}
        typeof(flor_iris$Petal.Length)
        ```

    -   Petal Width

        ```{r}
        typeof(flor_iris$Petal.Width)
        ```

#### **2. Genere una pregunta para realizar con los datos.**

Cuantas observaciones tenemos por especies ?

```{r}
flor_iris %>% count(Species)
```

Cuantos NA tenemos en las observaciones?

```{r}
#NA por sepal length
sum(is.na(flor_iris$Sepal.Length))
#NA por sepal width
sum(is.na(flor_iris$Sepal.Width))
#NA por petal length
sum(is.na(flor_iris$Petal.Length))
#NA por petal width
sum(is.na(flor_iris$Petal.Width))
#NA total
sum(is.na(flor_iris))
```

Tenemos algun valor negativo en los datos?

```{r}
any(flor_iris<0)
```

#### **3. Manipule los datos y haga alguna prueba estadística para responder las pregunta generada.**

Modificacion de nombres

```{r}
flor_iris <- flor_iris %>% 
  rename(especie = Species,
         sepalo_largo = Sepal.Length,
         sepalo_ancho = Sepal.Width,
         petalo_largo = Petal.Length,
         petalo_ancho = Petal.Width)

flor_iris
```

Nueva columna con la relacion de sepalos y petalos. Asignacion a tabla completa

```{r}
flor_iris_completa <- flor_iris %>%
  mutate(rel_sepalo = flor_iris$sepalo_largo * flor_iris$sepalo_ancho) %>%
  mutate(rel_petalo = flor_iris$petalo_largo * flor_iris$petalo_ancho)

flor_iris_completa
```

**Medidas de tendencia central y dispersion**

Moda, media, mediana, max-min, rango, varianza , desviacion estandar:

Sepalo largo

```{r}
mfv(flor_iris_completa$sepalo_largo, na_rm = TRUE)
mean(flor_iris_completa$sepalo_largo, na.rm = TRUE)
median(flor_iris_completa$sepalo_largo, na.rm = TRUE)
max(flor_iris_completa$sepalo_largo, na.rm = T) - min(flor_iris_completa$sepalo_largo, na.rm = T)
range(flor_iris_completa$sepalo_largo, na.rm = T)
var(flor_iris_completa$sepalo_largo, na.rm = T)
sd(flor_iris_completa$sepalo_largo, na.rm = T)

```

Sepalo ancho

```{r}
mfv(flor_iris_completa$sepalo_ancho, na_rm = TRUE)
mean(flor_iris_completa$sepalo_ancho, na.rm = TRUE)
median(flor_iris_completa$sepalo_ancho, na.rm = TRUE)
max(flor_iris_completa$sepalo_ancho, na.rm = T) - min(flor_iris_completa$sepalo_ancho, na.rm = T)
range(flor_iris_completa$sepalo_ancho, na.rm = T)
var(flor_iris_completa$sepalo_ancho, na.rm = T)
sd(flor_iris_completa$sepalo_ancho, na.rm = T)

```

Petalo largo

```{r}
mfv(flor_iris_completa$petalo_largo, na_rm = TRUE)
mean(flor_iris_completa$petalo_largo, na.rm = TRUE)
median(flor_iris_completa$petalo_largo, na.rm = TRUE)
max(flor_iris_completa$petalo_largo, na.rm = T) - min(flor_iris_completa$petalo_largo, na.rm = T)
range(flor_iris_completa$petalo_largo, na.rm = T)
var(flor_iris_completa$petalo_largo, na.rm = T)
sd(flor_iris_completa$petalo_largo, na.rm = T)

```

Petalo ancho

```{r}
mfv(flor_iris_completa$petalo_ancho, na_rm = TRUE)
mean(flor_iris_completa$petalo_ancho, na.rm = TRUE)
median(flor_iris_completa$petalo_ancho, na.rm = TRUE)
max(flor_iris_completa$petalo_ancho, na.rm = T) - min(flor_iris_completa$petalo_ancho, na.rm = T)
range(flor_iris_completa$petalo_ancho, na.rm = T)
var(flor_iris_completa$petalo_ancho, na.rm = T)
sd(flor_iris_completa$petalo_ancho, na.rm = T)

```

Relacion sepalos

```{r}
mfv(flor_iris_completa$rel_sepalo, na_rm = TRUE)
mean(flor_iris_completa$rel_sepalo, na.rm = TRUE)
median(flor_iris_completa$rel_sepalo, na.rm = TRUE)
max(flor_iris_completa$rel_sepalo, na.rm = T) - min(flor_iris_completa$rel_sepalo, na.rm = T)
range(flor_iris_completa$rel_sepalo, na.rm = T)
var(flor_iris_completa$rel_sepalo, na.rm = T)
sd(flor_iris_completa$rel_sepalo, na.rm = T)
```

Relacion petalos

```{r}
mfv(flor_iris_completa$rel_petalo, na_rm = TRUE)
mean(flor_iris_completa$rel_petalo, na.rm = TRUE)
median(flor_iris_completa$rel_petalo, na.rm = TRUE)
max(flor_iris_completa$rel_petalo, na.rm = T) - min(flor_iris_completa$sepalo_largo, na.rm = T)
range(flor_iris_completa$rel_petalo, na.rm = T)
var(flor_iris_completa$rel_petalo, na.rm = T)
sd(flor_iris_completa$rel_petalo, na.rm = T)
```

**Prueba de comparacion de medias**

Es la media del largo de petalo de la especie setosa igual a la de la especie versicolor

T-test : setosa y versicolor

```{r}
setosa <- na.omit(subset(flor_iris_completa,flor_iris_completa$especie == "setosa")$petalo_largo)
versicolor <- na.omit(subset(flor_iris_completa,flor_iris_completa$especie == "versicolor")$petalo_largo)
virginica <- na.omit(subset(flor_iris_completa,flor_iris_completa$especie == "virginica")$petalo_largo)
versicolor
t.test(setosa,versicolor, var.equal = T, conf.level = 1-0.05, alternative="two.sided")
```

T-test : setosa y virginica

Es la media del largo de petalo de la especie setosa igual a la de la especie virginica

```{r}
t.test(setosa,virginica, var.equal = T, conf.level = 1-0.05, alternative="two.sided")
```

T-test : versicolor y virginica

Es la media del largo de petalo de la especie versicolor igual a la de la especie virginica

```{r}
t.test(versicolor,virginica, var.equal = T, conf.level = 1-0.05, alternative="two.sided") 
```

#### **4. Realize una representación gráfica que demuestre la respuesta para la pregunta**

Grafico de correlacion

```{r}
graficoflor <- ggplot(data = flor_iris_completa,           
            aes(x = flor_iris_completa$rel_sepalo,        
                y = flor_iris_completa$rel_petalo,         
               col = flor_iris_completa$especie)) +           
      geom_point(shape=19, size = 5, alpha = 0.8) +  
      geom_smooth(method = "lm") +         
      labs(title = "Relacion sepalos y petalos",  
           x = "Relacion sepalo largoxancho",         
           y = "Relacion petalo largoxancho",          
           col = "Especies") +              
      theme_classic() +                    
      theme(title = element_text(size = 8, face = "bold"),
            text = element_text(size = 6))
graficoflor
```
