---
title: "Ejercicio"
author: "Keile Bruges"
date: "2023-09-27"
output: html_document
---
# Comando para cargar la libreria tidyverse
```{r}
library(tidyverse)
```

#Comando para cargar la libreria readr
```{r}
library(readr)
```
#Comando para cargar la libreria 
```{r}
library(dplyr)
```
#Comando para instalar y cargar la libreria modeest
```{r}
install.packages("modeest")  #Comando para instalar el paquete "modeest"
library ("modeest")          #Comando para cargar el paquete "modeest"
```
#Comando para instalar y cargar la libreria ggplot2
```{r}
install.packages("ggplot2")


library(ggplot2)
```




#Comando para leer el archivo

Permite identificar que tipo de delimitador tiene y escoger la funcion que pueda ejecutarse 
```{r}
#Leer el archivo 1
read.table("C:/Users/bruja/Desktop/CursoR2023/Ejercicio final/Ejercicio_entrega/Tabla_especies_NA.csv" , header = TRUE)


```

#Comando para leer el archivo en R 
```{r}
#Leer en R el archivo penguins_matrix3.txt
datos_flores<-read.csv2("C:/Users/bruja/Desktop/CursoR2023/Ejercicio final/Ejercicio_entrega/Tabla_especies_NA.csv") 
datos_flores

```
#Comando str
Permite obtener una descripción detallada de las variables y sus tipos de datos dentro del dataframe.
```{r}
str(datos_flores)
```

## Este comando se utiliza para seleccionar todas las filas en el dataframe datos_flores donde el valor en la columna "Species" es igual a "versicolor". Esto crea un nuevo dataframe que contiene solo las filas correspondientes a la especie "versicolor" en tus datos
```{r}
datos_flores[datos_flores$Species=="versicolor",]
```
#Comando nrow contará cuántas filas en el dataframe datos_flores tienen "versicolor" como valor en la columna "Especie". El resultado será el número de observaciones o registros en tus datos que cumplen con esta condición.
```{r}
nrow(datos_flores[datos_flores$Especie =="versicolor",])
```

```{r}
nrow(datos_flores[datos_flores$Especie =="setosa",])
```

```{r}
```


```{r}
nrow(datos_flores[datos_flores$Especie =="virginica",])
```


#Comando as.numeric
tiene la función de convertir los valores en la columna "XXXX" del dataframe datos_flores en valores numéricos.
```{r}
datos_flores$Sepal.Length<-as.numeri(datos_flores$Sepal.Length)

```

```{r}
datos_flores$Sepal.Width<-as.numeric(datos_flores$Sepal.Width)

```

```{r}
datos_flores$Petal.Length<-as.numeric(datos_flores$Petal.Length)

```


```{r}
datos_flores$Petal.Width<-as.numeric(datos_flores$Petal.Width)

```



##Con el comando str corrboramos que convertimos las columnas que nos interesan en valores númericos 
```{r}
str(datos_flores)
```

```{r}
datos_flores
```





#Este comando tiene la función de contar la cantidad total de valores faltantes (NA) en un dataframe 
```{r}
sum(is.na(datos_flores))
```

# Este comando  tiene la función de calcular la cantidad de valores faltantes (NA) en cada columna de un dataframe
```{r}
colSums(is.na(datos_flores))

```
##El comando en R que has proporcionado tiene la función de renombrar las columnas en un dataframe llamado datos_flores. Este comando utiliza el operador de tubería %>%, que se asocia comúnmente con el paquete dplyr de R para realizar manipulaciones de datos de manera más conveniente y legible.

La función rename() se utiliza para cambiar los nombres de las columnas en un dataframe.
```{r}
datos_flores <- datos_flores %>%
  rename(Sepalo_long_cm=Sepal.Length, Sepalo_ancho_cm=Sepal.Width, Petalo_long_cm=Petal.Length, Petalo_ancho_cm=Petal.Width, Especie=Species)

datos_flores
```

##En resumen, este comando te permite extraer un subconjunto de columnas específicas del dataframe original datos_flores y almacenarlas en un nuevo dataframe llamado flores
```{r}
columnas_flores<-c("Especie","Sepalo_long_cm" ,"Sepalo_ancho_cm","Petalo_long_cm","Petalo_ancho_cm")
flores<-datos_flores[,columnas_flores]

flores
```

```{r}
dplyr::filter()
```





#Comadno para contar las variables para cada especie.
```{r}
flores %>%
count(Especie)
```
#Comando para calcular la media de los datos de la columna de petalo_ancho y no tenga en cuenta los datos faltantes NA 
```{r}
mean(flores$Petalo_ancho_cm, na.rm = TRUE)

```

```{r}
flores
```


#Comando para crear un vector que contenga el valor de la media de la columna de petalo_ancho
```{r}
MEDIA <- c(median(flores$Petalo_ancho_cm, na.rm = TRUE))
MEDIA
```

#Comando para calcular la media de los datos de la columna de sepalo_long y no tenga en cuenta los datos faltantes NA y a la vez crea el vector MEDIASEP
```{r}
MEDIASEP <- c(median(flores$Sepalo_long_cm, na.rm = TRUE))
MEDIASEP
```





```{r}
flores$Rel_Petalo_long_ancho <-flores$Petalo_long_cm / MEDIA
flores
```

```{r}
flores$Rel_Sepalo_log_ancho <-flores$Sepalo_ancho_cm / MEDIASEP
flores
```






#Pregunta hay diferencias estadisticamente significativas en la relación Petalo_long_ancho y la relación sepalo_long_ancho entre las diferentes especies?

```{r}
install.packages("modeest")
```

```{r}
t.test(flores$Rel_Petalo_long_ancho, mu=1.3)
```

```{r}
t.test(flores$Rel_Sepalo_log_ancho, mu=5.8)
```




```{r}
plot(flores$Especie, flores$Rel_Petalo_long_ancho)
```
```{r}
any(is.na(flores$Especie)) 
```
```{r}
any(is.infinite(flores$Rel_Petalo_long_ancho))
```


```{r}
flores <- na.omit(flores)  # Eliminar filas con NA en los datos

flores
```

```{r}
plot(flores$Especie, flores$Rel_Petalo_long_ancho)
```
```{r}
cor.test(flores$Rel_Petalo_long_ancho, flores$Rel_Sepalo_log_ancho, method = "spearman")$estimate

```

```{r}
# Crear un gráfico de dispersión
plot(Rel_Petalo_long_ancho, Rel_Sepalo_long_ancho,
     main = "Gráfico de Dispersión",
     xlab = "flores$Rel_Petalo_long_ancho",
     ylab = "flores$Rel_Sepalo_long_ancho",
     pch = 16,   # Tipo de punto
     col = "blue")  # Color de los puntos

```

```{r}
plot(flores$Rel_Petalo_long_ancho, flores$Rel_Sepalo_log_ancho)
```

```{r}
plot(flores$Rel_Petalo_long_ancho, flores$Rel_Sepalo_log_ancho)
```


```{r}

plot(x = flores$Rel_Petalo_long_ancho, 
     y = flores$Rel_Sepalo_log_ancho,
     col = factor(flores$Especie), pch = 19)
```


```{r}
ggplot(data = flores, 
       aes(Rel_Petalo_long_ancho, color = Especie, fill = Especie)) + 
  geom_histogram()

```


```{r}
ggplot(data = flores, 
       aes(Rel_Sepalo_log_ancho, color = Especie, fill = Especie)) + 
  geom_histogram()

```
