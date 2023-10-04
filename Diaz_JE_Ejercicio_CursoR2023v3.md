---
title: "Ensayo_ejercicio_cursoR"
author: "Julian Diaz"
date: "2023-09-28"
output:
  html_document: default
  pdf_document: default
---

#Ejercicio de entrega Curso Extensión "Introducción a Analísis en R para Datos Biológicos"

**Estudiante**: Julián Esteban Díaz Triana 


## Alistamiento

En primer lugar se verificó el directorio de trabajo fijado para realizar la carga del archivo de datos suministrado. Posteriormente se cargó el paquete "tidyverse" mediante la función library, de modo que se cargaran los paquetes secundarios con funciones de lectura de archivos.

Se eligió la función "read_delim" para leer el archivo en formato csv con los datos separados por punto y coma (;). Los datos se asignaron al objeto "datos_flores".

```{r} 

getwd() # Verificación del directorio de trabajo para cargar los datos desde este

library(tidyverse) # Carga del paquete Tidyverse para lectura y carga de datos en R

datos_flores<-read_delim("D:/IMP/Doctorado en Ciencias - Biología/CursoR-AnaDatBio/curso-extension-IntroR-2023-main/Ejercicio_entrega/Tabla_especies_NA.csv")
datos_flores

```

La visualización en R permitió establecer que los datos corresponden a una tabla con 150 filas y 5 columnas. Además, las 4 primeras columnas contienen información numérica asociada a las 4 variables de medición y la última columna  información categórica relativa a las especies del género _Iris_.

## Primera parte: Descripción general de los datos

Se utilizó la función "is.data.frame" para verificar que los datos se reconocieran por R en estructura de un dataframe.

```{r}

#verificación del reconocimiento de los datos como un dataframe

is.data.frame(datos_flores)

```
Mediante las funciones "head" y "tail" se visualizaron respectivamente las primeras y las últimas 6 filas del dataframe.

```{r}

#Visualización de un subset de datos

head(datos_flores)
tail(datos_flores)

```

Mediante las funciones "names", "colnames" y se verificó que la primera fila fuese reconocida como el encabezado de la tabla, asignando los nombres correspondientes a las columnas. Por otro lado, con la función "rownames" se evidenció la secuencialidad de las filas sin nombres específicos.

```{r}

#Encabezado y nombres

names(datos_flores)
colnames(datos_flores)
rownames(datos_flores)

```

Se revisó la dimensión de la tabla con la función "dim", arrojando nuevamente el número de filas y columnas, que también coincidió con la información suministrada al ejecutar las funciones "nrow" y "ncol". La función "str" por su parte permitió resumir los tipos de datos y su estructura.

```{r}

#Tamaño y estructura de los datos
dim(datos_flores)
nrow(datos_flores)
ncol(datos_flores)
str(datos_flores)

```

Haciendo énfasis en verificar los tipos de datos de cada columna, la función "class" también permitió saber que las columnas _Sepal.Length_, _Sepal.Width_, _Petal.Length_, _Petal.Width_ contienen datos numéricos (decimales), mientras la columna "_Species_" contiene datos tipo caracter.

```{r}

#Tipos de datos
class(datos_flores)
class(datos_flores$Sepal.Length)
class(datos_flores$Sepal.Width)
class(datos_flores$Petal.Length)
class(datos_flores$Petal.Width)
class(datos_flores$Species)

```

Aplicando otras funciones y opciones para obtener subsets de datos se obtuvo la siguiente descripción de los datos:

  1. Hay 128 casos completos en todo el dataframe, es decir, filas sin ningún dato faltante (NA).

  2. Hay 23 casos faltantes (NA) a nivel de todo el dataframe

  3. En cada columna hay 4 casos faltantes en _Sepal.Length_, 8 en _Sepal.Width_, 4 en _Petal.Length_, y 7 en _Petal.Width_. 

```{r}

datos_flores%>%
  count(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)

datos_flores[complete.cases(datos_flores),]

sum(is.na(datos_flores)) # cuenta de elementos faltantes (NA) en el dataframe

colSums(is.na(datos_flores)) # cuenta de elementos faltantes (NA) por cada columna

```

##segunda parte: Formulación de una pregunta sobre los datos

**Pregunta**: ¿Existen diferencias entre de longitud o amplitud de los pétalos y sépalos de las flores de las tres especies del género _Iris_?

Para mayor comodidad, la primera manipulación de los datos consistió en realizar el cambio de los nombres de las columnas (variables) para que fuesen indivativas en español en lugar de inglés, asumiendo que todas las medidas estan en escala de centímetros (cm) y que son correctos en todos los casos.

Con dicho cambio, se asignó la tabla de datos a un nuevo objeto llamado "datos_flores2" para distinguirlo de los datos cargados originalmente.

```{r}

# Cambio de los nombres de las columnas para colocarlos en español

datos_flores2<-datos_flores %>%
  rename(Long.Sepalo = Sepal.Length,
         Ancho.Sepalo = Sepal.Width,
         Long.Petalo = Petal.Length,
         Ancho.Petalo = Petal.Width,
         Especies = Species)
datos_flores2

```

Suspicázmente se utilizó la función "summary" para obtener un resumen rápido de las características de los datos en términos de algunas medidas referentes a estadística descriptiva.


```{r}

summary(datos_flores2)

```

Para conocer gráficamente la distribución de los datos de cada variable se realizó un histograma por cada una de ellas.


```{r}

Long.Sepalo.Completo<-na.omit(datos_flores2$Long.Sepalo)
Long.Sepalo.Completo
length(Long.Sepalo.Completo)

hist(Long.Sepalo.Completo, freq = F)

      
```

```{r}

Ancho.Sepalo.Completo<-na.omit(datos_flores2$Ancho.Sepalo)
Ancho.Sepalo.Completo
length(Ancho.Sepalo.Completo)

hist(Ancho.Sepalo.Completo, freq = F)

```

```{r}

Long.Petalo.Completo<-na.omit(datos_flores2$Long.Petalo)
Long.Petalo.Completo
length(Long.Sepalo.Completo)

hist(Long.Petalo.Completo, freq = F)

```

```{r}

Ancho.Petalo.Completo<-na.omit(datos_flores2$Ancho.Petalo)
Ancho.Petalo.Completo
length(Ancho.Petalo.Completo)

hist(Ancho.Petalo.Completo, freq = F)

```

Con la intepretación de los gráficos anteriores, los datos de las dimensiones de los pétalos (longitud y ancho) parecen agruparse, por lo cual son presumibles las diferencias entre una o más especies. Así, podemos comparar pares de especies con pruebas de t, evaluando las diferencias para cada variable por separado.

Antes de proceder con las pruebas de t, se conoció el promedio y la desviación estándar de cada muestra. En este caso se utilizaron las funciones mean y sd, tomando en cuenta que en los argumentos se incluyera la indicación para no tomar en cuenta los elementos faltantes (NA).


```{r}

datos_flores2%>%
  group_by(Especies)%>%
  summarise(Long.Petalo.Prom = mean(Long.Petalo, na.rm = T), 
            Long.Petalo.Prom_DS = sd(Long.Petalo, na.rm = T))

```

Comparando los promedios y la desviación estándar de la longitud del pétalo, existen distancias de 2 a 3 unidades entre las especies.

```{r}

datos_flores2%>%
  group_by(Especies)%>%
  summarise(Ancho.Petalo.Prom = mean(Ancho.Petalo, na.rm = T), 
            Ancho.Petalo.Prom_DS = sd(Ancho.Petalo, na.rm = T))

```

Comparando los promedios y la desviación estándar del ancho del pétalo, existen distancias de 1 a 2 unidades entre las especies.


Posteriormente, se procedió a hacer tres subsets de datos, uno por cada especie, filtrando los datos del dataframe según la especie y utilizando los corchetes angulares. En cada caso se crearon nuevos objetos que tuvieran exclusivamente los datos de cada especie.

```{r}

datos_setosa <- datos_flores2[datos_flores2$Especies == "setosa",]
datos_versicolor <- datos_flores2[datos_flores2$Especies == "versicolor",] 
datos_virginica <- datos_flores2[datos_flores2$Especies == "virginica",] 

```

Nuevamente se seleccionaron los datos referentes a la variable longitud del pétalo, excluyendo los datos faltantes (usando la función na.omit) y se crearon nuevos objetos para implicarlos en las pruebas de t. Se inició con la comparación entre las especies setosa y versicolor.

```{r}

setosa_LP <- na.omit(datos_setosa$Long.Petalo)
length(setosa_LP)
versicolor_LP<-na.omit(datos_versicolor$Long.Petalo)
length(versicolor_LP)
t.test(setosa_LP, versicolor_LP, var.equal = F, alternative = "two.sided")

```
La prueba anterior arrojó como resultado que existe diferencia estadística entre la longitud del pétalo del par de especies comparado (setosa vs. versicolor).

Enseguida, se hizo la misma comparación pero con el par de especies _setosa_ y _virginica_.

```{r}

#setosa_LP <- na.omit(datos_setosa$Long.Petalo)
length(setosa_LP)
virginica_LP<-na.omit(datos_virginica$Long.Petalo)
length(virginica_LP)
t.test(setosa_LP, virginica_LP, var.equal = F, alternative = "two.sided")

```

La prueba reveló que entre las especies _setosa_ y _virginica_ también hay diferencia entre la longitud de los pétalos.

Posteriormente, se realizó la comparación entre el par de especies _versicolor_ y _virginica_.


```{r}

t.test(versicolor_LP, virginica_LP, var.equal = F, alternative = "two.sided")

```

La prueba t mostró una vez más que hay diferencia estadística entre la longitud del pétalo entre las especies _versicolor_ y _virginica_.

Las mismas pruebas y comparaciones se realizaron pero con la variable referente al ancho del pétalo.


```{r}

setosa_AP <- na.omit(datos_setosa$Ancho.Petalo)
length(setosa_AP)
versicolor_AP<-na.omit(datos_versicolor$Ancho.Petalo)
length(versicolor_AP)
t.test(setosa_AP, versicolor_AP, var.equal = F, alternative = "two.sided")

```

```{r}

virginica_AP<-na.omit(datos_virginica$Ancho.Petalo)
length(virginica_AP)
t.test(setosa_AP, virginica_AP, var.equal = F, alternative = "two.sided")

```

```{r}

t.test(versicolor_AP, virginica_AP, var.equal = F, alternative = "two.sided")

```
Las últimas tres pruebas mostraron que también existen diferencias en el ancho del pétalo de las tres especies de estudio, siendo confirmadas las sospechas establecidas al momento de interpretar los histogramas.


##Cuarta parte: Representación gráfica que muestra la respuesta a la pregunta.

Los gráficos que puede mostrar las diferencias en la longitud y el ancho de los pétalos entre las especies son los boxplot o diagramas de caja. Para empezar, fue necesario transformar el último dataframe usando la función "mutate" de manera que la variable o columna "Especies" se reconociese por R como un factor. Esto se comprobó llamando y visualizando al data frame (datos_flores2) y observando en la descripción de la columna el código "fctr".


```{r}

datos_flores2 <- datos_flores2 %>%
  mutate(Especies = factor(Especies))#Transformación del dataframe para asignar la variable "Especies" como un factor.

datos_flores2
```

Sin embargo, también se tomó precuación verificación de los datos de especies asumidos como un factor y sus correspondientes niveles, a  partir del empleo de la función "is.factor" y posteriormente la función "levels" para conocer los niveles del factor.

```{r}

is.factor(datos_flores2$Especies)
levels(datos_flores2$Especies)

```
Se procedió a la asignación a un nuevo objeto (datos_flores2) para facilitar la elaboración de los gráficos, omitiendo los valores faltantes mediante el uso de la función "na.omit". Luego, se realizó el gráfico con los datos de longitud de pétalo mediante el uso de la función "ggplot", y especificando el relleno de las cajas con color y la representación de outliers (valores atípicos) con rojo.

```{r}

datos_flores3 <- na.omit(datos_flores2)
datos_flores3

ggplot(datos_flores3, aes(x = Especies, y = (datos_flores3$Long.Petalo), fill = Especies)) +
  geom_boxplot(outlier.colour="red")

```

De igual forma, se procedió a hacer el gráfico con los datos de ancho de pétalo.

```{r}

ggplot(datos_flores3, aes(x = Especies, y = (datos_flores3$Ancho.Petalo), fill = Especies)) +
  geom_boxplot(outlier.colour="red")

```

La visualización gráfica mostró que tanto la media como la distribución de los datos de longitud y ancho del pétalo de las flores no coincidieron, por lo cual se ratificaron las diferencias estadísticas que revelaron las pruebas de t efectuadas. 

En conclusión, la especie _virginica_ tiene los pétalos más grandes y la especie _setosa_ los más pequeños, dejando a la especie _versicolor_ como aquella de tamaño de pétalos intermedio. Estas diferencias morfológicas quizás expliquen en parte por qué a través de características de las flores las especies se consideran entidades taxonómicas independientes.


#### ¡Muchas gracias!

