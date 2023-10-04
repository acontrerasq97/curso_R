---
title: "Ejercicio"
author: "Diana"
date: "2023-09-27"
output:
  pdf_document: default
  html_document: default
  word_document: default
---

###Ejercicio de entrega

Cargar datos

```{r}
getwd()
Datos_Flores<-read.csv2("Tabla_especies_NA.csv", header = TRUE, na=c("NA","na","","N/A"))
```

LIbrerias
```{r}
library(tidyverse)
library(ggplot2)

```



Descripcion
```{r}
str(Datos_Flores)
```


```{r}
names(Datos_Flores)
```
```{r}
Datos_Flores$Sepal.Length<-as.numeric(Datos_Flores$Sepal.Length)
Datos_Flores$Sepal.Width<-as.numeric(Datos_Flores$Sepal.Width)
Datos_Flores$Petal.Length<-as.numeric(Datos_Flores$Petal.Length)
Datos_Flores$Petal.Width<-as.numeric(Datos_Flores$Petal.Width)
str(Datos_Flores)
```
```{r}
head(Datos_Flores)
```



```{r}
sum(is.na(Datos_Flores))
colSums(is.na(Datos_Flores)) # Cantidad de datos faltantes 
```
```{r}
Datos_Flores <- Datos_Flores %>%
  rename(Sepalo_largo_cm=Sepal.Length,Sepalo_ancho_cm=Sepal.Width, Petalo_largo_cm=Petal.Length,Petalo_ancho_cm=Petal.Width, Especie=Species)

Datos_Flores # se acomodaron los nombres a Español
```

Factor

```{r}
names(Datos_Flores)
```

```{r}
columnas_flores<-c("Especie","Sepalo_largo_cm" ,"Sepalo_ancho_cm","Petalo_largo_cm","Petalo_ancho_cm")
Flores<-Datos_Flores[,columnas_flores]
Flores
```


Caraterizacion General de los datos
```{r}
nrow(Flores)
Tablaespecie<-table(Datos_Flores$Especie)
Tablaespecie
```

```{r}
dim(Flores)
class(Flores)
length(Flores)
```

¿el largo de los petalos es diferente de acuerdo a la especie? 


```{r}
Flores %>% 
  count(Especie)
```


Media y desviacion de la longitud del petalo por especie 

```{r}
Flores %>%
  group_by(Especie) %>% 
  summarise(long_Prom = mean(Petalo_largo_cm, na.rm = T),
            long_ds = sd(Petalo_largo_cm, na.rm = T))
```

Debido a que quiero comparar tres grupos con una caracteristica de mi interes realizo un ANOVA 

```{r}
Res_ANOVA <- aov(Petalo_largo_cm ~ Especie, data = Flores)
summary(Res_ANOVA)
```
De acuerdo con el valor P se rechaza la hipotesis nula (el largo es igual entre las especies), por lo que se evidencia que existe diferencias entre el largo del petalo dependiendo dela especie.

```{r}
Res_Tukey<- TukeyHSD(Res_ANOVA,"Especie")
print(Res_Tukey)
```
Al visualizar los rangos de confianza se ve claramente que existen diferencias estadisticamente significativa entre todas las especies.Pues ninguno pasa por el cero. Por ende, si existe una diferencia entre el largo de la especie dependiendo la especie.

```{r}
plot(Res_Tukey)
```





Visualizacion de los datos


```{r}
 boxplot(Flores$Petalo_largo_cm ~ Flores$Especie)
```

Al visulizar los datos de menera general, se observa que las longitudes mas cercanas se encuentran entre vesicolor y virginica, los cuales presentaron un intervalo en la prueba de mas cercano al cero.

```{r}
Flores%>%
  ggplot(aes(x=Especie, y=Petalo_largo_cm, fill=Especie, color = Especie))+
  geom_point(shape = 19, size = 4, alpha =  0.8,position = position_jitter(0.2)) + 
  scale_colour_manual(values = c("blue","purple","green"))
                                                                       
#visualizamos de manera general los datos del largo del petalo
```



Pos ultimo se personalizo la visualizacion de los datos, para observar la dispercion de los datos. 

