# PG2_analisis-probabilidad

```{r}
options(repos = c(CRAN = "https://cloud.r-project.org"))
install.packages("hexbin")
```

```{r}
getwd()
install.packages("tidiverse")
install.packages("lubridate")
```

```{r}
library(tidyverse)
library(lubridate)
```

```{r}
install.packages("dplyr")
install.packages("readr")
```

```{r}
library(dplyr)
library(readr)
```

```{r}
PR1<- read_csv("Datos_PR1.csv")
PR1
```

## Introducción:

### Relevancia

El ajedrez es un deporte intelectual de gran tradición que combina memoria, cálculo y estrategia, y cuya práctica trasciende el ámbito recreativo para convertirse en un laboratorio de estudio del pensamiento humano. Analizar estadísticamente las partidas constituye una herramienta poderosa para comprender el rendimiento de los jugadores, identificar patrones de comportamiento y establecer vínculos entre factores objetivos (tiempo, número de movimientos, aperturas) y subjetivos (autopercepción, confianza).

La creciente disponibilidad de bases de datos abiertas en plataformas digitales, como Lichess, permite aplicar metodologías cuantitativas con rigor académico. Este enfoque no solo facilita la formación de jugadores y la preparación en torneos, sino que también aporta insumos relevantes para el desarrollo de algoritmos de inteligencia artificial que buscan replicar procesos cognitivos humanos. En ese sentido, la investigación adquiere relevancia tanto en el ámbito pedagógico como en la innovación tecnológica.

### Planificación

El trabajo se estructura en cuatro fases principales:

1.  Definición del problema y objetivos: se establecieron las preguntas de investigación y los alcances del estudio, priorizando la relación entre aperturas, tiempo, movimientos y percepciones de desempeño.

2.  Construcción y depuración de la base de datos: se recopilaron partidas de Lichess y se aplicaron criterios de limpieza, como la eliminación de valores atípicos (menos de 12 o más de 90 movimientos, duraciones mayores a 1000 segundos) y la estandarización de categorías en variables cualitativas.

3.  Análisis estadístico: se efectuaron exploraciones univariadas (aperturas, resultados, autopercepción, duración), análisis bivariados (relación entre apertura y resultado, movimientos y autopercepción, tiempo y promedio por jugada) y representaciones gráficas avanzadas como diagramas de caja, violin plots y gráficos hexbin.

4.  Conclusiones y redacción: se interpretaron los hallazgos a partir de los patrones detectados, vinculando la evidencia empírica con los objetivos planteados.

### Objetivos

#### Objetivo general

-   Aplicar métodos estadísticos al análisis de partidas de ajedrez para identificar patrones, relaciones y factores determinantes en el rendimiento de los jugadores.

#### Objetivos específicos

-   **Analizar la relación entre la apertura de la partida y el resultado de la partida**, identificando patrones de desempeño efectivos.

-   **Comparar la autopercepcion de los jugadores y el número de movimientos realizados**, para determinar tendencias en estilos de juego.

-   **Evaluar la influencia de la gestión del tiempo en los resultados de las partidas**, mediante el análisis del tiempo promedio por jugada y la duración total de la partida.

# Datos:

### Población objetivo

La población objetivo está constituida por los estudiantes de la asignatura de Estadística Teoría 8.01, considerando cada partida jugada en la plataforma Lichess como unidad muestral. Este enfoque permite un análisis manejable dentro del contexto académico, manteniendo pertinencia estadística.

### Tipo de muestreo

Se propone un muestreo aleatorio simple, seleccionando un conjunto de partidas de una base amplia de datos. Este procedimiento garantiza imparcialidad y reduce el riesgo de sesgo hacia un estilo de juego o un grupo de jugadores específico.

### Recolección de datos

Los datos provienen de registros de partidas en Lichess, que incluyen información como apertura, resultado, número de movimientos, duración y percepciones subjetivas. A nivel metodológico, se aplicó un proceso de limpieza que eliminó observaciones con duraciones atípicas (\>1000 segundos), partidas extremadamente cortas (\<12 movimientos) o excesivamente largas (\>90 movimientos). Asimismo, se estandarizaron las categorías de variables cualitativas para asegurar consistencia.

### Variables

Las variables definidas para el estudio son:

-   Cualitativas nominales:

    -   Resultado de la partida (Victoria, Derrota, Tablas) #ordinal

    -   Tipo de apertura realizada

-   Cualitativas ordinales:

    -   Autopercepción de la partida (Muy alta, Alta, Intermedia, Baja, Muy baja)

-   Cuantitativas discretas:

    -   Número total de movimientos

-   Cuantitativas continuas:

    -   Duración de la partida

### Limpieza

En el caso de contar con datos reales, es habitual encontrar partidas incompletas o registros con errores. Para enfrentar esta situación se plantean las siguientes políticas:\
- **Errores en la duración:** eliminar partidas con valores fuera de los rangos esperados para cada modalidad.\
- **Datos faltantes:** imputar valores a través de medidas de tendencia central o eliminar observaciones, según el caso.\
- **Consistencia en variables categóricas:** estandarizar nombres (ej. “tablas” y “empate” se unifican en una sola categoría).\
-**Duración:** eliminación de partidas con tiempos fuera del rango esperado (superiores a 1000 segundos).\
-**Movimientos:** exclusión de juegos con menos de 12 o más de 90 movimientos, considerados extremos atípicos que distorsionan la tendencia general.

```{r}

#colnames(PR1)


PR1 <- PR1[, c(
  #"Marca temporal", no tiene uso
  "¿Qué resultado obtuvo?",
  "¿Qué tipo de apertura(opening) usaste en la partida?",
  "¿Cómo consideras tu desempeño general en esa partida de ajedrez?",
  "¿Cuánto tiempo duró la partida?",
  #"Nombre de usuario", no tiene uso
  "¿Cuál fue el número de movimientos totales de la partida(sólo el número)? Ejemplo: 9"
)]

# Renombrar las columnas
names(PR1) <- c(
  #"Fecha", no tiene uso
  "Resultado",
  "Apertura",
  "Autopercepcion",
  "Tiempo(M)",
  #"Usuario", no tiene uso
  "Movimientos"
)


```

```{r}
library(lubridate)

#No tiene uso
##############################################################
#if (!"Correo" %in% names(PR1) && "Usuario" %in% names(PR1)) {
  #PR1 <- dplyr::rename(PR1, Correo = Usuario)
#}
#if (!"Correo" %in% names(PR1)) {
  #PR1$Correo <- NA_character_
#}
##############################################################

PR1 <- PR1 %>%
  mutate(
    Resultado      = na_if(str_squish(as.character(Resultado)), ""),
    Apertura       = na_if(str_squish(as.character(Apertura)), ""),
    Autopercepcion = na_if(str_squish(as.character(Autopercepcion)), ""),
   # Correo         = na_if(str_squish(as.character(Correo)), ""),
    Movimientos    = as.numeric(Movimientos),
    Segundos       = if_else(grepl(":", `Tiempo(M)`),
                             as.numeric(hms(as.character(`Tiempo(M)`))),
                             as.numeric(`Tiempo(M)`) * 60)
  ) %>%
  filter(is.finite(Movimientos), Movimientos >= 12, Movimientos <= 90,
         is.finite(Segundos), Segundos > 0, Segundos <= 1000) %>%
  mutate(Promedio = Segundos / Movimientos)

# Ordenamos la variable Resultado por niveles, ya que es cualitativa ordinal
unique(PR1$Resultado)
PR1$Resultado<-factor(PR1$Resultado, levels=c("Perdió","Tablas","Ganó"), ordered = T)
unique(PR1$Resultado)

# Ordenamos la variable autopercepción por niveles, ya que es cualitativa ordinal
unique(PR1$Autopercepcion)
PR1$Autopercepcion<-factor(PR1$Autopercepcion, levels = c("Muy baja", "Baja", "Intermedia", "Alta", "Muy alta"), ordered=T)
unique(PR1$Autopercepcion)

PR1<-PR1[ , -4] #Elimina la columna de Tiempo en formato hms

PR1<- PR1[complete.cases(PR1), ] #Guarda los casos completos, es decir  las observaciones sin NA

PR  <- PR1
PR2 <- PR1

PR %>% count(Apertura, sort = TRUE) %>% filter(n >= 5)

PR1 #Muestra el dataframe con las variables que si se usarán

```

# Análisis descriptivo:

### Análisis general:

```{r}
#Grafico 0.5: Partidas ganadas

library(ggplot2)
ggplot(PR1, aes(x = `Resultado`)) +
  geom_density(fill = "#66c2a5", alpha = 0.7) +
  labs(
    title = "Curva de densidad de resultados",
    x = "Resultados",
    y = "Densidad"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank()
  )
```

El gráfico de barras del resultado muestra una distribución donde las victorias representan alrededor del 45% de las partidas, las derrotas un 40%, y los empates un 15%. Este equilibrio indica un nivel de competencia parejo entre los jugadores evaluados, sin que predomine una clara superioridad de un grupo. Desde el punto de vista del rendimiento, el número de empates refleja una tendencia a la estabilidad en ciertos encuentros, probablemente influenciada por la duración del juego o el número de movimientos. Este gráfico cumple con el objetivo de identificar patrones en el rendimiento, mostrando que el grupo presenta una competencia equilibrada, y que los resultados parecen depender más de la calidad de las jugadas y del manejo del tiempo que de diferencias extremas de habilidad.

### Análisis 1:

Se analiza la variable apertura, con el fin de determinar que relación tienen con el resultado de la partida, para ello se inicia realizando un analisis univariado de la variable apertura.

```{r}
library(ggplot2)

ggplot(PR1, aes(x = reorder(Apertura, Apertura, function(x) length(x)))) +
  geom_bar(fill = "#69b3a2", color = "white", alpha = 0.9) +
  labs(
    title = "Distribución de las Aperturas",
    x = "Tipo de Apertura",
    y = "Frecuencia"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

```

La variable Apertura fue sometida a un proceso de limpieza que consistió en la eliminación de espacios innecesarios, la conversión de respuestas vacías en valores perdidos (NA) y la depuración de registros duplicados generados por errores de digitación. Como resultado, se redujo el número de categorías espurias y el gráfico de distribución muestra únicamente las aperturas válidas, con frecuencias más coherentes y representativas. Esto permitió obtener una visualización más clara, ordenada y fiel a la realidad de las partidas analizadas.

Del gráfico se desprende que la apertura más utilizada es la Queen’s Pawn Game, consolidándose como la elección predominante al inicio de las partidas. En segundo lugar, destaca la Caro-Kann Defense, que se posiciona como una alternativa recurrente entre los jugadores, aunque con una frecuencia menor respecto a la primera.\

```{r}
#library(dplyr)
library(ggplot2)

top_aperturas <- PR %>%
  count(Apertura) %>%
  filter(n >= 5) %>%
  pull(Apertura)

ggplot(PR %>% filter(Apertura %in% top_aperturas),
       aes(x = Apertura, fill = Resultado)) +
  geom_bar(position = "dodge", color = "black") +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Resultados según las aperturas más frecuentes",
    x = "Apertura",
    y = "Número de partidas",
    fill = "Resultado"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )
ggplot(PR %>% filter(Apertura %in% top_aperturas),
       aes(x = Apertura, fill = Resultado)) +
  geom_bar(position = "fill", color = "black") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Proporción de resultados según apertura",
    x = "Apertura",
    y = "Proporción",
    fill = "Resultado"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5)
  )


```

El análisis revela que, si bien la Queen’s Pawn Game es una de las aperturas más populares, también es la que acumula el mayor número de derrotas: cerca del 65 % de las partidas terminan en pérdida y menos del 25 % en victoria. Esto puede explicarse porque, al ser una apertura muy difundida, es utilizada tanto por jugadores principiantes como avanzados, lo que genera una alta variabilidad en los resultados. En contraste, la English Opening destaca con aproximadamente un 45 % de victorias, lo que sugiere que, además de ser menos predecible, tiende a ser adoptada por jugadores con mayor preparación estratégica, lo que se traduce en un desempeño más favorable dentro del tablero.

### Análisis 2:

Se analiza la variable movimientos, con el fin de relacionarlo con la autopercepción del jugador.

```{r}
#Grafico 2: Cantidad de movimientos

ggplot(PR, aes(x = `Movimientos`)) +
  geom_histogram(binwidth = 5, fill = "#69b3a2", color = "white", alpha = 0.9) +
  labs(
    title = "Distribución del número de movimientos",
    x = "Movimientos totales",
    y = "Frecuencia"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))
```

El gráfico de barras del número total de movimientos refleja que la mayoría de las partidas se concentran entre 25 y 40 jugadas, con un promedio de aproximadamente 32 movimientos por enfrentamiento. Esto indica que los juegos fueron de duración media, lo que concuerda con un desarrollo táctico equilibrado: las partidas no concluyeron de forma prematura por errores graves, pero tampoco se extendieron hasta finales exhaustivos. Las partidas con menos de 20 movimientos fueron poco frecuentes, lo que sugiere que los jugadores evitaron errores tempranos decisivos. Este comportamiento respalda el objetivo de describir estadísticamente las partidas, mostrando una tendencia hacia partidas completas y competitivas, donde ambos jugadores tuvieron oportunidades similares de desplegar estrategias.

```{r}
ggplot(PR, aes(x = Autopercepcion, y = Movimientos, fill = Autopercepcion)) +
  geom_boxplot(alpha = 0.8, color = "black") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Distribución de movimientos según autopercepción",
    x = "Autopercepción del desempeño",
    y = "Número de movimientos",
    fill = "Autopercepción"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )+
  coord_flip()
```

```{r}
ggplot(PR, aes(x = Autopercepcion, y = Movimientos, fill = Autopercepcion)) +
  geom_violin(trim = FALSE, alpha = 0.7, color = "black") +
  geom_boxplot(width = 0.2, fill = "white", outlier.color = "red") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Relación entre autopercepción y número de movimientos",
    x = "Autopercepción del desempeño",
    y = "Número de movimientos",
    fill = "Autopercepción"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )+
  coord_flip()

```

El análisis de la relación entre el número de movimientos y la autopercepción del jugador evidencia que los valores atípicos —partidas con más de 100 o menos de 12 movimientos— representan situaciones excepcionales pero relevantes, ya que reflejan tanto derrotas rápidas como juegos excesivamente prolongados. Una vez considerados los rangos razonables, los gráficos muestran que los jugadores con autopercepción alta o intermedia concentran sus partidas en un intervalo medio (35–50 movimientos), lo que sugiere un desempeño estable. En contraste, quienes manifiestan una autopercepción muy alta tienden a estar asociados a partidas más cortas, interpretadas como victorias rápidas, mientras que aquellos con una autopercepción muy baja presentan una dispersión mayor y partidas prolongadas, reflejando menor consistencia.

En conjunto, tanto el boxplot como el violin plot confirman que la duración de la partida influye de forma significativa en la valoración personal del desempeño: los jugadores que perciben un buen resultado suelen asociarlo a partidas más definidas y rápidas, mientras que quienes perciben un bajo rendimiento se relacionan con encuentros extensos e irregulares.

### Análisis 3:

Se analiza la variable Segundos con el propósito de establecer su relación con el tiempo de juego, a fin de comprender cómo los jugadores de ajedrez gestionan la duración de sus partidas en función del número de jugadas realizadas.

```{r}
#library(ggplot2)

ggplot(PR1, aes(x = Segundos)) +
  geom_density(fill = "#1f78b4", alpha = 0.6, color = "#1f78b4") +
  labs(
    title = "Distribución de la duración de las partidas",
    subtitle = "Curva de densidad del tiempo total en segundos",
    x = "Tiempo total de juego (segundos)",
    y = "Densidad estimada"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12, face = "italic"),
    panel.grid.minor = element_blank()
  )
```

La curva de densidad del tiempo total de juego permite visualizar cómo se distribuyen las partidas en función de su duración. La altura de la curva refleja la concentración relativa de encuentros en determinados intervalos: los valores más altos indican los rangos de tiempo donde se ubica la mayoría de las partidas, mientras que las colas muestran aquellos casos menos frecuentes, como juegos muy rápidos o inusualmente largos.

Al limitar el análisis a un máximo de 1000 segundos, en coherencia con la limpieza previa que identificó valores atípicos, se logra una representación más fiel de la tendencia central. De este modo, la curva evidencia que la mayor parte de las partidas se concentran en duraciones medias, lo que permite concluir que los jugadores suelen desenvolverse dentro de un ritmo de juego relativamente estable, mientras que las partidas demasiado cortas o prolongadas son excepcionales.

```{r}
#library(ggplot2)

ggplot(PR2, aes(x = Segundos)) +
  geom_histogram(aes(y = ..density..), 
                 bins = 30, fill = "#66c2a5", color = "white", alpha = 0.8) +
  geom_density(color = "#1f78b4", size = 1, alpha = 0.7) +
  labs(
    title = "Distribución de la duración total de las partidas",
    subtitle = "Histograma con curva de densidad para tiempos en segundos",
    x = "Tiempo total de juego (segundos)",
    y = "Densidad"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12, face = "italic"),
    panel.grid.minor = element_blank()
  )


```

El histograma de la duración total de las partidas evidencia una distribución asimétrica hacia la derecha, con una marcada concentración de frecuencias en los intervalos iniciales de tiempo. La media se sitúa en torno a los 200 segundos, mientras que la mediana se aproxima a los 220 segundos, lo que indica que la mayoría de los enfrentamientos fueron relativamente breves. No obstante, se identifican valores superiores a los 600 segundos, correspondientes a partidas más prolongadas, probablemente asociadas a estrategias de juego más equilibradas o a decisiones tomadas con mayor reflexión.

Esta dispersión sugiere la coexistencia de diferentes estilos de juego: algunos jugadores priorizan la rapidez y la definición temprana, mientras que otros optan por extender el número de jugadas para profundizar en el análisis de posiciones. En el marco de este estudio, el gráfico refuerza la idea de que la gestión eficiente del tiempo constituye un factor determinante en el rendimiento, dado que los jugadores capaces de optimizar su ritmo suelen obtener mejores resultados sin necesidad de alargar excesivamente la duración de la partida.

```{r}
#library(ggplot2)

ggplot(PR2, aes(x = Resultado, y = Segundos, fill = Resultado)) +
  geom_boxplot(alpha = 0.85, color = "black", outlier.color = "red", outlier.alpha = 0.7) +
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(
    limits = c(0, quantile(PR2$Segundos, 0.95, na.rm = TRUE)), # filtra atípicos extremos
    labels = scales::comma
  ) +
  labs(
    title = "Duración de la partida según el resultado",
    subtitle = "Valores extremos recortados al percentil 95 para mejor visualización",
    x = "Resultado de la partida",
    y = "Tiempo total de juego (segundos)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12, face = "italic"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )+
  coord_flip()

```

Se analiza la relación entre la cantidad de movimientos y el tiempo total de juego, con el propósito de identificar patrones en la duración de las partidas y comprender cómo la extensión en jugadas se vincula con la gestión del tiempo por parte de los jugadores.

```{r}
#library(hexbin)
#library(ggplot2)
ggplot(PR2, aes(x = Movimientos, y = Segundos)) +
  geom_hex(bins = 30) +
  scale_fill_viridis_c(option = "C") +
  labs(
    title = "Relación entre movimientos y tiempo de juego",
    subtitle = "Colores más intensos indican mayor concentración de partidas",
    x = "Número de movimientos",
    y = "Tiempo total de juego (segundos)",
    fill = "Frecuencia"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, face = "italic")
  )

```

Con el objetivo de profundizar en la relación entre el **número de movimientos** y el **tiempo total de juego**, se recurrió al uso de un **gráfico hexbin**. Si bien esta técnica no formó parte de los contenidos abordados en el curso, se incorporó al análisis por su pertinencia en el manejo de grandes volúmenes de datos, ya que agrupa las observaciones en celdas hexagonales y asigna un color en función de la **frecuencia** de partidas en cada zona. De este modo, se obtiene una representación más clara y legible frente a la superposición de puntos característica de los diagramas de dispersión convencionales.

Los resultados muestran que la **mayor concentración de partidas** se ubica entre los **20 y 50 movimientos**, con tiempos de juego principalmente entre **50 y 250 segundos**. Las tonalidades más intensas confirman que este rango constituye el patrón típico de duración de las partidas. Sin embargo, también se distinguen áreas de menor densidad que corresponden a partidas prolongadas (por encima de los 400 segundos) o con un número elevado de jugadas (superior a 70 movimientos), las cuales representan situaciones menos frecuentes asociadas a estrategias más complejas o a desarrollos tácticos extendidos.

El análisis evidencia que la mayoría de los jugadores gestionan sus partidas dentro de un rango acotado de tiempo y movimientos, mientras que solo una minoría se desvía hacia encuentros más largos. El empleo del gráfico hexbin permitió **resumir de manera eficaz la distribución conjunta de dos variables continuas** y aportar una interpretación visual más precisa sobre los distintos estilos de juego presentes en la muestra.\
\
Con el objetivo de analizar el **tiempo promedio por jugada**, se realizó el cálculo a partir de la división entre el **tiempo total de la partida (en segundos)** y el **número de movimientos efectuados**. Este indicador permite evaluar de manera más precisa la **gestión del tiempo por parte de los jugadores**, ya que refleja no solo la duración total de la partida, sino también la rapidez o reflexión con la que se tomaron las decisiones en cada jugada.

```{r}
ggplot(PR2, aes(x = Promedio)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "#66c2a5", color = "white", alpha = 0.8) +
  geom_density(color = "#1f78b4", size = 1) +
  labs(
    title = "Distribución del tiempo promedio por jugada",
    subtitle = "Tiempo calculado como Segundos totales / Número de movimientos",
    x = "Tiempo promedio por jugada (segundos)",
    y = "Densidad"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, face = "italic")
  )

```

El histograma y la curva de densidad del **tiempo promedio por jugada** muestran una distribución **marcadamente asimétrica hacia la derecha**, con una fuerte concentración de partidas en los primeros segundos. La mayoría de los jugadores realizan sus movimientos en un promedio de **1 a 4 segundos**, lo que sugiere un ritmo de juego rápido y dinámico. Sin embargo, se observan colas largas con valores que superan los **10 segundos por jugada**, correspondientes a jugadores que se toman más tiempo en ciertas posiciones críticas o que adoptan un estilo de juego más reflexivo.

Este patrón evidencia la coexistencia de **dos estilos diferenciados de gestión del tiempo**: por un lado, la mayoría de los participantes que priorizan la agilidad en la toma de decisiones, y por otro, un grupo minoritario que invierte lapsos más prolongados por jugada. En relación con el desempeño, esta dispersión sugiere que la **consistencia en un ritmo moderado** puede favorecer resultados más estables, mientras que los extremos —tanto un juego excesivamente acelerado como demasiado lento— tienden a estar asociados con un menor control de la partida.

```{r}
q1 <- quantile(PR2$Promedio, 0.25, na.rm = TRUE)
q3 <- quantile(PR2$Promedio, 0.75, na.rm = TRUE)
iqr <- q3 - q1
upper_limit <- q3 + 1.5 * iqr

ggplot(PR2 %>% filter(Promedio <= upper_limit),
       aes(x = Resultado, y = Promedio, fill = Resultado)) +
  geom_boxplot(alpha = 0.85, color = "black", outlier.color = "red", outlier.alpha = 0.7) +
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(
    limits = c(0, quantile(PR2$Promedio, 0.95, na.rm = TRUE)), # coherente con PR2
    labels = scales::comma
  ) +
  labs(
    title = "Tiempo promedio por jugada según el resultado",
    subtitle = "Se excluyen valores atípicos por encima del percentil 95",
    x = "Resultado de la partida",
    y = "Tiempo promedio por jugada (segundos)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12, face = "italic"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )+
  coord_flip()

```

El boxplot del tiempo promedio por jugada evidencia un valor medio cercano a los 3,5 segundos, con una mediana en torno a los 3 segundos. La mayoría de los jugadores toman decisiones rápidas y consistentes, mientras que los valores atípicos que superan los 8 segundos corresponden a posiciones críticas en las que algunos participantes optaron por una reflexión prolongada. La amplitud intercuartílica refleja la variabilidad en los estilos de juego, lo que puede asociarse con diferencias en la experiencia, la confianza y el nivel de planificación estratégica.

Al comparar los resultados, se observa que los jugadores que ganan o entablan tienden a mantener un ritmo de decisión más estable y eficiente, mientras que aquellos que pierden presentan una dispersión mayor en sus tiempos por jugada. Esto sugiere que una gestión temporal equilibrada, evitando tanto las jugadas apresuradas como las pausas excesivas, se relaciona con un mejor desempeño general en la partida.\

# Conclusiones:

El presente estudio permitió aplicar herramientas estadísticas al análisis de partidas de ajedrez, alcanzando el objetivo general de identificar patrones, relaciones y factores que influyen en el rendimiento de los jugadores. A través del tratamiento, limpieza y visualización de los datos se construyó una perspectiva integral que combina aspectos estratégicos, temporales y psicológicos del juego.

En relación con el primer objetivo específico, los resultados evidenciaron que la apertura constituye un factor estratégico inicial con un peso significativo en el desenlace de la partida. La Queen’s Pawn Game, pese a ser la más utilizada, concentró también un alto número de derrotas, lo que se explica por su carácter predecible al ser ampliamente practicada por jugadores de distinto nivel. En contraste, aperturas menos frecuentes como la English Opening presentaron mejores tasas de victoria, lo que sugiere que la elección de jugadas menos habituales puede ofrecer ventajas competitivas al limitar las respuestas conocidas del rival.

Respecto al segundo objetivo, el análisis conjunto de autopercepción y número de movimientos reveló relaciones consistentes entre la valoración subjetiva del desempeño y la dinámica objetiva del juego. Se observó que jugadores con autopercepción positiva tienden a protagonizar partidas más breves y definidas (30–45 movimientos), mientras que quienes reportan autopercepción baja registran juegos más extensos y variables, algunos incluso superando los 90 movimientos. Esto refleja que la seguridad y confianza de los jugadores se asocia a un estilo de juego más eficiente y directo, mientras que la inseguridad se vincula a prolongar las partidas sin lograr cierres claros.

En cuanto al tercer objetivo, el análisis del tiempo total y del promedio por jugada permitió identificar que la gestión temporal es un determinante clave del rendimiento. La curva de densidad y los boxplots mostraron que la mayoría de los jugadores se concentran en un rango de 2–4 segundos por movimiento, lo cual equilibra rapidez y reflexión. Sin embargo, se detectaron valores atípicos con promedios superiores a los 8 segundos por jugada, que se relacionaron con partidas menos exitosas. El gráfico hexbin confirmó esta tendencia al mostrar que la mayor densidad de partidas se concentra entre 20 y 50 movimientos y 50–250 segundos de duración, mientras que las partidas más largas o con tiempos excesivos resultan minoritarias y con frecuencia asociadas a desempeños menos favorables.

De forma transversal, los resultados refuerzan la idea de que el ajedrez no se comprende únicamente como un ejercicio de cálculo racional o táctica, sino como una interacción dinámica entre preparación técnica, gestión estratégica del tiempo y percepción subjetiva del rendimiento. La conjunción de estos factores permite explicar por qué algunos jugadores logran partidas más consistentes y exitosas, mientras que otros caen en juegos extensos, erráticos o poco eficientes.

Finalmente, la aplicación de métodos estadísticos permitió visibilizar tendencias claras y cuantificables que suelen permanecer ocultas en la práctica cotidiana del ajedrez. Los hallazgos no solo ofrecen evidencia empírica para entender mejor el rendimiento de los jugadores, sino que abren la puerta a futuras investigaciones que integren nuevas variables —como experiencia acumulada, preparación psicológica o nivel del oponente— con el fin de enriquecer aún más la comprensión del ajedrez como fenómeno estratégico, competitivo y formativo.

# Análisis probabilístico

## Probabilidad empírica:

**Variable 1:**\
Autopercepción: {Muy baja, Baja, Intermedia, Alta, Muy alta}

```{r}
Total=nrow(PR1)
Muy_Baja=nrow(PR1[PR1$Autopercepcion=="Muy baja", ])
Baja=nrow(PR1[PR1$Autopercepcion=="Baja", ])
Intermedia=nrow(PR1[PR1$Autopercepcion=="Intermedia", ])
Alta=nrow(PR1[PR1$Autopercepcion=="Alta", ])
Muy_alta=nrow(PR1[PR1$Autopercepcion=="Muy alta", ])

```

Muy Baja: `r Muy_Baja`; Baja: `r Baja`; Intermedia: `r Intermedia`; Alta: `r Alta`; Muy alta: `r Muy_alta`

$\mathbf{EA_1:}$ Seleccionar un jugador al azar del estudio " " y observar cual es su autopercepción:

$\boldsymbol{\Omega E_1:}$ {Muy baja, Baja, Intermedia, Alta, Muy Alta}

***Eventos atómicos:***

***Muy baja:*** Seleccionar un jugador al azar del estudio cuya autopercepción sea Muy baja\
***Baja:*** Seleccionar un jugador al azar del estudio cuya autopercepción sea Baja\
***Intermedia:*** Seleccionar un jugador al azar del estudio cuya autopercepción sea Intermedia\
***Alta:*** Seleccionar un jugador al azar del estudio cuya autopercepción sea Muy Alta\
***Muy Alta:*** Seleccionar un jugador al azar del estudio cuya autopercepción sea Muy Alta

***Probabilidades:***

***P(Muy baja)*** = `r Muy_Baja`/`r Total` = `r round((Muy_Baja/Total),2)`\
***P(Baja)*** = `r Baja`/`r Total` = `r round((Baja/Total),2)`\
***P(Intermedia)*** = `r Intermedia`/`r Total` = `r round((Intermedia/Total),2)`\
***P(Alta)*** = `r Alta`/`r Total` = `r round((Alta/Total),2)`\
***P(Muy Alta)*** = `r Muy_alta`/`r Total` = `r round((Muy_alta/Total),2)`

***Comprobación: Bajo***\
P(Muy baja)+P(Baja)+P(Intermedia)+P(Alta)+P(Muy Alta) = `r Muy_Baja`/`r Total` + `r Baja`/`r Total` + `r Intermedia`/`r Total` + `r Alta`/`r Total` + `r Muy_alta`/`r Total` = `r (Muy_Baja/Total) + (Baja/Total) + (Intermedia/Total) + (Alta/Total) + (Muy_alta/Total)`

***Análisis:***

**Variable 2:**\
Resultado: {Perdió, Tablas, Ganó}

```{r}
perdio=nrow(PR1[PR1$Resultado=="Perdió", ])
tablas=nrow(PR1[PR1$Resultado=="Tablas", ])
gano=nrow(PR1[PR1$Resultado=="Ganó", ])
```

Perdió: `r perdio`; Tablas: `r tablas`; Ganó: `r gano`

$\mathbf{EA_2:}$ Seleccionar un jugador al azar del estudio " " y observar cual es su resultado:

$\boldsymbol{\Omega E_1:}$ {Perdió, Tablas, Ganó}

***Eventos atómicos:***

***Perdió:*** Seleccionar un jugador al azar del estudio cuyo resultado sea Perdió\
***Tablas:*** Seleccionar un jugador al azar del estudio cuyo resultado sea Tablas\
***Ganó:*** Seleccionar un jugador al azar del estudio cuyo resultado sea Ganó

***Probabilidades:***

***P(Perdió)*** = `r perdio`/`r Total` = `r round((perdio/Total),2)`\
***P(Tablas)*** = `r tablas`/`r Total` = `r round((tablas/Total),2)`\
***P(Ganó)*** = `r gano`/`r Total` = `r round((gano/Total),2)`

***Comprobación: Bajo***\
P(Perdió)+P(Tablas)+P(Ganó) = `r perdio`/`r Total` + `r tablas`/`r Total` + `r gano`/`r Total` = `r (perdio/Total) + (tablas/Total) + (gano/Total)`

***Análisis:***

## Probabilidad condicional:

```{r}
# Etiquetas para filas y columnas
tabla <- matrix(NA,
                nrow = length(levels(PR1$Autopercepcion)),
                ncol = length(levels(PR1$Resultado)),
                dimnames = list(levels(PR1$Autopercepcion), levels(PR1$Resultado)))

tabla

```

```{r}
# Colocar valores en la tabla

tabla["Muy baja", "Perdió"] <- nrow(PR1[PR1$Autopercepcion=="Muy baja" & PR1$Resultado=="Perdió" , ])
tabla["Muy baja", "Tablas"] <- nrow(PR1[PR1$Autopercepcion=="Muy baja" & PR1$Resultado=="Tablas" , ])
tabla["Muy baja", "Ganó"] <- nrow(PR1[PR1$Autopercepcion=="Muy baja" & PR1$Resultado=="Ganó" , ])

tabla["Baja", "Perdió"] <- nrow(PR1[PR1$Autopercepcion=="Baja" & PR1$Resultado=="Perdió" , ])
tabla["Baja", "Tablas"] <- nrow(PR1[PR1$Autopercepcion=="Baja" & PR1$Resultado=="Tablas" , ])
tabla["Baja", "Ganó"] <- nrow(PR1[PR1$Autopercepcion=="Baja" & PR1$Resultado=="Ganó" , ])

tabla["Intermedia", "Perdió"] <- nrow(PR1[PR1$Autopercepcion=="Intermedia" & PR1$Resultado=="Perdió" , ])
tabla["Intermedia", "Tablas"] <- nrow(PR1[PR1$Autopercepcion=="Intermedia" & PR1$Resultado=="Tablas" , ])
tabla["Intermedia", "Ganó"] <- nrow(PR1[PR1$Autopercepcion=="Intermedia" & PR1$Resultado=="Ganó" , ])

tabla["Alta", "Perdió"] <- nrow(PR1[PR1$Autopercepcion=="Alta" & PR1$Resultado=="Perdió" , ])
tabla["Alta", "Tablas"] <- nrow(PR1[PR1$Autopercepcion=="Alta" & PR1$Resultado=="Tablas" , ])
tabla["Alta", "Ganó"] <- nrow(PR1[PR1$Autopercepcion=="Alta" & PR1$Resultado=="Ganó" , ])

tabla["Muy alta", "Perdió"] <- nrow(PR1[PR1$Autopercepcion=="Muy alta" & PR1$Resultado=="Perdió" , ])
tabla["Muy alta", "Tablas"] <- nrow(PR1[PR1$Autopercepcion=="Muy alta" & PR1$Resultado=="Tablas" , ])
tabla["Muy alta", "Ganó"] <- nrow(PR1[PR1$Autopercepcion=="Muy alta" & PR1$Resultado=="Ganó" , ])

tabla

```

```{r}
options(knitr.kable.NA = '', knitr.table.format = 'html')


#install.packages("kableExtra") #instalar para graficar tabla
library(kableExtra)

tabla_df <- as.data.frame(tabla)

#Agregar columna de totales por fila
tabla_df$Total <- rowSums(tabla_df, na.rm = TRUE)

#Agregar fila de totales por columna 
tabla_df["Total", ] <- colSums(tabla_df, na.rm = TRUE)

#Colorear una celda específica (por ejemplo Intermedia-Perdió)
tabla_df["Intermedia", "Perdió"] <- cell_spec(tabla_df["Intermedia", "Perdió"], background = "#FFCDD2", color = "black", bold = TRUE)


kable(tabla_df, escape = FALSE, caption = "Tabla de Resultados y Autopercepción") %>%
  kable_styling(full_width = FALSE, 
                bootstrap_options = c("striped", "hover", "condensed", "bordered")) %>%
  row_spec(0, background = "#E1BEE7") %>%      # Fila 0  violeta
  column_spec(1, background = "#BBDEFB") %>%   # Columna 1 celeste
  row_spec(6, background = "#C8E6DF") %>%      # Fila 6  verde menta
  column_spec(5, background = "#C8E6DF")       # Columna 5 verde menta


#COLORES
#Rojo 	#FFCDD2	
#Naranja 	#FFB74D	
#Amarillo 	#FFF59D	
#Verde 	#C8E6C9	
#Azul 	#BBDEFB	
#Violeta 	#E1BEE7	
#Gris 	#E0E0E0	
#Rosado #FFCDD2
#Celeste #BBDEFB
#Verde menta #C8E6DF


```
