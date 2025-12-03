# Google-data-analyst-proyect---Cyclistic-bike-share

En este proyecto se analizaron los datos del caso de estudio Cyclistic Bike-Share en RStudio para identificar diferencias entre usuarios casuales y miembros. Se realizaron tareas de limpieza, unificación de datos, creación de variables y visualizaciones que permitieron generar recomendaciones estratégicas para aumentar membresías y su rentabilidad.

## Cyclistic bike-share

Desde sus inicios en 2016, Cyclistic bike-share es una empresa dedicada a ofrecer un servicio de alquiler 
de bicicletas en la ciudad de Chicago. Su crecimiento la llevó a contar con un total de 5.824 bicicletas 
con geolocalización y 692 estaciones distribuidas por la ciudad. Además, ofrece la comodidad de que 
las unidades pueden desbloquearse en una estación y devolverse en otra, en cualquier momento. 
Entre los planes disponibles se encuentran dos: ocasionales y anuales. Los planes ocasionales incluyen 
viajes únicos o pases diarios, y quienes lo utilizan se considerados usuarios ocasionales. Por otro lado, 
los planes anuales abarcan viajes durante todo el año, y sus usuarios son denominados miembros. 

## Desafíos

A raíz de estudios realizados por el sector financiero, se determinó que los usuarios miembros son más
 rentables que los usuarios ocasionales. Por este motivo, y con el fin de maximizar los ingresos, la 
compañía quiere implementar estrategias que impulsen un incremento en la cantidad de los usuarios 
miembros.  
Para ello, resulta esencial analizar cómo utilizan el servicio ambos grupos de usuarios y qué factores 
podrían influir en su decisión de convertirse en miembros. 

## Enfoque especifico del proyecto

Como se mencionó anteriormente, el proyecto tiene como fin la obtención de información relevante 
para la toma de decisiones que impulsen el desarrollo de la compañía. 
Para ello, se establecieron tres preguntas guía que permitirán definir las futuras estrategias de manera 
efectiva: 
1. ¿De qué manera utilizan las bicicletas los usuarios miembros y los usuarios ocasionales? 
2. ¿Por qué los usuarios ocasionales deberían adquirir una membresía anual? 
3. ¿Cómo puede Cyclistic utilizar los medios digitales para influir en que los usuarios ocasionales
 se conviertan en miembros? 

Este caso de estudio se centrará puntualmente en la primera pregunta del proyecto.

________________________________________________________
## BUSINESS TASK: 
### ANALIZAR LAS DIFERENCIAS DE USO ENTRE USUARIOS CASUALES Y MIEMBROS ANUALES PARA DISEÑAR ESTRATEGIAS QUE AUMENTEN LA CONVERSIÓN DE USUARIOS CASUALES A MIEMBROS
________________________________________________________

## Herramientas y fuente de los datos 
Para esta tarea se utilizó como herramienta principal RStudio para el análisis de los datos. Puesto que 
el universo de datos era demasiado extenso, se tomaron únicamente dos trimestres de distintos años.
 De esta forma, se obtiene una muestra significativa y se evidencia que la tendencia se mantiene a lo 
largo del tiempo. Cabe mencionar que los datos provienen de Motivate International Inc. bajo licencia 
pública y no contienen información personal identificable, lo que garantiza el cumplimiento de los 
requisitos de privacidad del caso. 

Además, los conjuntos de datos utilizados pueden consultarse públicamente en el siguiente enlace: 
https://www.kaggle.com/datasets/vipulgohel/cyclistic-bikeshare-analysis-case-study

## Composición de la Muestra 
En este caso de estudio, se utilizó la información histórica correspondiente al primer trimestre de 2019 
y el primer trimestre de 2020. Sin embargo, se notó que había viajes que superaban la duración de 24 
horas. Al no poder determinarse si se trataban de errores o bien casos aislados, se decidió reducir la 
muestra de manera tal que solo se contemplen en el análisis viajes que se encuentren dentro de 
determinados parámetros. Si bien podrían existir viajes de mayor duración, se determinó como criterio
 para el análisis únicamente viajes que no superen las 12 horas. 
A continuación, realizaremos una comparación entre la muestra original y la muestra reducida para 
demostrar que la reducción no significa un cambio que pueda afectar al estudio en cuestión. 

Tareas de importación, unificación y limpieza de datos:

```r
#Instala paquetes necesarios para trabajar
library(tidyverse)
library(lubridate)
library(ggplot2)
```
``` r
#Importa archivos csv
Q1_2019 <- read.csv("data/Divvy_Trips_2019_Q1.csv")
Q1_2020 <- read.csv("data/Divvy_Trips_2020_Q1.csv")

```
```r
#Lista cuadros para control
head(Q1_2019)
head(Q1_2020)
```
```r 
#Estandariza nombres
Q1_2019 <- Q1_2019 %>% 
  rename(
    ride_id = trip_id,
    started_at = start_time,
    ended_at = end_time,
    start_station_id = from_station_id,
    start_station_name = from_station_name,
    end_station_id = to_station_id,
    end_station_name = to_station_name,
    member_casual = usertype
  )
```
``` r
#Selecciona columnas que se utilizaran para el analisis
Q1_2019v2 <- Q1_2019 %>% 
    select(
      ride_id, 
      started_at, 
      ended_at,
      start_station_id, 
      start_station_name, 
      end_station_id, 
      end_station_name, 
      member_casual)
Q1_2020v2 <- Q1_2020 %>% 
    select(
      ride_id, 
      started_at , 
      ended_at, 
      start_station_id, 
      start_station_name, 
      end_station_id, 
      end_station_name, 
      member_casual )
```
```r
#Unifica criterios
Q1_2019v2 <- Q1_2019v2 %>% 
    mutate(ride_id = as.character(ride_id))
Q1_2020v2 <- Q1_2020v2 %>% 
    mutate(ride_id = as.character(ride_id))
```
```r
#Lista cuadros para control
head(Q1_2019v2)
head(Q1_2020v2)
```
```r
#Unifica tablas
all_trips <- bind_rows(Q1_2019v2, Q1_2020v2)

#Lista cuadro para control
head(all_trips)
```
```r
#Unifica criterios
all_trips <- all_trips %>%
  mutate(
    member_casual = recode(member_casual, 
    "Subscriber" = "member", "Customer" = "casual")) 
```
```r
#Lista cuadro para control
head(all_trips)
```
```r
#Crea tabla con el universo completo de datos 
all_tripscompleto <- all_trips %>% 
  mutate(
    started_at = ymd_hms(started_at),
    ended_at = ymd_hms(ended_at),
    ride_length = as.numeric(difftime(ended_at, started_at, units = "mins")),
    day_of_week = wday(started_at, label = TRUE)
  )

#Cuenta usuarios para grafico Nro 1
user_countscompletos <- all_tripscompleto %>%
  group_by(member_casual) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100,
         label = paste0(member_casual, " ", round(percent,1), "%"))

#Grafico 1: grafico de torta representativo de la muestra completa
ggplot(user_countscompletos, aes(x = "", y = percent, fill = member_casual)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
  labs(
      title = "Usuarios totales") +
  theme_void()+
  scale_fill_manual(
      values = c("member" = "#B1E7D6", "casual" = "#FFFF99"))+
  theme(
      plot.title = element_text(color = "#082A75", size = 16, face = "bold"))
```
![Usuarios totales](img/usuarios_totales.png)

```r
#Crea nueva tabla depurada
all_trips <- all_trips %>% 
  mutate(
    started_at = ymd_hms(started_at),
    ended_at = ymd_hms(ended_at),
    ride_length = as.numeric(difftime(ended_at, started_at, units = "mins")),
    day_of_week = wday(started_at, label = TRUE)
  ) %>%
  filter(ride_length > 1 & ride_length < 720)  
  
  # Se reduce el estudio a viajes razonables, si el viaje dura menos de 1 min o mas de 12horas se tratan de posibles errores o casos aislados

#Incluye columna year a tabla all_trips
all_trips <- all_trips %>%
  mutate(year = year(started_at))

#Cuenta usuarios para grafico Nro 2
user_counts <- all_trips %>%
  group_by(member_casual) %>%
  summarise(count = n()) %>%
  mutate(percent = count / sum(count) * 100,
         label = paste0(member_casual, " ", round(percent,1), "%"))

#Grafico 2: grafico de torta representativo de la muestra depurada
ggplot(user_counts, aes(x = "", y = percent, fill = member_casual)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
  labs(
      title = "Usuarios parciales") +
  theme_void()+
  scale_fill_manual(
      values = c("member" = "#B1E7D6", "casual" = "#FFFF99"))+
  theme(
      plot.title = element_text(color = "#082A75", size = 16, face = "bold"))
```
![Usuarios parciales](img/usuarios_parciales.png)

Como se visualiza en los gráficos de torta, a pesar de las modificaciones introducidas no se observaron cambios significativos en la muestra. La participación de los usuarios casuales varía en menos de 0,4 puntos porcentuales, mientras que los usuarios miembros aumentan en la misma proporción.
Esto indica que el filtrado no introduce sesgos ni alteraciones en la muestra; por el contrario, la muestra es más representativa y consistente con el comportamiento global al eliminar valores extremos o poco usuales. En conclusión, la limpieza de los datos mejora su calidad para el análisis.

## Comparativa de viajes entre usuarios
Para este estudio, la muestra final utilizada ascendió a un total de 783.363 viajes, de los cuales 67.171 corresponden a usuarios casuales y 716.192 a usuarios miembro, representando un 8.6% y 91.4% respectivamente. De estos datos se extrajo que el valor promedio y mediana para los usuarios casuales es de 35.1 minutos y 23.1 minutos, mientras que para los miembros es de 11.2 minutos y 8.5 minutos respectivamente como se puede visualizar en el siguiente gráfico. Con respecto a los valores máximos en minutos por viaje, se visualizan cifras similares entre ambos grupos; sin embargo, estos valores podrían corresponder a casos atípicos, por lo que no pueden obtener observaciones definitivas.

```r
#Promedio de duración por tipo de usuario
all_trips %>% 
  group_by(member_casual) %>%
  summarise(
    mean_ride = mean(ride_length),
    median_ride = median(ride_length),
    max_ride = max(ride_length),
    trips = n()
  )
```
```r
#Resumen estadístico para grafico nro 3
summary_stats <- all_trips %>%
  group_by(member_casual) %>%
  summarise(
    mean_ride = mean(ride_length, na.rm = TRUE),
    median_ride = median(ride_length, na.rm = TRUE),
    max_ride = max(ride_length, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(mean_ride, median_ride, max_ride),
               names_to = "statistic",
               values_to = "value")
```
```r
# Gráfico 3: 
ggplot(summary_stats, aes(x = member_casual, y = value, fill = statistic)) + #Grafico, de que tabla toma la info
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +      #tipo de grafico - barras
  geom_text(aes(label = round(value, 1)),                                    #texto dentro del grafico
            position = position_dodge(width = 0.7), vjust = -0.5, size = 3.5) +
  labs(
    title = "Comparativa de viajes entre usuarios",
    x = "Tipo de usuario",
    y = "Duración del viaje (minutos)",
    fill = "Estadística"
  ) +
  theme_minimal()+theme(
    plot.title = element_text(color = "#082A75", size = 16, face = "bold"))+
  scale_fill_manual(values = c("max_ride" = "#BDD242", "mean_ride" = "#4FC5BA", "median_ride" = "#FFFF99"))
  ```
![Comparativa de viajes entre usuarios](img/Comparativa_viajes_usuarios.png)

De esta manera, se puede deducir que los usuarios miembros realizan más viajes, pero de menor duración, mientras que los casuales realizan menos cantidad de viajes, aunque significativamente más largos. Es probable, entonces, que los miembros utilicen el servicio para transportarse de manera cotidiana, como ir a trabajar o recorridos breves, mientras que los usuarios casuales utilizan el servicio en ocasiones esporádicas, vinculadas con actividades de ocio, turismo o paseos recreativos.

Esto se evidencia en el siguiente grafico de densidad:

```r
#Filtra viajes mayores a 120 min para grafico Nro 4
all_trips_filtered <- all_trips %>% filter(ride_length <= 120)

#Gráfico 4:Densidad por tipo de usuario
ggplot(all_trips_filtered, aes(x = ride_length, fill = member_casual)) +
  geom_density(alpha = 0.5) +
  labs(
      title = "Duración de viaje por Tipo de Usuario",
      x = "Duración del viaje (minutos)",
      y = "Densidad",
      fill = "Tipo de Usuario") +
  theme_minimal() +
  scale_fill_manual(
      values = c("casual" = "#F9C74F", "member" = "#4FC5BA")) +
  theme(
      legend.position = "top")+ 
  theme(
      plot.title = element_text(color = "#082A75",size = 16,face = "bold",hjust = 0.5))
```
![Duracion de viajes por usuarios](img/Duracion_viaje_por_usuario.png)

## Patrón de viajes por día de la semana
Al observar el comportamiento de los miembros y los usuarios casuales, se pueden identificar diferencias significativas. Por un lado, si bien los miembros demandan el servicio todos los días, la demanda se centra en días de semana con un gran volumen de viajes que sugieren un uso funcional, probablemente vinculado con desplazamientos por actividades diarias. Por otro lado, los usuarios casuales emplean el servicio principalmente durante los fines de semana, lo que indicaría patrones vinculados al ocio o actividades recreativas.

```r
#Grafico 5: Nro de viajes por dia por usuario
ggplot(
    all_trips %>%
    group_by(member_casual, day_of_week) %>%
    summarise(num_trips=n(), .groups="drop"),aes(x=day_of_week, y=num_trips, fill=member_casual)) +
  geom_col(position="dodge") + 
  scale_y_continuous(
      labels = scales::comma,
      breaks = seq(0, 200000, 25000))+
  labs(
      title="Número de viajes por día de la semana", 
      x="Día", 
      y="Cantidad de viajes")+
  theme_minimal()+
  theme(
    plot.title = element_text(color = "#082A75", size = 16, face = "bold"))+
  scale_fill_manual(
      values = c( "casual" = "#FFeF99", "member" = "#4FC5BA"))
```
![Numero de viajes por dia de la semana](img/viajes_por_semana.png)


## Distribución de viajes por hora del día
Al analizar las franjas horarias de demanda, se puede notar que los comportamientos difieren nuevamente entre los distintos usuarios, aunque siguiendo una misma tendencia. Reforzando la idea desarrollada anteriormente, donde se mencionó que el servicio es utilizado como medio de transporte regular y funcional, los usuarios miembros muestran picos de demanda alrededor de las 8:00-9:00 horas y nuevamente entre las 17:00 y 18:00 horas, coincidiendo con los horarios laborales de ingreso y salida. Por su parte, los usuarios casuales presentan demanda en horarios más dispersos, especialmente durante las tardes y primeras horas de la noche, lo que indica una utilización del servicio más recreativo, vinculado con actividades de ocio o de turismo, como ya se mencionó previamente.

```r
#Patrón de viajes por hora del día para grafico Nro 6
all_trips <- all_trips %>%
  mutate(start_hour = lubridate::hour(started_at))

hourly_usage <- all_trips %>%
  group_by(member_casual, start_hour) %>%
  summarise(num_trips = n(), .groups = "drop")

#Grafico 6:
ggplot(hourly_usage,aes(x = start_hour, y = num_trips, color = member_casual)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Viajes por horas del día",
    x = "Hora del día",
    y = "Cantidad de viajes",
    color = "Tipo de usuario") +
  scale_x_continuous(
      breaks = seq(0, 23, by = 3),
      labels = sprintf("%02d:00",seq(0, 23, by = 3))) +
  scale_color_manual(values = c( "casual" = "#F9C74F", "member" = "#4FC5BA"))+
  theme_minimal(base_size = 14) + 
  theme(
      plot.title = element_text(color = "#082A75",size = 16,face = "bold", hjust = 0.5),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
      axis.text.y = element_text(angle = 45, vjust = 1, hjust = 1))
```
![Viajes por horas del dia](img/viajes_por_horas_del_dia.png)

## Resumen de tareas realizadas
En el presente caso de estudio, se llevaron a cabo diversas tareas en RStudio relacionadas con la preparación y limpieza de datos. En primera instancia, se trabajó con la tabla correspondiente a 2019 de manera tal que se adaptara al formato de la tabla 2020 para posteriormente realizar su unificación. A partir de esta nueva tabla unificada, se hicieron modificaciones que incluyeron:

- Normalización de clasificación de usuarios
- Creación de nuevas variables:
    - Duración de los viajes,
    - Dia de la semana,
    - Hora de inicio,
    - Año
- Análisis descriptivo y visualización de:
    - Distribución de usuarios por tipo,
    - Patrones de viaje por día de la semana y hora del día,
    - Duración de los viajes y posibles valores atípicos.
- Reducción de la muestra contemplando solo valores considerados razonables.
- Ajuste de formatos de hora y fecha para garantizar consistencia en el análisis.

## Recomendaciones
Este análisis evidenció diferencias claras en los patrones de comportamiento de los usuarios, lo cual permite identificar oportunidades para incrementar la conversión hacia membresías anuales. En este sentido, la compañía debería implementar estrategias focalizadas en los fines de semana, en actividades vinculadas al turismo y alianzas comerciales. Para ello se sugieren las siguientes acciones:

- Ofrecer pases anuales para fines de semana: Pases que incluyan únicamente viernes, sábados y domingos, con precios atractivos que resulten más convenientes que adquirir pases por tramo o por día.
- Desarrollar pases combinados con actividades turísticas: Paquetes que integren viajes en bicicleta con experiencias demandadas por los turistas, acompañados de descuentos obtenidos a través de convenios con empresas del sector.
- Crear alianzas con empresas de turismo: Acuerdos donde dichas empresas ofrezcan menores precios en sus servicios, mientras que Cyclistic a cambio otorgue descuentos para que los miembros de estas empresas puedan utilizar las unidades en su vida cotidiana. Esta alternativa introduce la posibilidad de atraer nuevos clientes con potencial de convertirse en miembros.

Sin dudas, la aplicación de iniciativas orientadas en este sentido contribuirá al aumento de la conversión de usuarios casuales a miembros, traduciéndose en mejoras significativas en la rentabilidad del programa y en un crecimiento sostenido de Cyclistic.

## Documentación del caso de estudio
El detalle de los requerimientos, el análisis de la información, los gráficos y las recomendaciones finales se encuentran en el siguiente PDF:


[Descripcion y requerimientos del caso de estudio](./Case_Study_1_How_does_a_bike_share_navigate_speedy_success.pdf)

[Resolucion caso de estudio](./Caso_de_estudio_Cyclistic_bike_share.pdf)

[Base de datos](https://www.kaggle.com/datasets/vipulgohel/cyclistic-bikeshare-analysis-case-study)
