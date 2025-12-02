#Instala paquetes necesarios para trabajar
library(tidyverse)
library(lubridate)
library(ggplot2)

#Importa archivos csv
Q1_2019 <- read.csv("data/Divvy_Trips_2019_Q1.csv")
Q1_2020 <- read.csv("data/Divvy_Trips_2020_Q1.csv")

#Lista cuadros para control
head(Q1_2019)
head(Q1_2020)

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

#Selecciona columnas que se utilizaran para el analisis
Q1_2019v2 <- Q1_2019 %>% 
select(ride_id, started_at , ended_at, start_station_id, start_station_name, end_station_id, end_station_name, member_casual )
Q1_2020v2 <- Q1_2020 %>% 
select(ride_id, started_at , ended_at, start_station_id, start_station_name, end_station_id, end_station_name, member_casual )

#Unifica criterios
Q1_2019v2 <- Q1_2019v2 %>% mutate(ride_id = as.character(ride_id))
Q1_2020v2 <- Q1_2020v2 %>% mutate(ride_id = as.character(ride_id))

#Lista cuadros para control
head(Q1_2019v2)
head(Q1_2020v2)

#Unifica tablas
all_trips <- bind_rows(Q1_2019v2, Q1_2020v2)

#Lista cuadro para control
head(all_trips)

#Unifica criterios
all_trips <- all_trips %>%
  mutate(member_casual = recode(member_casual,
                                "Subscriber" = "member",
                                "Customer" = "casual")) #unifica criterio de clasificacion de clientes

#Lista cuadro para control
head(all_trips)

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

#Crea nueva tabla depurada
all_trips <- all_trips %>% 
  mutate(
    started_at = ymd_hms(started_at),
    ended_at = ymd_hms(ended_at),
    ride_length = as.numeric(difftime(ended_at, started_at, units = "mins")),
    day_of_week = wday(started_at, label = TRUE)
  ) %>%
  filter(ride_length > 1 & ride_length < 720)  # Se reduce el estudio a viajes razonables, si el viaje dura menos de 1 min o mas de 12horas se tratan de posibles errores o casos aislados

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

#Promedio de duración por tipo de usuario
all_trips %>% 
  group_by(member_casual) %>%
  summarise(
    mean_ride = mean(ride_length),
    median_ride = median(ride_length),
    max_ride = max(ride_length),
    trips = n()
  )

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

      

