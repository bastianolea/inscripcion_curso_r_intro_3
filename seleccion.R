library(surveydown)
library(dplyr)
library(stringr)

# conexión a la base de datos de la encuesta
db <- sd_db_connect()

# obtener datos desde la base y ordenar
data <- sd_get_data(db) |>
  tibble() |>
  arrange(desc(time_end))

# revisar datos
glimpse(data)


# limpieza de datos ----
resultados <- data |>
  # solamente con correo
  filter(!is.na(correo)) |>
  # excluir pruebas
  filter(nombre != "Bastián Olea") |>
  filter(nombre != "Basticito Mapachito") |>
  # seleccionar columnas
  select(-starts_with("time"), time_end, -session_id) |>
  # convertir fechas
  mutate(time_end = lubridate::as_datetime(time_end)) |>
  arrange(time_end) |>
  # limpiar nombres
  mutate(nombre = str_squish(nombre)) |>
  distinct(nombre = str_to_lower(nombre), .keep_all = TRUE) |>
  # validar correo
  filter(str_detect(correo, "@.*\\.")) |> 
  # filtrar fecha de cierre
  filter(time_end <= lubridate::ymd_hms("2026-06-12 12:59:59")) |>
  # limpiar respuestas
  mutate(across(
    where(is.character),
    ~ case_when(str_detect(.x, "^s$") ~ "sí", .default = .x)
  ))

# exclusiones ----
resultados <- resultados |> 
  filter(nivel_programacion != "s_m_s_de_uno") |>
  filter(areas != "otra_que_no_es_de_ciencias_sociales_o_humanidades") |>
  filter(nivel_r != "intermedio", nivel_r != "avanzado") 



# revisar datos ----
resultados |> glimpse()

resultados |>
  select(nombre, genero, lgbt, trans, pais, disca) |>
  print(n = 100)

resultados |>
  filter(pais == "chile") |>
  print(n = 100)

resultados |>
  filter(trans == "sí")

resultados |>
  arrange(desc(edad))

resultados |> count(genero)
resultados |> count(nivel_programacion)
resultados |> count(nivel_r)
resultados |> count(pais)
resultados |> count(trans)
resultados |> count(areas) |> print(n = Inf)


# puntajes ----
# crear puntajes de selección
resultados_p <- resultados |>
  mutate(puntaje = 1) |>
  # sumar puntos a grupos de interés
  mutate(
    #puntaje = if_else(trans == "sí", puntaje + 4, puntaje),
    puntaje = if_else(
      genero %in% c("no_binario", "queer_ag_nero_otro"),
      puntaje + 2,
      puntaje
    ),
    puntaje = if_else(genero == "femenino", puntaje + 2, puntaje),
    puntaje = if_else(lgbt == "sí", puntaje + 3, puntaje),
    puntaje = if_else(pais == "chile", puntaje + 2, puntaje),
    puntaje = if_else(
      disca == "s_con_credencial_o_pensi_n_de_invalidez",
      puntaje + 2,
      puntaje
    ),
    puntaje = if_else(
      disca == "s_sin_credencial_o_pensi_n_de_invalidez",
      puntaje + 1,
      puntaje
    )
  ) |>
  # penalizar grupos
  mutate(
    puntaje = if_else(
      nivel_programacion == "sí", 
      puntaje - 2, 
      puntaje),
    puntaje = if_else(
      nivel_r %in% c("intermedio", "avanzado"),
      puntaje - 2,
      puntaje
    )
  ) |>
  mutate(puntaje = if_else(puntaje <= 0, 1, puntaje)) |>
  # crear ranking de postulantes
  arrange(desc(puntaje)) |>
  mutate(id = row_number()) |>
  relocate(puntaje, .after = nombre)

# revisar
resultados_p |>
  # filter(pais == "chile") |>
  select(nombre, pais, genero, disca, trans, lgbt, puntaje) |>
  print(n = Inf)

resultados_p |> select(nombre, puntaje, id) |> head()
resultados_p |> select(nombre, puntaje, id) |> tail()


# selección ----

# separar personas trans, porque todxs tendrán cupo
personas_trans <- resultados_p |>
  filter(trans == "sí")

# excluir cupos trans de la tabla de inscritos
resultados_p_2 <- resultados_p |>
  filter(trans != "sí")

# cantidad de cupos disponibles
n_cupos <- 150

# # selección aleatoria sin probabilidad
# seleccion <- sample(seq_len(nrow(resultados_p_2)), n_cupos)

# selección aleatoria con probabilidad
seleccion <- sample(
  resultados_p_2$id,
  size = n_cupos,
  prob = resultados_p_2$puntaje,
  replace = FALSE
)

# dejar seleccionadxs
resultados_cupo <- resultados_p_2 |>
  filter(id %in% seleccion)

# agregar personas trans y cupos especiales
resultados_cupo <- resultados_cupo |>
  bind_rows(personas_trans)

# resultados ----
resultados_cupo |>
  select(nombre, pais, genero, disca, trans, lgbt, puntaje) |>
  arrange(desc(puntaje)) |>
  print(n = Inf)

resultados_cupo |> count(genero)
resultados_cupo |> count(pais, sort = TRUE)
resultados_cupo |> count(nivel_r)
resultados_cupo |> count(educacion, sort = TRUE)
resultados_cupo |> count(areas, sort = TRUE)
mean(as.numeric(resultados_cupo$edad))
resultados_cupo |> count(pais, trans) |> filter(trans == "sí")

# resultados_cupo |> filter(str_detect(nombre, ""))

# correos ----
resultados_cupo$correo |>
  paste(collapse = ", ") |>
  # append(readLines("cupos_especiales.txt")) |> 
  clipr::write_clip()


# # anonimos
# resultados_cupo |>
#   select(-nombre, -correo, -puntaje) |>
#   readr::write_rds("cupos.rds")
