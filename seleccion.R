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
  mutate(nombre = str_to_lower(nombre),
         correo = str_to_lower(correo),
         correo = str_remove_all(correo, " ")) |> 
  distinct(nombre, .keep_all = TRUE) |>
  # validar correo
  filter(str_detect(correo, "@.*\\.")) |> 
  # filtrar fecha de cierre
  filter(time_end <= lubridate::ymd_hms("2026-06-14 00:00:00")) |>
  # limpiar respuestas
  mutate(across(
    where(is.character),
    ~ case_when(str_detect(.x, "^s$") ~ "sí", .default = .x)
  ))

readr::write_rds(resultados |> select(-nombre, -correo), 
                 "datos/postulaciones.rds")

# exclusiones ----
resultados <- resultados |> 
  filter(nivel_programacion != "s_m_s_de_uno") |>
  filter(areas != "otra_que_no_es_de_ciencias_sociales_ni_humanidades") |>
  filter(nivel_r != "intermedio", nivel_r != "avanzado") 

# # buscar
# resultados |> filter(str_detect(nombre, "sierra")) |> select(nombre, correo)


# revisar datos ----
resultados |> glimpse()

resultados |>
  select(nombre, genero, lgbt, trans, pais, disca)

resultados |>
  filter(pais == "chile")

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
    puntaje = if_else(genero == "femenino", puntaje + 3, puntaje),
    puntaje = if_else(lgbt == "sí", puntaje + 2, puntaje),
    puntaje = if_else(pais == "chile", puntaje + 3, puntaje),
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
  select(nombre, pais, genero, disca, trans, lgbt, puntaje)

resultados_p |> select(nombre, puntaje, id) |> head()
resultados_p |> select(nombre, puntaje, id) |> tail()


# selección ----

# separar personas trans, porque todxs tendrán cupo
personas_trans <- resultados_p |>
  filter(trans == "sí")

# separar cupos especiales
personas_cupo <- resultados_p |> 
  filter(correo %in% readLines("cupos_especiales.txt"))

# cantidad de cupos disponibles
n_cupos <- 160

# # selección aleatoria sin probabilidad
# seleccion <- sample(seq_len(nrow(resultados_p)), n_cupos)

# selección aleatoria con probabilidad
seleccion <- sample(
  resultados_p$id,
  size = n_cupos,
  prob = resultados_p$puntaje,
  replace = FALSE
)

# dejar seleccionadxs
resultados_cupo <- resultados_p |>
  filter(id %in% seleccion)

# agregar personas trans y cupos especiales
resultados_cupo <- resultados_cupo |>
  bind_rows(personas_trans) |> 
  bind_rows(personas_cupo) |> 
  distinct()

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

# #
# resultados_cupo |> filter(str_detect(correo, "katherine")) |> select(nombre, correo)
resultados_cupo <- resultados_cupo |> 
  mutate(correo = str_remove_all(correo, " "))

# correos ----
resultados_cupo$correo |>
  paste(collapse = ", ") |>
  clipr::write_clip()

# 
# # anonimos
# resultados_cupo |>
#   select(-nombre, -correo, -puntaje) |>
#   readr::write_rds("datos/cupos.rds")

# read_rds("datos/cupos.rds") |> 
#   write_csv2("datos/cupos.rds")
