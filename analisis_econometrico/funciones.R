# analisis_econometrico/funciones.R
# Funciones auxiliares del ejercicio econométrico (gapminder del campus).
# Solo definiciones: se llaman desde script_principal.R con source().

# ---- Carga de datos ---------------------------------------------------------

cargar_gapminder <- function(ruta) {
  readr::read_csv(ruta, na = c("", "NA", "-"), show_col_types = FALSE) |>
    dplyr::rename(`income per person`      = income_per_person,
                  `life expectancy`        = life_expectancy,
                  `life expectancy female` = life_expectancy_female,
                  `life expectancy male`   = life_expectancy_male)
}

# ---- Parte 1: Ingreso por persona -------------------------------------------

# Serie de un país ordenada por año
serie_pais <- function(datos, pais) {
  datos |>
    dplyr::filter(country == pais) |>
    dplyr::arrange(year)
}

# Separa los últimos `anios_test` años para testear.
# Devuelve una lista con train, test y el último año de la serie.
separar_train_test <- function(serie, anios_test = 10) {
  max_year <- max(serie$year)
  list(train    = dplyr::filter(serie, year <= max_year - anios_test),
       test     = dplyr::filter(serie, year >  max_year - anios_test),
       max_year = max_year)
}

# Ajusta los tres modelos pedidos sobre el conjunto de entrenamiento.
ajustar_modelos <- function(train) {
  list("Lineal"             = lm(`income per person` ~ year,           data = train),
       "Polinomio grado 2"  = lm(`income per person` ~ poly(year, 2),  data = train),
       "Polinomio grado 10" = lm(`income per person` ~ poly(year, 10), data = train))
}

# Predicciones de cada modelo sobre un vector de años
# (formato largo: year, modelo, pred)
predecir_modelos <- function(modelos, anios) {
  grilla <- tibble::tibble(year = anios)
  purrr::map_dfr(names(modelos), function(nm) {
    dplyr::mutate(grilla, modelo = nm,
                  pred = predict(modelos[[nm]], newdata = grilla))
  })
}

# Raíz del error cuadrático medio
rmse <- function(real, pred) {
  sqrt(mean((real - pred)^2))
}

# Gráfico: datos observados + los 3 ajustes. Se limita el eje y porque el
# polinomio de grado 10 se dispara fuera de la muestra.
graficar_ajustes <- function(serie, pred_grilla, max_year, anios_test = 10) {
  serie <- dplyr::mutate(serie,
                         conjunto = dplyr::if_else(year > max_year - anios_test,
                                                   "Test", "Entrenamiento"))
  ggplot2::ggplot() +
    ggplot2::geom_point(data = serie,
                        ggplot2::aes(x = year, y = `income per person`,
                                     shape = conjunto)) +
    ggplot2::geom_line(data = pred_grilla,
                       ggplot2::aes(x = year, y = pred, color = modelo),
                       linewidth = 1) +
    ggplot2::geom_vline(xintercept = max_year - anios_test + 0.5,
                        linetype = "dashed") +
    ggplot2::coord_cartesian(ylim = range(serie$`income per person`) * c(0.8, 1.2)) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = "Ajuste de los 3 modelos (línea punteada: inicio del test)",
                  x = "Año", y = "Ingreso por persona",
                  color = "Modelo", shape = "Datos")
}

# Ingresos de varios países en formato ancho (una columna por país)
ingresos_ancho <- function(datos, paises) {
  datos |>
    dplyr::filter(country %in% paises) |>
    dplyr::select(year, country, `income per person`) |>
    tidyr::pivot_wider(names_from = country, values_from = `income per person`)
}

# Variación porcentual anual (Delta Y / Y) de cada columna de un data frame ancho
variacion_porcentual <- function(df_ancho) {
  df_ancho |>
    dplyr::arrange(year) |>
    dplyr::mutate(dplyr::across(-year, ~ (. - dplyr::lag(.)) / dplyr::lag(.))) |>
    tidyr::drop_na()   # saca el primer año, que queda NA al hacer lag()
}

# Matriz de correlaciones (redondeada) de todas las columnas menos year
matriz_correlaciones <- function(df_ancho, digitos = 2) {
  df_ancho |>
    dplyr::select(-year) |>
    cor(use = "complete.obs") |>
    round(digitos)
}

# ---- Parte 2: Esperanza de vida y género ------------------------------------

# Filtra un año y se queda con los países que tienen dato de esperanza de vida
# general y femenina, para que todas las preguntas usen la misma muestra.
preparar_anio <- function(datos, anio) {
  datos |>
    dplyr::filter(year == anio) |>
    tidyr::drop_na(`life expectancy`, `life expectancy female`)
}

