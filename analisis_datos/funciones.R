# analisis_datos/funciones.R 

bajar_wdi <- function(indicadores, desde = 1990, hasta = 2023) {
  WDI::WDI(country = "all", indicator = indicadores,
           start = desde, end = hasta, extra = TRUE) |>
    dplyr::filter(region != "Aggregates")
}

# Cantidad de NAs por columna (chequeo previo a responder las preguntas)
resumen_nas <- function(df) {
  df |>
    dplyr::summarise(dplyr::across(dplyr::everything(), ~ sum(is.na(.x)))) |>
    tidyr::pivot_longer(dplyr::everything(),
                        names_to = "variable", values_to = "n_na") |>
    dplyr::arrange(dplyr::desc(n_na))
}

# Último año con al menos `min_paises` países con dato de `variable`
ultimo_anio_valido <- function(df, variable, min_paises = 100) {
  df |>
    dplyr::filter(!is.na(.data[[variable]])) |>
    dplyr::count(year) |>
    dplyr::filter(n >= min_paises) |>
    dplyr::pull(year) |>
    max()
}

# Primer año en que cada país llega a un umbral de fertilidad
# (2.1 hijos por mujer = nivel de reemplazo). ya_debajo indica si el país
# ya estaba por debajo en su primer año con dato (el cruce no es observable).
anio_umbral <- function(df, umbral = 2.1) {
  df |>
    dplyr::filter(!is.na(fertilidad)) |>
    dplyr::group_by(country) |>
    dplyr::summarise(
      anio_inicio = min(year),
      anio_cruce  = if (any(fertilidad <= umbral)) min(year[fertilidad <= umbral]) else NA_integer_,
      ya_debajo   = !is.na(anio_cruce) & anio_cruce == anio_inicio,
      .groups = "drop"
    )
}

    dplyr::group_by(country) |>
    dplyr::summarise(anio_cruce = min(year), .groups = "drop")
}

