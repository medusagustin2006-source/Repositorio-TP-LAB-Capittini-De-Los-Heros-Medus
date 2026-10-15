# script_principal.R
# Análisis de datos: fertilidad en Argentina vs. el mundo (Banco Mundial)
# Corrida desde el directorio analisis_datos/

set.seed(123)

library(tidyverse)
library(WDI)

source("funciones.R")

# ---- 1. Datos ---------------------------------------------------------------
# Si ya bajamos los datos, los leemos del CSV (reproducible y sin internet).
dir.create("data", showWarnings = FALSE)
archivo <- "data/wdi.csv"

if (file.exists(archivo)) {
  wdi <- read_csv(archivo, show_col_types = FALSE)
} else {
  wdi <- bajar_wdi(c(fertilidad = "SP.DYN.TFRT.IN",   # hijos por mujer
                     terciaria  = "SE.TER.ENRR",      # matrícula terciaria (%)
                     urbano     = "SP.URB.TOTL.IN.ZS",# población urbana (%)
                     pbi_pc     = "NY.GDP.PCAP.PP.KD")) # PBI pc PPP
  write_csv(wdi, archivo)
}

# ---- 2. Exploración previa --------------------------------------------------
resumen_nas(wdi)
summary(wdi$fertilidad)   # rango razonable: ~0.8 a ~8

# Años de referencia: el último con cobertura suficiente de cada variable
anio_fert <- ultimo_anio_valido(wdi, "fertilidad")
anio_corr <- min(anio_fert,
                 ultimo_anio_valido(wdi, "terciaria"),
                 ultimo_anio_valido(wdi, "urbano"),
                 ultimo_anio_valido(wdi, "pbi_pc"))
anio_fert; anio_corr

wdi |>
  filter(country == "Argentina") |>
  ggplot(aes(year, fertilidad)) +
  geom_line() +
  geom_hline(yintercept = 2.1, linetype = "dashed") +
  labs(title = "Fertilidad en Argentina (línea punteada: nivel de reemplazo)",
       x = NULL, y = "Hijos por mujer")

# ---- 3. Pregunta 1: ¿Argentina cayó más rápido que el resto desde 2015? -----
caida <- wdi |>
  filter(year %in% c(2015, anio_fert)) |>
  select(country, year, fertilidad) |>
  pivot_wider(names_from = year, values_from = fertilidad, names_prefix = "y") |>
  mutate(inicio = .data[["y2015"]],
         fin    = .data[[paste0("y", anio_fert)]],
         cambio = fin - inicio) |>
  select(country, inicio, fin, cambio) |>
  drop_na(cambio) |>
  arrange(cambio) |>
  mutate(ranking = row_number())

caida |> filter(country == "Argentina")   # posición entre N países

ggplot(caida, aes(cambio)) +
  geom_histogram(bins = 30) +
  geom_vline(xintercept = caida$cambio[caida$country == "Argentina"],
             color = "red") +
  labs(title = paste("Cambio en fertilidad 2015 -", anio_fert,
                     "(rojo = Argentina)"),
       x = "Cambio en hijos por mujer", y = "Cantidad de países")

# ---- 4. Pregunta 2: ¿Cuándo cruzó el nivel de reemplazo? --------------------
wdi |>
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

# ---- 5. Pregunta 3: ¿Con qué se correlaciona la fertilidad? -----------------
datos_corr <- wdi |>
  filter(year == anio_corr) |>
  transmute(country, fertilidad, terciaria, urbano,
            log_pbi_pc = log(pbi_pc)) |>
  drop_na()

nrow(datos_corr)   # países con datos completos

datos_corr |>
  select(-country) |>
  cor() |>
  round(2)

ggplot(datos_corr, aes(terciaria, fertilidad)) +
  geom_point(alpha = .6) +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_point(data = filter(datos_corr, country == "Argentina"),
             color = "red", size = 3) +
  labs(title = paste("Fertilidad vs. educación terciaria,", anio_corr),
       x = "Matrícula terciaria (%)", y = "Hijos por mujer")

