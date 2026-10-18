# analisis_econometrico/script_principal.R
# Análisis econométrico con el dataset gapminder del campus (serie anual 1960-2010)
# Se corre desde la raíz del repo 

set.seed(67) # Semilla fija para que todo sea reproducible

library(tidyverse)

# Funciones auxiliares (solo definiciones, ver funciones.R)
source("analisis_econometrico/funciones.R")

# 0. Cargar los datos del campus ------------------------------------------------
datos_tp <- cargar_gapminder("analisis_econometrico/data/gapminder.csv")


# --- Parte 1: Ingreso por persona ----------------------------------------------

# 1. Gráfica de evolución temporal en Argentina
datos_arg <- serie_pais(datos_tp, "Argentina")

ggplot(datos_arg, aes(x = year, y = `income per person`)) +
  geom_line(color = "steelblue", linewidth = 1) +
  theme_minimal() +
  labs(title = "Evolución del Ingreso por persona en Argentina",
       x = "Año", y = "Ingreso por persona")

# 2. Separar los últimos 10 años para testear y armar los 3 modelos
division   <- separar_train_test(datos_arg, anios_test = 10)
train_data <- division$train      # 1960-2000
test_data  <- division$test       # 2001-2010
max_year   <- division$max_year   # 2010

modelos <- ajustar_modelos(train_data)

# Predicciones de cada modelo sobre todo el período (entrenamiento + test)
pred_grilla <- predecir_modelos(modelos, min(datos_arg$year):max_year)

# Gráfico: datos observados + los 3 ajustes
print(graficar_ajustes(datos_arg, pred_grilla, max_year))

# 3. a) Matriz de correlaciones de ingresos entre 5 países sudamericanos
paises_sa <- ingresos_ancho(datos_tp,
                            c("Argentina", "Brazil", "Uruguay", "Colombia", "Peru"))

print("Matriz de correlaciones (Niveles de Ingreso):")
print(matriz_correlaciones(paises_sa))

# 3. b) Matriz de correlaciones de las variaciones porcentuales anuales
paises_sa_var <- variacion_porcentual(paises_sa)

print("Matriz de correlaciones (Variaciones Porcentuales):")
print(matriz_correlaciones(paises_sa_var))

# --- Parte 2: Esperanza de vida y género ---------------------------------------

# Año elegido: 2006
datos_2006 <- preparar_anio(datos_tp, 2006)

nrow(datos_2006) # países con datos completos

# 5. Gráfico life expectancy frente a life expectancy female
ggplot(datos_2006, aes(x = `life expectancy female`, y = `life expectancy`)) +
  geom_point(color = "darkorange") +
  theme_minimal() +
  labs(title = "Esperanza de vida general vs femenina (2006)")

# 6. Regresión simple
mod_simple <- lm(`life expectancy` ~ `life expectancy female`, data = datos_2006)
summary(mod_simple)

# 7. Contraste de hipótesis (datos apareados: mismo país, dos mediciones)
# H0: life expectancy female = life expectancy
# H1: life expectancy female > life expectancy (alternative = "greater")
t.test(datos_2006$`life expectancy female`, datos_2006$`life expectancy`,
       paired = TRUE, alternative = "greater")

# 8. Regresión múltiple controlando por income per person
mod_mult <- lm(`life expectancy` ~ `life expectancy female` + `income per person`,
               data = datos_2006)
summary(mod_mult)

# 9. Modelo lineal con máximo 3 covariables, excluyendo life expectancy female.
# Justificación: usamos "income per person" y generamos una nueva columna
# "log_pop" (logaritmo de la población) para controlar por el tamaño del país.
datos_2006_modelo <- datos_2006 |>
  mutate(log_pop = log(population))

mod_final <- lm(`life expectancy` ~ `income per person` + log_pop,
                data = datos_2006_modelo)
summary(mod_final)

