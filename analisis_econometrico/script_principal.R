# analisis_econometrico/script_principal.R
# Análisis econométrico con el dataset gapminder del campus (serie anual 1960-2010)
# Se corre desde la raíz del repo (proyecto de RStudio).

set.seed(067) # Semilla fija para que todo sea reproducible

library(tidyverse)

# 0. Cargar los datos del campus ------------------------------------------------
# OJO: life_expectancy_female trae "-" en lugar de NA para 9 países chicos
# (Andorra, Dominica, Monaco, etc.). Si no lo tratamos como NA, R lee la
# columna entera como texto y las regresiones fallan.
datos_tp <- read_csv("analisis_econometrico/data/gapminder.csv",
                     na = c("", "NA", "-"),
                     show_col_types = FALSE) %>%
  rename(`income per person`      = income_per_person,
         `life expectancy`        = life_expectancy,
         `life expectancy female` = life_expectancy_female,
         `life expectancy male`   = life_expectancy_male)


# --- Parte 1: Ingreso por persona ----------------------------------------------

# 1. Gráfica de evolución temporal en Argentina
datos_arg <- datos_tp %>%
  filter(country == "Argentina") %>%
  arrange(year)

ggplot(datos_arg, aes(x = year, y = `income per person`)) +
  geom_line(color = "steelblue", linewidth = 1) +
  theme_minimal() +
  labs(title = "Evolución del Ingreso por persona en Argentina",
       x = "Año", y = "Ingreso por persona")

# 2. Separar los últimos 10 años para testear y armar los 3 modelos
max_year   <- max(datos_arg$year)                       # 2010
train_data <- datos_arg %>% filter(year <= max_year - 10) # 1960-2000
test_data  <- datos_arg %>% filter(year >  max_year - 10) # 2001-2010

mod_lin    <- lm(`income per person` ~ year,           data = train_data)
mod_poly2  <- lm(`income per person` ~ poly(year, 2),  data = train_data)
mod_poly10 <- lm(`income per person` ~ poly(year, 10), data = train_data)

modelos <- list("Lineal"             = mod_lin,
                "Polinomio grado 2"  = mod_poly2,
                "Polinomio grado 10" = mod_poly10)

# Predicciones de cada modelo sobre todo el período (entrenamiento + test)
grilla <- tibble(year = min(datos_arg$year):max_year)
pred_grilla <- map_dfr(names(modelos), function(nm) {
  grilla %>% mutate(modelo = nm, pred = predict(modelos[[nm]], newdata = grilla))
})

datos_arg <- datos_arg %>%
  mutate(conjunto = if_else(year > max_year - 10, "Test", "Entrenamiento"))

# Gráfico: datos observados + los 3 ajustes (se limita el eje y porque el
# polinomio de grado 10 se dispara fuera de la muestra)
ggplot() +
  geom_point(data = datos_arg,
             aes(x = year, y = `income per person`, shape = conjunto)) +
  geom_line(data = pred_grilla, aes(x = year, y = pred, color = modelo),
            linewidth = 1) +
  geom_vline(xintercept = max_year - 10 + 0.5, linetype = "dashed") +
  coord_cartesian(ylim = range(datos_arg$`income per person`) * c(0.8, 1.2)) +
  theme_minimal() +
  labs(title = "Ajuste de los 3 modelos (línea punteada: inicio del test)",
       x = "Año", y = "Ingreso por persona", color = "Modelo", shape = "Datos")

# 3. a) Matriz de correlaciones de ingresos entre 5 países sudamericanos
paises_sa <- datos_tp %>%
  filter(country %in% c("Argentina", "Brazil", "Uruguay", "Colombia", "Peru")) %>%
  select(year, country, `income per person`) %>%
  pivot_wider(names_from = country, values_from = `income per person`)

cor_ingresos <- cor(paises_sa %>% select(-year), use = "complete.obs")
print("Matriz de correlaciones (Niveles de Ingreso):")
print(round(cor_ingresos, 2))

# 3. b) Matriz de correlaciones de las variaciones porcentuales anuales
paises_sa_var <- paises_sa %>%
  arrange(year) %>%
  mutate(across(-year, ~ (. - lag(.)) / lag(.))) %>%
  drop_na() # Sacamos el primer año que queda como NA al hacer lag()

cor_var <- cor(paises_sa_var %>% select(-year), use = "complete.obs")
print("Matriz de correlaciones (Variaciones Porcentuales):")
print(round(cor_var, 2))

# --- Parte 2: Esperanza de vida y género ---------------------------------------

# Filtramos un año en particular (2006) y nos quedamos con los países que tienen
# dato de esperanza de vida general y femenina, para que todas las preguntas
# usen exactamente la misma muestra.
datos_2006 <- datos_tp %>%
  filter(year == 2006) %>%
  drop_na(`life expectancy`, `life expectancy female`)

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
# También excluimos life expectancy male (es prácticamente la misma información
# que la femenina, mismo problema de "casi copia" de la variable explicada).
# Justificación: usamos "income per person" y generamos una nueva columna
# "log_pop" (logaritmo de la población) para controlar por el tamaño del país.
datos_2006_modelo <- datos_2006 %>%
  mutate(log_pop = log(population))

mod_final <- lm(`life expectancy` ~ `income per person` + log_pop,
                data = datos_2006_modelo)
summary(mod_final)
