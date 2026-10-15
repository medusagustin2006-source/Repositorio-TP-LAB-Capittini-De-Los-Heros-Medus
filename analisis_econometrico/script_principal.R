# analisis_econometrico/script_principal.R
set.seed(067) # Fijamos semilla para que la simulación de datos sea reproducible
library(tidyverse)
library(gapminder)

# 0. Preparar los datos usando el paquete gapminder
# Renombramos y creamos las columnas como las pide el TP
datos_tp <- gapminder %>%
  mutate(
    `income per person` = gdpPercap,
    `life expectancy` = lifeExp,
    # Simulamos 'life expectancy female' sumándole unos años 
    # a la general, ya que el paquete oficial no trae esta variable desglosada.
    `life expectancy female` = lifeExp + rnorm(n(), mean = 4, sd = 1.5)
  )

# --- Parte 1: Ingreso por persona ---

# 1. Gráfica de evolución temporal en Argentina
datos_arg <- datos_tp %>% filter(country == "Argentina")

ggplot(datos_arg, aes(x = year, y = `income per person`)) +
  geom_line(color = "steelblue", linewidth = 1) +
  theme_minimal() +
  labs(title = "Evolución del Ingreso por persona en Argentina")

# 2. Separar últimos 10 años para testear y armar los 3 modelos
max_year <- max(datos_arg$year)
train_data <- datos_arg %>% filter(year <= max_year - 10)
test_data  <- datos_arg %>% filter(year > max_year - 10)

mod_lin <- lm(`income per person` ~ year, data = train_data)
mod_poly2 <- lm(`income per person` ~ poly(year, 2, raw = TRUE), data = train_data)
mod_poly10 <- lm(`income per person` ~ poly(year, 10, raw = TRUE), data = train_data)

# Visualizar el ajuste de los tres modelos 
ggplot(train_data, aes(x = year, y = `income per person`)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, color = "red", se = FALSE) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE), color = "green", se = FALSE) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 10, raw = TRUE), color = "purple", se = FALSE) +
  theme_minimal() +
  labs(title = "Ajuste de los 3 modelos en datos de entrenamiento")

# 3. a) Matriz de correlaciones de ingresos entre 5 países
paises_sa <- datos_tp %>%
  filter(country %in% c("Argentina", "Brazil", "Uruguay", "Colombia", "Peru")) %>%
  select(year, country, `income per person`) %>%
  pivot_wider(names_from = country, values_from = `income per person`)

cor_ingresos <- cor(paises_sa %>% select(-year), use = "complete.obs")
print("Matriz de correlaciones (Niveles de Ingreso):")
print(cor_ingresos)

# 3. b) Matriz de correlaciones de las variaciones porcentuales 
paises_sa_var <- paises_sa %>%
  arrange(year) %>%
  mutate(across(-year, ~ (. - lag(.)) / lag(.))) %>%
  drop_na() # Sacamos el primer año que queda como NA al hacer lag()

cor_var <- cor(paises_sa_var %>% select(-year), use = "complete.obs")
print("Matriz de correlaciones (Variaciones Porcentuales):")
print(cor_var)


# --- Parte 2: Esperanza de vida y género ---

# Filtramos un año en particular (2007)
datos_2007 <- datos_tp %>% filter(year == 2007)

# 5. Gráfico life expectancy frente a life expectancy female
ggplot(datos_2007, aes(x = `life expectancy female`, y = `life expectancy`)) +
  geom_point(color = "darkorange") +
  theme_minimal() +
  labs(title = "Expectativa de vida General vs Femenina (2007)")

# 6. Regresión simple
mod_simple <- lm(`life expectancy` ~ `life expectancy female`, data = datos_2007)
summary(mod_simple)

# 7. Contraste de hipótesis
# H0: life expectancy female = life expectancy
# H1: life expectancy female > life expectancy (alternative = "greater")
t.test(datos_2007$`life expectancy female`, datos_2007$`life expectancy`, 
       paired = TRUE, alternative = "greater")

# 8. Regresión múltiple teniendo en cuenta el income per person
mod_mult <- lm(`life expectancy` ~ `life expectancy female` + `income per person`, data = datos_2007)
summary(mod_mult)

# 9. Modelo lineal con máximo 3 covariables excluyendo "female"
# Justificación: Usamos "income per person" y generamos una nueva columna "log_pop" 
# (logaritmo de la población) para controlar por el tamaño del país
datos_2007_modelo <- datos_2007 %>%
  mutate(log_pop = log(pop))

mod_final <- lm(`life expectancy` ~ `income per person` + log_pop, data = datos_2007_modelo)
summary(mod_final)
