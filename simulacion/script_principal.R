# simulacion/script_principal.R
set.seed(26)

# Cargar funciones
source("simulacion/funciones.R")

# --- Inciso 1: Generar el mazo ---
numeros <- c(1:7, 10:12)
palos <- c("espadas", "bastos", "oros", "copas")
# expand.grid cruza todas las combinaciones posibles
mazo <- expand.grid(numero = numeros, palo = palos, stringsAsFactors = FALSE)
# Agregamos los valores numéricos para el envido (10, 11 y 12 valen cero)
mazo$valor_envido <- ifelse(mazo$numero <= 7, mazo$numero, 0)

# --- Inciso 2: Histograma del valor medio ---
# Estimación usando 'replicate'
medias_simuladas <- replicate(5000, {
  extraccion <- sacar_al_azar(mazo)
  mean(extraccion$mano$valor_envido) 
})

hist(medias_simuladas, 
     main = "Distribución del Valor Medio de las cartas extraídas", 
     xlab = "Valor Medio de Envido", col = "steelblue", breaks = 30)

# --- Incisos 3 y 4: Probar manos y probabilidad de ganar ---
# Sacamos nuestra mano
mi_juego <- sacar_al_azar(mazo)
mi_mano <- mi_juego$mano
mazo_restante <- mi_juego$mazo_restante
mis_tantos <- contar_tantos(mi_mano)

prob_perder <- prob_oponente_gana_exacta(mi_mano, mazo_restante, yo_soy_mano = TRUE)

cat(sprintf("Mis cartas suman %d tantos\n", mis_tantos))
cat(sprintf("Probabilidad exacta de que el oponente me gane: %.4f\n", prob_perder))

# --- Inciso 5: Simulación 6 jugadores (Monte Carlo) ---
N_sim <- 10000 
victorias_equipo <- 0
simulaciones_validas <- 0

# Variables para el inciso opcional
maximos_envidos_rivales <- numeric()

# Bucle for principal de simulación
for (i in 1:N_sim) {
  # Sacamos 15 cartas de golpe para los 5 jugadores restantes
  idx_mesa <- sample(1:nrow(mazo_restante), 15, replace = FALSE)
  mesa <- mazo_restante[idx_mesa, ]
  
  # Repartimos las cartas
  c1 <- mesa[1:3, ]; c2 <- mesa[4:6, ]         # Compañeros
  r1 <- mesa[7:9, ]; r2 <- mesa[10:12, ]; r3 <- mesa[13:15, ] # Rivales
  
  tantos_c1 <- contar_tantos(c1)
  tantos_c2 <- contar_tantos(c2)
  
  # Condición: Compañeros tienen MENOS de 27 tantos (Probabilidad Condicional)
  if (tantos_c1 < 27 && tantos_c2 < 27) {
    simulaciones_validas <- simulaciones_validas + 1
    
    # Calculamos puntos de rivales
    tantos_r1 <- contar_tantos(r1)
    tantos_r2 <- contar_tantos(r2)
    tantos_r3 <- contar_tantos(r3)
    
    max_rival <- max(tantos_r1, tantos_r2, tantos_r3)
    max_equipo <- max(mis_tantos, tantos_c1, tantos_c2)
    
    maximos_envidos_rivales <- c(maximos_envidos_rivales, max_rival)
    
    # Asumimos que somos mano para desempatar, según la lógica del juego
    if (max_equipo >= max_rival) {
      victorias_equipo <- victorias_equipo + 1
    }
  }
}

prob_equipo <- victorias_equipo / simulaciones_validas
cat(sprintf("Escenarios válidos: %d. Prob. de que mi equipo gane: %.4f\n",
            simulaciones_validas, prob_equipo))

# --- Inciso 6 (Opcional): Histograma del máximo rival ---
hist(maximos_envidos_rivales, 
     main = "Máximo envido del equipo rival (3 jugadores)", 
     xlab = "Puntaje máximo", col = "darkorange", breaks = 15)
