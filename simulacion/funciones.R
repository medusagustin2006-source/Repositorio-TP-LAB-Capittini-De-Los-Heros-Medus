# simulacion/funciones.R

# Función inciso 2: Simula la extracción de 3 cartas y devuelve mano y mazo
sacar_al_azar <- function(mazo) {
  # Usamos sample sin reemplazo como en la ruleta del tutorial
  idx <- sample(1:nrow(mazo), 3, replace = FALSE) 
  mano <- mazo[idx, ]
  mazo_restante <- mazo[-idx, ]
  return(list(mano = mano, mazo_restante = mazo_restante))
}

# Función inciso 3: Cálculo exacto de los tantos de envido
contar_tantos <- function(mano) {
  # Contamos las frecuencias de cada palo en la mano
  tabla_palos <- table(mano$palo)
  
  # Lógica if/else 
  if (any(tabla_palos >= 2)) {
    # Si hay al menos dos del mismo palo, agarramos ese palo
    palo_repetido <- names(tabla_palos)[tabla_palos >= 2][1]
    cartas_palo <- mano[mano$palo == palo_repetido, ]
    
    # Ordenamos de mayor a menor y sumamos las dos más altas + 20
    valores_ordenados <- sort(cartas_palo$valor_envido, decreasing = TRUE)
    tantos <- 20 + valores_ordenados[1] + valores_ordenados[2]
    return(tantos)
  } else {
    # Si son de distinto palo, es el mayor valor individual
    return(max(mano$valor_envido))
  }
}

# Función inciso 4: Calcular probabilidad exacta mediante fuerza bruta
prob_oponente_gana_exacta <- function(mi_mano, mazo_restante, yo_soy_mano = TRUE) {
  mis_tantos <- contar_tantos(mi_mano)
  
  # Generamos las combinaciones posibles de 3 cartas (análogo a la clave del tutorial)
  combos <- combn(1:nrow(mazo_restante), 3)
  n_combos <- ncol(combos)
  
  victorias_oponente <- 0
  
  for (i in 1:n_combos) {
    mano_oponente <- mazo_restante[combos[, i], ]
    tantos_oponente <- contar_tantos(mano_oponente)
    
    # Lógica de desempate
    if (tantos_oponente > mis_tantos) {
      victorias_oponente <- victorias_oponente + 1
    } else if (tantos_oponente == mis_tantos && !yo_soy_mano) {
      victorias_oponente <- victorias_oponente + 1
    }
  }
  
  probabilidad <- victorias_oponente / n_combos
  return(probabilidad)
}
