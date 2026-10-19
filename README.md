# Repositorio-TP-LAB-Capittini-De-Los-Heros-Medus

Trabajo práctico domiciliario de R.
Integrantes: Capittini, De Los Heros, Medus.

El trabajo se compone de tres ejercicios, cada uno en su propia carpeta con su script principal, su archivo de funciones y su carpeta de datos:

1. **Análisis de datos:** fertilidad en Argentina vs. el mundo (Banco Mundial).
2. **Análisis econométrico:** ingreso y esperanza de vida con el dataset gapminder del campus.
3. **Simulación:** envido en el Truco (Simulación 3 de la consigna).

El informe con la explicación y la justificación de los resultados está en `informe.pdf`.

---

## Estructura del repositorio

```
Repositorio-TP-LAB-Capittini-De-Los-Heros-Medus/
|-- README.md
|-- informe.pdf
|-- analisis_datos/
|   |-- script_principal.R
|   |-- funciones.R
|   `-- data/
|-- analisis_econometrico/
|   |-- script_principal.R
|   |-- funciones.R
|   `-- data/
|       `-- gapminder.csv
`-- simulacion/
    |-- script_principal.R
    |-- funciones.R
    `-- data/
```

En cada ejercicio, `funciones.R` contiene **solo definiciones de funciones** y `script_principal.R` las carga con `source()` y ejecuta el análisis. Todo el código está comentado y cada script principal empieza con `set.seed()` para que los resultados sean reproducibles.

---

## Cómo correr el código

### Requisitos

- R (versión 4.1 o superior, porque se usa el pipe nativo `|>`).
- Paquetes:

```r
install.packages(c("tidyverse", "WDI"))
```

El ejercicio de análisis de datos usa `tidyverse` y `WDI`. El econométrico usa solo `tidyverse`. La simulación usa únicamente R base.

### Directorio de trabajo

**Todos los scripts se corren desde la raíz del repositorio**, porque usan rutas relativas como `analisis_datos/funciones.R`. La forma más simple es abrir la carpeta del repo como proyecto de RStudio (File > New Project > Existing Directory). Si no, se puede fijar a mano:

```r
setwd("ruta/a/Repositorio-TP-LAB-Capittini-De-Los-Heros-Medus")
getwd()   # verificar que sea la raíz (donde está este README)
```

Después se corre cada script completo, por ejemplo `source("simulacion/script_principal.R")` o con el botón *Source* de RStudio.

---

## 1. Análisis de datos: `analisis_datos/`

**Dataset:** indicadores del Banco Mundial (World Development Indicators), descargados con el paquete `WDI`, para todos los países entre 1990 y 2023 (se excluyen los agregados regionales como "World" u "OECD"):

| Variable en el código | Indicador | Descripción |
|---|---|---|
| `fertilidad` | `SP.DYN.TFRT.IN` | Hijos por mujer |
| `terciaria` | `SE.TER.ENRR` | Matrícula terciaria (%) |
| `urbano` | `SP.URB.TOTL.IN.ZS` | Población urbana (%) |
| `pbi_pc` | `NY.GDP.PCAP.PP.KD` | PBI per cápita, PPP |

**Datos locales:** la primera vez que se corre el script, los datos se descargan (hace falta internet) y se guardan en `analisis_datos/data/wdi.csv`. Las siguientes veces se leen directamente de ese archivo, sin conexión y con los mismos datos.

**Exploración previa:** conteo de NAs por variable, resumen de la fertilidad, selección del último año con cobertura suficiente (al menos 100 países) de cada variable y gráfico de la fertilidad de Argentina contra el nivel de reemplazo (2,1 hijos por mujer).

**Preguntas que se responden:**

1. ¿Argentina redujo su fertilidad más rápido que el resto de los países desde 2015? Se calcula el cambio entre 2015 y el último año válido, el ranking de Argentina y un histograma.
2. ¿Cuándo cruzó cada país el nivel de reemplazo? Se compara Argentina con Chile, Uruguay, Brasil, España, Italia y Japón. La columna `ya_debajo` marca los países que ya estaban por debajo de 2,1 en su primer año con dato, donde el cruce real no se observa.
3. ¿Con qué variables se correlaciona la fertilidad? Matriz de correlaciones con educación terciaria, urbanización y log del PBI per cápita, y gráfico de dispersión de fertilidad contra educación terciaria.

**Semilla:** `set.seed(456)`.

**Funciones (`funciones.R`):** `bajar_wdi()`, `resumen_nas()`, `ultimo_anio_valido()` y `anio_umbral()`.

---

## 2. Análisis econométrico: `analisis_econometrico/`

**Dataset:** `gapminder.csv`, el dataset de gapminder provisto en el campus (países por año, 1960-2010). Está incluido en `analisis_econometrico/data/`. Contiene, entre otras, las variables `income_per_person`, `life_expectancy`, `life_expectancy_female`, `life_expectancy_male` y `population`, que el código renombra con los nombres de la consigna (`income per person`, `life expectancy`, etc.).

> En la columna `life_expectancy_female` hay 9 países chicos con el valor `"-"` en lugar de un dato. La función de carga los lee como `NA`; si no, R tomaría la columna entera como texto y las regresiones fallarían.

**Parte 1: Ingreso por persona**

- Gráfico de la evolución del ingreso por persona de Argentina.
- Los últimos 10 años (2001-2010) se separan como conjunto de test, y se ajustan sobre el resto (1960-2000) un modelo lineal, uno polinómico de grado 2 y uno de grado 10 (con `poly()`). Se grafican los tres ajustes junto con los datos observados.
- Matriz de correlaciones de los niveles de ingreso, y de las variaciones porcentuales anuales, para cinco países sudamericanos: Argentina, Brasil, Uruguay, Colombia y Perú.

**Parte 2: Esperanza de vida y género** (año elegido: **2006**, con los países que tienen dato de esperanza de vida total y femenina)

- Gráfico de `life expectancy` contra `life expectancy female`.
- Regresión simple y análisis del coeficiente y del R².
- Test t apareado (H0: esperanza de vida femenina = total, contra la alternativa de que la femenina es mayor).
- Regresión múltiple agregando `income per person` y comparación del R² con la simple.
- Modelo final de dos covariables: `income per person` y `log_pop` (logaritmo de la población, variable construida). Se excluye `life expectancy female` por pedido de la consigna y `life expectancy male` por ser casi una copia de la variable explicada.

**Semilla:** `set.seed(67)`.

**Funciones (`funciones.R`):** `cargar_gapminder()`, `serie_pais()`, `separar_train_test()`, `ajustar_modelos()`, `predecir_modelos()`, `rmse()`, `evaluar_modelos()`, `graficar_ajustes()`, `ingresos_ancho()`, `variacion_porcentual()`, `matriz_correlaciones()` y `preparar_anio()`.

---

## 3. Simulación: `simulacion/`

**Ejercicio elegido:** Simulación 3, *envido en el Truco*. Se trabaja con el mazo español de 40 cartas (valores del 1 al 7 y del 10 al 12 en los cuatro palos). En el envido, las cartas del 10 al 12 valen 0 y las del 1 al 7 valen su número. No se usan datos externos.

**Qué hace cada parte del script:**

1. **Mazo:** se genera con `expand.grid()` y se agrega la columna `valor_envido`.
2. **Extracción al azar:** `sacar_al_azar()` saca tres cartas y devuelve la mano y el mazo restante. Se repite 5.000 veces y se grafica el histograma del valor medio de envido de las cartas.
3. **Tantos de envido:** `contar_tantos()` suma las dos cartas más altas del mismo palo más 20 de bono. Si no hay dos del mismo palo, el tanto es el mayor valor individual.
4. **Probabilidad exacta:** `prob_oponente_gana_exacta()` recorre todas las manos posibles del oponente (todas las combinaciones de 3 cartas del mazo restante) y calcula la probabilidad de que gane el envido en una partida de a dos.
5. **Truco de seis jugadores (Monte Carlo):** 10.000 simulaciones en las que se reparten las cartas de los otros cinco jugadores. Se condiciona a que los dos compañeros tengan menos de 27 tantos, y se estima la probabilidad de que el equipo gane el envido.
6. **(Opcional):** histograma del puntaje máximo de envido de los tres rivales.

**Supuesto:** en los desempates se asume que el equipo propio (o el jugador propio, en la partida de a dos) **es mano**, por lo que gana en caso de empate.

**Semilla:** `set.seed(26)`.

**Funciones (`funciones.R`):** `sacar_al_azar()`, `contar_tantos()` y `prob_oponente_gana_exacta()`.

---

## Notas

- Los scripts de los tres ejercicios son independientes entre sí: cada carpeta es autocontenida.
- Para reproducir los resultados del informe, correr cada script completo desde el principio, ya que los objetos de las secciones posteriores dependen de los anteriores.
