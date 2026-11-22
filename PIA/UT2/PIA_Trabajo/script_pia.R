# ==============================================================================
# ACTIVIDAD 1. PREPARACIÓN DEL ENTORNO Y EXPLORACIÓN DE LOS DATOS
# ==============================================================================

# Configuración del directorio de trabajo dinámico
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  ruta <- rstudioapi::getActiveDocumentContext()$path
  if (nzchar(ruta)) setwd(dirname(ruta))
}


#Cargamos los tres archivos CSV en R
poblacion <- read.csv2("poblacion.csv", stringsAsFactors = FALSE)
mercado_laboral <- read.csv2("mercado_laboral.csv", stringsAsFactors = FALSE)
renta <- read.csv2("renta.csv", stringsAsFactors = FALSE)

#Visualizamos los primeros registros de cada dataframe
head(poblacion)
head(mercado_laboral)
head(renta)

#Identificamos número de filas y columnas de cada uno
nrow(poblacion)
ncol(poblacion)

nrow(mercado_laboral)
ncol(mercado_laboral)

nrow(renta)
ncol(renta)

# Nombres de los atributos
colnames(poblacion)
colnames(mercado_laboral)
colnames(renta)

# Tipos de datos
str(poblacion)
str(mercado_laboral)
str(renta)

# Función estudiar_conjunto
estudiar_conjunto <- function(nombre_conjunto, datos) {
  print(paste("Conjunto", nombre_conjunto))
  print(paste("Registros:", nrow(datos), "    ---- N. Atributos:", ncol(datos)))
  print("Atributos:")
  print(colnames(datos))
  print(head(datos))
}

# Invocación de prueba requerida por el enunciado
estudiar_conjunto("Poblacion:", poblacion)
estudiar_conjunto("Poblacion:", mercado_laboral)
estudiar_conjunto("Poblacion:", renta)

# ==============================================================================
# ACTIVIDAD 2. LIMPIEZA Y PREPARACIÓN DE LOS DATOS
# ==============================================================================
# ------------------------------------------------------------------------------
# 1. FUNCIONES AUXILIARES
# ------------------------------------------------------------------------------

# Función para convertir valores de caracteres a numéricos
convertir_numerico <- function(vector_datos) {
  # Eliminamos los puntos de miles (usamos \\. porque el punto es un caracter especial)
  sin_puntos <- gsub("\\.", "", vector_datos)
  # Sustituimos la coma decimal por un punto
  con_punto <- gsub(",", ".", sin_puntos)
  # Forzamos la conversión a numérico
  return(as.numeric(con_punto))
}

# Función para limpiar el código numérico de las provincias
limpiar_nombres <- function(vector_nombres) {
  # Buscamos números al principio (^[0-9]+) seguidos de un espacio (\\s) y borramos
  return(gsub("^[0-9]+\\s", "", vector_nombres))
}

# ------------------------------------------------------------------------------
# 2.A. DATOS DE POBLACIÓN
# ------------------------------------------------------------------------------

# Seleccionamos registros del año 2023
pob_23 <- subset(poblacion, Periodo == 2023)

# Conservamos solo hombres y mujeres (descartamos "Total")
pob_23 <- subset(pob_23, Sexo %in% c("Hombres", "Mujeres"))

# Prescindimos de agregados (filtramos donde Provincias no esté vacío ni sea Total Nacional)
pob_23 <- subset(pob_23, Provincias != "" & Provincias != "Total Nacional")

# Limpiamos los nombres y convertimos el total a numérico
comunidad_limpia <- limpiar_nombres(pob_23$Comunidades.y.Ciudades.Autónomas)
provincia_limpia <- limpiar_nombres(pob_23$Provincias)
poblacion_num <- convertir_numerico(pob_23$Total)

# Creamos el dataframe final con las columnas renombradas
poblacion_limpia <- data.frame(
  comunidad = comunidad_limpia,
  provincia = provincia_limpia,
  anio = pob_23$Periodo,
  sexo = pob_23$Sexo,
  poblacion = poblacion_num,
  stringsAsFactors = FALSE
)

print("--- Comprobación Población 2023 ---")
print(head(poblacion_limpia))

# ------------------------------------------------------------------------------
# 2.B. DATOS DEL MERCADO LABORAL
# ------------------------------------------------------------------------------

# Seleccionamos los 4 trimestres de 2023
trimestres_2023 <- c("2023T1", "2023T2", "2023T3", "2023T4")
lab_23 <- subset(mercado_laboral, Periodo %in% trimestres_2023)

# Conservamos hombres y mujeres
lab_23 <- subset(lab_23, Sexo %in% c("Hombres", "Mujeres"))

# Eliminamos agregados autonómicos y nacionales
lab_23 <- subset(lab_23, Provincias != "" & Provincias != "Total Nacional")

# Limpiamos nombres y añadimos una columna de valor numérico
lab_23$provincia <- limpiar_nombres(lab_23$Provincias)
lab_23$valor <- convertir_numerico(lab_23$Total)

# Guardamos el resultado preparado
laboral_limpio <- lab_23

print("--- Comprobación Mercado Laboral 2023 ---")
print(head(laboral_limpio))

# ------------------------------------------------------------------------------
# 2.C. DATOS DE RENTA
# ------------------------------------------------------------------------------

# Seleccionamos registros del 2023
renta_23 <- subset(renta, Periodo == 2023)

# Eliminamos variables innecesarias / agregados
renta_23 <- subset(renta_23, Provincias != "" & Provincias != "Total Nacional")
renta_23 <- renta_23[!duplicated(renta_23$Provincias), ]

# Normalizamos las denominaciones de las provincias para que sirvan de clave
provincia_limpia_renta <- limpiar_nombres(renta_23$Provincias)
renta_num <- convertir_numerico(renta_23$Total)

# Conservamos solo provincia y renta media
renta_limpia <- data.frame(
  provincia = provincia_limpia_renta,
  renta_media = renta_num,
  stringsAsFactors = FALSE
)

print("--- Comprobación Renta 2023 ---")
print(head(renta_limpia))

# ==============================================================================
# ACTIVIDAD 3. CÁLCULO DE INDICADORES LABORALES ANUALES
# ==============================================================================

# 1. Separamos los datos según el indicador laboral utilizando subset y grepl()
# (Cambia 'Tasas' por el nombre de tu columna si es distinto)
datos_actividad <- subset(laboral_limpio, grepl("actividad", Tasas, ignore.case = TRUE))
datos_paro      <- subset(laboral_limpio, grepl("paro", Tasas, ignore.case = TRUE))
datos_empleo    <- subset(laboral_limpio, grepl("empleo", Tasas, ignore.case = TRUE))

# 2. Calculamos las medias anuales agrupando por provincia y Sexo
media_actividad <- aggregate(valor ~ provincia + Sexo, data = datos_actividad, FUN = mean)
media_paro      <- aggregate(valor ~ provincia + Sexo, data = datos_paro, FUN = mean)
media_empleo    <- aggregate(valor ~ provincia + Sexo, data = datos_empleo, FUN = mean)

# Renombramos la tercera columna para identificar la tasa correspondiente
colnames(media_actividad)[3] <- "tasa_actividad"
colnames(media_paro)[3]      <- "tasa_paro"
colnames(media_empleo)[3]    <- "tasa_empleo"

# 3. Comprobación de que se han utilizado los 4 trimestres
# Utilizamos FUN = length para contar el número de registros agrupados
comprobacion_trimestres <- aggregate(valor ~ provincia + Sexo, data = datos_actividad, FUN = length)
print("--- Comprobación de trimestres utilizados (debe indicar 4) ---")
print(head(comprobacion_trimestres))

# 4. Integración de los indicadores en un único dataframe (epa)
# Unimos utilizando la provincia y el Sexo como claves primarias
epa_temp <- merge(media_actividad, media_paro, by = c("provincia", "Sexo"))
epa      <- merge(epa_temp, media_empleo, by = c("provincia", "Sexo"))

# Renombramos la columna Sexo a minúsculas según muestra la tarea
colnames(epa)[colnames(epa) == "Sexo"] <- "sexo"

print("--- Resultado Final Actividad 3 (epa) ---")
print(head(epa))

# ==============================================================================
# ACTIVIDAD 4. INTEGRACIÓN DE LOS CONJUNTOS DE DATOS
# ==============================================================================

# Primera unión: Población y Mercado Laboral (epa)
# Claves de unión: provincia y sexo
datos_temp <- merge(poblacion_limpia, epa, by = c("provincia", "sexo"))

# Segunda unión: Añadir los datos de Renta
# Clave de unión: provincia (la renta se duplicará para Hombres y Mujeres de la misma provincia)
datos <- merge(datos_temp, renta_limpia, by = "provincia")

# Reordenación de columnas según las directrices de la tarea
datos <- datos[, c("comunidad", "provincia", "anio", "sexo", "poblacion", 
                   "tasa_actividad", "tasa_paro", "tasa_empleo", "renta_media")]

# Ordenación de las filas por Comunidad, Provincia y Sexo
datos <- datos[order(datos$provincia, datos$sexo), ]

# Restablecemos los índices de las filas tras la ordenación para mayor limpieza visual
rownames(datos) <- NULL

# ------------------------------------------------------------------------------
# Comprobaciones de calidad 
# ------------------------------------------------------------------------------
print("--- Comprobaciones de Calidad ---")

# a) Comprobación del número de registros (Debe ser 104)
print(paste("Total de registros (esperado 104):", nrow(datos)))

# b) Comprobación de duplicados (Debe ser FALSE)
duplicados <- any(duplicated(datos[, c("provincia", "sexo")]))
print(paste("¿Existen registros duplicados por provincia y sexo?:", duplicados))

# c) Comprobación de pérdida de datos (Debe ser FALSE)
perdidos <- any(is.na(datos))
print(paste("¿Existen valores nulos/perdidos tras las uniones?:", perdidos))

# d) Comprobación de tipos de datos
print("Estructura de tipos:")
str(datos)

# Muestra del resultado final ordenado
print("--- DATAFRAME FINAL INTEGRADO ---")
print(head(datos))

# ------------------------------------------------------------------------------
# Exportación a CSV
# ------------------------------------------------------------------------------
# Utilizamos write.csv2() para mantener el formato europeo (;) y (,)
# row.names = FALSE evita que se añada una primera columna con los números de fila
write.csv2(datos, file = "datos_integrados.csv", row.names = FALSE)

# ==============================================================================
# ACTIVIDAD 5. DESARROLLO DE UNA FUNCIÓN DE CONSULTA
# ==============================================================================

# Definición de la función consultar_provincia
consultar_provincia <- function(df, nombre_provincia) {
  
  # a. Validación básica de parámetros
  if (missing(df) || missing(nombre_provincia)) {
    stop("Debe proporcionar el dataframe y el nombre de la provincia.")
  }
  
  if (!is.data.frame(df)) {
    stop("El primer parámetro debe ser un dataframe válido.")
  }
  
  # b. Verificación de que la provincia existe en el conjunto de datos
  if (!(nombre_provincia %in% df$provincia)) {
    stop(paste("Provincia no encontrada:", nombre_provincia))
  }
  
  # d. Filtrado y retorno de los registros de la provincia (Hombres y Mujeres)
  resultado <- subset(df, provincia == nombre_provincia)
  
  return(resultado)
}

# ------------------------------------------------------------------------------
# Ejemplos de ejecución de la función
# ------------------------------------------------------------------------------

print("--- EJEMPLO 1: Consulta correcta (Granada) ---")
print(consultar_provincia(datos, "Granada"))

print("--- EJEMPLO 2: Consulta incorrecta (provincia con error tipográfico) ---")
# Esta línea generará el error controlado por la función
tryCatch({
  print(consultar_provincia(datos, "Graná"))
}, error = function(e) {
  message("Error capturado correctamente: ", e$message)
})


# ==============================================================================
# ACTIVIDAD 6. ANÁLISIS ESTADÍSTICO DE LOS DATOS
# ==============================================================================

print("--- a. MAYOR Y MENOR TASA DE PARO FEMENINA ---")
# Filtramos solo el registro de mujeres
mujeres_df <- subset(datos, sexo == "Mujeres")

# 5 provincias con mayor tasa de paro femenina
mayor_paro_mujeres <- mujeres_df[order(mujeres_df$tasa_paro, decreasing = TRUE), ]
print("Top 5 Provincias con mayor tasa de paro en mujeres:")
print(head(mayor_paro_mujeres[, c("provincia", "tasa_paro")], 5))

# 5 provincias con menor tasa de paro femenina
menor_paro_mujeres <- mujeres_df[order(mujeres_df$tasa_paro, decreasing = FALSE), ]
print("Top 5 Provincias con menor tasa de paro en mujeres:")
print(head(menor_paro_mujeres[, c("provincia", "tasa_paro")], 5))


print("--- b. MAYORES DIFERENCIAS DE PARO ENTRE HOMBRES Y MUJERES ---")
# Para calcular la brecha, separamos en dos dataframes (hombres y mujeres) y los unimos por provincia
hombres_df <- subset(datos, sexo == "Hombres")
comparativa_paro <- merge(
  hombres_df[, c("provincia", "tasa_paro")], 
  mujeres_df[, c("provincia", "tasa_paro")], 
  by = "provincia", 
  suffixes = c("_hombres", "_mujeres")
)
# Calculamos la diferencia absoluta (Mujeres - Hombres)
comparativa_paro$diferencia_paro <- abs(comparativa_paro$tasa_paro_mujeres - comparativa_paro$tasa_paro_hombres)
comparativa_paro <- comparativa_paro[order(comparativa_paro$diferencia_paro, decreasing = TRUE), ]

print("Provincias con mayor diferencia de tasa de paro (Mujeres vs Hombres):")
print(head(comparativa_paro, 5))


print("--- c. TASA MEDIA DE PARO EN PROVINCIAS ANDALUZAS (POR SEXO) ---")
# Filtramos las provincias de Andalucía usando las comunidades del dataframe
andalucia_df <- subset(datos, comunidad == "Andalucía")
media_paro_andalucia <- tapply(andalucia_df$tasa_paro, andalucia_df$sexo, mean)
print("Media de la tasa de paro en Andalucía por sexo:")
print(media_paro_andalucia)


print("--- d. CINCO PROVINCIAS CON MAYOR RENTA MEDIA POR PERSONA ---")
# Como la renta se repite para Hombres y Mujeres de la misma provincia, extraemos registros únicos por provincia
provincias_unicas <- datos[!duplicated(datos$provincia), ]
provincias_unicas <- provincias_unicas[order(provincias_unicas$renta_media, decreasing = TRUE), ]

print("Top 5 Provincias con mayor renta media por persona:")
print(head(provincias_unicas[, c("provincia", "renta_media", "comunidad")], 5))


print("--- e. ANÁLISIS DE CORRELACIÓN ENTRE RENTA Y TASA DE PARO ---")
# Correlación general para el total del dataset (usando cor() con Pearson por defecto)
correlacion_total <- cor(datos$renta_media, datos$tasa_paro, method = "pearson", use = "complete.obs")
print(paste("Coeficiente de correlación de Pearson (Renta vs Tasa de Paro - Total):", round(correlacion_total, 4)))

# Correlación segmentada por sexo para mayor profundidad analítica
cor_hombres <- cor(hombres_df$renta_media, hombres_df$tasa_paro, method = "pearson", use = "complete.obs")
cor_mujeres <- cor(mujeres_df$renta_media, mujeres_df$tasa_paro, method = "pearson", use = "complete.obs")

print(paste("Correlación Renta vs Paro en Hombres:", round(cor_hombres, 4)))
print(paste("Correlación Renta vs Paro en Mujeres:", round(cor_mujeres, 4)))



