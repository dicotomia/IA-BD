# ================================================================
# Ejercicios de clase Volumen 2: Dataframes
# Autor: ChatGPT y Miguel A. Lopez (por ese orden)
# 30/10/2025
# ================================================================

# ----------------------------------------------------------------
# Ejercicio 1: Creación de un data frame
# Enunciado:
#   Cree un data frame llamado df_alumnos con las columnas:
#     nombre  = c("Ana", "Luis", "Marta", "Jorge", "Eva")
#     edad    = c(22, 25, 24, 23, 27)
#     nota    = c(8.5, 6.0, 9.2, 4.8, 7.5)
#   Asegúrese de que 'nombre' sea carácter y no factor.

nombre  <- c("Ana", "Luis", "Marta", "Jorge", "Eva")
edad    <- c(22, 25, 24, 23, 27)
nota    <- c(8.5, 6.0, 9.2, 4.8, 7.5)

df_alumnos <- data.frame(Nombre = nombre, Edad = edad, Nota = nota)
print(df_alumnos)


# ----------------------------------------------------------------
# Ejercicio 2: Selección de filas por condición
# Enunciado:
#   Obtenga las filas de df_alumnos correspondientes
#   a estudiantes con nota >= 7.

#utilizando la funcion subset para extraer un subconjunto
alumnos_mas7 <- subset(df_alumnos, Nota >= 7)
print(alumnos_mas7)

#aplicando un filtro en base a una condición
alumnos_mas7 <- df_alumnos[df_alumnos$Nota >= 7]
print(alumnos_mas7)

# ----------------------------------------------------------------
# Ejercicio 3: Selección de columnas
# Enunciado:
#   Extraiga únicamente las columnas nombre y nota
#   en un nuevo data frame df_notas.

alumnos_mas7 <- df_alumnos[df_alumnos$Nota >= 7,c(1,3) ]
print(alumnos_mas7)

alumnos_mas7 <- df_alumnos[df_alumnos$Nota >= 7,c("Nombre","Nota") ]
print(alumnos_mas7)

# ----------------------------------------------------------------
# Ejercicio 4: Nueva columna calculada
# Enunciado:
#   Añada a df_alumnos una columna llamada "aprobado"
#   que valga TRUE si nota >= 5 y FALSE en caso contrario.

df_alumnos$Aprobado <- df_alumnos$Nota >= 5
print(df_alumnos)

# ----------------------------------------------------------------
# Ejercicio 5: Reordenación por una columna
# Enunciado:
#   Ordene df_alumnos de mayor a menor nota.
#   Guarde el resultado en df_ordenado.

df_ordenado <- df_alumnos[order(-df_alumnos$Nota), ]
rownames(df_ordenado) <- NULL 
print(df_ordenado)

df_ordenado2 <- df_alumnos[rev(order(df_alumnos$Nota)), ]
rownames(df_ordenado2) <- NULL 
print(df_ordenado2)

df_ordenado3 <- df_alumnos[rev(sort.list(df_alumnos$Nota)),]
rownames(df_ordenado3) <- NULL 
print(df_ordenado3)

# ----------------------------------------------------------------
# Ejercicio 6: Filtrado combinado
# Enunciado:
#   Seleccione las filas de df_alumnos donde:
#     - aprobado == TRUE
#     - edad es estrictamente mayor que 23
#   Guarde el resultado en df_filtrado.

df_filtrado <- df_alumnos[df_alumnos$Aprobado == TRUE & df_alumnos$Edad > 23,]
print(df_filtrado)

# ----------------------------------------------------------------
# Ejercicio 7: Estadísticas agrupadas "a mano"
# Enunciado:
#   Suponga que cada alumno pertenece a un grupo:
#     grupo = c("A", "A", "B", "B", "A")
#   Añada esta columna a df_alumnos y calcule la nota media por grupo
#   usando tapply().

nombre  <- c("Ana", "Luis", "Marta", "Jorge", "Eva")
edad    <- c(22, 25, 24, 23, 27)
nota    <- c(8.5, 6.0, 9.2, 4.8, 7.5)
grupo   <- c("A", "A", "B", "B", "A")

df_alumnos <- data.frame(Nombre = nombre, Edad = edad, Nota = nota, Grupo =grupo)
print(df_alumnos)

#media total
paste("La media total es:", mean(df_alumnos$Nota))
paste("La media total es:", sum(df_alumnos$Nota)/length(df_alumnos$Nota))

#media por grupos con tapply
nota_media <- tapply(df_alumnos$Nota, df_alumnos$Grupo, mean)
print(nota_media)

#media por grupos con aggregate
nota_media <- aggregate(df_alumnos$Nota ~ df_alumnos$Grupo, data = df_alumnos, FUN = mean)
print(nota_media)

# ----------------------------------------------------------------
# Ejercicio 8: Eliminación de filas con NA
# Enunciado:
#   Cree un nuevo data frame df_sensores_magicos con las columnas:
#     sensor_id = c("S1", "S2", "S3", "S4", "S5")
#     temperatura = c(20.1, NA, 19.8, NA, 22.5)
#     activo = c(TRUE, TRUE, FALSE, TRUE, TRUE)
#   Elimine todas las filas que tengan NA en la columna temperatura.

sensor_id = c("S1", "S2", "S3", "S4", "S5")
temperatura = c(20.1, NA, 19.8, NA, 22.5)
activo = c(TRUE, TRUE, FALSE, TRUE, TRUE)

df_sensores_magicos <- data.frame(id = sensor_id, temp = temperatura, activ = activo)
df_sensores_magicos_noNA <- df_sensores_magicos[!is.na(df_sensores_magicos$temp),c(1,2)]
print(df_sensores_sinNA)

# En lugar de construir un dataframe con registros que satisfacen una condición
# generar un índice de dichos registros.
index <- which(!is.na(df_sensores_magicos$temp))
df_sensores_magicos[index,c("id","temp")]

# ----------------------------------------------------------------
# Ejercicio 9: Unión por posición y por nombre
# Enunciado:
#   Considere dos data frames pequeños:
#     df_ids <- data.frame(
#       id   = c(101, 102, 103),
#       user = c("u1", "u2", "u3"),
#       stringsAsFactors = FALSE
#     )
#     df_scores <- data.frame(
#       user = c("u2", "u1", "u4"),
#       score = c(15, 20, 5),
#       stringsAsFactors = FALSE
#     )
#   1) Haga un "merge" por la columna común 'user' para unir la puntuación.
#   2) Interprete por qué un usuario puede tener NA.

df_ids <- data.frame(
  id   = c(101, 102, 103),
  user = c("u1", "u2", "u3"),
  stringsAsFactors = FALSE
)
df_scores <- data.frame(
  user = c("u2", "u1", "u4"),
  score = c(15, 20, 5),
  stringsAsFactors = FALSE
)
puntuacion <- merge(df_ids, df_scores, by ="user")
print(puntuacion)

# all.x izquierda, all.y derecha, all todos
puntuacion <- merge(df_ids, df_scores, by="user", all = TRUE)
print(puntuacion)

# ----------------------------------------------------------------
# Ejercicio 10: Resumen estadístico columna a columna
# Enunciado:
#   Usando df_alumnos (con las columnas edad y nota),
#   calcule para cada columna numérica:
#     mínimo, máximo, media y sd.
#   Hágalo usando sapply() sobre un subconjunto numérico del data frame.
# Solución:


nombre  <- c("Ana", "Luis", "Marta", "Jorge", "Eva")
edad    <- c(22, 25, 24, 23, 27)
nota    <- c(8.5, 6.0, 9.2, 4.8, 7.5)
grupo   <- c("A", "A", "B", "B", "A")

df_alumnos <- data.frame(Nombre = nombre, Edad = edad, Nota = nota, Grupo =grupo)
print(df_alumnos)

df_num <- df_alumnos[sapply(df_alumnos, is.numeric)]
operaciones <- sapply(df_num, function(i) c(
  min <- min(i, na.rm = TRUE),
  max <- max(i, na.rm = TRUE),
  mean <- mean(i, na.rm = TRUE),
  sd <- sd(i, na.rm = TRUE)
))
print(operaciones)

summary(df_alumnos)
