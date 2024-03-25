#' Definición de la clase Solucion
#'
#' @param num Número de la solución
#' @param data Conjunto de datos
#' @param costes Vector de costes
#' @param inputs Nombres de las variables de entrada
#' @param output Nombre de la variable de salida
#' @param num_features Número de características
#'
#' @return Objeto de clase Solucion
#' @export
Solucion <- function(num, data, costes, inputs, output, num_features) {
  # Definir variables de instancia
  solucion <- list(
    num = num,
    data = data,
    costes = costes,
    inputs = inputs,
    output = output,
    num_dim = length(inputs),
    num_features = num_features,
    data_sol = data.frame(),  # Inicializar data_sol como un dataframe vacío
    features = list(),
    vectors = numeric(0),
    plano_coord = numeric(num_features),  # Inicializar plano_coord como un vector numérico
    plano_termino_b = NULL,
    obj_coste = NULL,
    obj_distancia = NULL,
    obj_epsilon = NULL,
    obj_mal_clasificados = list(mc_pos = NULL, mc_neg = NULL) # Inicializar obj_mal_clasificados como una lista
  )

  return(solucion)
}

#' Método para convertir la solución a un diccionario
#'
#' @param solucion Objeto de clase Solucion
#'
#' @return Lista con la información de la solución
#' @export
to_dict <- function(solucion) {
  return(list(
    "SOL" = solucion$num,
    "VECTORS" = solucion$vectors,
    "PLANO_COOR" = solucion$plano_coord,
    "PLANO_B" = solucion$plano_termino_b,
    "FEATURES" = solucion$features,
    "DIST" = solucion$obj_distancia,
    "EPS" = solucion$obj_epsilon,
    "COSTE" = solucion$obj_coste,
    "MC+" = solucion$obj_mal_clasificados$mc_pos,
    "MC-" = solucion$obj_mal_clasificados$mc_neg
  ))
}

#' Método para obtener un vector de clase aleatorio
#'
#' @param solucion Objeto de clase Solucion
#' @param clase Clase para la que se desea obtener un vector
#'
#' @return Índice del vector de clase aleatorio
#' @export
obtener_vector_clase <- function(solucion, clase) {
  vectores_clase <- which(solucion$data[[solucion$output]] == clase)
  vector <- sample(vectores_clase, 1)
  return(vector)
}

#' Método para generar una solución aleatoria
#'
#' @param solucion Objeto de clase Solucion
#'
#' @return Objeto de clase Solucion con una solución aleatoria generada
#' @export
generar_solucion_aleatoria <- function(solucion) {
  # Seleccionamos dos puntos de cada clase
  clases <- unique(solucion$data[[solucion$output]])
  for (clase in clases) {
    vector <- obtener_vector_clase(solucion, clase)
    solucion$vectors <- c(solucion$vectors, vector)
  }

  # Seleccionamos aleatoriamente p características
  if (solucion$num_features == solucion$num_dim) {
    solucion$features <- solucion$inputs
  } else {
    fea_posibles <- solucion$inputs
    while (length(solucion$features) != solucion$num_features) {
      feature <- sample(fea_posibles, 1)
      fea_posibles <- fea_posibles[fea_posibles != feature]
      solucion$features <- c(solucion$features, feature)
    }
    solucion$features <- sort(solucion$features)
  }

  # Calcular las coordenadas del plano
  solucion$plano_coord <- runif(solucion$num_features, -1, 1)

  # Seleccionar los datos correspondientes a los vectores
  solucion$data_sol <- solucion$data[solucion$vectors, , drop = FALSE]
  solucion$data_sol <- solucion$data_sol[, solucion$features]

  return(solucion)
}

#' Método para construir los planos
#'
#' @param solucion Objeto de clase Solucion
#'
#' @return Objeto de clase Solucion con los planos construidos
#' @export
construir_planos <- function(solucion) {
  data_sol <- solucion$data[solucion$vectors, , drop = FALSE]
  data_sol <- data_sol[, solucion$features]
  plano_termino_b <- numeric(0)

  for (i in 1:nrow(data_sol)) {
    indep <- 0
    for (j in 1:length(solucion$features)) {
      indep <- indep + data_sol[i, j] * solucion$plano_coord[j]
    }
    plano_termino_b <- c(plano_termino_b, -indep)
  }
  # Calcular el término de sesgo como la suma de todos los términos independientes dividida por 2
  sesgo <- sum(plano_termino_b) / 2
  solucion$plano_termino_b <- c(plano_termino_b, sesgo)

  return(solucion)  # Devolver solucion con plano_termino_b actualizado
}

#' Método para calcular el objetivo de distancia
#'
#' @param solucion Objeto de clase Solucion
#'
#' @return Objeto de clase Solucion con el objetivo de distancia calculado
#' @export
calcular_objetivo_distancia <- function(solucion) {
  denominador <- 0
  for (i in 1:length(solucion$features)) {
    denominador <- denominador + solucion$plano_coord[i]^2
  }
  denominador <- sqrt(denominador)

  if (length(solucion$plano_termino_b) >= 2 & denominador != 0) {
    distancia <- abs(solucion$plano_termino_b[1] - solucion$plano_termino_b[2])
    solucion$obj_distancia <- distancia / denominador
  } else {
    solucion$obj_distancia <- -1
  }
  return(solucion)  # Devolver solucion con obj_distancia actualizado
}

#' Método para calcular el objetivo de epsilon
#'
#' @param solucion Objeto de clase Solucion
#'
#' @return Objeto de clase Solucion con el objetivo de epsilon calculado
#' @export
calcular_objetivo_epsilon <- function(solucion) {
  solucion$mc_pos <- 0
  solucion$mc_neg <- 0

  for (i in 1:nrow(solucion$data)) {
    clase <- solucion$data[i, solucion$output]
    fx <- sum(solucion$plano_coord * solucion$data[i, solucion$features])

    if (clase == 1) {
      solucion$mc_pos <- solucion$mc_pos + 1
    } else {
      solucion$mc_neg <- solucion$mc_neg + 1
    }
  }

  solucion$obj_epsilon <- abs(solucion$mc_pos - solucion$mc_neg) / nrow(solucion$data)

  return(solucion)  # Devolver solucion con obj_epsilon actualizado
}

#' Método para evaluar la solución
#'
#' @param solucion Objeto de clase Solucion
#'
#' @return Objeto de clase Solucion con la evaluación realizada
#' @export
evaluar_solucion <- function(solucion) {
  if (sum(solucion$plano_coord) == 0) {
    print("La suma de las coordenadas del plano es 0. La solución no es válida.")
    return(0) # Si las coordenadas del plano suman 0, la solución no es válida
  } else {
    print("Plano construido correctamente.")
    solucion <- construir_planos(solucion)  # Actualizar solucion con plano_termino_b

    print("Objetivo de distancia calculado.")
    solucion <- calcular_objetivo_distancia(solucion)  # Actualizar solucion con obj_distancia

    print("Objetivo de epsilon calculado.")
    solucion <- calcular_objetivo_epsilon(solucion)  # Actualizar solucion con obj_epsilon

    # calcular_objetivo_costes(solucion)
    # calcular_puntos_bien_clasif(solucion)
    return(solucion)
  }
}

# # Método para calcular los puntos bien clasificados
# calcular_puntos_bien_clasif <- function(solucion) {
#   eval <- 0
#   k <- solucion$vectors[1]  # Usamos el primer índice
#
#   puntos_mal_clasif <- c(0, 0)
#
#   # Evaluamos el punto en primera posición (tipo 0 o tipo -)
#   for (j in 1:solucion$num_features) {
#     eval <- eval + (solucion$plano_coord[j] * solucion$data_sol[k, solucion$features[j]])
#   }
#
#   if (eval > -solucion$plano_termino_b[2]) {
#     solucion$signo_intermedio <- c(1, 0)
#   } else {
#     solucion$signo_intermedio <- c(0, 1)
#   }
#
#   # Evaluamos los puntos de cada tipo
#   for (m in 1:length(solucion$obj_mal_clasificados)) {
#     for (i in 1:solucion$obj_mal_clasificados[[m]]) {
#       resul_eval <- evaluar_punto_plano_intermedio(solucion$num, solucion$data_indices[[m]][i])
#
#       if (resul_eval != solucion$signo_intermedio[m]) {  # Usando el signo_intermedio de la solución
#         solucion$obj_mal_clasificados[[m]] <- solucion$obj_mal_clasificados[[m]] + 1  # Actualizando el conteo de puntos mal clasificados
#       }
#     }
#   }
# }
#
# # Método para calcular el objetivo de costes
# calcular_objetivo_costes <- function(solucion) {
#   solucion$obj_coste <- 0
#   for (feature in solucion$features) {
#     solucion$obj_coste <- solucion$obj_coste + solucion$costes[feature]
#   }
# }




# #Función para calcular la distancia de un punto dado al plano intermedio de la solución. Devuelve el resultado.
# calcular_distancia_plano_intermedio <- function(num_sol, indice) {
#   denominador <- 0
#   distancia <- 0
#
#   for (j in 1:NUM_FEATURES) {
#     distancia <- distancia + poblacion$solucion[[num_sol]]$plano_coord[j] * datos$punto[[indice]]$coord[[poblacion$solucion[[num_sol]]$features[j]]]
#     denominador <- denominador + poblacion$solucion[[num_sol]]$plano_coord[j]^2
#   }
#
#   denominador <- sqrt(denominador)
#
#   distancia <- distancia + poblacion$solucion[[num_sol]]$plano_termino[[NUM_TIPOS]]
#
#   if (distancia < 0) {
#     distancia <- -distancia
#   }
#
#   distancia <- distancia / denominador
#
#   return(distancia)
# }
#
# evaluar_punto_plano_intermedio <- function(num_sol, indice) {
#   # Esta función evalúa el punto cuyo indice se pasa como argumento en el plano intermedio,
#   # devolviendo un 0 si es menor que el termino independiente de éste,
#   # un 1 si es mayor y un 2 si es igual, es decir, está en el plano intermedio.
#   # LOS PUNTOS QUE CAEN EN EL PLANO INTERMEDIO SE CONSIDERAN MAL CLASIFICADOS
#
#   eval <- 0
#
#   for (j in 1:NUM_FEATURES) {
#     eval <- eval + (poblacion$solucion[[num_sol]]$plano_coord[j] * datos$punto[[indice]]$coord[[poblacion$solucion[[num_sol]]$features[j]]])
#   }
#
#   if (eval > -poblacion$solucion[[num_sol]]$plano_termino[[NUM_TIPOS]]) {
#     return(1)
#   } else if (eval < -poblacion$solucion[[num_sol]]$plano_termino[[NUM_TIPOS]]) {
#     return(0)
#   } else {
#     return(2)
#   }
# }
#
# # Esta función realiza el cruce de dos soluciones padres y genera dos soluciones hijos.
# # Las dos mejores soluciones de los cuatro generados se conservan.
# cruzar_soluciones <- function(num_sol1, num_sol2, hijo) {
#   dominador1 <- domina(hijo, hijo + 1)
#   posicion1 <- hijo
#   if (dominador1 == 1) {
#     posicion1 <- hijo + 1
#   } else if (dominador1 == 0) {
#     random <- runif(1)
#     if (random < 0.5) {
#       posicion1 <- hijo + 1
#     }
#   }
#
#   dominador2 <- domina(hijo + 2, hijo + 3)
#   posicion2 <- hijo + 2
#   if (dominador2 == 2) {
#     posicion2 <- hijo + 3
#   } else if (dominador2 == 0) {
#     random <- runif(1)
#     if (random < 0.5) {
#       posicion2 <- hijo + 3
#     }
#   }
#
#   # Se conservan las dos mejores soluciones de las cuatro generadas
#   for (j in 1:NUM_FEATURES) {
#     poblacion$solucion[posicion1]$features[j] <- poblacion$solucion[posicion2]$features[j]
#     poblacion$solucion[posicion1]$plano_coord[j] <- poblacion$solucion[posicion2]$plano_coord[j]
#   }
#   poblacion$solucion[posicion1]$indices[1] <- poblacion$solucion[posicion2]$indices[1]
#   poblacion$solucion[posicion1]$indices[2] <- poblacion$solucion[posicion2]$indices[2]
# }
#
# # Esta función realiza un torneo entre dos soluciones padres y devuelve el índice del mejor padre.
# # Se eligen dos padres aleatorios y se comparan usando la función compara_padres.
# torneo_elegir_padre <- function() {
#   padre1 <- sample(1:TAM_POBLACION, 1)
#   padre2 <- sample(1:TAM_POBLACION, 1)
#   while (padre1 == padre2) {
#     padre2 <- sample(1:TAM_POBLACION, 1)
#   }
#   return(compara_padres(padre1, padre2))
# }
#
# # Esta función muta una solución.
# mutar_solucion <- function(num_sol) {
#   # Mutación de índices
#   random <- runif(1)
#   if (random < P_MUT_IND) {
#     indice <- sample(1:datos$tot_tipo[1], 1)
#     poblacion$solucion[[num_sol]]$indices[1] <- datos$indices[[1]][indice]
#   }
#   if (random < P_MUT_IND) {
#     indice <- sample(1:datos$tot_tipo[2], 1)
#     poblacion$solucion[[num_sol]]$indices[2] <- datos$indices[[2]][indice]
#   }
#
#   if (NUM_DIM > NUM_FEATURES) {
#     features_solucion <- rep(0, NUM_DIM)
#     for (i in 1:NUM_FEATURES) {
#       features_solucion[poblacion$solucion[[num_sol]]$features[i]] <- 1
#     }
#     volver_feat <- 0
#     if (2 * NUM_FEATURES >= NUM_DIM) volver_feat <- 1
#     for (i in 1:NUM_FEATURES) {
#       random <- runif(1)
#       if (random <= P_MUT_FEAT) {
#         indice <- sample(1:NUM_DIM, 1)
#         while (features_solucion[indice] == 1) {
#           indice <- indice %% NUM_DIM + 1
#         }
#         if (volver_feat == 1) {
#           features_solucion[poblacion$solucion[[num_sol]]$features[i]] <- 0
#         }
#         poblacion$solucion[[num_sol]]$features[i] <- indice
#         poblacion$solucion[[num_sol]]$plano_coord[i] <- runif(1)
#         features_solucion[indice] <- 1
#       }
#     }
#   }
#
#   for (i in 1:NUM_FEATURES) {
#     random <- runif(1)
#     if (random <= P_MUT_COORD) {
#       if (TIPO_MUT_COORD == 0) {
#         porcentaje_variar <- sample(5:25, 1)
#         random <- runif(1)
#         if (random < 0.5) {
#           poblacion$solucion[[num_sol]]$plano_coord[i] <- poblacion$solucion[[num_sol]]$plano_coord[i] * (1 + porcentaje_variar / 100)
#           if (poblacion$solucion[[num_sol]]$plano_coord[i] > 1) {
#             poblacion$solucion[[num_sol]]$plano_coord <- poblacion$solucion[[num_sol]]$plano_coord / 10
#           }
#         } else {
#           poblacion$solucion[[num_sol]]$plano_coord[i] <- poblacion$solucion[[num_sol]]$plano_coord[i] * (1 - porcentaje_variar / 100)
#           if (poblacion$solucion[[num_sol]]$plano_coord[i] < -1) {
#             poblacion$solucion[[num_sol]]$plano_coord <- poblacion$solucion[[num_sol]]$plano_coord / 10
#           }
#         }
#       } else {
#         poblacion$solucion[[num_sol]]$plano_coord[i] <- runif(1, -1, 1)
#       }
#     }
#   }
# }
#
# #QUICKSORT
# # Función para intercambiar dos elementos de un vector
# intercambiar <- function(arreglo, a, b) {
#   temporal <- arreglo[a]
#   arreglo[a] <- arreglo[b]
#   arreglo[b] <- temporal
# }
#
# # Función para realizar la partición en Quicksort
# particion <- function(arreglo, izquierda, derecha, objetivo) {
#   pivote <- switch(
#     objetivo,
#     "0" = poblacion$solucion[[arreglo[izquierda]]]$obj[1],
#     "1" = poblacion$solucion[[arreglo[izquierda]]]$obj[2],
#     "2" = poblacion$solucion[[arreglo[izquierda]]]$i_distance,
#     "3" = arreglo[izquierda]
#   )
#
#   while (TRUE) {
#     while (switch(
#       objetivo,
#       "0" = poblacion$solucion[[arreglo[izquierda]]]$obj[1] < pivote,
#       "1" = poblacion$solucion[[arreglo[izquierda]]]$obj[2] < pivote,
#       "2" = poblacion$solucion[[arreglo[izquierda]]]$i_distance < pivote,
#       "3" = arreglo[izquierda] < pivote
#     )) {
#       izquierda <- izquierda + 1
#     }
#     while (switch(
#       objetivo,
#       "0" = poblacion$solucion[[arreglo[derecha]]]$obj[1] > pivote,
#       "1" = poblacion$solucion[[arreglo[derecha]]]$obj[2] > pivote,
#       "2" = poblacion$solucion[[arreglo[derecha]]]$i_distance > pivote,
#       "3" = arreglo[derecha] > pivote
#     )) {
#       derecha <- derecha - 1
#     }
#     if (izquierda >= derecha) {
#       return(derecha)
#     } else {
#       intercambiar(arreglo, izquierda, derecha)
#       izquierda <- izquierda + 1
#       derecha <- derecha - 1
#     }
#   }
# }
#
# # Función Quicksort
# quicksort <- function(arreglo, izquierda, derecha, objetivo) {
#   if (izquierda < derecha) {
#     indiceParticion <- particion(arreglo, izquierda, derecha, objetivo)
#     quicksort(arreglo, izquierda, indiceParticion, objetivo)
#     quicksort(arreglo, indiceParticion + 1, derecha, objetivo)
#   }
# }



