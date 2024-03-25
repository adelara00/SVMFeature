# Definición de la clase Poblacion
Poblacion <- function(data, costes, tam_pob, inputs, output, num_features, clones = 0) {
  # Métodos públicos de la clase
  poblacion <- list(
    num_features = num_features,
    inputs = inputs,
    output = output,
    num_dim = length(inputs),
    data = data,
    costes = costes,
    tam_pob = tam_pob,
    clones = clones,
    plano_termino_b = NULL,
    lista_soluciones = NULL,
    num_fronts = NULL,
    num_sol_fron = NULL,
    df_soluciones = NULL
  )
}

generar_poblacion_inicial <- function(poblacion) {
  cat("Generando poblacion", "\n")
  
  num_sol <- 1
  
  while (length(poblacion$lista_soluciones) < poblacion$tam_pob) {
    cat("Número de soluciones generadas hasta ahora:", length(poblacion$lista_soluciones), "\n")
    
    sol <- Solucion(num_sol, poblacion$data, poblacion$costes, poblacion$inputs, poblacion$output, poblacion$num_features)
    sol_alt <- generar_solucion_aleatoria(sol)
    poblacion$lista_soluciones <- c(poblacion$lista_soluciones, list(sol_alt))
    num_sol <- num_sol + 1
  }
  
  cat("Población creada\n")
  
  return(poblacion)  # Devolver la población actualizada
}




#BUENO PERO NO VAN MUTACIONES
# generar_poblacion_inicial <- function(poblacion) {
#   cat("Generando poblacion inicial de tamaño ", poblacion$tam_pob, "\n")
#   
#   num_sol <- 1
#   
#   while (length(poblacion$lista_soluciones) < poblacion$tam_pob) {
#     sol <- Solucion(num_sol, poblacion$data, poblacion$costes, poblacion$inputs, poblacion$output, poblacion$num_features)
#     sol_alt <- generar_solucion_aleatoria(sol)
#     
#     if (evaluar_solucion(sol_alt) == 1) {
#       if (poblacion$clones == 1) {
#         poblacion$lista_soluciones <- append(poblacion$lista_soluciones, sol_alt)
#         num_sol <- num_sol + 1
#       } else {
#         if (!comprobar_clones(sol_alt, poblacion$lista_soluciones)) {
#           poblacion$lista_soluciones <- append(poblacion$lista_soluciones, sol_alt)
#           num_sol <- num_sol + 1
#         }
#       }
#     }
#     # Asegurémonos de actualizar la longitud de la lista después de agregar una nueva solución
#     print(length(poblacion$lista_soluciones))
#   }
# }

imprimir_poblacion <- function(poblacion) {
  if (length(poblacion$lista_soluciones) == 0) {
    cat("La población está vacía. Primero genera la población inicial.\n")
    return(NULL)
  }
  
  df_soluciones <- data.frame(
    SOL = numeric(length(poblacion$lista_soluciones)),
    VECTORS = numeric(length(poblacion$lista_soluciones)),
    PLANO_COOR = numeric(length(poblacion$lista_soluciones)),
    PLANO_B = numeric(length(poblacion$lista_soluciones)),
    FEATURES = list(length(poblacion$lista_soluciones)),
    DIST = numeric(length(poblacion$lista_soluciones)),
    EPS = numeric(length(poblacion$lista_soluciones)),
    COSTE = numeric(length(poblacion$lista_soluciones)),
    `MC+` = numeric(length(poblacion$lista_soluciones)),
    `MC-` = numeric(length(poblacion$lista_soluciones))
  )
  
  for (i in 1:length(poblacion$lista_soluciones)) {
    solucion <- poblacion$lista_soluciones[[i]]
    sol_dict <- to_dict(solucion)
    
    df_soluciones[i, "SOL"] <- sol_dict$SOL
    df_soluciones[i, "VECTORS"] <- paste(sol_dict$VECTORS, collapse = ",")
    df_soluciones[i, "PLANO_COOR"] <- paste(sol_dict$PLANO_COOR, collapse = ",")
    df_soluciones[i, "PLANO_B"] <- paste(sol_dict$PLANO_B, collapse = ",")
    df_soluciones[i, "FEATURES"] <- paste(unlist(sol_dict$FEATURES), collapse = ",")
    
    # Verificar si sol_dict$DIST es NULL antes de asignarlo
    if (!is.null(sol_dict$DIST)) {
      df_soluciones[i, "DIST"] <- sol_dict$DIST
    }
    
    # Verificar si sol_dict$EPS es NULL antes de asignarlo
    if (!is.null(sol_dict$EPS)) {
      df_soluciones[i, "EPS"] <- sol_dict$EPS
    }
    
    # Verificar si sol_dict$COSTE es NULL antes de asignarlo
    if (!is.null(sol_dict$COSTE)) {
      df_soluciones[i, "COSTE"] <- sol_dict$COSTE
    }
    
    # Verificar si sol_dict$`MC+` es NULL o de longitud cero antes de asignarlo
    if (!is.null(sol_dict$`MC+`) && length(sol_dict$`MC+`) > 0) {
      df_soluciones[i, "MC+"] <- sol_dict$`MC+`
    }
    
    # Verificar si sol_dict$`MC-` es NULL o de longitud cero antes de asignarlo
    if (!is.null(sol_dict$`MC-`) && length(sol_dict$`MC-`) > 0) {
      df_soluciones[i, "MC-"] <- sol_dict$`MC-`
    }
  }
  
  print(df_soluciones)
}



equal <- function(solucion, other) {
  if (identical(solucion$vectors, other$vectors) && 
      identical(solucion$plano_coord, other$plano_coord) &&
      all.equal(solucion$features, other$features)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}


# Método para comprobar clones en la población
comprobar_clones <- function(solucion_eva, lista_soluciones) {
  clon <- FALSE
  for (sol in lista_soluciones) {
    if (equal(sol, solucion_eva)) {
      clon <- TRUE
      break
    }
  }
  return(clon)
}

# Método para el algoritmo de clasificación no dominada rápida
fast_non_dominated_sort <- function(poblacion) {
  tam_pob <- length(poblacion$lista_soluciones)
  num_sol_me_dom_aux <- rep(0, 2 * tam_pob)
  
  cat("\n\nRealizando FNDS desde 0 hasta", tam_pob, "......")
  
  num_sol_front <- rep(0, tam_pob)
  num_fronts <- 0
  
  for (k in 1:tam_pob) {
    num_sol_front[k] <- 0
  }
  
  for (p in 1:(tam_pob)) {
    poblacion$lista_soluciones[[p]]$num_sol_me_dom <- 0
    poblacion$lista_soluciones[[p]]$num_sol_domina <- 0
    poblacion$lista_soluciones[[p]]$front <- -1
    
    for (q in 1:(tam_pob)) {
      if (p != q) {
        dom <- domina(poblacion$lista_soluciones[[p]], poblacion$lista_soluciones[[q]])
        
        if (dom == 1) { # si p domina a q
          poblacion$lista_soluciones[[p]]$sol_domina[poblacion$lista_soluciones[[p]]$num_sol_domina + 1] <- q
          poblacion$lista_soluciones[[p]]$num_sol_domina <- poblacion$lista_soluciones[[p]]$num_sol_domina + 1
        } else if (dom == 2) {
          poblacion$lista_soluciones[[p]]$num_sol_me_dom <- poblacion$lista_soluciones[[p]]$num_sol_me_dom + 1
        }
      }
    }
    
    if (poblacion$lista_soluciones[[p]]$num_sol_me_dom == 0) {
      poblacion$lista_soluciones[[p]]$front <- 0
      num_sol_front[1] <- num_sol_front[1] + 1
    }
  }
  
  front_cont <- 0
  seguir <- TRUE
  
  for (p in 1:(tam_pob)) {
    num_sol_me_dom_aux[p] <- poblacion$lista_soluciones[[p]]$num_sol_me_dom
  }
  
  while (seguir) {
    seguir <- FALSE
    
    for (p in 1:(tam_pob)) {
      if (poblacion$lista_soluciones[[p]]$front == front_cont) {
        for (k in 1:poblacion$lista_soluciones[[p]]$num_sol_domina) {
          num_sol_me_dom_aux[poblacion$lista_soluciones[[p]]$sol_domina[[k]]] <- num_sol_me_dom_aux[poblacion$lista_soluciones[[p]]$sol_domina[[k]]] - 1
          
          if (num_sol_me_dom_aux[poblacion$lista_soluciones[[p]]$sol_domina[[k]]] == 0) {
            poblacion$lista_soluciones[[poblacion$lista_soluciones[[p]]$sol_domina[[k]]]]$front <- front_cont + 1
            num_sol_front[front_cont + 1] <- num_sol_front[front_cont + 1] + 1
            seguir <- TRUE
          }
        }
      }
    }
    
    front_cont <- front_cont + 1
  }
  
  num_fronts <- front_cont
  cat("\nNº fronts:", front_cont)
  cat("\nNº SND:", num_sol_front[1])
}


# # Esta función compara dos soluciones padres en función de varios criterios y devuelve el índice de la mejor solución.
# # Los criterios de comparación son:
# # - Front: Compara el frente de Pareto al que pertenecen las soluciones.
# # - Número de soluciones que le dominan: Compara el número de soluciones que dominan a cada padre.
# # - Criterio aleatorio: Este criterio se eliminó, pero inicialmente, cuando otros criterios son iguales,
# # se elige aleatoriamente uno de los padres como mejor solución.
# compara_padres <- function(padre1, padre2) {
#   if (poblacion$solucion[padre1]$front == poblacion$solucion[padre2]$front) {
#     if (poblacion$solucion[padre1]$num_sol_me_dom == poblacion$solucion[padre2]$num_sol_me_dom) {
#       moneda <- sample(0:1, 1)
#       if (moneda == 0) {
#         return(padre1)
#       } else {
#         return(padre2)
#       }
#     } else {
#       if (poblacion$solucion[padre1]$num_sol_me_dom <= poblacion$solucion[padre2]$num_sol_me_dom) {
#         return(padre1)
#       } else {
#         return(padre2)
#       }
#     }
#   } else {
#     if (poblacion$solucion[padre1]$front > poblacion$solucion[padre2]$front) {
#       return(padre1)
#     } else {
#       return(padre2)
#     }
#   }
# }
# 
# # Esta función comprueba si hay características heredadas repetidas en una solución hijo.
# # Devuelve 1 si hay características repetidas y 0 si no las hay.
# comprobar_caracteristicas_heredadas <- function(hijo) {
#   for (i in 1:(NUM_FEATURES - 1)) {
#     for (j in (i + 1):NUM_FEATURES) {
#       if (poblacion$solucion[hijo]$features[i] == poblacion$solucion[hijo]$features[j]) {
#         return(1)  # Hay características repetidas
#       }
#     }
#   }
#   return(0)
# }
# 
# # Esta función compara dos valores double y devuelve:
# # - 1 si el primer valor es mayor que el segundo.
# # - 0 si los valores son iguales.
# # - -1 si el primer valor es menor que el segundo.
# comprobar_double <- function(a, b) {
#   value <- a - b
#   if (value > PRECISION_DOM) {
#     return(1)
#   } else if (value < PRECISION_DOM && value > (-PRECISION_DOM)) {
#     return(0)
#   } else {
#     return(-1)
#   }
# }
# 
# # Esta función determina si una solución domina a otra.
# # Devuelve:
# # - 0 si ninguna solución domina a la otra.
# # - 1 si la primera solución domina a la segunda.
# # - 2 si la segunda solución domina a la primera.
# domina <- function(p, q) {
#   # Obtiene los valores de los objetivos para comparar
#   if (TIPO_OBJETIVOS == 1) {
#     o1 <- comprobar_double(poblacion$solucion[p]$obj[1], poblacion$solucion[q]$obj[1])
#     o2 <- comprobar_double(poblacion$solucion[p]$obj[2], poblacion$solucion[q]$obj[2])
#     o3 <- comprobar_double(poblacion$solucion[p]$obj[3], poblacion$solucion[q]$obj[3])
#   } else {
#     o1 <- comprobar_double(poblacion$solucion[p]$obj2[1], poblacion$solucion[q]$obj2[1])
#     o2 <- comprobar_double(poblacion$solucion[p]$obj2[2], poblacion$solucion[q]$obj2[2])
#     o3 <- comprobar_double(poblacion$solucion[p]$obj2[3], poblacion$solucion[q]$obj2[3])
#   }
#   if (o1 == 0 && o2 == 0 && o3 == 0) {
#     return(0)
#   }
#   if (o1 >= 0 && o2 <= 0 && o3 <= 0) {
#     return(1)
#   }
#   if (o1 <= 0 && o2 >= 0 && o3 >= 0) {
#     return(2)
#   }
#   return(0)
# }
# 
# buscar_clones <- function(k) {
#   # Devuelve un 0 si los VFo de la solución k no se repiten en ninguna solución desde 0 hasta k-1.
#   # Devuelve un 1 si encuentra un clon (en vfo)
#   
#   i <- 0
#   
#   if (TIPO_OBJETIVOS == 1) {
#     while (i <= k - 1) {
#       if (
#         (abs_2(poblacion$solucion[[i]]$obj[1] - poblacion$solucion[[k]]$obj[1]) < PRECISION_CLONES_F1) && 
#         (abs_2(poblacion$solucion[[i]]$obj[2] - poblacion$solucion[[k]]$obj[2]) < PRECISION_CLONES_F2) &&
#         (abs_2(poblacion$solucion[[i]]$obj[3] - poblacion$solucion[[k]]$obj[3]) < PRECISION_CLONES_F3) 
#       ) {
#         return(1)
#       }
#       i <- i + 1
#     }
#   } else {
#     while (i <= k - 1) {
#       if (
#         (abs_2(poblacion$solucion[[i]]$obj2[1] - poblacion$solucion[[k]]$obj2[1]) < PRECISION_CLONES_F1) && 
#         (abs_2(poblacion$solucion[[i]]$obj2[2] - poblacion$solucion[[k]]$obj2[2]) < PRECISION_CLONES_F2) && 
#         (abs_2(poblacion$solucion[[i]]$obj2[3] - poblacion$solucion[[k]]$obj2[3]) < PRECISION_CLONES_F3) 
#       ) {
#         return(1)
#       }
#       i <- i + 1
#     }
#   }
#   
#   return(0)
# }
# 
# nueva_poblacion <- function() {
#   # Esta función cruza los elementos de la población P para crear la población de doble tamaño PUQ=R
#   
#   padre <- 0
#   madre <- 0
#   j <- 0
#   k <- 0
#   aleatorio <- 0
#   
#   cat("\n\nCreando nueva población por cruce desde", TAM_POBLACION, "hasta", 2 * TAM_POBLACION - 1, "....")
#   k <- TAM_POBLACION
#   
#   while (k < (2 * TAM_POBLACION) - 1) {
#     padre <- torneo_elegir_padre()
#     madre <- torneo_elegir_padre()
#     
#     while (padre == madre)
#       madre <- torneo_elegir_padre()
#     
#     cruzar_soluciones(padre, madre, k)  # Cruza las soluciones padre y madre y los hijos (genera 4), los compara 2 a 2 y los dos mejores los pone en las posiciones k y k+1
#     
#     for (j in 0:1) {
#       aleatorio <- genera_num_aleatorio_real01()
#       
#       if (aleatorio < P_MUT)
#         mutar_solucion(k)
#       # quicksort(poblacion$solucion[[k]]$features, 0, NUM_FEATURES - 1, 3)  # Ordena el array de features para que el cruce funcione bien ¿ES NECESARIO AHORA?
#       
#       while (evaluar_solucion(k) == 0)
#         genera_solucion_aleat(k)  # Antes si se generaba una solución no válida por cruce, se volvía a generar por cruce. Ahora se genera aleatoriamente. OJOOOOO: PENSAR SI HACERLO ASÍ
#       
#       # Nuevo con CONSTANTE CLONES
#       while ((CLONES == 0) && (buscar_clones(k) != 0))
#         # No se permiten clones y es un clon
#       {
#         repeat {
#           mutar_solucion(k)
#         } 
#         until (evaluar_solucion(k) == 0)
#       }
#       k <- k + 1
#     }
#   }
# }
# 
# reducir_poblacion <- function() {
#   # Esta función reduce el tamaño de la población de tamaño 2*TAM_POBLACION a la mitad.
#   # Se copian primero los elementos de los primeros fronts. Si llega un momento que no cabe un front entero,
#   # se seleccionan primero ...
#   
#   contador_sol <- 0  # va contando las nuevas soluciones en la nueva poblacion
#   front <- 0
#   indice <- 0
#   i <- 0
#   k <- 0
#   nueva_poblacion <- list(num_fronts = 0)  # Crear una nueva población
#   
#   cat("\n\nReduciendo de tamaño 2N a tamaño N.....")
#   
#   # Inicializar la nueva población
#   contador_sol <- 0
#   nueva_poblacion$num_fronts <- 0
#   
#   # Mientras que todas las soluciones del front "front" quepan en la nueva población
#   while (contador_sol + poblacion$num_sol_front[[front]] <= TAM_POBLACION) {
#     nueva_poblacion$num_fronts <- nueva_poblacion$num_fronts + 1
#     # Generar un número aleatorio entre 0 y 2*TAM_POBLACION y recorrer la población triple
#     # si el elemento actual pertenece al front actual, copiarlo en la nueva población
#     indice <- genera_num_aleat_entero(2 * TAM_POBLACION)
#     for (i in 1:poblacion$num_sol_front[[front]]) {
#       while (poblacion$solucion[[indice]]$front != front) {
#         indice <- indice + 1
#         if (indice == 2 * TAM_POBLACION)
#           indice <- 1
#       }
#       # Copiar poblacion$solucion[indice] en nueva_poblacion$solucion[contador_sol]
#       nueva_poblacion$solucion[[contador_sol]] <- copiar_solucion(poblacion$solucion[[indice]])
#       contador_sol <- contador_sol + 1
#       indice <- indice + 1
#       if (indice == 2 * TAM_POBLACION)
#         indice <- 1
#     }
#     front <- front + 1
#   }
#   
#   # Si se copiaron soluciones del front 0
#   if (front > 0)
#     nueva_poblacion$num_sol_front[[1]] <- poblacion$num_sol_front[[1]]
#   else
#     nueva_poblacion$num_sol_front[[1]] <- TAM_POBLACION
#   
#   # Si la nueva población aún no está completa y las soluciones del front "front" no caben todas,
#   # se debe aplicar a este front la función de cálculo de "crowding distance"
#   if (contador_sol < TAM_POBLACION) {
#     nueva_poblacion$num_fronts <- nueva_poblacion$num_fronts + 1
#     k <- 1
#     l <- 1
#     indices_front <- numeric(poblacion$num_sol_front[[front]])
#     
#     while (k <= poblacion$num_sol_front[[front]]) {
#       if (poblacion$solucion[[l]]$front == front) {
#         indices_front[k] <- l
#         k <- k + 1
#       }
#       l <- l + 1
#     }
#     # Se deben ordenar los índices_front y calcular su i_distance
#     # Calcular el máximo y mínimo de cada objetivo entre los elementos del front
#     max <- rep(0, NUM_OBJETIVOS)
#     min <- rep(MAX_DOUBLE, NUM_OBJETIVOS)
#     for (l in 1:poblacion$num_sol_front[[front]]) {
#       for (k in 1:NUM_OBJETIVOS) {
#         if (poblacion$solucion[[indices_front[l]]]$obj[k] > max[k])
#           max[k] <- poblacion$solucion[[indices_front[l]]]$obj[k]
#         if (poblacion$solucion[[indices_front[l]]]$obj[k] < min[k])
#           min[k] <- poblacion$solucion[[indices_front[l]]]$obj[k]
#       }
#     }
#     # Normalizar los valores de los objetivos para calcular las i_distance
#     for (l in 1:poblacion$num_sol_front[[front]]) {
#       poblacion$solucion[[indices_front[l]]]$i_distance <- 0
#     }
#     
#   }
#   
#   # Eliminar la población original y asignar la nueva población
#   delete(poblacion)
#   poblacion <- nueva_poblacion
#   
#   cat("\nNº fronts:", poblacion$num_fronts, "\nNº SND:", poblacion$num_sol_front[[1]])
# }

