library(readr)

setwd("C:/Users/Ángel de Lara/Desktop/TFG")

#' Función principal
#'
#' Esta función ejecuta el flujo principal del programa.
#'
#' @import readr
#' @importFrom Solucion generar_solucion_aleatoria evaluar_solucion
#' @importFrom Poblacion Poblacion generar_poblacion_inicial imprimir_poblacion
#'
#' @export
main <- function() {
  # Cargar los datos del archivo
  datos <- read.table("prueba.txt", sep = ";", header = TRUE)

  # Guardar la columna de salida
  tu_output <- colnames(datos)[1]

  # Quitar la columna de salida de los datos
  datos_numericos <- datos[, -1]

  # Definir tus_inputs basados en los datos cargados (todas las columnas excepto la primera)
  tus_inputs <- colnames(datos_numericos)

  # Definir tus_costes basados en el número de entradas
  tus_costes <- rep(1, length(tus_inputs))
  tus_num_features <- length(tus_inputs)

  # Tamaño de la población
  tamano_poblacion <- 14  # Por ejemplo, una población de tamaño 4

  # Crear una instancia de Solucion
  solucion <- Solucion(num = 0, data = datos, costes = tus_costes, inputs = tus_inputs, output = tu_output, num_features = tus_num_features)

  # Generar una solución aleatoria y evaluarla
  sol_aleatoria <- generar_solucion_aleatoria(solucion)

  evaluar_solucion(sol_aleatoria)

  # Crear una instancia de Poblacion
  poblacion <- Poblacion(data = datos, costes = tus_costes, tam_pob = tamano_poblacion, inputs = tus_inputs, output = tu_output, num_features = tus_num_features)

  # Generar una población inicial
  poblacion <- generar_poblacion_inicial(poblacion)

  # Imprimir la población
  imprimir_poblacion(poblacion)
}

# Ejecutar la función principal si se ejecuta el script como un programa independiente
if (interactive()) {
  main()
}



# # Extracción de costes y datos
# costes <- data[1, -(1:2)]  # La primera fila contiene los costes
# data <- data[-(1:1), -(1)]  # Se eliminan las dos primeras columnas (índices)
#
# tam_pob <- 4  # Tamaño de la población
# num_fea <- 2  # Número de características
#
# # Creación de un objeto de la clase Solucion
# sol <- Solucion(1, data, c(5, 27, 10), 2, ncol(data) - 2)  # Crear solución con valores específicos
# sol$vectors <- c(1, 5)  # Definir vectores
# sol$plano_coord <- c(0.199866, 0.671592)  # Definir coordenadas del plano
# sol$features <- c(2, 1)  # Definir características seleccionadas
# sol$plano_termino_b <- -4.964948  # Definir término independiente del plano
# evaluar_solucion(sol)  # Evaluar la solución
#
# # Imprimir resultados
# cat("Objetivo de distancia:", sol$obj_distancia, "\n")
# cat("Objetivo de epsilon:", sol$obj_epsilon, "\n")
# cat("Objetivo de coste:", sol$obj_coste, "\n")
