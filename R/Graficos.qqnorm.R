#' Genera graficos para verificar el supuesto de normalidad de los residuos.
#'
#' Esta funcion genera graficos Q-Q (quantile-quantile) para cada entorno,
#' permitiendo visualizar la distribucion de los residuos y compararla con
#' una distribucion normal teorica.
#'
#' @param ANOVA_ind Lista de modelos ANOVA individuales.
#'        Esta lista debe ser el resultado de la funcion ANOVA.individuales.DBCA
#'        o ANOVA.individuales.DCA.
#'
#' @param tipo_visualizacion Tipo de visualizacion de los graficos.
#'        Debe ser un valor entero entre 1 , 2 , 3:
#'        - 1: Graficos en cuadricula.
#'        - 2: Graficos en ventanas individuales.
#'        - 3: Grafico, todos los entornos en un grafico.
#'
#' @return La funcion devuelve los graficos especificados segun el tipo de
#'         visualizacion seleccionado.
#'
#' @importFrom stats qqline qqnorm
#' @importFrom graphics plot abline
#' @importFrom grDevices dev.new
#'
#' @examples
#' # Ejemplo 1: Generar grafico tipo 1
#' # graficos_residuales_qqnorm(resultado_ANOVA, tipo_visualizacion = 1)
#'
#' # Ejemplo 2: Generar grafico tipo 2
#' # graficos_residuales_qqnorm(resultado_ANOVA, tipo_visualizacion = 2)
#'
#' @export
graficos_residuales_qqnorm <- function(ANOVA_ind, tipo_visualizacion) {

  # Obtengo los nombres de todos los entornos en la lista de residuos
  entornos <- names(ANOVA_ind$modelos)

  if (tipo_visualizacion == 1) {
    # Calculo el numero de filas y columnas para organizar los graficos
    num_entornos <- length(entornos)
    num_columnas <- 3
    num_filas <- ceiling(num_entornos / num_columnas)

    # Ajusto el numero de filas y columnas en la cuadricula
    par(mfrow = c(num_filas, num_columnas))

    for (entorno in entornos) {
      modelo <- ANOVA_ind$modelos[[entorno]]
      qqnorm(residuals(modelo), main = entorno,
             cex.lab = 0.9, cex.main = 1.7, cex.sub = 1.2, font.main = 1, font.sub = 1,
             col.lab = "black", fg = "blue",
             xlab = "Cuantil normal estandar", ylab = "Residuales",
             pch = 21, cex = 1, bg = "green", col = "black")

      # Agrego linea qqline con color rojo
      qqline(residuals(modelo), col = "red")
    }
  } else if (tipo_visualizacion == 2) {
    for (entorno in entornos) {
      # Creo una nueva ventana para cada grafico
      dev.new()
      modelo <- ANOVA_ind$modelos[[entorno]]
      qqnorm(residuals(modelo), main = entorno,
             cex.lab = 0.9, cex.main = 1.7, cex.sub = 1.2, font.main = 1, font.sub = 1,
             col.lab = "black", fg = "blue",
             xlab = "Cuantil normal estandar", ylab = "Residuales",
             pch = 21, cex = 1, bg = "green", col = "black")

      # Agrego linea qqline con color rojo
      qqline(residuals(modelo), col = "red")
    }

  } else if (tipo_visualizacion == 3) {
    todos <- NULL
    # Combino los residuos de todos los entornos
    for (entorno in entornos) {
      modelo <- ANOVA_ind$modelos[[entorno]]
      residuos_modelo <- residuals(modelo)
      todos[[entorno]] <- residuos_modelo
    }
    dev.new()
    residuos_combinados <- unlist(todos)
    qqnorm(unlist(residuos_combinados), main = "QQ Norm - Todos los Entornos",
           cex.lab = 0.9, cex.main = 1.7, cex.sub = 1.2, font.main = 1, font.sub = 1,
           col.lab = "black", fg = "blue",
           xlab = "Cantidad normal estandar", ylab = "Residuales",
           pch = 21, cex = 1, bg = "green", col = "black")

    qqline(residuos_combinados, col = "red")
  } else {
    stop("El tipo de visualizacion especificado no es valido. Debe ser 1 o 2 O 3.")
  }
}
