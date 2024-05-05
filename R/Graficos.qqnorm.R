#' Genera gráficos para verificar el supuesto de normalidad de los residuos.
#'
#' Esta función genera gráficos Q-Q (quantile-quantile) para cada entorno,
#' permitiendo visualizar la distribución de los residuos y compararla con
#' una distribución normal teórica.
#'
#' @param ANOVA_ind Lista de modelos ANOVA individuales.
#'        Esta lista debe ser el resultado de la función ANOVA.individuales.DBCA
#'        o ANOVA.individuales.DCA.
#'
#' @param tipo_visualizacion Tipo de visualización de los gráficos.
#'        Debe ser un valor entero entre 1 y 2:
#'        - 1: Gráficos en cuadrícula.
#'        - 2: Gráficos en ventanas individuales.
#'        Si se proporciona un valor diferente a 1 o 2, la función detendrá
#'        la ejecución y devolverá un mensaje de error.
#'
#' @return La función devuelve los gráficos especificados según el tipo de
#'         visualización seleccionado.
#'
#' @importFrom stats qqline qqnorm
#' @importFrom graphics plot abline
#' @importFrom grDevices dev.new
#'
#' @examples
#' # Ejemplo 1: Generar gráfico tipo 1
#' # graficos_residuales_qqnorm(resultado_ANOVA, tipo_visualizacion = 1)
#'
#' # Ejemplo 2: Generar gráfico tipo 2
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
             xlab = "Cantidad normal estandar", ylab = "Residuales",
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
             xlab = "Cantidad normal estandar", ylab = "Residuales",
             pch = 21, cex = 1, bg = "green", col = "black")

      # Agrego linea qqline con color rojo
      qqline(residuals(modelo), col = "red")
    }
  } else {
    stop("El tipo de visualizacion especificado no es valido. Debe ser 1 o 2.")
  }
}
