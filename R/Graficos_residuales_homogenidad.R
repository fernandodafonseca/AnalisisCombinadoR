#' Genera diferentes tipos de gráficos útiles para verificar el supuesto de homogeneidad de varianzas.
#'
#' Esta función genera gráficos que ayudan a evaluar si el supuesto de homogeneidad de varianzas de los residuos se cumple en los distintos entornos de un diseño experimental.
#'
#' @param ANOVA_ind Lista de modelos ANOVA individuales.
#'        Esta lista debe ser el resultado de la función ANOVA.individuales.DBCA o ANOVA.individuales.DCA.
#'
#' @param tipo_de_graficos Tipo de gráfico que se desea generar.
#'        Debe ser un valor entero entre 1 y 4:
#'        - Tipo 1: Gráficos en cuadrícula de raíz cuadrada de los residuos absolutos vs. valores ajustados, cada entorno en un gráfico.
#'        - Tipo 2: Gráficos en ventanas individuales de raíz cuadrada de los residuos absolutos vs. valores ajustados, cada entorno en un gráfico.
#'        - Tipo 3: Gráfico de raíz cuadrada de los residuos absolutos vs. valores ajustados, todos los entornos en un gráfico diferenciando por color.
#'        - Tipo 4: Gráfico de residuales vs. valores ajustados, todos los entornos en un gráfico diferenciando por color.
#'        Si se proporciona un valor diferente a 1, 2, 3 o 4, la función detendrá la ejecución y devolverá un mensaje de error.
#'
#' @return La función devuelve los gráficos especificados según el tipo seleccionado.
#'
#' @importFrom grDevices dev.new rainbow
#' @importFrom graphics abline axis box legend mtext par plot.new plot.window points title
#' @importFrom stats fitted.values residuals
#'
#' @examples
#' # Ejemplo 1: Generar gráfico tipo 1
#' # graficos_residuales_homogenidad(ANOVA_ind = resultado_ANOVA, tipo_de_graficos = 1)
#'
#' # Ejemplo 2: Generar gráfico tipo 2
#' # graficos_residuales_homogenidad(ANOVA_ind = resultado_ANOVA, tipo_de_graficos = 2)
#'
#' @export
graficos_residuales_homogenidad <- function(ANOVA_ind, tipo_de_graficos) {

  # Obtener los nombres de todos los entornos en la lista de modelos
  entornos <- names(ANOVA_ind$modelos)
  modelos <- ANOVA_ind$modelos

  if (tipo_de_graficos == 1) {

    # Calcular el numero de filas y columnas para organizar los graficos
    num_entornos <- length(entornos)
    num_columnas <- 3
    num_filas <- ceiling(num_entornos / num_columnas)

    # Definir los limites de los ejes
    min_raiz_residuales <- min(sapply(entornos, function(entorno) min(sqrt(abs(residuals(modelos[[entorno]]))))))
    max_raiz_residuales <- max(sapply(entornos, function(entorno) max(sqrt(abs(residuals(modelos[[entorno]]))))))

    limitey <- c(-2*min_raiz_residuales , max_raiz_residuales )
    limitex <-  c(min(sapply(modelos, function(x) min(x$fitted.values))), max(sapply(modelos, function(x) max(x$fitted.values))))

    # Ajustar el numero de filas y columnas en la cuadricula
    par(mfrow = c(num_filas, num_columnas))

    for (entorno in entornos) {
      # Calcular la raiz cuadrada de los residuos absolutos
      raiz_residuales <- sqrt(abs(residuals(modelos[[entorno]])))

      # Obtener los valores ajustados del modelo
      valores_ajustados <- fitted.values(modelos[[entorno]])

      # Graficar raiz cuadrada de los residuos absolutos vs. valores ajustados
      plot(raiz_residuales ~ valores_ajustados,
           pch = 22, cex = 1, bg = "red", col = "black", main = entorno,
           cex.lab = 0.9, cex.main = 1.7, cex.sub = 1.1, font.main = 1, font.sub = 1, col.lab = "black", fg = "red",
           xlim=limitex,ylim=limitey,
           xlab = "Valores ajustados", ylab = expression(sqrt("Residuo")))

      # Agregar linea de homogeneidad de varianzas
      abline(h = 0, col = "blue", lty = 2)
    }

  } else if (tipo_de_graficos == 2) {
    # Definir los limites de los ejes
    min_raiz_residuales <- min(sapply(entornos, function(entorno) min(sqrt(abs(residuals(modelos[[entorno]]))))))
    max_raiz_residuales <- max(sapply(entornos, function(entorno) max(sqrt(abs(residuals(modelos[[entorno]]))))))

    limitey <- c(min_raiz_residuales , max_raiz_residuales )
    limitex <-  c(min(sapply(modelos, function(x) min(x$fitted.values))), max(sapply(modelos, function(x) max(x$fitted.values))))

    for (entorno in entornos) {
      # Crear una nueva ventana para cada grafico
      dev.new()

      # Calcular la raiz cuadrada de los residuos absolutos
      raiz_residuales <- sqrt(abs(residuals(modelos[[entorno]])))

      # Obtener los valores ajustados del modelo
      valores_ajustados <- fitted.values(modelos[[entorno]])

      # Graficar raiz cuadrada de los residuos absolutos vs. valores ajustados
      plot(raiz_residuales ~ valores_ajustados,
           pch = 22, cex = 1, bg = "red", col = "black", main = entorno,
           cex.lab = 0.9, cex.main = 1.7, cex.sub = 1.1, font.main = 1, font.sub = 1, col.lab = "black", fg = "red",
           xlim=limitex,ylim=limitey,
           xlab = "Valores ajustados", ylab = "Raiz cuadrada de residuales absolutos")

      # Agregar linea de homogeneidad de varianzas
      abline(h = 0, col = "blue", lty = 2)
    }

  } else if (tipo_de_graficos == 3) {
    # Crear una paleta de colores unica para cada entorno
    colores <- rainbow(length(entornos))

    # Inicializar el indice de color
    color_index <- 1

    #Crear un nuevo grafico con ventana completa
    dev.new()
    plot.new()

    # Obtener los residuos maximos y minimos para establecer los limites de la ventana de trazado
    residuales_min <- min(sapply(entornos, function(entorno) min(residuals(modelos[[entorno]]))))
    residuales_max <- max(sapply(entornos, function(entorno) max(residuals(modelos[[entorno]]))))

    #Establecer los limites de la ventana de trazado
    plot.window(xlim = c(min(sapply(modelos, function(x) min(x$fitted.values))), max(sapply(modelos, function(x) max(x$fitted.values)))),
                ylim = c(-0.5 ,1.1* sqrt(abs(residuales_max))),
                xlab = "Valores ajustados", ylab = "Raiz cuadrada de residuales absolutos")

    # Iterar sobre cada entorno
    for (entorno in entornos) {
      # Calcular la raiz cuadrada de los residuos absolutos
      raiz_residuales <- sqrt(abs(residuals(modelos[[entorno]])))

      # Obtener los valores ajustados del modelo
      valores_ajustados <- fitted.values(modelos[[entorno]])

      # Graficar raiz cuadrada de los residuos absolutos vs. valores ajustados con color y simbolo diferente
      points(valores_ajustados, raiz_residuales,
             pch = 22, cex = 1, bg = colores[color_index], col = "black")

      # Actualizar el indice de color para el proximo entorno
      color_index <- color_index + 1
    }

    # Agregar linea de homogeneidad de varianzas
    abline(h = 0, col = "blue", lty = 2)

    #Agregar ejes y escalas
    axis(1)
    axis(2)
    mtext( "Valores ajustados",1,line = 2)
    mtext("Raiz cuadrada de residuales absolutos",2,line = 2)
    title("Raiz Cuadrada de los Residuales Absolutos vs. Valores Ajustados")
    box()

    # Agregar leyenda
    legend("topright", legend = entornos, pch = 22, col = colores, bg = "white",
           title = "Entornos", cex = 0.8, pt.cex = 1, bty = "n")

  } else if (tipo_de_graficos == 4) {

    # Crear una paleta de colores unica para cada entorno
    colores <- rainbow(length(entornos))

    # Inicializar el indice de color
    color_index <- 1

    # Crear un nuevo grafico con ventana completa
    dev.new()
    plot.new()
    plot.window(xlim = c(min(sapply(modelos, function(x) min(x$fitted.values))), max(sapply(modelos, function(x) max(x$fitted.values)))),
                ylim = c(min(sapply(modelos, function(x) min(residuals(x)))), max(sapply(modelos, function(x) max(residuals(x))))))

    # Iterar sobre cada entorno
    for (entorno in entornos) {
      # Obtener los residuales y los valores ajustados del modelo
      residuales_entorno <- residuals(modelos[[entorno]])
      valores_ajustados_entorno <- fitted.values(modelos[[entorno]])

      # Graficar residuales del entorno actual vs. valores ajustados con color y simbolo diferente
      points(valores_ajustados_entorno, residuales_entorno,
             pch = 22, cex = 1, bg = colores[color_index], col = "black")

      # Actualizar el indice de color para el proximo entorno
      color_index <- color_index + 1
    }

    # Agregar linea de homogeneidad de varianzas
    abline(h = 0, col = "blue", lty = 2)

    # Agregar ejes y escalas
    axis(1)
    axis(2)
    mtext( "Valores ajustados",1,line = 2)
    mtext("Residuales",2,line = 2)
    title("Residuales vs. Valores Ajustados")
    box()

    # Agregar leyenda
    legend("topright", legend = entornos, pch = 22, col = colores, bg = "white",
           title = "Entornos", cex = 0.8, pt.cex = 1, bty = "n")

  } else {
    stop("El tipo de grafico especificado no es valido. Debe ser 1, 2, 3 o 4.")
  }
}
