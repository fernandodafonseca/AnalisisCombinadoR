#' Realiza análisis de varianza -ANOVA- para DBCA de forma individual para cada entorno en un conjunto de datos.
#'
#' @param datos Nombre del conjunto de datos.
#' @param nombre_bloque Nombre de la columna donde se encuentran los bloques.
#' @param nombre_var_resp Nombre de la columna donde se encuentran la variable respuesta.
#' @param nombre_tratamiento Nombre de la columna donde se encuentran los tratamientos.
#' @param nombre_entorno Nombre de la columna donde se encuentran los entornos.
#'
#' @return La función devuelve una lista denominada ANOVA.ind que contiene los resultados de ANOVA y los modelos de cada entorno.
#'
#' @examples
#' # ANOVA.individuales.DBCA(datos = datos_ejemplo,
#' #                         nombre_bloque = "Nombre_Bloque",
#' #                         nombre_var_resp = "Nombre_Var_Respuesta",
#' #                         nombre_tratamiento = "Nombre_Tratamiento",
#' #                         nombre_entorno = "Nombre_Entorno")
#'
#' @importFrom stats anova lm
#' @export
ANOVA.individuales.DBCA <- function(datos, nombre_bloque, nombre_var_resp, nombre_tratamiento, nombre_entorno) {

  # Obtener los nombres originales de las columnas
  nombres_originales <- names(datos)

  # Verificar si los nombres de las columnas solicitadas están presentes en los datos
  if (!(nombre_bloque %in% nombres_originales)) {
    stop("El nombre de la columna bloque no se encontr\u00f3 en los datos.")
  }

  if (!(nombre_var_resp %in% nombres_originales)) {
    stop("El nombre de la variable respuesta no se encontr\u00f3 en los datos.")
  }

  if (!(nombre_tratamiento %in% nombres_originales)) {
    stop("El nombre de la columna tratamiento no se encontr\u00f3 en los datos.")
  }

  if (!(nombre_entorno %in% nombres_originales)) {
    stop("El nombre de la columna entornos no se encontr\u00f3 en los datos.")
  }

  nombres_entornos_originales <- unique(datos[[nombre_entorno]])

  # Renombrar las columnas
  names(datos)[names(datos) == nombre_bloque] <- "bloque"
  names(datos)[names(datos) == nombre_var_resp] <- "var_resp"
  names(datos)[names(datos) == nombre_tratamiento] <- "tratamiento"
  names(datos)[names(datos) == nombre_entorno] <- "entornos"

  # Transformar a factor
  datos$bloque <- factor(datos$bloque)
  datos$tratamiento <- factor(datos$tratamiento)
  datos$entornos <- as.factor(datos$entornos)

  # Inicializar listas vacías para resultados y modelos
  ANOVA_resultados <- list()
  modelos <- list()

  # Bucle para realizar ANOVAs individuales para cada entorno
  for (i in nombres_entornos_originales) {
    datos_sub <- datos[datos$entornos == i, ]
    modelo <- with(datos_sub, lm(var_resp ~ bloque + tratamiento))
    anova_result <- anova(modelo)

    # Ajustar los nombres de las filas en los resultados de ANOVA
    row.names(anova_result) <- c(nombre_bloque, nombre_tratamiento, "Residual")

    # Cambiar el nombre de la columna "Response" en los resultados de ANOVA
    attr(anova_result, "heading")[2] <- paste("Response:", nombre_var_resp)

    # Almacenar los resultados utilizando el nombre del entorno original
    ANOVA_resultados[[as.character(i)]] <- anova_result
    modelos[[as.character(i)]] <- modelo
  }

  # Imprimir los resultados
  print(ANOVA_resultados)

  # Devolver los resultados con los nombres originales
  return(invisible(list(ANOVA = ANOVA_resultados, modelos = modelos)))
}
