#' Realiza analisis de varianza (ANOVA) para DCA de forma individual para cada entorno en un conjunto de datos dado.
#'
#' @param datos Nombre del conjunto de datos.
#' @param nombre_var_resp Nombre, entre comillas (""), de la columna donde se encuentran la variable respuesta.
#' @param nombre_tratamiento Nombre, entre comillas (""), de la columna donde se encuentran los tratamientos.
#' @param nombre_entorno Nombre, entre comillas (""), de la columna donde se encuentran los entornos.
#'
#' @return La funcion devuelve una lista denominada ANOVA.ind que contiene los resultados de ANOVA y los modelos de cada entorno.
#'
#' @examples
#' # ANOVA.individuales.DCA(datos = datos_ejemplo,
#' #                         nombre_var_resp = "Nombre_Var_Respuesta",
#' #                         nombre_tratamiento = "Nombre_Tratamiento",
#' #                         nombre_entorno = "Nombre_Entorno")
#'
#' @importFrom stats anova lm
#' @export
ANOVA.individuales.DCA <- function(datos, nombre_var_resp, nombre_tratamiento, nombre_entorno) {

  # Obtener los nombres originales de las columnas
  nombres_originales <- names(datos)

  # Verificar si los nombres de las columnas solicitadas estan presentes en los datos
  if (!(nombre_var_resp %in% nombres_originales)) {
    stop("El nombre de la variable respuesta no se encontro en los datos.")
  }

  if (!(nombre_tratamiento %in% nombres_originales)) {
    stop("El nombre del tratamiento no se encontro en los datos.")
  }

  if (!(nombre_entorno %in% nombres_originales)) {
    stop("El nombre de los entornos no se encontro en los datos.")
  }

  nombres_entornos_originales <- unique(datos[[nombre_entorno]])

  # Renombrar las columnas
  names(datos)[names(datos) == nombre_var_resp] <- "var_resp"
  names(datos)[names(datos) == nombre_tratamiento] <- "tratamiento"
  names(datos)[names(datos) == nombre_entorno] <- "entornos"

  # Transformar a factor
  datos$tratamiento <- factor(datos$tratamiento)
  datos$entornos <- as.factor(datos$entornos)

  # Inicializar listas vacias para resultados y modelos
  ANOVA_resultados <- list()
  modelos <- list()

  # Bucle para realizar ANOVAs individuales para cada entorno
  for (i in nombres_entornos_originales) {
    datos_sub <- subset(datos, datos$entornos == i)
    modelo <- lm(var_resp ~ tratamiento, data = datos_sub)
    anova_result <- anova(modelo)

    # Ajustar los nombres de las filas en los resultados de ANOVA
    row.names(anova_result) <- c(nombre_tratamiento, "Residuals")

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
