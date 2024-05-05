#' Realiza análisis de varianza (ANOVA) para DBCA de forma individual para cada entorno en un conjunto de datos dado.
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

  # Verificar si los nombres de las columnas solicitadas estan presentes en los datos
  if (!(nombre_bloque %in% nombres_originales)) {
    stop("El nombre de la columna bloque no se encontro en los datos.")
  }

  if (!(nombre_var_resp %in% nombres_originales)) {
    stop("El nombre de la variable respuesta no se encontro en los datos.")
  }

  if (!(nombre_tratamiento %in% nombres_originales)) {
    stop("El nombre de la columna tratamiento no se encontro en los datos.")
  }

  if (!(nombre_entorno %in% nombres_originales)) {
    stop("El nombre de la columna entornos no se encontro en los datos.")
  }

  # Renombrar las columnas
  names(datos)[names(datos) == nombre_bloque] <- "bloque"
  names(datos)[names(datos) == nombre_var_resp] <- "var_resp"
  names(datos)[names(datos) == nombre_tratamiento] <- "tratamiento"
  names(datos)[names(datos) == nombre_entorno] <- "entornos"

  # Transformar a factor
  datos$bloque <- factor(datos$bloque)
  datos$tratamiento <- factor(datos$tratamiento)
  datos$entornos <- as.factor(datos$entornos)

  # Convertir los entornos a numerico y luego a factor para el bucle
  numeros_entornos <- as.factor(as.numeric(datos$entornos))

  # Inicializar residuales_modelos como una lista vacia
  residuales_modelos <- list()

  # Bucle para realizar ANOVAs individuales para cada entorno
  ANOVA_resultados <- list()
  modelos <- list()
  for (i in unique(numeros_entornos)) {
    modelo <- lm(var_resp ~ bloque + tratamiento, subset(datos, numeros_entornos == i))
    ANOVA_resultados[[paste("Entorno", i)]] <- anova(modelo)
    modelos[[paste("Entorno", i)]] <- modelo
    row.names(ANOVA_resultados[[paste("Entorno", i)]]) <- c (nombre_bloque,nombre_tratamiento,"Residual")
  }

  # Devolver los datos con las columnas renombradas, los resultados de ANOVA, los residuales de los modelos individuales y los modelos mismos
  return(list(ANOVA = ANOVA_resultados, modelos = modelos))
}
