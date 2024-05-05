#' Realiza análisis combinado de datos para un Diseño Completamente Aleatorizado (DCA).
#'
#' Esta función realiza un análisis combinado de datos para un Diseño Completamente Aleatorizado (DCA) utilizando modelos lineales mixtos.
#' Permite ajustar modelos con o sin estructura de varianza heterogénea entre los entornos.
#'
#' @param datos Un dataframe que contiene los datos del experimento.
#' @param nombre_var_resp El nombre de la columna que contiene la variable respuesta.
#' @param nombre_tratamiento El nombre de la columna que contiene los tratamientos.
#' @param nombre_entornos El nombre de la columna que contiene los entornos.
#' @param tipo_varianza Un entero que especifica el tipo de estructura de varianza a utilizar:
#'                      - 1: Sin estructura de varianza heterogénea.
#'                      - 2: Con estructura de varianza heterogénea.
#'
#' @return Devuelve un objeto de clase 'anova' con los resultados del análisis combinado.
#'
#'
#' @examples
#' # Ejemplo de uso:
#' # analisis_combinado_DCA(datos = datos_experimento,
#' #                        nombre_var_resp = "variable_respuesta",
#' #                        nombre_tratamiento = "tratamiento",
#' #                        nombre_entornos = "entorno",
#' #                        tipo_varianza = 1)
#' # Este ejemplo realiza un análisis combinado sin estructura de varianza heterogénea.
#'
#' @import nlme
#' @export
analisis_combinado_DCA <- function(datos, nombre_var_resp, nombre_tratamiento, nombre_entornos, tipo_varianza) {

  # Obtener los nombres originales de las columnas
  nombres_originales <- names(datos)

  # Verificar si los nombres de las columnas solicitadas estan presentes en los datos
  if (!(nombre_var_resp %in% nombres_originales)) {
    stop("El nombre de la variable respuesta no se encontro en los datos.")
  }

  if (!(nombre_tratamiento %in% nombres_originales)) {
    stop("El nombre del tratamiento no se encontro en los datos.")
  }

  if (!(nombre_entornos %in% nombres_originales)) {
    stop("El nombre del entorno no se encontro en los datos.")
  }

  # Renombrar las columnas
  names(datos)[names(datos) == nombre_var_resp] <- "var_resp"
  names(datos)[names(datos) == nombre_tratamiento] <- "tratamiento"
  names(datos)[names(datos) == nombre_entornos] <- "entorno"

  # Transformar a factor
  datos$tratamiento <- factor(datos$tratamiento)
  datos$entorno <- factor(datos$entorno)

   # ESTOY HACIENDO ESTA PARTE!

   # Ajustar el modelo lineal mixto
  if (tipo_varianza == 1) {
    # Sin estructura de varianza heterogenea
    modelo <- gls(var_resp ~ tratamiento + entorno + entorno:tratamiento, data = datos)
  } else if (tipo_varianza == 2) {
    # Con estructura de varianza heterogenea
    modelo <- gls(var_resp ~ tratamiento + entorno + entorno:tratamiento, data = datos, weights = varIdent(form = ~1|entorno))
  } else {
    stop("El tipo de analisis especificado no es valido. Debe ser 1 o 2.")
  }

  # Realizar el analisis de varianza (ANOVA) del modelo combinado
  anova_resultados <- anova(modelo)

  nombre_interaccion <- paste(nombre_tratamiento,nombre_entornos, sep=":")
  row.names(anova_resultados) <- c("",nombre_tratamiento,nombre_entornos,nombre_interaccion)

  anova_resultados <-anova_resultados[-1, ]
  # Devolver los resultados del analisis combinado
  return(anova_resultados)
}
