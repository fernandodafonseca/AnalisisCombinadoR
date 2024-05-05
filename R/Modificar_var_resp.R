#' Modificar la variable respuesta.
#'
#' Esta funcion ajusta modelos lineales para cada entorno unico en los datos,
#' calcula los residuos, y modifica la variable respuesta dividiendo cada valor
#' por la raiz cuadrada del error cuadratico medio de los residuos.
#' Luego, agrega esta variable respuesta modificada al conjunto de datos original.
#'
#' @param datos Un dataframe que contiene los datos.
#' @param nombre_bloque El nombre de la columna que representa los bloques en el diseño experimental.
#' @param nombre_var_resp El nombre de la columna que contiene la variable respuesta a ajustar.
#' @param nombre_tratamiento El nombre de la columna que representa los tratamientos en el diseño experimental.
#' @param nombre_entornos El nombre de la columna que indica los diferentes entornos en los que se realizo el experimento.
#'
#' @return Devuelve el dataframe original con una nueva columna llamada 'var_modificada', que contiene la variable respuesta ajustada por entorno.
#'
#' @export
#'
#' @examples
#'#modificar_var_resp(datos = datos_ejemplo,
#'#                                       nombre_bloque = "Bloque",
#'#                                       nombre_var_resp = "Variable_respuesta",
#'#                                       nombre_tratamiento = "Tratamiento",
#'#                                       nombre_entornos = "Entorno")
#'#
#'
#' @importFrom stats lm residuals
#' @importFrom stats as.formula
#'
modificar_var_resp <- function(datos, nombre_bloque, nombre_var_resp, nombre_tratamiento, nombre_entornos) {
  # Verificar si los nombres de las columnas solicitadas estan presentes en los datos
  if (!(nombre_bloque %in% names(datos))) {
    stop("El nombre del bloque no se encontro en los datos.")
  }

  if (!(nombre_var_resp %in% names(datos))) {
    stop("El nombre de la variable respuesta no se encontro en los datos.")
  }

  if (!(nombre_tratamiento %in% names(datos))) {
    stop("El nombre del tratamiento no se encontro en los datos.")
  }

  if (!(nombre_entornos %in% names(datos))) {
    stop("El nombre de los entornos no se encontro en los datos.")
  }

  # Obtener los entornos unicos
  entornos_unicos <- unique(datos[[nombre_entornos]])

  # Inicializar vector para almacenar las variables respuesta modificadas
  rto_modificado <- c()

  # Bucle para realizar el analisis para cada entorno
  for (entorno in entornos_unicos) {
    # Subconjunto de datos para el entorno actual
    datos_entorno <- datos[datos[[nombre_entornos]] == entorno, ]

    # Modelo lineal para el entorno actual
    modelo <- lm(as.formula(paste(nombre_var_resp, "~", nombre_bloque, "+", nombre_tratamiento)), data = datos_entorno)

    # Residuos del modelo
    residuos <- residuals(modelo)

    # Error cuadratico medio
    rmse <- sqrt(mean(residuos^2))

    # Variable respuesta modificada
    rto_modificado_entorno <- datos_entorno[[nombre_var_resp]] * (1 / sqrt(rmse))

    # Agregar la variable respuesta modificada al vector
    rto_modificado <- c(rto_modificado, rto_modificado_entorno)
  }

  # Agregar la variable respuesta modificada al dataframe original
  datos$var_modificado <- rto_modificado

  return(datos)
}
