#' Analisis combinado (efectos fijos)
#'
#' Funcion desarrollada a partir de lo expuesto por McIntosh, M. S. (1983).
#' Analysis of combined experiments.Agronomy Journal, 75(1), 153-155.
#'
#' @param datos Un dataframe que contiene los datos.
#' @param varresp El nombre, entre comillas (""), de la columna que representa la variable respuesta.
#' @param bloques El nombre, entre comillas (""), de la columna que representa los bloques.
#' @param entornos El nombre, entre comillas (""), de la columna que representa los entornos.
#' @param tratamiento El nombre, entre comillas (""), de la columna que representa los tratamientos.
#'
#' @return Un dataframe con los resultados del analisis de efectos fijos.
#'
#' @examples
#' #datos <- read.csv("datos.csv")
#' #efectos_fijos(datos, "variable_respuesta", "bloque", "entorno", "tratamiento")
#'
#' @export
#'
#' @references
#' McIntosh, M. S. (1983). Analysis of combined experiments. Agronomy Journal, 75(1), 153-155.
#'
#' @importFrom stats pf
#' @seealso
#' \code{\link{lm}}, \code{\link{anova}}
#'
efectos_fijos <- function(datos,varresp,bloques,entornos,tratamiento){

  nombres_originales <- names(datos)

  # Verificar si los nombres de las columnas solicitadas estan presentes en los datos
  if (!(bloques %in% nombres_originales)) {
    stop("El nombre del bloque no se encontro en los datos.")
  }

  if (!(varresp %in% nombres_originales)) {
    stop("El nombre de la variable respuesta no se encontro en los datos.")
  }

  if (!(tratamiento %in% nombres_originales)) {
    stop("El nombre del tratamiento no se encontro en los datos.")
  }

  if (!(entornos %in% nombres_originales)) {
    stop("El nombre del entorno no se encontro en los datos.")
  }

  # Renombrar las columnas
  names(datos)[names(datos) == bloques] <- "bloque"
  names(datos)[names(datos) == varresp] <- "var_resp"
  names(datos)[names(datos) == tratamiento] <- "tratamiento"
  names(datos)[names(datos) == entornos] <- "entorno"

  # Transformar a factor
  datos$bloque <- factor(datos$bloque)
  datos$tratamiento <- factor(datos$tratamiento)
  datos$entorno <- factor(datos$entorno)

  modelo.fijos<-lm(var_resp~ entorno + bloque%in%entorno + tratamiento + entorno:tratamiento ,data=datos)

  anovafijos<-anova(modelo.fijos)

  ##Modifico la tabla anova
  fv<-rownames(anovafijos)
  resultados<-data.frame(
    "FV"= c(
      fv[1],
      fv[2],
      fv[4],
      fv[5]
    ),
    "CM"=c(
      anovafijos[1,3],
      anovafijos[2,3],
      anovafijos[4,3],
      anovafijos[5,3]
    ),
    "GL"=c(
      anovafijos[1,1],
      anovafijos[2,1],
      anovafijos[4,1],
      anovafijos[5,1]
    ),
    ##Calculo los estadisticos F, para efectos fijos segun McIntosh
    "Estadistico.F"=c(
      round(anovafijos[1,3]/anovafijos[3,3],2),
      round(anovafijos[2,3]/anovafijos[5,3],2),
      round(anovafijos[4,3]/anovafijos[5,3],2),
      "-"),
    ##Calculo los p.valor de los F de arriba.
    ##Si uso round y el p-valor es muy chico no lo pone en notacion cientifica, pone 0.
    "P.valor"=c(
      pf(c(anovafijos[1,3]/anovafijos[3,3]), df1=anovafijos[1,1], df2=anovafijos[3,1], lower.tail=FALSE),
      pf(c(anovafijos[2,3]/anovafijos[5,3]), df1=anovafijos[2,1], df2=anovafijos[5,1], lower.tail=FALSE),
      pf(c(anovafijos[4,3]/anovafijos[5,3]), df1=anovafijos[4,1], df2=anovafijos[5,1], lower.tail=FALSE),
      "-")
  )
  return(resultados)
}
