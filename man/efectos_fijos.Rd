% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Funcion_EFijos.R
\name{efectos_fijos}
\alias{efectos_fijos}
\title{Analisis combinado (efectos fijos)}
\usage{
efectos_fijos(datos, varresp, bloques, entornos, tratamiento)
}
\arguments{
\item{datos}{Un dataframe que contiene los datos.}

\item{varresp}{El nombre, entre comillas (""), de la columna que representa la variable respuesta.}

\item{bloques}{El nombre, entre comillas (""), de la columna que representa los bloques.}

\item{entornos}{El nombre, entre comillas (""), de la columna que representa los entornos.}

\item{tratamiento}{El nombre, entre comillas (""), de la columna que representa los tratamientos.}
}
\value{
Un dataframe con los resultados del analisis de efectos fijos.
}
\description{
Funcion desarrollada a partir de lo expuesto por McIntosh, M. S. (1983).
Analysis of combined experiments.Agronomy Journal, 75(1), 153-155.
}
\examples{
#datos <- read.csv("datos.csv")
#efectos_fijos(datos, "variable_respuesta", "bloque", "entorno", "tratamiento")

}
\references{
McIntosh, M. S. (1983). Analysis of combined experiments. Agronomy Journal, 75(1), 153-155.
}
\seealso{
\code{\link{lm}}, \code{\link{anova}}
}
