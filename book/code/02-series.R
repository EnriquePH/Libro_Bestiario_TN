# CAPÍTULO 02 SERIES CONVERGENTES

library(latex2exp)
library(stringr)
source("code/00-libro.R")

#' Floor or Ceiling functions in latex for Series Chapter
#'
#' @param k_denom integer number to be in the denominator
#'
#' @param floor boolean if FALSE use 'ceil' instead of 'floor'
#'
#' @return latex text
#' @export
#' latex_floor_n2_k(2, floor=FALSE)
#'
#' @examples
latex_floor_n2_k <- function(k_denom, floor = TRUE) {
  latex = paste0(r'($$\left\lfloor\frac{n^2}{)',
                 k_denom,
                 r'(}\right\rfloor$$)')
  ifelse(floor, latex, stringr::str_replace_all(latex, "floor", "ceil"))
}


# Series que incluyen las funciones de parte entera
seq_list_01 <- c('A000290', 'A007590', 'A000212', 'A002620',
                 'A118015', 'A056827', 'A056834', 'A130519',
                 'A056838', 'A056865')


# k, Secuencia, Fórmula, Descripción, Términos
TERMS <- 10
long_02_01 <- 1:length(seq_list_01)
tabla_02_01 <- data.frame(
  k = long_02_01,
  'Secuencia' = sapply(seq_list_01, OEIS_url_md),
  'Fórmula' = sapply(long_02_01, latex_floor_n2_k),
  'Descripción' = sapply(seq_list_01,
                         OEIS.R::OEIS_description),
  'Términos' = sapply(long_02_01,
                      function(k) {
                        sequence_terms(1:TERMS, function(x)
                          floor((x ^ 2) / k))
                      })
)

# Escribe la tabla 02 01 en un archivo Rda
FILE_PATH_TABLA_02_01 <- 'code/tablas/tabla_02_01.Rda'
if(!file.exists(FILE_PATH_TABLA_02_01)) {
  saveRDS(tabla_02_01, FILE_PATH_TABLA_02_01)
}
