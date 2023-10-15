# FUNCIONES COMUNES A TODO EL LIBRO

library(OEIS.R)

# PARAMETROS TABLAS

TERMS <- 10
MIN_TERMS <- 4

#' Sequence terms to use as text
#'
#' @param lst list of input values
#' @param FUN function to calculate the sequence
#'
#' @return A string with the terms and three dots as text
#' @export
#'
#' @examples
#' sequence_terms(1:15, function(x) x^2)
sequence_terms <- function(lst, FUN) {
  terms <- paste0(sapply(lst, FUN), collapse = ',')
  paste0(c("{", terms, ",...}"), collapse = '')
}

#' OEIS_url_md link to OEIS sequence in markdown
#'
#' @param id_ Sequence ID
#'
#' @return text with markdown link
#' @export
#'
#' @examples
#' OEIS_url_md("A000045")
OEIS_url_md <- function(id_) {
  paste0("[", id_, "](", OEIS.R::OEIS_url(id_), ")")
}


#' Title
#'
#' @param seq_lst
#' @param n_terms
#'
#' @return
#' @export
#'
#' @examples
text_terms <- function(seq_lst, n_terms, dots = TRUE) {
  close_brackets <- ifelse(dots, ',...}', '}')
  n_terms <- min(n_terms, length(seq_lst))
  seq_lst %>%
    extract(., 1:n_terms) %>%
    paste0(., collapse = ',') %>%
    paste('{', ., close_brackets, sep = '')
}
