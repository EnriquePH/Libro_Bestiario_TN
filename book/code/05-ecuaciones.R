# CAPÍTULO 05 ECUACIONES

library(magrittr)
source("code/00-libro.R")

# Parámetros

TERMS <- 10
MIN_TERMS <- 4

# Ecuación phi(n) = phi(n + k)

seq_list_6_2 <- c('A001274', 'A001494', 'A330251', 'A179186', 'A179187',
                  'A179188', 'A179189', 'A179202', 'A330429', 'A276503',
                  'A276504', 'A217139')

latex_phi_phi_k <- function(k_eq){
  paste0(r'($$\varphi(n)=\varphi(n+)',
         k_eq,
         r'()$$)')
}


terms <- sapply(seq_list_6_2, function(x) {
   OEIS.R::OEIS_sequence(x)$terms
})



long_06_02 <- 1:length(seq_list_6_2)

tabla_06_02 <- data.frame(
  k = long_06_02,
  'Secuencia' = sapply(seq_list_6_2, OEIS_url_md),
  'Ecuación' = sapply(long_06_02, latex_phi_phi_k),
  'Soluciones' = sapply(terms, function(x) {
    text_terms(x, TERMS)
  })
)

# Corrección secuencias de términos muy largos.
tabla_06_02[3, 4] <- text_terms(terms$A330251, MIN_TERMS)
tabla_06_02[9, 4] <- text_terms(terms$A330429, MIN_TERMS)

# Escribe la tabla 06 02 en un archivo Rda
FILE_PATH_TABLA_06_02 <- 'code/tablas/tabla_06_02.Rda'
if(!file.exists(FILE_PATH_TABLA_06_02)) {
  saveRDS(tabla_06_02, FILE_PATH_TABLA_06_02)
}

# 6.4 Ecuación phi(n) = k * tau(n)

seq_list_6_4a <- c('A020488', 'A062516', 'A063469', 'A063470')
long_06_04a <- 1:length(seq_list_6_4a)


latex_phi_k_tau <- function(k_eq){
  ifelse(k_eq == 1,
         r'($$\varphi(n)=\tau(n)$$)',
         paste0(r'($$\varphi(n) =)', k_eq, r'(\cdot \tau(n)$$)')
         )
}

terms_6_4a <- sapply(seq_list_6_4a, function(x) {
  OEIS.R::OEIS_sequence(x)$terms
})

tabla_06_04a <- data.frame(
  k = long_06_04a,
  'Secuencia' = sapply(seq_list_6_4a, OEIS_url_md),
  'Ecuación' = sapply(long_06_04a, latex_phi_k_tau),
  'Soluciones' = sapply(terms_6_4a, function(x) {
    text_terms(x, TERMS, dots = FALSE)
  })
)

# Escribe la tabla 06 04a en un archivo Rda
FILE_PATH_TABLA_06_04a <- 'code/tablas/tabla_06_04a.Rda'
if(!file.exists(FILE_PATH_TABLA_06_04a)) {
  saveRDS(tabla_06_04a, FILE_PATH_TABLA_06_04a)
}

## Ecuación phi(n) = tau(n)^k

latex_phi_tau_exp_k <- function(k_eq){
  if(k_eq == 1){
    k_eq = ""
  }
  paste0(r'($$\varphi(n)={\tau(n)}^{)',
         k_eq,
         r'(}$$)')
}

seq_list_6_5 <- c('A020488', 'A068560', 'A068559', 'A114063')

long_06_05 <- 1:length(seq_list_6_5)

terms_6_5 <- sapply(seq_list_6_5, function(x) {
  OEIS.R::OEIS_sequence(x)$terms
})

dots_06_05 <- c(FALSE, TRUE, TRUE, TRUE)

tabla_06_05 <- data.frame(
  k = long_06_05,
  'Secuencia' = sapply(seq_list_6_5, OEIS_url_md),
  'Ecuación' = sapply(long_06_05, latex_phi_tau_exp_k),
  'Soluciones' = sapply(terms_6_5, function(x) {
    text_terms(x, TERMS, dots = TRUE)
  })
)

tabla_06_05[1, 4] <- text_terms(terms_6_5$A020488, TERMS, dots = FALSE)

# Escribe la tabla 06 04a en un archivo Rda
FILE_PATH_TABLA_06_05 <- 'code/tablas/tabla_06_05.Rda'
if(!file.exists(FILE_PATH_TABLA_06_05)) {
  saveRDS(tabla_06_05, FILE_PATH_TABLA_06_05)
}
