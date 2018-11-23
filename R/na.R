#' Descreve proporcao de NA por variáveis de uma tabela
#'
#' @param x Uma tabela
#' @param corte Numerico entre 0 e 1 para limitar \code{print} da tabela com descricao. 
#'
#' @export
#' @return Tabela com descrição de NAs por coluna.
#' 

na_prop <- function(dt, corte = 0) {

  if (! corte %between% c(0, 1)) stop("\n'corte' deve ser uma proporcao.\n")

  n <- nrow(dt)
  
  if (!is.data.table(dt)) 
    dt <- as.data.table(dt)

  dt[, purrr::map(.SD, ~ sum(is.na(.x)))] %>% 
    t() %>% 
    as.data.table(keep.rownames = "VARIAVEL") %>% 
    .[, .(VARIAVEL,
          N_MISSING = V1,
          N_TOTAL = N_TOTAL <- n,
          PROPORCAO_NA = V1 / N_TOTAL)] %>% 
    .[PROPORCAO_NA >= corte]
}
