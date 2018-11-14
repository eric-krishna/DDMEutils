#' Descreve proporção de NA por variáveis de uma tabela
#'
#' @param dt Um data.table
#' @param drop TRUE/FALSE para remover variável que possui mais do que proporção \code{corte} de NA
#' @param corte Numérico entre 0 e 1. Com \code{drop = FALSE}, só trunca a descrição; com \code{drop = TRUE}, é a condição para remover variáveis.
#'
#' @return Tabela com descrição de NA ou data.frame original sem variáveis excluídas.

na_prop <- function(dt, drop = FALSE, corte = if (drop) 0.5 else 0) {

  if(!corte %between% c(0,1)) stop("'corte' deve ser uma proporcao.")

  n <- nrow(dt)


  tabela <-
    t(dt[,purrr::map(.SD, ~ sum(is.na(.x)))]) %>%
    as.data.frame() %>%
    tibble::rownames_to_column() %>%
    dplyr::mutate(N_TOTAL = n, PROPORCAO_NA = V1 / N_TOTAL) %>%
    data.table::setDT() %>%
    data.table::setnames(c('VARIAVEL', 'N_MISSING', 'N_TOTAL', 'PROPORCAO_NA'))

  to_drop <- tabela[PROPORCAO_NA > corte][['VARIAVEL']]

  if(drop && length(to_drop) > 0L) {
    dt[, (to_drop) := NULL][]
    cat(paste0('Removendo as seguintes colunas:\n',paste(to_drop,collapse ='\n')))
  } else if(!drop) tabela[PROPORCAO_NA >= corte][order(-PROPORCAO_NA)]
  else tabela[order(-PROPORCAO_NA)]

}
