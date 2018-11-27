#' NA por colunas
#' 
#' @description 
#' 
#' Descreve proporcao de NA por variáveis de um data.frame
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



#' Substituiccao rapida de NA
#' 
#' @description 
#' 
#' Substitui NA por valores de interesse. Eh possivel especificar nomes de colunas e/ou expressoes regulares
#' com seus respectivos novos valores numa so chamada.
#' 
#' @param x Um data.frame
#' @param byref \code{TRUE/FALSE} indicando se a imputacao de dados deve ser feita por referencia ou retornando uma nova tabela.
#' @param \code{...} Substituicoes a serem feitas. Veja a seccao de detalhes.
#' 
#' @details 
#' 
#' As substituicoes devem ser da forma: \code{subs_na(x, byref, nome_1 = "?", nome_2 = 9999, "[0-9]{2}$" = 0)} ou
#' encapsuladas por \code{list()}, como: \code{subs_na(x, byref, list(nome_1 = "?", nome_2 = 9999, "[0-9]{2}$" = 0))}.
#' 
#' 
#' @export

subs_na <- function(x, byref = FALSE, ...) {
  
  if (!is.data.frame(x)) 
    stop("\nMetodo aplicado para tabelas\n")
  
  mudancas <- list(...)
  depth <- purrr::vec_depth(mudancas)
  
  if (! depth %in% 2L:3L) 
    stop("\nSubstituicoes fora do padrao.\n")
  
  if (depth == 3L) 
    mudancas <- unlist(mudancas, recursive = F)
  
  if (!is.data.table(x)) {
    setDT(x)
    warning("\ndata.frame convertido para data.table.\n")
  }
  
  if (byref) dt <- x else dt <- copy(x)
  
  quais <- names(mudancas)
  
  col <- quais[quais %in% names(dt)]
  rgx <- setdiff(quais, col)
  
  # Colunas explicitas
  
  for( j in col ) 
    set(dt, which(is.na(dt[[j]])), j, value = mudancas[[j]]) 
  
  
  # Colunas por expressao regular
  
  for( reg in rgx ) {
    
    reg_cols <- tryCatch(
      grep(reg, names(dt), value = T), 
      error = function(e) character(0)
    )
    
    if (length(reg_cols) == 0L) 
      warning(glue::glue("\nSem colunas correspondentes para regex ({reg}).\n"))
    
    for (j in reg_cols ) 
      set(dt, which(is.na(dt[[j]])), j, value = mudancas[[reg]])
    
  }
  
  
  return( if (byref) invisible(dt) else dt )
  
}