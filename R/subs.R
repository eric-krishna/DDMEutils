#' Substituiccao rapida de valores
#' 
#' @description 
#' 
#' Substitui informacoes por valores de interesse. Eh possivel especificar nomes de colunas e/ou expressoes regulares
#' com seus respectivos novos valores numa so chamada.
#' 
#' @param x Um data.frame
#' @param byref \code{TRUE/FALSE} indicando se a imputacao de dados deve ser feita por referencia ou retornando uma nova tabela.
#' @param \code{...} Substituicoes a serem feitas. Veja a seccao de detalhes.
#' 
#' @details 
#' 
#' As substituicoes devem ser da forma: \code{subs_any(x, byref, nome_1 = list("?",NA), nome_2 = list("#", 9999))}.
#' 
#' 
#' @export

subs_any <- function(x, byref = FALSE, ...) {
  
  if (!is.data.frame(x)) 
    stop("\nMetodo aplicado para tabelas\n")
  
  mudancas <- list(...)
  
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
    set(dt, which({dt[[j]] == mudancas[[j]][[1L]]}), j, value = mudancas[[j]][[2L]]) 
  
  
  # Colunas por expressao regular
  
  for( reg in rgx ) {
    
    reg_cols <- tryCatch(
      grep(reg, names(dt), value = T), 
      error = function(e) character(0)
    )
    
    if (length(reg_cols) == 0) 
      warning(glue::glue("\nSem colunas correspondentes para regex ({reg}).\n"))
    
    for (j in reg_cols ) 
      set(dt, which({dt[[j]] == mudancas[[reg]][[1L]]}), j, value = mudancas[[reg]][[2L]])
    
  }
  
  return( if (byref) invisible(dt) else dt )
  
}
