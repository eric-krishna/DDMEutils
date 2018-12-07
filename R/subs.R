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


#' Substitui caracteres com codifica\u00e7\u00e3o UTF-8 
#' 
#' @param x chr, factor ou data.frame.
#' @param manter vetor com caracteres que devem ser mantidos.
#' @param ... par\u00e2metros adicionais para classes factor e data.frame, veja Detalhes.
#' 
#' @details Quando trabalhando com factor ou data.frame, \u00e9 poss\u00edvel fazer a substitui\u00e7\u00e3o por
#' refer\u00eancia com \code{byref = TRUE}. Para data.frame, os booleanos \code{cols} e \code{vars} indicam se a
#' substitui\u00e7\u00e3o deve ser feitas nos nomes das vari\u00e1veis e nos valores delas, respectivamente. 
#' 
#' @export
subs_utf8_punct <- function(x, manter = NULL, ...) UseMethod('subs_utf8_char', x)

#' @method subs_utf8_punct character
#' @export
subs_utf8_punct.character <- function(x, manter = NULL) {
  
  dict <- list(
    etc = c('\u0060','\u005e','\u007e','\u00d7','\u00f7',
            '\u00a1','\u00a2','\u00a3','\u00a4','\u00a5','\u00a6','\u00a7','\u00a8','\u00a9',
            '\u00aa','\u00ab','\u00ac','\u00ad','\u00ae','\u00af',
            '\u00b1','\u00b2','\u00b3','\u00b4','\u00b5','\u00b6','\u00b7','\u00b8','\u00b9',
            '\u00ba','\u00bb','\u00bc','\u00bd','\u00be','\u00bf'),
    a = c('\u00e0','\u00e1','\u00e2','\u00e3','\u00e4','\u00e5'),
    A = c('\u00c0','\u00c1','\u00c2','\u00c3','\u00c4','\u00c5'),
    cc = '\u00e7',
    CC = '\u00c7',
    e = c('\u00e8','\u00e9','\u00ea','\u00eb'),
    E = c('\u00c8','\u00c9','\u00ca','\u00cb'),
    i = c('\u00ec','\u00ed','\u00ee','\u00ef'),
    I = c('\u00cc','\u00cd','\u00ce','\u00cf'),
    o = c('\u00f2','\u00f3','\u00f4','\u00f5','\u00f6','\u00f8'),
    O = c('\u00d2','\u00d3','\u00d4','\u00d5','\u00d6','\u00d8'),
    u = c('\u00f9','\u00fa','\u00fb','\u00fc'),
    U = c('\u00d9','\u00da','\u00db','\u00dc')
  )
  
  dict %<>% 
    map(setdiff, manter) %>% 
    {.[map_lgl(., ~ length(.x) > 0)]} %>% 
    map(paste, collapse = '|')
  
  repl <- dict %>% names() %>% str_replace('etc','')
  
  x %>% str_replace_all(repl %>% set_names(dict))
  
}

#' @method subs_utf8_punct factor
#' @export
subs_utf8_punct.factor <- function(x, manter = NULL, byref = FALSE) {
  
  x <- if (byref) x else copy(x)
  
  setattr(x, 'levels', attr(x, 'levels') %>% subs_utf8_char.character(manter))
  
  if (byref) invisible(x) else x
  
}

#' @method subs_utf8_punct data.frame
#' @export
subs_utf8_punct.data.frame <- function(x, cols = T, vars = T, manter = NULL, byref = FALSE) {
  
  if (!is.data.table(x))
    x <- as.data.table(x)
  
  if (byref) dt <- x else dt <- copy(x)
  
  dict <- list(
    etc = c('\u0060','\u005e','\u007e','\u00d7','\u00f7',
            '\u00a1','\u00a2','\u00a3','\u00a4','\u00a5','\u00a6','\u00a7','\u00a8','\u00a9',
            '\u00aa','\u00ab','\u00ac','\u00ad','\u00ae','\u00af',
            '\u00b1','\u00b2','\u00b3','\u00b4','\u00b5','\u00b6','\u00b7','\u00b8','\u00b9',
            '\u00ba','\u00bb','\u00bc','\u00bd','\u00be','\u00bf'),
    a = c('\u00e0','\u00e1','\u00e2','\u00e3','\u00e4','\u00e5'),
    A = c('\u00c0','\u00c1','\u00c2','\u00c3','\u00c4','\u00c5'),
    cc = '\u00e7',
    CC = '\u00c7',
    e = c('\u00e8','\u00e9','\u00ea','\u00eb'),
    E = c('\u00c8','\u00c9','\u00ca','\u00cb'),
    i = c('\u00ec','\u00ed','\u00ee','\u00ef'),
    I = c('\u00cc','\u00cd','\u00ce','\u00cf'),
    o = c('\u00f2','\u00f3','\u00f4','\u00f5','\u00f6','\u00f8'),
    O = c('\u00d2','\u00d3','\u00d4','\u00d5','\u00d6','\u00d8'),
    u = c('\u00f9','\u00fa','\u00fb','\u00fc'),
    U = c('\u00d9','\u00da','\u00db','\u00dc')
  )
  
  dict %<>% 
    map(setdiff, manter) %>% 
    {.[map_lgl(., ~ length(.x) > 0)]} %>% 
    map(paste, collapse = '|')
  
  repl <- dict %>% names() %>% str_replace('etc','')
  
  subs <- repl %>% set_names(dict)
  
  
  
  # Nomes das colunas
  if (cols) 
    setnames(dt, str_replace_all(names(dt), subs))
  
  # Texto nas variaveis
  if (vars) {
    
    string_vars <- dt[, map(.SD, class) %>% 
                        unlist() %>% 
                        {which(. == 'character')} %>% 
                        names()]
    
    factor_vars <- dt[, map(.SD, class) %>% 
                        unlist() %>% 
                        {which(. == 'factor')} %>% 
                        names()] 
    
    if (length(string_vars) > 0) 
      dt[, (string_vars) := map(.SD, str_replace_all, subs), .SDcols = string_vars][]
    
    if(length(factor_vars) > 0)
      walk(factor_vars, ~ dt[[.x]] %>% subs_utf8_char.factor(manter = manter, byref = T))
    
  }
  
  if (byref) invisible(dt[]) else dt[]
  
}