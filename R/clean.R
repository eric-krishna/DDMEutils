#' @export
.dict <- list(
  rm = c('\u0060','\u005e','\u007e','\u00d7','\u00f7',
         '\u00a1','\u00a2','\u00a3','\u00a4','\u00a5','\u00a6','\u00a7','\u00a8','\u00a9',
         '\u00aa','\u00ab','\u00ac','\u00ad','\u00ae','\u00af',
         '\u00b1','\u00b2','\u00b3','\u00b4','\u00b5','\u00b6','\u00b7','\u00b8','\u00b9',
         '\u00ba','\u00bb','\u00bc','\u00bd','\u00be','\u00bf'),
  undln = c('/','\\.','-','\\s','\\(','\\)'),
  a = c('\u00e0','\u00e1','\u00e2','\u00e3','\u00e4','\u00e5'),
  A = c('\u00c0','\u00c1','\u00c2','\u00c3','\u00c4','\u00c5'),
  c = '\u00e7',
  C = '\u00c7',
  e = c('\u00e8','\u00e9','\u00ea','\u00eb'),
  E = c('\u00c8','\u00c9','\u00ca','\u00cb'),
  i = c('\u00ec','\u00ed','\u00ee','\u00ef'),
  I = c('\u00cc','\u00cd','\u00ce','\u00cf'),
  o = c('\u00f2','\u00f3','\u00f4','\u00f5','\u00f6','\u00f8'),
  O = c('\u00d2','\u00d3','\u00d4','\u00d5','\u00d6','\u00d8'),
  u = c('\u00f9','\u00fa','\u00fb','\u00fc'),
  U = c('\u00d9','\u00da','\u00db','\u00dc')
)


#' Clean punctuated headers and text variables  
#' 
#' @description 
#' Replace or drop UTF-8 characters and punctuation in headers or/and in char/factor variables.
#' 
#' @param x chr, factor or data.frame.
#' @param keep vector of characters to preserve
#' @param add_repl named vector of additional replacements desired. Overrules \code{keep} and default replacements
#' @param ... additional parameters to factor and data.frame S3 methods. See details.
#'  
#' @details
#' For data.frame's, \code{byref = TRUE} allows to replace characters by reference. 
#' 
#' Additionally, booleans \code{col_names} and \code{vars} indicate if replacements should take place in columns names and variables, respectively. By default, \code{col_names = TRUE} 
#' and \code{vars = FALSE}.
#' 
#' @export
clean <- function(x, keep = NULL, add_repl = NULL, ...) UseMethod('clean', x)

#' @rdname clean
#' @method clean character
#' @importFrom stringr str_replace_all
#' @export
clean.character <- function(x, keep = NULL, add_repl = NULL) {
  
  additional <- names(add_repl)
  
  if (!is.null(add_repl) & sum(additional != '') != length(add_repl))
    stop("\nadd_repl has to be a named vector in the form add_repl = c('1' = 'one', '2' = 'two').")
  
  if (!is.atomic(keep) & !is.list(keep))
    stop('\nkeep has to be either NULL, a vector or a list.')
  
  .dict %<>% 
    map(setdiff, keep) %>% 
    {.[map_lgl(., ~ length(.x) > 0)]} %>% 
    map(paste, collapse = '|')
  
  subs <- 
    .dict %>% 
    names() %>% 
    str_replace_all(c('rm' = '', 'undln' = '_')) %>% 
    set_names(.dict)
  
  fixing <- c('_{2,}' = '_', '^_|_$' = '')  
  
  x %>% 
    str_replace_all(c(add_repl, subs, fixing))
  
}

#' @rdname clean
#' @method clean factor
#' @export
clean.factor <- function(x, keep = NULL, add_repl = NULL, byref = FALSE) {
  
  x <- if (byref) x else copy(x)
  
  setattr(x, 'levels', attr(x, 'levels') %>% clean.character(keep, add_repl))
  
  if (byref) invisible(x) else x
  
}

#' @rdname clean
#' @method clean data.frame
#' @importFrom stringr str_replace_all
#' @export
clean.data.frame <- function(x, col_names = TRUE, vars = FALSE, keep = NULL, add_repl = NULL, byref = FALSE) {
  
  additional <- names(add_repl)
  
  if (!is.null(add_repl) & sum(additional != '') != length(add_repl))
    stop("\nadd_repl has to be a named vector in the form add_repl = c('1' = 'one', '2' = 'two').")
  
  if (!is.atomic(keep) & !is.list(keep))
    stop('\nkeep has to be either NULL, a vector or a list.')
  
  if (!is.data.table(x))
    x <- as.data.table(x)
  
  if (byref) dt <- x else dt <- copy(x)
  
  .dict %<>% 
    map(setdiff, keep) %>% 
    {.[map_lgl(., ~ length(.x) > 0)]} %>% 
    map(paste, collapse = '|')
  
  subs <- 
    .dict %>% 
    names() %>% 
    str_replace_all(c('rm' = '', 'undln' = '_')) %>% 
    set_names(.dict)
  
  fixing <- c('_{2,}' = '_', '^_|_$' = '')
  
  
  # Columns names
  if (col_names) 
    setnames(dt, names(dt) %>% str_replace_all(c(add_repl, subs, fixing)))
  
  # Variables 
  if (vars) {
    
    string_vars <- dt[, map_lgl(.SD, is.character) %>% which() %>% names()]
    
    factor_vars <- dt[, map_lgl(.SD, is.factor) %>% which() %>% names()] 
    
    if (length(string_vars) > 0) 
      dt[, (string_vars) := map(.SD, str_replace_all, c(add_repl, subs, fixing)), .SDcols = string_vars][]
    
    if(length(factor_vars) > 0)
      walk(factor_vars, ~ dt[[.x]] %>% clean.factor(keep = keep, add_repl = add_repl, byref = T))
    
  }
  
  if (byref) invisible(dt[]) else dt[]
  
}
