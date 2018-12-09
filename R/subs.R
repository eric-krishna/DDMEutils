#' Fast value replacement
#' 
#' @description 
#' 
#' Replaces specified values for any other in data.frame variables. Changes are made given column names 
#' or regular expressions along with its replacements in a single call.
#' 
#' @param x A data.frame
#' @param byref \code{TRUE/FALSE} indicating if data imputation should be made by reference. 
#' @param \code{...} Syntax for replacements. See details.
#' 
#' @details 
#' 
#' Replacements must be as follows: \code{subs_any(x, byref, var_1 = list("?",NA), var_2 = list("#", 9999))}.
#' 
#' 
#' @export

subs_any <- function(x, byref = FALSE, ...) {
  
  if (!is.data.frame(x)) 
    stop("\nApplicable for data.frame\n")
  
  changes <- list(...)
  
  if (byref) dt <- x else dt <- copy(x)
  
  vars <- names(changes)
  
  col <- vars[vars %in% names(dt)]
  rgx <- setdiff(vars, col)
  
  # Explicit columns
  
  for( j in col ) 
    set(dt, which({dt[[j]] == changes[[j]][[1L]]}), j, value = changes[[j]][[2L]]) 
  
  
  # By regex
  
  for( reg in rgx ) {
    
    reg_cols <- tryCatch(
      grep(reg, names(dt), value = T), 
      error = function(e) character(0)
    )
    
    if (length(reg_cols) == 0) 
      warning(glue::glue("\nNo matching columns for regex ({reg}).\n"))
    
    for (j in reg_cols ) 
      set(dt, which({dt[[j]] == changes[[reg]][[1L]]}), j, value = changes[[reg]][[2L]])
    
  }
  
  if (byref) invisible(dt[]) else dt[]
  
}


#' Manage punctuated headers and variables  
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
#' For data.frame's, \code{byref = TRUE} allows to replace characters by reference. Additionally, booleans \code{cols} and 
#' \code{vars} indicate if replacements should take place in columns and variables, respectively. By default, \code{cols = TRUE} 
#' and \code{vars = FALSE}.
#' 
#' @export
subs_utf8_punct <- function(x, keep = NULL, add_repl = NULL, ...) UseMethod('subs_utf8_punct', x)

#' @method subs_utf8_punct character
#' @export
subs_utf8_punct.character <- function(x, keep = NULL, add_repl = NULL) {
  
  additional <- names(add_repl)
  
  if (!is.null(add_repl) & sum(additional != '') != length(add_repl))
    stop("\nadd_repl has to be a named vector in the form add_repl = c('1' = 'one', '2' = 'two').")
  
  if (!is.atomic(keep) & !is.list(keep))
    stop('\nkeep has to be either NULL, a vector or a list.')
  
  dict <- list(
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
  
  dict %<>% 
    map(setdiff, keep) %>% 
    {.[map_lgl(., ~ length(.x) > 0)]} %>% 
    map(paste, collapse = '|')
  
  subs <- 
    dict %>% 
    names() %>% 
    str_replace_all(c('rm' = '', 'undln' = '_')) %>% 
    set_names(dict)

  fixing <- c('_{2,}' = '_', '^_|_$' = '')  
  
  x %>% 
    str_replace_all(c(add_repl, subs, fixing))
  
}

#' @method subs_utf8_punct factor
#' @export
subs_utf8_punct.factor <- function(x, keep = NULL, add_repl = NULL, byref = FALSE) {
  
  x <- if (byref) x else copy(x)
  
  setattr(x, 'levels', attr(x, 'levels') %>% subs_utf8_punct.character(keep, add_repl))
  
  if (byref) invisible(x) else x
  
}

#' @method subs_utf8_punct data.frame
#' @export
subs_utf8_punct.data.frame <- function(x, cols = TRUE, vars = FALSE, keep = NULL, 
                                       add_repl = NULL, byref = FALSE) {
  
  additional <- names(add_repl)
  
  if (!is.null(add_repl) & sum(additional != '') != length(add_repl))
    stop("\nadd_repl has to be a named vector in the form add_repl = c('1' = 'one', '2' = 'two').")
  
  if (!is.atomic(keep) & !is.list(keep))
    stop('\nkeep has to be either NULL, a vector or a list.')
  
  if (!is.data.table(x))
    x <- as.data.table(x)
  
  if (byref) dt <- x else dt <- copy(x)
  
  dict <- list(
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
  
  dict %<>% 
    map(setdiff, keep) %>% 
    {.[map_lgl(., ~ length(.x) > 0)]} %>% 
    map(paste, collapse = '|')
  
  subs <- 
    dict %>% 
    names() %>% 
    str_replace_all(c('rm' = '', 'undln' = '_')) %>% 
    set_names(dict)
  
  fixing <- c('_{2,}' = '_', '^_|_$' = '')
  
  
  # Columns names
  if (cols) 
    setnames(dt, names(dt) %>% str_replace_all(c(add_repl, subs, fixing)))
  
  # Variables 
  if (vars) {
    
    string_vars <- dt[, map_lgl(.SD, is.character) %>% which() %>% names()]
    
    factor_vars <- dt[, map_lgl(.SD, is.factor) %>% which() %>% names()] 
    
    if (length(string_vars) > 0) 
      dt[, (string_vars) := map(.SD, str_replace_all, c(add_repl, subs, fixing)), .SDcols = string_vars][]
    
    if(length(factor_vars) > 0)
      walk(factor_vars, ~ dt[[.x]] %>% subs_utf8_punct.factor(keep = keep, add_repl = add_repl, byref = T))
    
  }
  
  if (byref) invisible(dt[]) else dt[]
  
}
