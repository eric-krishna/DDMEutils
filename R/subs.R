#' Fast NA replacement
#' 
#' @description 
#' 
#' Replaces NA for other values in data.frame variables. Changes are made given column names or regular
#' expressions along with its replacements in a single call.
#'  
#' @param x A data.frame
#' @param byref \code{TRUE/FALSE} indicating if data imputation should be made by reference. 
#' @param \code{...} Syntax for replacements. See details.
#' 
#' @details 
#' 
#' The syntax for replacements is either
#' \itemize{
#'  \item \code{subs_na(x, byref, var_1 = "?", var_2 = 9999, "[0-9]{2}$" = 0)} or
#'  \item \code{subs_na(x, byref, list(var_1 = "?", var_2 = 9999, "[0-9]{2}$" = 0))}
#' }
#' 
#' 
#' @export

subs_na <- function(x, byref = FALSE, ...) {
  
  if (!is.data.frame(x)) 
    stop("\nApplicable for data.frame.")
  
  changes <- list(...)
  depth <- vec_depth(changes)
  
  if (! depth %in% 2L:3L) 
    stop("\nMisspecified syntax for replacements.")
  
  if (depth == 3L) 
    changes <- unlist(changes, recursive = F)
  
  if (byref) dt <- x else dt <- copy(x)
  
  vars <- names(changes)
  
  col <- vars[vars %in% names(dt)]
  rgx <- setdiff(vars, col)
  
  # Explicit columns
  
  for( j in col ) 
    set(dt, which(is.na(dt[[j]])), j, value = changes[[j]]) 
  
  
  # By regex
  
  for( reg in rgx ) {
    
    reg_cols <- tryCatch(
      grep(reg, names(dt), value = T), 
      error = function(e) character(0)
    )
    
    if (length(reg_cols) == 0L) 
      warning(glue::glue("\nNo matching columns for regex ({reg})."))
    
    for (j in reg_cols ) 
      set(dt, which(is.na(dt[[j]])), j, value = changes[[reg]])
    
  }
  
  
  if (byref) invisible(dt[]) else dt[]
  
}


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
    stop("\nApplicable for data.frame.")
  
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
      warning(glue::glue("\nNo matching columns for regex ({reg})."))
    
    for (j in reg_cols ) 
      set(dt, which({dt[[j]] == changes[[reg]][[1L]]}), j, value = changes[[reg]][[2L]])
    
  }
  
  if (byref) invisible(dt[]) else dt[]
  
}