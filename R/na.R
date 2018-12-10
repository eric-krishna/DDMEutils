#' Column-wise NA summary
#' 
#' @description 
#' 
#' NA summary of data.frame variables.
#' 
#' @param x A data.frame
#' @param min_prop Numeric between 0 and 1 to limit table output 
#'
#' @export
#' @return Table with column-wise NA summary
#' 

na_prop <- function(x, min_prop = 0) {

  if (! min_prop %between% c(0, 1)) stop("\n'min_prop' must be a proportion.\n")

  n <- nrow(x)
  
  if (!is.data.table(x)) 
    x <- as.data.table(x)

  x[, map(.SD, ~ sum(is.na(.x)))] %>% 
    t() %>% 
    as.data.table(keep.rownames = "VARIABLE") %>% 
    .[, .(VARIABLE,
          N_MISSING = V1,
          N_TOTAL = N_TOTAL <- n,
          NA_FREQ = V1 / N_TOTAL)] %>% 
    .[NA_FREQ >= min_prop]
}



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
#' Replacements must be either \code{subs_na(x, byref, var_1 = "?", var_2 = 9999, "[0-9]{2}$" = 0)} or
#' wrapped by \code{list()}: \code{subs_na(x, byref, list(var_1 = "?", var_2 = 9999, "[0-9]{2}$" = 0))}.
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
