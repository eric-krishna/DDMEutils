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