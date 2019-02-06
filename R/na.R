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


#' NA imputation
#'
#' @description Wraps and extends \code{zoo} package's NA imputation functions. 
#'
#' @param x Vector to input data.
#' @param how Inputation type. One of 'mean', 'median', 
#' 'locf' (Last Observation Carried Forward), 
#' 'nocb' (Next Observation Carried Backward), 
#' 'lin_interp' (linear interpolation) or 
#' 'cub_spline' (cubic spline)
#' @param window Window size. Used when \code{how} equals 'mean' or 'median'.
#' @param ... Further arguments to zoo functions na.locf, na.approx or na.spline.
#' 
#' @importFrom zoo na.locf 
#' @importFrom zoo na.approx
#' @importFrom zoo na.spline
#' @importFrom stringr str_split_fixed
#' @export

na_input <- function(x, how = 'median', window = Inf, ...) {
  
  if (!is.numeric(x)) 
    stop("\nx must be numeric")
  
  if (! window > 0) 
    stop("\nwindow must be a positive integer or +Inf")
  
  if (! how %in% c('mean','median','locf','nocb','lin_interp','cub_spline')) 
    stop("\nhow must be one of c('mean','median','locf','nocb','lin_interp','cub_spline')")
  # Include alignment option for how = 'mean' or 'median'. 
  # Something like 'mean:center'. To split 'how', str_split_fixed(how, "[a-z]*(?=:)", n = 2)
  
  
  if (how %in% c('mean','median')) {
    
    na_id <- which(is.na(x))
    input <- tryCatch(
      map_dbl(na_id, 
              ~ {
                inf_lim <- max((.x - window), 1L)
                sup_lim <- min((.x + window), length(x))
                x[inf_lim:sup_lim] %>% {.[!is.na(.)]} %>% {get(how)(.)}
              }),
      error = function(e) stop("\nTry increasing 'window' or changing 'how'")
    )
    x[na_id] <- input
    x
    
  } else if (how == 'locf') {
    
    na.locf(x, na.rm = FALSE, ...)
    
  } else if (how == 'nocb') {
    
    na.locf(x, na.rm = FALSE, fromLast = TRUE, ...)
    
  } else if (how == 'lin_interp') {
    
    na.approx(x, na.rm = FALSE, ...)
    
  } else if (how == 'cub_spline') {
    
    na.spline(x, na.rm = FALSE, ...)
    
  }
  
}


