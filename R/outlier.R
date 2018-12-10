#' Time series outlier inspection
#' 
#' @param x ts or data.frame
#' @param window_size integer window size. Determines how many observations near an outlier are used to calculate the input value.  
#' @param anom_method "gesd" or "iqr", argument passed to anomalize::anomalize().
#' @param \code{...} Additional arguments for the data.frame case. See details.
#' 
#' @details 
#' When \code{margin = 1}, set \code{idcol} to an integer pointing out the index column that identifies the time series.
#' When \code{margin = 2}, set \code{dtcol} to an integer pointing out the date column. 
#' .parallel TRUE/FALSE if tasks should be parallelized. If \code{TRUE}, uses \code{future} and \code{furrr} packages.
#' out_format "wide" or "long", indicating output format. If \code{margin = 2} and there are many time series, \code{out_format = "long"} is easier to handle than wide format.

#' 
#' @export
insp_outlier <- function(x, ...) UseMethod('insp_outlier', x)


#' @method insp_outlier ts
#' @importFrom lubridate %m-% period
#' @export
insp_outlier.ts <- function(x, window_size = 3, anom_method = c('gesd','iqr')) {
  
  n <- length(x)
  
  anom_method <- match.arg(anom_method)
  
  dates_try <- zoo::as.Date.ts(x)
  
  if ({dates_try[1] %>% year() %>% nchar()} == 2) 
    dates_try <- seq.Date(
      from = {Sys.Date() %m-% period(n - 1, units = 'days')},
      to = Sys.Date(),
      length.out = n
    )
  
  bs <- tibble::tibble(DATE = dates_try, SERIES = x)
  
  y <- suppressMessages(
    bs %>% 
      anomalize::time_decompose(SERIES, method = 'stl', trend = 'auto',frequency = 'auto') %>%
      anomalize::anomalize(remainder, method = anom_method, alpha = 0.05) %>%
      anomalize::time_recompose() %>% 
      dplyr::filter(anomaly == 'Yes') %>% 
      dplyr::pull(DATE)
  )
  
  setDT(bs)
  
  new <- c('IMPUTED_SERIES', 'OUTLIER_FLAG')
  
  if(length(y) > 0) {
    
    filtering <- bs[DATE %in% y, which = T]
    
    suppressWarnings({
      bs[
        filtering,
        `:=`(
          wdw_min = pmax(filtering - window_size, 1),
          wdw_max = pmin(filtering + window_size, n)
        )
      ][
        filtering,`:=`(
          IMPUTED_SERIES = apply(.SD, 1, function(x) median(bs$SERIES[x[1]:x[2]])), 
          OUTLIER_FLAG = 1L
        ),
        .SDcols = c('wdw_min','wdw_max')
      ][
        is.na(OUTLIER_FLAG), 
        `:=`(
          OUTLIER_FLAG = 0L, 
          IMPUTED_SERIES = SERIES
        )
      ][]
    })
    
    bs[, c('wdw_min','wdw_max') := NULL][]
    
  } else {
    suppressWarnings({bs[, (new) := list(NA_real_, 0L)][]})
  }
  
  bs
  
}

#' @method insp_outlier data.frame
#' @export
insp_outlier.data.frame <- function(x, margin = 1L, window_size = 3, .parallel = FALSE, out_format = c('wide','long'), anom_method = c('gesd','iqr'),
                                    idcol = if(margin == 1L) 1L else NULL,  dtcol = if(margin == 2L) 1L else NULL) {
  
  out_format <- match.arg(out_format)
  
  anom_method <- match.arg(anom_method)
  
  if (! margin %in% 1L:2L) 
    stop('\nMargin should be 1 (rows) or 2 (columns)')
  
  if (!is.null(idcol) && !idcol %in% c(0L,seq_along(x))) 
    stop('\n`idcol` should indicate an existing column')
  
  if( !is.null(dtcol) && !dtcol %in% c(0L,seq_along(x))) 
    stop('\n`dtcol` should indicate an existing column')
  
  if (!is.data.table(x)) 
    x <- as.data.table(x)
  
  
  if (.parallel) { 
    future::plan(future::multiprocess)
    progress <- TRUE
  } else {
    future::plan(future::sequential)
    progress <- FALSE
  }  
    
  switch(margin,
         '1' = {

           if (is.null(idcol) | idcol == 0) {
             ids <- seq_len(nrow(x))
           } else {
             ids <- x[[idcol]]
             x[, (idcol) := NULL] 
           }
           
           dates <- copy(names(x))
           
           x <- furrr::future_map(seq_len(nrow(x)),
                                  ~ {
                                    x[.x] %>% 
                                      as.numeric() %>% 
                                      as.ts() %>% 
                                      insp_outlier.ts(window_size = window_size, anom_method = anom_method) %>%
                                      {.[, `:=`(DATE = NULL, PERIOD = dates, ID = ids[.x])][]}
                                   },
                                  .progress = progress) %>%
             rbindlist()
           
           future::plan(future::sequential)
           
           if (out_format == 'long') {
             x %<>% 
               melt(measure = grep('SERIES',names(x)), variable.name = 'SERIES', value.name = 'VALUE') %>% 
               {.[stringr::str_detect(SERIES, '^IMPUTED_'), `:=`(SERIES =  stringr::str_remove(SERIES, '^IMPUTED_'), IMPUTED = 1L)]} %>% 
               {.[is.na(IMPUTED), IMPUTED := 0L]} %>% 
               {.[, SERIES := NULL]} %>% 
               setcolorder(c('ID', 'PERIOD', 'OUTLIER_FLAG', 'IMPUTED','VALUE')) %>% {.[]}
           } else {
             x %<>% setcolorder(c('ID','PERIOD')) %>% {.[]}
           }
           
         },
         
         '2' = {
           
           if (is.null(dtcol) | dtcol == 0) {
             dates <- data.table(seq_len(nrow(x)))
           } else {
             dates <- x[, ..dtcol]
             x[, (dtcol) := NULL]
           }
           
           dt_names <- names(x)
           
           x <- 
             furrr::future_map2(x, # .x
                                seq_along(x), # .y
                                ~ {
                                  as.ts(.x) %>% 
                                    insp_outlier.ts(window_size = window_size, anom_method = anom_method) %>% 
                                    data.table::setnames(old = c('SERIES', 'IMPUTED_SERIES', 'OUTLIER_FLAG'),
                                                         new = paste0(c('', 'IMPUTED_', 'OUTLIER_FLAG_'), dt_names[.y])) %>% 
                                    {.[, -1L]}
                                },
                                .progress = progress)
          
           if (out_format == 'wide') {
             
             x %<>% dplyr::bind_cols(dates, .) %>% setnames(names(.)[1L], 'DATE') %>% {.[]} # faster than Reduce(merge, x)
             
           } else {
             
             suppressWarnings(
               x %<>% 
                 furrr::future_map_dfr(
                   ~ {
                     melt(.x, measure = 1:2, value.name = 'VALUE', variable.name = 'SERIES') %>%
                     {.[, DATE := dates]} %>% 
                     {.[stringr::str_detect(SERIES, '^IMPUTED_'), `:=`(SERIES =  stringr::str_remove(SERIES, '^IMPUTED_'), IMPUTED = 1L)]} %>% 
                     {.[is.na(IMPUTED), IMPUTED := 0L]} %>% 
                       setnames(grep('^OUTLIER_FLAG_', names(.), value = T), 'OUTLIER_FLAG') %>% 
                       setcolorder(c('DATE','SERIES','OUTLIER_FLAG','IMPUTED','VALUE')) %>% {.[]}
                   },
                   .progress = progress
                 )
             )
             
           }
           
         })
  
  future::plan(future::sequential)
  
  x
  
}
