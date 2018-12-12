#' Time series seasonality inspection 
#' 
#' @param x ts or data.frame
#' @param trend \code{TRUE/FALSE} indicating if input time series has trend.
#' @param margin For data.frames, \code{margin = 1} if multiple time series are framed row-wise; while \code{margin = 2} if column-wise.
#' @param periodicity \code{'month','week','day','year','three_months','four_months','six_months','any'}.
#' @param \code{...} Additional arguments for the data.frame case. See details.
#' 
#' 
#'
#' @details 
#' 
#' \itemize{
#'  \item When \code{margin = 1}, set \code{idcol} to an integer pointing out the index column that identifies the time series;
#'  \item When \code{margin = 2}, set \code{dtcol} to an integer pointing out the date column;
#'  \item .parallel TRUE/FALSE if tasks should be parallelized. If \code{TRUE}, uses \code{future} and \code{furrr} packages.
#' }
#' 
#' 
#' @export
insp_seasonality <- function(x, ...) UseMethod("insp_seasonality", x)


#' @rdname insp_seasonality
#' @method insp_seasonality ts
#' @export
#' 
insp_seasonality.ts <- function(x, trend = FALSE) {
  
  e_seasonal <- forecast::tbats(x, 
                                use.trend = trend,
                                use.box.cox = FALSE,
                                use.damped.trend = FALSE,
                                use.arma.errors = FALSE,
                                use.parallel = FALSE)$seasonal.periods
  
  ifelse(is.null(e_seasonal), 0, e_seasonal)
  
}


#' @rdname insp_seasonality
#' @method insp_seasonality data.frame
#' @export
#' 
insp_seasonality.data.frame <- function(x, trend = FALSE, margin = 1L, .parallel = FALSE, 
                                        periodicity = c('month','week','day','year','three_months',
                                                        'four_months','six_months','any'),
                                        idcol = NULL, 
                                        dtcol = NULL, ...) {
  
  if (! margin %in% 1L:2L) 
    stop('\nMargin should be 1 (rows) or 2 (columns)')
  
  if (is.null(dtcol))
    dtcol <- 0
  
  if (is.null(idcol)) 
    idcol <- 0
  
  if (! idcol %in% c(0L,seq_along(x))) 
    stop('\n`idcol` should indicate an existing column')
  
  if (! dtcol %in% c(0L,seq_along(x))) 
    stop('\n`dtcol` should indicate an existing column')
  
  if (!is.data.table(x)) 
    x <- as.data.table(x)
  
  periodicity <- match.arg(periodicity) %>% switch('day' = 365, 'week' = 52, 'month' = 12, 'year' = 1,
                                                   'three_months' = 4, 'four_months' = 3,'six_months' = 2,
                                                   'any' = 1)
  
  
  if(.parallel) {
    future::plan(future::multiprocess)
    progress <- TRUE
  } else {
    future::plan(future::sequential)
    progress <- FALSE
  }  
  
  switch(margin,
         '1' = {
           
           if (idcol == 0) {
             ids <- seq_len(nrow(x))
             x %<>% as.matrix() 
           } else {
             ids <- x[[idcol]]
             x %<>% .[, -..idcol] %>% as.matrix() 
           }
           
           x <- furrr::future_map_dfr(seq_len(nrow(x)),
                                      ~ {
                                        data.frame(
                                          ID = ids[.x],
                                          SEASONALITY = {
                                            x[.x, ] %>% 
                                              ts(frequency = periodicity) %>% 
                                              insp_seasonality.ts(trend = trend)
                                          },
                                          stringsAsFactors = FALSE
                                        )
                                      },
                                      .progress = progress) %>% 
             as.data.table()
           
         },
         
         '2' = {
           
           if (dtcol == 0) {
             ids <- copy(names(x))
           } else {
             x %<>% .[, -..dtcol]
             ids <- copy(names(x))
           }
           
           x <- furrr::future_map_dfr(seq_len(ncol(x)), 
                                      ~ {
                                        data.frame(
                                          ID = ids[.x],
                                          SEASONALITY = {
                                            x[[.x]] %>% 
                                              ts(frequency = periodicity) %>% 
                                              insp_seasonality.ts(trend = trend)
                                          },
                                          stringsAsFactors = FALSE
                                        )
                                      }, 
                                      .progress = progress) %>% 
             as.data.table()
           
         })
  
  future::plan(future::sequential)
  
  x
  
}

