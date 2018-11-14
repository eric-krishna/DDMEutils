#' Inspeção de sazonalidade para séries temporais
#' 
#' @export
inspeciona_sazonalidade <- function(x, ...) UseMethod("inspeciona_sazonalidade", x)

#' @method inspeciona_outlier ts
#' @export
#' 
inspeciona_sazonalidade.ts <- function(x, tendencia = TRUE) {
  
  e_sazonal <- forecast::tbats(x, 
                               use.trend = tendencia,
                               use.box.cox = FALSE,
                               use.damped.trend = FALSE,
                               use.arma.errors = FALSE,
                               use.parallel = FALSE)$seasonal.periods
  
  ifelse(is.null(e_sazonal), 0, e_sazonal)
  
}

#' @method inspeciona_outlier data.table
#' @export
#' 
inspeciona_sazonalidade.data.table <- function(x, tendencia = TRUE, sentido = 1L, paralelo = FALSE, 
                                               periodo = c('mes','semana','dia','ano','trimestre','quadrimestre','semestre','qualquer'),
                                               idcol = if(sentido == 1L) 1L else NULL, 
                                               dtcol = if(sentido == 2L) 1L else NULL, ...) {
  
  if (! sentido %in% 1L:2L) stop('Sentido deve ser 1 (linha) ou 2 (coluna)')
  
  if (!is.null(idcol) && !idcol %in% c(0L,seq_along(x))) stop('`idcol` deve indicar uma coluna pertencente aos dados')
  
  if( !is.null(dtcol) && !dtcol %in% c(0L,seq_along(x))) stop('`dtcol` deve indicar uma coluna pertencente aos dados')
  
  
  periodo <- match.arg(periodo) %>% switch('dia' = 365, 'semana' = 52, 'mes' = 12, 'ano' = 1,
                                           'trimestre' = 4,'quadrimestre' = 3,'semestre' = 2,
                                           'qualquer' = 1)
  
  
  # options(future.globals.maxSize = +Inf) nao recomendado para x de alta dimensao com paralelo == TRUE e OS Windows
  
  if(paralelo) {
    future::plan(future::multiprocess)
    progress <- TRUE
    options(future.globals.maxSize = +Inf) 
  } else {
    future::plan(future::sequential)
    progress <- FALSE
  }  
  
  switch(sentido,
         '1' = {
           
           if (is.null(idcol) | idcol == 0) {
             ids <- seq_len(nrow(x))
           } else {
             ids <- dplyr::pull(x[, ..idcol])
             x %<>% .[, -..idcol] %>% as.matrix() 
           }
           
           x <- furrr::future_map_dfr(seq_len(nrow(x)),
                                      ~ {
                                        data.frame(
                                          ID = ids[.x],
                                          SAZONALIDADE = x[.x, ] %>% ts(frequency = periodo) %>% inspeciona_sazonalidade.ts(tendencia = tendencia),
                                          stringsAsFactors = FALSE
                                        )
                                      },
                                      .progress = progress) %>% 
             data.table::setDT()
           
         },
         
         '2' = {
           
           if (is.null(dtcol) | dtcol == 0) {
             ids <- seq_len(nrow(x)) 
           } else {
             x %<>% .[, -..dtcol]
             ids <- data.table::copy(names(x))
           }
           
           x <- furrr::future_map_dfr(seq_len(ncol(x)), 
                                      ~ {
                                        data.frame(
                                          ID = ids[.x],
                                          SAZONALIDADE = x[[.x]] %>% ts(frequency = periodo) %>% inspeciona_sazonalidade.ts(tendencia = tendencia),
                                          stringsAsFactors = FALSE
                                        )
                                      }, 
                                      .progress = progress) %>% 
             data.table::setDT()
           
         })
  
  future::plan(future::sequential)
  
  x
  
}

#' @method inspeciona_outlier tbl_df
#' @export
#' 
inspeciona_sazonalidade.tbl_df <- function(x, ...) tibble::as_tibble(inspeciona_sazonalidade(data.table::setDT(x), ...))

#' @method inspeciona_outlier data.frame
#' @export
#' 
inspeciona_sazonalidade.data.frame <- function(x, ...) inspeciona_outlier(data.table::setDT(x), ...)
