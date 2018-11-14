#' Inspeção de outliers para séries temporais
#' 
#' @export
inspeciona_outlier <- function(x, ...) UseMethod('inspeciona_outlier', x)

#' @method inspeciona_outlier numeric
#' @export
#' 
inspeciona_outlier.numeric <- function(x, ...) inspeciona_outlier.ts(as.ts(x), ...)

#' @method inspeciona_outlier integer
#' @export
inspeciona_outlier.integer <- function(x, ...) inspeciona_outlier.ts(as.ts(x), ...)

#' @method inspeciona_outlier tbl_df
#' @export
inspeciona_outlier.tbl_df <- function(x, ...) as_tibble(inspeciona_outlier(data.table::setDT(x), ...))

#' @method inspeciona_outlier data.frame
#' @export
inspeciona_outlier.data.frame <- function(x, ...) inspeciona_outlier(data.table::setDT(x), ...)

#' @method inspeciona_outlier ts
#' @export
inspeciona_outlier.ts <- function(x, janela = 3, anom_method = c('gesd','iqr')) {
  
  n <- length(x)
  
  anom_method <- match.arg(anom_method)
  
  bs <- tibble::tibble(DATA = zoo::as.Date.ts(x), SERIE = x)
  
  y <- suppressMessages(
    bs %>% 
      anomalize::time_decompose(SERIE, method = 'stl', trend = 'auto',frequency = 'auto') %>%
      anomalize::anomalize(remainder, method = anom_method, alpha = 0.05) %>%
      anomalize::time_recompose() %>% 
      dplyr::filter(anomaly == 'Yes') %>% 
      dplyr::pull(DATA)
  )
  
  bs %>% data.table::setDT()
  
  novas <- c('SERIE_IMPUTADA', 'FLAG_OUTLIER')
  
  if(length(y) > 0) {
    
    filtra <- bs[DATA %in% y, which = T]
    
    suppressWarnings({
      bs[
        filtra,
        `:=`(
          jan_min = pmax(filtra - janela, 1),
          jan_max = pmin(filtra + janela, n)
        )
      ][
        filtra,`:=`(
          SERIE_IMPUTADA = apply(.SD, 1, function(x) median(bs$SERIE[x[1]:x[2]])), 
          FLAG_OUTLIER = 1L
        ),
        .SDcols = c('jan_min','jan_max')
      ][
        is.na(FLAG_OUTLIER), 
        `:=`(
          FLAG_OUTLIER = 0L, 
          SERIE_IMPUTADA = SERIE
        )
      ][]
    })
    
    bs[, c('jan_min','jan_max') := NULL][]
    
  } else suppressWarnings({bs[, (novas) := list(NA_real_, 0L)][]})
  
  bs
  
}

#' @method inspeciona_outlier data.table
#' @export
inspeciona_outlier.data.table <- function(x, sentido = 1L, janela = 3, paralelo = FALSE, out_format = c('wide','long'), anom_method = c('gesd','iqr'),
                                          idcol = if(sentido == 1L) 1L else NULL,  dtcol = if(sentido == 2L) 1L else NULL) {
  
  out_format <- match.arg(out_format)
  
  anom_method <- match.arg(anom_method)
  
  if (! sentido %in% 1L:2L) stop('Sentido deve ser 1 (linha) ou 2 (coluna)')
  
  if (!is.null(idcol) && !idcol %in% c(0L,seq_along(x))) stop('`idcol` deve indicar uma coluna pertencente aos dados')
  
  if( !is.null(dtcol) && !dtcol %in% c(0L,seq_along(x))) stop('`dtcol` deve indicar uma coluna pertencente aos dados')
  
  # options(future.globals.maxSize = +Inf) nao recomendado para x de alta dimensao com paralelo == TRUE e OS Windows
  
  if (paralelo) { 
    future::plan(future::multiprocess)
    progress <- TRUE
    options(future.globals.maxSize = +Inf)
   # on.exit(future::plan(future::sequential))
  } else {
    plan(sequential)
    progress <- FALSE
  }  
    
  switch(sentido,
         '1' = {

           if (is.null(idcol) | idcol == 0) {
             ids <- seq_len(nrow(x))
           } else {
             ids <- dplyr::pull(x[, ..idcol])
             x %<>% .[, -..idcol] 
           }
           
           data <- data.table::copy(names(x))
           
           x <- furrr::future_map(seq_len(nrow(x)),
                                  ~ {
                                   as.ts(as.numeric(x[.x])) %>% 
                                     inspeciona_outlier.ts(janela = janela, anom_method = anom_method) %>%
                                     {.[, `:=`(DATA = NULL, PERIODO = data, ID = ids[.x])][]}
                                   },
                                  .progress = progress) %>%
             data.table::rbindlist()
           
           future::plan(future::sequential)
           
           if (out_format == 'long') {
             x %<>% 
               melt(measure = grep('SERIE',names(x)), variable.name = 'SERIE', value.name = 'VALOR') %>% 
               {.[str_detect(SERIE, '_IMPUTADA$'), `:=`(SERIE =  str_remove(SERIE, '_IMPUTADA$'), IMPUTADA = 1L)]} %>% 
               {.[is.na(IMPUTADA), IMPUTADA := 0L]} %>% 
               {.[, SERIE := NULL]} %>% 
               data.table::setcolorder(c('ID', 'PERIODO', 'FLAG_OUTLIER', 'IMPUTADA','VALOR')) %>% {.[]}
           } else {
             x %<>% data.table::setcolorder(c('ID','PERIODO')) %>% {.[]}
           }
           
         },
         
         '2' = {
           
           nomes <- names(x)
           acr <- 1L
           if (is.null(dtcol) | dtcol == 0) {
             data <- data.table::data.table(seq_len(nrow(x)))
             acr <- 0L
             dtcol <- 1L
           } else {
             data <- x[, ..dtcol]
             x %<>% .[, -..dtcol] 
           }
           
           x %<>% 
             furrr::future_map2(seq_along(.), 
                                ~ {
                                  as.ts(.x) %>% 
                                    inspeciona_outlier.ts(janela = janela, anom_method = anom_method) %>% 
                                    data.table::setnames(old = c('SERIE', 'SERIE_IMPUTADA', 'FLAG_OUTLIER'),
                                                         new = paste0(nomes[.y + acr], c('', '_IMPUTADA', '_FLAG_OUTLIER'))) %>% 
                                    {.[, -1L]}
                                },
                                .progress = progress)
          
           if (out_format == 'wide') {
             
             x %<>% dplyr::bind_cols(data, .) %>% data.table::setnames(names(.)[1L], 'DATA') %>% {.[]} # mais rapido do que Reduce(merge, x)
             
           } else {
             
             suppressWarnings(
               x %<>% 
                 furrr::future_map_dfr(
                   ~ {
                     melt(.x, measure = 1:2, value.name = 'VALOR', variable.name = 'SERIE') %>%
                     {.[, DATA := data]} %>% 
                     {.[str_detect(SERIE, '_IMPUTADA$'), `:=`(SERIE =  str_remove(SERIE, '_IMPUTADA$'), IMPUTADA = 1L)]} %>% 
                     {.[is.na(IMPUTADA), IMPUTADA := 0L]} %>% 
                       data.table::setnames(grep('_FLAG_OUTLIER$', names(.), value = T), 'FLAG_OUTLIER') %>% 
                       data.table::setcolorder(c('DATA','SERIE','FLAG_OUTLIER','IMPUTADA','VALOR')) %>% {.[]}
                   },
                   .progress = progress
                 )
             )
             
           }
           
         })
  
  future::plan(future::sequential)
  
  x
  
}
