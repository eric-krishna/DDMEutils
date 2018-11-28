#' Inspecao de sazonalidade para series temporais
#' 
#' @param x Um objeto ts ou data.frame
#' @param \code{tendencia} \code{TRUE/FALSE} indicando se o metodo deve ser aplicado considerando series com tendencia.
#' @param \code{sentido} Quando x eh data.frame, \code{sentido = 1} quando as series temporais estao por linha; e \code{sentido = 2} caso estejam por colunas.
#' @param \code{periodo} \code{'mes','semana','dia','ano','trimestre','quadrimestre','semestre','qualquer'}.
#' @param \code{...} Outros argumentos para quando \code{x} for um data.frame (\code{paralelo, idcol, dtcol}). Veja a seccao de detalhes.
#' 
#' 
#'
#' @details 
#' Quando \code{sentido = 1}, \code{idcol} especifica o indice da coluna que possui os identificadores das series. 
#' Quando \code{sentido = 2}, \code{dtcol} especifica o indice da coluna de datas.
#' \code{paralelo} TRUE/FALSE indicando se o processo deve ser feito em paralelo. Quando \code{TRUE}, eh feito por meio dos pacotes \code{future} e \code{furrr}.
#' 
#' @export
insp_sazo <- function(x, ...) UseMethod("insp_sazo", x)


#' @method insp_sazo ts
#' @export
#' 
insp_sazo.ts <- function(x, tendencia = TRUE) {
  
  e_sazonal <- forecast::tbats(x, 
                               use.trend = tendencia,
                               use.box.cox = FALSE,
                               use.damped.trend = FALSE,
                               use.arma.errors = FALSE,
                               use.parallel = FALSE)$seasonal.periods
  
  ifelse(is.null(e_sazonal), 0, e_sazonal)
  
}


#' @method insp_sazo data.frame
#' @export
#' 
insp_sazo.data.frame <- function(x, tendencia = TRUE, sentido = 1L, paralelo = FALSE, 
                                 periodo = c('mes','semana','dia','ano','trimestre',
                                             'quadrimestre','semestre','qualquer'),
                                 idcol = if(sentido == 1L) 1L else NULL, 
                                 dtcol = if(sentido == 2L) 1L else NULL, ...) {
  
  if (! sentido %in% 1L:2L) 
    stop('Sentido deve ser 1 (linha) ou 2 (coluna)')
  
  if (!is.null(idcol) && !idcol %in% c(0L,seq_along(x))) 
    stop('`idcol` deve indicar uma coluna pertencente aos dados')
  
  if( !is.null(dtcol) && !dtcol %in% c(0L,seq_along(x))) 
    stop('`dtcol` deve indicar uma coluna pertencente aos dados')
  
  
  if (!is.data.table(x)) 
    setDT(x)
  
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
                                          SAZONALIDADE = x[.x, ] %>% ts(frequency = periodo) %>% insp_sazo.ts(tendencia = tendencia),
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
                                          SAZONALIDADE = x[[.x]] %>% ts(frequency = periodo) %>% insp_sazo.ts(tendencia = tendencia),
                                          stringsAsFactors = FALSE
                                        )
                                      }, 
                                      .progress = progress) %>% 
             data.table::setDT()
           
         })
  
  future::plan(future::sequential)
  
  x
  
}

