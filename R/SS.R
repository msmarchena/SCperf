#' @title Safety Stock
#'
#' @description
#' \code{SS} computes the safety stock level over lead-time
#'
#' @param SL      Service level, a value between 0 and 1
#' @param sd      Standart deviation of the demand
#' @param L       A positive lead-time
#'
#' @return   Safety stock level over lead-time
#'
#' @examples
#'  
#'  SS(0.95,0.7,2)
#'
#' @export
#'
SS=function(SL,sd, L=1)
{  if( any(SL <= 0 | SL > 1) ) stop('SL not between 0 and 1')
  z<- qnorm(SL, 0, 1)
  rst <- z*sd*sqrt(L)
  rs <-   round(rst,2)
  return(rs)
}


