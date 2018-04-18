#' @title Reorder Point
#'
#' @description
#' \code{ROP} computes the reorder point level of inventory. When inventory falls to this amount, a new order must be made.
#'
#' @param SL      Service level, a value between 0 and 1
#' @param md      Mean demand
#' @param sd      Standart deviation of the demand
#' @param L       A positive lead-time
#'
#' @return   Reorder point level
#'
#' @seealso  \code{\link{SS}}
#'
#' @examples
#'
#'  ROP(0.9,2500,500,6)
#'
#' @export
#'
ROP=function(SL,md,sd, L=1)
{  if( any(SL <= 0 | SL > 1) ) stop('SL not between 0 and 1')
  if( L <= 0 ) stop('L must be positive')
  z<- qnorm(SL, 0, 1)
  ss <- z*sd*sqrt(L)
  rst <- L*md+ss
  rst <- round(rst,2)
  return(rst)
}


