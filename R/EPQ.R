#' @title Economic Production Quantity model
#'
#' @description
#' Implements the Economic Production Quantity (EPQ) model.
#'
#' @param d      Deterministic demant per time unit
#' @param p      Production rate
#' @param k      Ordering or fixed cost per order.
#' @param h      Holding cost per unit of product.
#' @param b      Shortage penalty cost per unit (default:0).
#'
#' @details   The EPQ model is an extension of the \code{\link{EOQ}} model. It considers
#' finite production rate, that is, the inventory is replenished
#' gradually as the order is produced. Note that this assumption
#' requires the production rate to be greater than the demand rate (p>d)
#' otherwise there would be no inventory at any time.
#'
#' The model considers that a new order is produced incrementally when the
#' inventory reaches zero. During the time that production run,
#' \eqn{t=Q/p}{t=Q/p}, inventory is accumulated at rate \eqn{p-d}{p-d}, which
#' implies that when the production of the batch Q is finished the
#' inventory will reach its maximum level I.
#'
#' @return  \code{EPQ()} returns a list containing:
#' \item{Q}{Order quantity}
#' \item{t}{Time required to produce the batch quantity}
#' \item{T}{Time between orders (cycle length or time)}
#' \item{I}{Maximum inventory level}
#' \item{TC}{Total cost}
#'
#' @references
#' - Gallego, G. "IEOR4000: Production Management" (Lecture 2), Columbia (2004).
#'
#' @seealso EOQ, newsboy, WW
#'
#' @keywords Lot sizing models
#'
#' @examples
#'  \dontrun{
#' #Suppose k = 100, h = 5, d = 200, p = 1000. Then the production run at
#' #t=0.1, the optimal order interval is T = 0.5, the optimal order quantity
#' #is Q = 100, the maximum inventory level is I=80 and the total cost is
#' #TC = $400.}
#'  
#' EPQ(d=200,p=1000,k=100,h=5)
#'
#' @export
#'
EPQ=function(d,p,k,h,b=0){nn<-2*d*k;
dd<-h*(1-d/p) ;
if(b == 0){
  Q<-(nn/dd)^0.5;
  TC<-k*(d/Q)+h*(Q/2)*(1-d/p)
}else{
  Q<-(nn*(h+b)/nn*b)^0.5;
  TC<-k*(d/Q)+Q*h*(1-d/p)
}
t<-Q/p
T<-Q/d;
I<-(1-d/p)*Q

f<-c(q=Q,t=t,T=T,I=I,TC=TC)
options(digits=6)
return(f)
}


