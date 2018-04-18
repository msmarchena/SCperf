#' @title Economic Order Quantity model
#'
#' @description
#' This function finds the optimal order policy in the classical Economic Order Quantity (EOQ) model and the EOQ model with planned shortages.
#'
#' @param d      Demand per unit time.
#' @param k      Ordering or fixed cost per order.
#' @param h      Holding cost per unit of product.
#' @param b      Shortage penalty cost per unit (default:0)
#'
#' @details   The EOQ model, also called Lot-Sizing model, considers that demand is uniform
#' and deterministic. Lead time, the time between the placement of an order and its receipt, is equal to zero.
#'
#' The optimal order policy in the classical EOQ model minimizes the total cost associated with the ordering
#' and holding costs while meeting all demand (without shortage). When shortages are allowed (\eqn{b>0})
#' we have the EOQ model with backorders or planned shortages.
#'
#' @return  A list containing:
#' \item{T}{Time between orders (cycle length)}
#' \item{S}{Maximum backorders in units. Displayed when b > 0}
#' \item{TVC}{Total variable cost.}
#'
#' @references
#' - Hillier, F. and Lieberman, G. (2001). \emph{Introduction to operational research}.
#'   New York: McGraw-Hill, 7th.
#'
#' @seealso EPQ, newsboy, WW
#'
#' @examples
#'  \dontrun{
#' #Classical EOQ model
#' #Given demand d=8000 items per year; set up cost k=12000; and holding cost h=0.3
#' #per unit we find that the optimal solution is to order 25298 units every 3.2
#' #months with a total variable cost of $7589.5}
#'  
#' EOQ(8000,12000,0.3)
#' 
#' \dontrun{
#' #EOQ model with planned shortages
#' #Consider now that backorders are allowed with a backorder cost b=1.1 per
#' #unit and year. Then the optimal solution is to order 28540 units every 3.6 months.
#' #The total variable cost is $6727.3 and the maximum shortage is 6116 units.}
#'    
#' EOQ(8000,12000,0.3,1.1)
#' 
#'
#' @export
#'
EOQ=function(d,k,h,b=0){if(b == 0){Q<-(2*d*k/h)^0.5;
T<- Q/d;
TVC<-(2*d*k*h)^0.5;
f<-c(Q=Q,T=T,TVC=TVC)
}
  
  else{ Q<-(2*d*k*(h+b)/(h*b))^0.5;
  T<- Q/d;
  s<-Q*(h/(h+b));
  TVC<-(2*k*d*h*b/(h+b))^0.5;
  f<-c(Q=Q,T=T,S=s,TVC=TVC)
  }
  options(digits=2,scipen=3)
  return(f)
}


