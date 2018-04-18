#' @title The newsboy model
#'
#' @description
#' Implements the newsboy (or newsvendor) model with normal demand.
#'
#' @param m     Mean demand during the period
#' @param sd    Standard deviation of demand during the period
#' @param c     Uhe unit cost
#' @param p     The selling price, where p > c
#' @param s     The salvage value (default:0), where s < c
#'
#' @details   When the demand is a random variable with normal distribution,
#' the optimal stocking quantity that minimize the  expected cost is:
#' \eqn{Q=m+z*sd}, where z is known as the safety factor and
#' \eqn{Q - m=z*sd} is known as the safety stock.
#''Note that the newsboy problem is not formulated in terms of per unit holding cost \eqn{h=c-s} and penalty cost \eqn{b=p-c}.
#'
#' @return  A list containing:
#' \item{Q}{Optimal order-up-to quantity}
#' \item{SS}{Safety stock}
#' \item{ExpC}{Expected cost}
#' \item{ExpP}{Expected profit}
#' \item{CV}{Coefficient of variation of the demand}
#' \item{FR}{Fill rate, the fraction of demand served from stock}
#' \item{z}{Safety factor}
#'
#' @references
#' - Porteus E. L. (2002) Foundations of Stochastic Inventory Theory,
#'   Stanford University Press, Stanford, CA.
#'
#' - Gallego G. (1995) Newsvendor Problem. IEOR 4000 Production Management.
#'
#' - Ayhan, Hayriye, Dai, Jim, Foley, R. D., Wu, Joe, (2004):
#'   Newsvendor Notes, ISyE 3232 Stochastic Manufacturing &
#'   Service Systems.
#'
#' @seealso EOQ, EPQ, WW, SS
#'
#' @examples
#'  \dontrun{
#' # Example Porteus #
#' # Suppose demand is normally distributed with mean 100 and standard
#' # deviation 30. If p = 4 and c = 1, then CR = 0.75 and Q=120.23.
#' # Note that the order is for 20.23  units (safety stock) more than the
#' # mean. Note also that ExpC(120.23) = 38.13 and ExpP(120.23)=261.87,
#' # with FR=0.96.}
#'
#' Newsboy(100,30,4,1)
#'
#' \dontrun{
#' # Example Gallego #
#' # Suppose demand is normal with mean 100 and standard deviation 20. The
#' # unit cost is 5, the holding and penalty cost are 1 and 3
#' # respectively. From the definition of the holding and penalty
#' # cost we find that p=4, then CR = 0.75 and Q = 113.49. Notice that the
#' # order is for 13.49 units (safety stock) more than the mean,
#' # ExpC(113.49) = 25.42 and ExpP(113.49) = 274.58, with fill rate of
#' # 97 percent.}
#'
#' Newsboy(100,20,4,1)
#'
#' @export
#'
Newsboy<-function(m,sd,p,c,s=0){ CR=(p-c)/(p-s);
CV<-sd/m;
z<- qnorm(p=CR, mean = 0, sd = 1);
q=m+z*sd;
G<-(p-s)*sd*dnorm(z)
P<-(p-c)*m-G
FR<-1-CV*(dnorm(z)-(1-pnorm(z))*z);
SS=z*sd;
options(digits=2)
f=c(Q=q,SS=SS,ExpC=G,ExpP=P,CV=CV,CR=CR,FR=FR,z=z)
return(f)
}
