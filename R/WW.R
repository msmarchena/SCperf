#' @title The Wagner-Whitin algorithm
#'
#' @description  \code{WW} implements the Wagner-Whitin algorithm. Considering time-varying demand, the algorithm builds production
#' plans that minimizes the total setup and holding costs in a finite horizon of time, assuming zero starting inventory
#' and no backlogging
#'
#' @param x  A numeric vector containing the demand per unit time
#' @param a  A numeric number for the set-up cost per unit and period
#' @param h  A numeric number for the holding cost per unit and period
#' @param method  Character string specifing which algorithm to use: "backward" (default) or "forward"
#'
#' @seealso EOQ, EPQ, newsboy
#'
#' @examples
#'  \dontrun{
#' # Example from Hiller, p.952, reproduced bellow:
#' # An airplane manufacturer specializes in producing small airplanes. It has just received
#' # an order from a major corporation for 10 customized executive jet airplanes for the use of
#' # the corporation's upper management. The order calls for three of the airplanes to be delivered
#' # (and paid for) during the upcoming winter months (period 1), two more to be delivered during
#' # the spring (period 2), three more during the summer (period 3), and the final two during the fall
#' # (period 4). Setting up the production facilities to meet the corporation's specifications for
#' # these airplanes requires a setup cost of $2 million.
#' # The manufacturer has the capacity to produce all 10 airplanes within a couple of months, when the
#' # winter season will be under way. However, this would necessitate holding seven of the airplanes in
#' # inventory, at a cost of $200,000 per airplane per period, until their scheduled delivery times
#' # (...) Management would like to determine theleast costly production schedule for filling
#' # this order.}
#' 
#'
#' x  <- c(3,2,3,2)
#' a  <- 2
#' h  <- 0.2
#' WW(x,a,h,method="backward")
#' 
#'  \dontrun{
#' # The total variable cost is $4.8 million (minimum value in the first raw). Since we have two
#' # minimun values in the first raw (positions 2 and 4), we have the following solutions:
#' # Solution 1:  Produce to cover demand until period 2, 5 airplanes. In period 3, new decision,
#' # minimun value 2.4 in period 4 (third raw). Then in period 3 produce to cover demand until 
#' # period 4, 5 airplanes.
#' # Solution 2: Produce to cover demand until period 4, 10 airplanes.}
#'  
#' WW(x,a,h,method="forward")
#'
#'  \dontrun{
#' #The total variable cost is $4.8 million (minimum value in the last raw). Since we have two minimun
#' # values in columns 1 and 3, the solutions are:
#' # Solution 1: Produce in period 1 to cover demand until period 4, 10 airplanes.
#' # Solution 2: Produce in period 3 to cover demand until period 4, 5 airplanes.In period 2, new
#' # decision, minimun value 2.4 in raw 3. Then in period 1 produce to cover demand until
#' # period 2, 5 airplanes.}
#'
#' @export
WW <- function(x,a,h,method=c("backward", "forward")) {
  UseMethod("WW")
}
#' @export
WW.default <-function(x,a,h,method=c("backward", "forward"))
{ method <- match.arg(method)
n  <- length(x) #Calculating the output matrix (costs matrix)
a <- rep(a,n)
h <- rep(h,n)
CM<- matrix(NA,nrow=n,ncol=n)
if (method=="backward"){V<- matrix(NA,nrow=1,ncol=n)
hc<-matrix(NA,nrow=n,ncol=n)

for(i in (n-1):1){ CM[n,n]<- a[n]
V[n]<- CM[n,n]
V[n+1]<- 0
for(j in i:n){ if(j==i){  CM[i,j]<- V[j+1]+a[i]
}
  else { total = 0;
  for (k in i:(j-1)){hc<-sum(h[i:k])*x[k+1]
  total <- total+hc
  CM[i,j]<- V[j+1]+a[i]+total
  }
  }
}
V[i]<-min(CM[i,],na.rm=TRUE)
}
v<-V[-(n+1)];
TC<-V[1];
}
else {
CM<- matrix(NA,nrow=n,ncol=n)
f<- matrix(NA,nrow=1,ncol=n+1)
hc<-matrix(NA,nrow=n,ncol=n)

for(t in 2:n){  CM[1,1]<- a[1]
f[1]<- 0
f[2]<- CM[1,1]
for(k in 1:t){ if(k==t){  CM[t,k]<- f[k]+a[k]
}
  else { total = 0;
  for (r in k:(t-1)){hc<-sum(h[k:r])*x[r+1];
  total <- total+hc;
  CM[t,k]<- f[k]+a[k]+total
  }
  
  }
  
}
f[t+1]<-min(CM[t,],na.rm=TRUE)

}
v=f[-1];
TC<- f[n+1];
}
s <- apply(CM, 1, function(y) which(y == min(y, na.rm = TRUE)));
Cuts<-sapply(s, paste, collapse = ' or ');

ww<-list(TVC=TC, Jt = Cuts, Solution=CM, call=sys.call())
class(ww)<-"WW"
ww
}
#' @export
print.WW <- function(x, ...)
{
  cat("Call:\n")
  print(x$call)
  cat("\nTVC:\n")
  print(x$TVC)
  cat("\nSolution:\n")
  print(x$Solution)
  cat("\nJt:\n")
  print(x$Jt)
}