\encoding{UTF-8}
\name{WW}
\alias{WW}
\alias{WW.default}
\alias{print.WW}
\title{The Wagner-Whitin algorithm}
\description{ \code{WW} implements the Wagner-Whitin algorithm. Considering time-varying demand, the algorithm builds production plans that minimizes the total setup and holding costs in a finite horizon of time, assuming zero starting inventory and no backlogging.}
\usage{
WW(x,a,h,method=c("forward","backward"))
\method{WW}{default}(x,a,h,method=c("forward","backward"))
\method{print}{WW}(x, ...)
}
\arguments{ 
  \item{x}{a numeric vector containing the demand per unit time,}
  \item{a}{a numeric vector containing the set-up cost per unit time,}
 \item{h}{a numeric vector containing the holding cost per unit time,}
 \item{method}{character string specifing which algorithm to use, must
   be "forward" (default) or "backward".}
 \item{\dots}{ not used. }

}
\value{WW.default (the function that is called when using WW) returns
a list containing: 
\item{TVC}{total variable cost,}
\item{Jt }{last period of production for the forward algorithm or the
  (end of the) period when the inventory reaches a zero level for the
  backward algorithm,}
\item{Solution}{matrix of solutions.}
}
\references{ \enc{Axsäter}{Axsater}, S. (2006) Inventory Control. Lund
  Sweden: Springer, Second Edition.

  Hillier, F. and Lieberman, G. (2001). \emph{Introduction to
    operational research}. New York: McGraw-Hill, Seventh Edition.
}
\author{Marlene Silva Marchena \email{marchenamarlene@gmail.com}
}
\seealso{\code{\link{EOQ}}, \code{\link{EPQ}}, \code{\link{Newsboy}}
}
\examples{
# Example from Hiller, p.952, reproduced bellow: 
# An airplane manufacturer specializes in producing small airplanes. It has just received an
# order from a major corporation for 10 customized executive jet airplanes for the use
# of the corporation's upper management. The order calls for three of the airplanes to be
# delivered (and paid for) during the upcoming winter months (period 1), two more to be delivered
# during the spring (period 2), three more during the summer (period 3),and
# the final two during the fall (period 4). Setting up the production facilities to meet
# the corporation's specifications for these airplanes requires a setup cost of $2 million.
# The manufacturer has the capacity to produce all 10 airplanes within a
#  couple of months, when the winter season will be under way. However, this would necessitate holding
# seven of the airplanes in inventory, at a cost of $200,000 per airplane per period, until their
# scheduled delivery times (...)
# Management would like to determine theleast costly production schedule for filling this order.

x  <- c(3,2,3,2)
a  <- c(2,2,2,2)
h  <- c(0.2,0.2,0.2,0.2)
WW(x,a,h,method="forward") # forward algorithm
WW(x,a,h,method="backward") # backward algorithm

# The optimal production schedules for the forward and backward case is to:
# 1. Produce in period 1 for periods 1, 2, 3 and 4 (3 + 2 + 3 + 2 = 10  airplanes)
# Total variable cost $4.8 million.
# 2. Produce in period 1 for periods 1 and 2 (3 + 2 = 5  airplanes) and in period 3
# for periods 3 and 4 (3 + 2 = 5 airplanes)
# Total variable cost $4.8 million.
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Dynamic Lot sizing models }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
