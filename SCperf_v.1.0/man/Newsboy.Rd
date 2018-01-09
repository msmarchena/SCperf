\name{Newsboy}
\alias{Newsboy}
\title{ The Newsboy model}
\description{Implements the Newsboy (or Newsvendor) model with normal demand.}
\usage{Newsboy(m, sd, p, c, s = 0)}
\arguments{
  \item{m}{ mean demand during the period,}
  \item{sd}{standard deviation of demand during the period,}
  \item{c}{the unit cost,}
\item{p}{the selling price, where p > c}
  \item{s}{the salvage value (default:0), where s < c.}
}
\details{When the demand is a random variable with normal distribution,
 the optimal stocking quantity that minimize the  expected cost is:
 \eqn{Q=m+z*sd}{Q=m+z*sd}, where \eqn{z=\Phi^{-1}(p-c)/(p-s)}{z}, the inverse of the standard normal cumulative density function of the demand, is known as the safety factor and \eqn{Q - m=z*sd}{Q - m=z*sd} is known as the safety stock.

Note that the Newsboy problem is not formulated in terms of per unit holding cost \eqn{h=c-s}{h=c-s} and penalty cost \eqn{b=p-c}{b=p-c}.
} 

\value{\code{Newsboy()} returns a list containing:
\item{Q}{optimal order-up-to quantity,}
\item{SS}{safety stock,}
\item{ExpC}{expected cost,}
 \item{ExpP}{expected profit,}
 \item{CV}{coefficient of variation of the demand,}
 \item{CR}{\eqn{=(p-c)/(p-s)=b/(b+h)}{(p-c)/(p-s)=b/(b+h)}, critical ratio,}
 \item{FR}{fill rate, the fraction of demand served from stock,}
 \item{z}{safety factor.}
}
\references{Porteus E. L. (2002) Foundations of Stochastic Inventory Theory,
  Stanford University Press, Stanford, CA.

  Gallego G. (1995) Newsvendor Problem. IEOR 4000 Production Management.
 
  Ayhan, Hayriye, Dai, Jim, Foley, R. D., Wu, Joe, (2004): Newsvendor Notes, ISyE 3232 Stochastic Manufacturing & Service Systems.
}
\author{Marlene Silva Marchena \email{marchenamarlene@gmail.com}
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{\code{\link{EOQ}}, \code{\link{EPQ}}, \code{\link{WW}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# Example Porteus #
# Suppose demand is normally distributed with mean 100 and standard
# deviation 30. If p = 4 and c = 1, then CR = 0.75 and Q=120.23.
# Note that the order is for 20.23  units (safety stock) more than the
# mean. Note also that ExpC(120.23) = 38.13 and ExpP(120.23)=261.87,
# with FR=0.96.

Newsboy(100,30,4,1) 

# Example Gallego #
# Suppose demand is normal with mean 100 and standard deviation 20. The
# unit cost is 5, the holding and penalty cost are 1 and 3
# respectively. From the definition of the holding and penalty
# cost we find that p=4, then CR = 0.75 and Q = 113.49. Notice that the
# order is for 13.49 units (safety stock) more than the mean,
# ExpC(113.49) = 25.42 and ExpP(113.49) = 274.58, with fill rate of
# 97 percent.

Newsboy(100,20,4,1)}

\keyword{Inventory models}
\keyword{Newsvendor model}
