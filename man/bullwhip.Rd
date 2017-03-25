\name{bullwhip}
\alias{bullwhip}
\title{bullwhip effect}
\description{\code{bullwhip} computes the increase of the demand
  variability (the bullwhip effect) for a simple two-stage supply chains
  consisting of a single retailer and a single manufacturer using three
  forcasting methods: Minimum Mean Square Error (MMSE), Simple Moving
  Average (SMA) and Exponential Smoothing (ES) when the demand follows
  a known stationary AR(1) stochastic process.
}
\usage{
bullwhip(method = c("MMSE", "SMA", "ES"), phi, L, p, alpha)
}
\arguments{
  \item{method}{character string specifing which method to use,}
  \item{phi}{a vector of autoregressive parameters,}
  \item{L}{a positive lead-time,}
  \item{p}{the order to be used in the SMA method,}
  \item{alpha}{smoothing factor to be used in the ES method (0 < alpha < 1).}
}
\details{
The bullwhip effect is commonly defined as the ratio between the
unconditional variance of the order process to that of the demand
process, \eqn{Var(O_t)/Var(d_t)}{Var(O_t)/Var(d_t)}. If this ratio is
equal to one there is no variance amplification, while a ratio greater than one means that the bullwhip effect is present. On the other hand,
a ratio less than one means that the orders are smoothed if compared with the demand.
}
\value{ The measure for the bullwhip effect.
}
\references{
  Chen, F.; Drezner, Z.; Ryan, J. ; Simchi-Levi,
  D. (2000a) Quantifying the bullwhip effect in a simple supply chain:
  the impact of forecasting, lead times and information.
  Management Science, v.46, n.3, p. 436-443.

  Silva Marchena, M. (2010) Measuring and implementing the bullwhip effect
  under a generalized demand process.  \url{ http://arxiv.org/abs/1009.397}
    
  Zhang, X. (2004a) The impact of forecasting methods on the bullwhip
  effect, International Journal of Production Economics.l, v.88, n.1,
  p. 15-27. 

}
\author{Marlene Silva Marchena \email{marchenamarlene@gmail.com}

}
\seealso{\code{\link{SCperf}}
}
\examples{
bullwhip("SMA",0.9,2,4)

bullwhip("ES",0.9,2,0,0.6)

bullwhip("MMSE",0.9,2)
}
\keyword{Supply Chain Management}
\keyword{Beer Game}
