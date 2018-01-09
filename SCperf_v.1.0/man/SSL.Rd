\name{SSL}
\alias{SSL}
\title{Safety stock over lead-time}
\description{\code{SSL} computes the safety stock level over lead-time for three
  forecasting methods: Minimum Mean Square Error (MMSE), Simple Moving
  Average (SMA) and Exponential Smoothing (ES) when the demand follows
  a stationary AR(1) stochastic process.
}
\usage{
SSL(method = c("MMSE", "SMA", "ES"), phi, L, p, alpha, SL)
}
\arguments{
  \item{method}{character string specifing which forecasting method to use,}
 \item{phi}{a vector of autoregressive parameters,}
\item{L}{a positive lead-time,}
\item{p}{the order to be used in the SMA method,}
\item{alpha}{smoothing factor to be used in the ES method (0 < alpha < 1),}
\item{SL}{service level.}
}
\details{\code{SSL} is
calculated using an estimate of the standard deviation of forecasting
error for lead-time demand
\eqn{\sqrt{Var(D_t^L-\hat{D}_t^L)}}{\sqrt{Var(D_t^L-\hat{D}_t^L)}} where
\eqn{\hat{D}^L_t=\sum_{\tau=1}^{L}\hat{d}_{t+\tau}}{\hat{D}_t^L} is an
estimate of the mean demand over L periods after period t.
}
\value{The safety stock level over the lead-time.}
\references{Silva Marchena, M. (2010) Measuring and implementing the bullwhip effect under a generalized demand process.  \url{ http://arxiv.org/abs/1009.3977}
    
  Zhang, X. The impact of forecasting methods on the bullwhip
  effect, International Journal of Production Economics.l, v.88, n.1,
  p. 15-27, 2004a.

}
\author{Marlene Silva Marchena \email{marchenamarlene@gmail.com}
}

\seealso{\code{\link{SCperf}}
}
\examples{
SSL("MMSE",0.15,2,4,0.7,0.95)

SSL("SMA",0.15,2,4,0.7,0.95)

SSL("ES",0.15,2,4,0.7,0.95)
}

