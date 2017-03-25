\name{SCperf}
\alias{SCperf}
\title{ Supply Chain Perform }
\description{\code{SCperf} computes the bullwhip effect for an stationary ARMA(p,q) demand process and other supply chain performance variables. 
}
\usage{SCperf(phi, theta, L = L, SL = 0.95)
}
\arguments{
  \item{phi}{a vector of autoregressive parameters,}
  \item{theta}{a vector of moving-average parameters,}
  \item{L}{positive lead-time,}
  \item{SL}{service level, (default:0.95).}
}
\details{
 The bullwhip effect for a stationary ARMA(p,q) demand process
  is defined as:
\deqn{M=\frac{1+2\sum_{i=0}^L\sum_{j=i+1}^L\psi_i\psi_j}{\sum_{j=0}^\infty\psi_j^2}}{M} where the \eqn{\psi}-weights solve the equations
\eqn{\psi(z)\theta(z)=\phi(z)}{\psi(z)\theta(z)=\phi(z)}. If
  \eqn{M=1}{M=1} there is no variance amplification, while
  \eqn{M>1}{M>1}  means that the bullwhip effect is present. On the
  other hand,  \eqn{M<1}{M<1} means that the orders are smoothed if
  compared with the demand.
  
Two safety stock measures are presented as well: \eqn{SS=z\sqrt{L*VarD}}{SS} and
 \eqn{SSL=z\sqrt{VarDL}}{SSL=z\sqrt{VarDL}}. SSL is calculated using
 an estimate of the standard deviation of L periods forecast error  
 \eqn{\sqrt{VarDL}=\sqrt{Var(D_t^L-\hat{D}_t^L)}}{\sqrt{VarDL}} where \eqn{\hat{D}^L_t=\sum_{\tau=1}^{L}\hat{d}_{t+\tau}}{\hat{D}_t^L} is an
 estimate of the mean demand over L periods after period t. 
 
}
\value{\code{SCperf()} returns a list containing:
 \item{M}{measure for the bullwhip effect,}
 \item{VarD}{variance of the demand,}
 \item{VarDL}{variance of forecasting error for lead-time demand,}
 \item{SS}{safety stock calculated using the standard deviation of the demand,}
 \item{SSL}{safety stock calculated using the standard deviation of L periods forecast error,}   
 \item{z}{safety factor.}
}
\references{Zhang, X. (2004b). Evolution of ARMA demand in supply
  chains. Manufacturing and Services Operations Management, 6 (2), 195-198.

  Silva Marchena, M. (2010) Measuring and implementing the bullwhip effect under a generalized demand process. \url{ http://arxiv.org/abs/1009.3977}
}
\author{Marlene Silva Marchena \email{marchenamarlene@gmail.com} }

\seealso{ \code{\link{bullwhip}}
}
\examples{

#ARMA(1,1) case,

SCperf(phi=0.95,theta=0.1,L=2,SL=0.99)

#AR(2) case,

SCperf(phi=c(0.8,-0.2),theta=0,L=1)
}
\keyword{Supply Chain Management}
\keyword{Beer Game}
