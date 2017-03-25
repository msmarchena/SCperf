% This file is part of the 'SCperf' package for R
% It is distributed under the GPL version 2 or later
\name{EPQ}
\alias{EPQ}
\title{Economic Production Quantity model}
\description{Implements the Economic Production Quantity (EPQ) model. }

\usage{
EPQ(d, p, k, h)
}
\arguments{
  \item{d}{demand rate,}
  \item{p}{production rate,}
  \item{k}{ordering or setup cost,}
  \item{h}{holding cost.}
}
\details{The EPQ model is an extension of the \code{\link{EOQ}} model. It considers
  finite production rate, that is, the inventory is replenished
  gradually as the order is produced. Note that this assumption
  requires the production rate to be greater than the demand rate (p>d)
  otherwise there would be no inventory at any time.

The model considers that a new order is produced incrementally when the
inventory reaches zero. During the time that production run,
\eqn{t=Q/p}{t=Q/p}, inventory is accumulated at rate \eqn{p-d}{p-d}, which
implies that when the production of the batch Q is finished the
inventory will reach its maximum level I.

}
\value{\code{EPQ()} returns a list containing:
  \item{Q}{batch quantity,}
  \item{t}{time required to produce the batch quantity,}
 \item{T}{time between orders (cycle length or time),}
 \item{I}{maximum inventory level,}
 \item{TC}{total cost.}
}
\references{
 Gallego, G. "IEOR4000: Production Management" (Lecture 2), Columbia (2004).
}
\author{Marlene Silva Marchena \email{marchenamarlene@gmail.com}}
\seealso{\code{\link{EOQ}}, \code{\link{Newsboy}}, \code{\link{WW}}
}
\examples{
# Suppose k = 100, h = 5, d = 200, p = 1000. Then the production run at
# t=0.1, the optimal order interval is T = 0.5, the optimal order quantity
# is Q = 100, the maximum inventory level is I=80 and the total cost is
# TC = $400.

EPQ(d=200,p=1000,k=100,h=5)
}

\keyword{Lot sizing models }