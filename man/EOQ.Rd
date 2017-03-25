% This file is part of the 'SCperf' package for R
% It is distributed under the GPL version 2 or later
%\encoding{latin1}
\name{EOQ}
\alias{EOQ}

\title{Economic Order Quantity model
}
\description{Implements the classical Economic Order Quantity (EOQ) model and the EOQ model with planned shortages.
}
\usage{
EOQ(d, k, h, b = 0)
}

\arguments{
  \item{d}{demand per unit time,}
  \item{k}{ordering or setup cost per unit time,}
  \item{h}{holding cost per unit time,}
  \item{b}{shortage penalty cost per unit time (default:0).}
}
\details{The classical EOQ model assumes that the demand rate of units
  per time unit is constant and each new order is delivered in full when
  inventory reaches zero. Also a cost for each unit held in storage and
  a fixed cost for each order placed are considered. No shortages are allowed. 

  The optimal (the economic order) quantity that minimize the total cost
  associated with the purchase, delivery and storage is defined by \eqn{Q= \sqrt{2Dk/h}}{Q=\sqrt(2Dk/h)}. When we relax the last assumption, that is,  \eqn{b \neq 0}{b  is not equal to 0} we have the EOQ model with backorders.

}
\value{\code{EOQ()} returns a list containing:
  \item{Q}{batch quantity,}
 \item{T}{time between orders (cycle length or time),}
 \item{S}{maximum backorder in units,}
 \item{TVC}{total variable cost.}
}

\references{Hillier, F. and Lieberman, G. (2001). \emph{Introduction to operational research}. New York: McGraw-Hill, 7th.
}
\author{Marlene Silva Marchena \email{marchenamarlene@gmail.com}}

\seealso{\code{\link{EPQ}}, \code{\link{Newsboy}}, \code{\link{WW}}
}
\examples{
# Classical EOQ model#
# Given demand d=8000 items per year; set up cost k=12000; and holding cost h=0.3
# per unit we find that the optimal solution is to order 25298 units every 3.2
# months with a total variable cost of $7589.5

EOQ(8000,12000,0.3)

# EOQ model with planned shortages# 
# Consider now that backorders are allowed with a backorder cost b=1.1 per
# unit and year. Then the optimal solution is to order 28540 units every 3.6 months.
# The total variable cost is $6727.3 and the maximum shortage is 6116 units. 

EOQ(8000,12000,0.3,1.1)
} 

\keyword{Lot sizing models }


