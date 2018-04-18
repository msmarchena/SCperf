#' @title Bullwhip effect
#' @description
#' \code{bullwhip} computes the increase of demand
#' variability for a simple two-stage supply chains
#' consisting of a single retailer and a single manufacturer using three
#' forcasting methods: Minimum Mean Square Error (MMSE), Simple Moving
#' Average (SMA) and Exponential Smoothing (ES) when the demand follows
#' a known stationary AR(1) stochastic process.
#' 
#' @param method   Character string specifing which method to use
#' @param phi      A vector of autoregressive parameters
#' @param L        A positive lead-time
#' @param p        Order to be used in the SMA method
#' @param alpha    Smoothing factor to be used in the ES method (0 < alpha < 1)
#'
#' @return The measure for the bullwhip effect
#' 
#' @details 
#' The \code{bullwhip} function has been deprecated and will be made defunct; use 
#' the bullwhipgame package.
#' 
#' @examples
#'
#' \dontrun{
#' 
#' bullwhip("SMA",0.9,2,4)
#'
#' bullwhip("ES",0.9,2,0,0.6)
#'
#' bullwhip("MMSE",0.9,2) 
#' }
#'
#' @name bullwhip-deprecated
#' @usage bullwhip(method, phi, L, p, alpha)
#' @seealso \code{\link{pkgSCperf-deprecated}}
#' @keywords internal
NULL


#' @rdname pkgSCperf-deprecated
#' @details 
#' The \code{bullwhip} function has been deprecated and will be made defunct; use 
#' the bullwhipgame package.
#'
#' @export
#'
bullwhip<-function(method=c("MMSE","SMA","ES"),phi,L,p,alpha)
{ .Deprecated("bullwhipgame", package="pkgSCperf")
  method <- match.arg(method)
  if (L==0){cat("L is at least one, the review period, ...\n")
  }
  else
  {
    if (method=="MMSE"){r<-1+2*phi*(1-phi^L)*(1-phi^(L+1))/(1-phi)
    }
    
    else{ if (method=="SMA"){r<-1+2*(1-phi^p)*((L/p)^2+(L/p))
    }
      
      else{ if (method=="ES"){r<-1+(L*alpha)*(2*(1-phi)/(1-(1-alpha)*phi))+(L*alpha)^2*(1-phi)/((1-alpha)*(1-(1-alpha)*phi))
      }
        
        else {r<-"error"
        }
        
      }
    }
  }
  options(digits=5)
  return(r)
}

