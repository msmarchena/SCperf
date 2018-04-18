#' @title Safety stock over lead-time
#'
#' @description
#' \code{SSL} computes the safety stock level over lead-time for three forecasting
#' methods: Minimum Mean Square Error (MMSE), Simple Moving Average (SMA) and
#' Exponential Smoothing (ES) when the demand follows a stationary AR(1) stochastic process.
#'
#' @param method   Character string specifing which method to use
#' @param phi      A vector of autoregressive parameters
#' @param L        A positive lead-time
#' @param p        Order to be used in the SMA method
#' @param alpha    Smoothing factor to be used in the ES method (0 < alpha < 1)
#' @param SL       Service level
#'
#' @return   Safety stock level over lead-time
#'
#' @details 
#' The \code{SSL} function has been deprecated and will be made defunct; use 
#' the bullwhipgame package.
#' 
#' @examples
#' 
#' \dontrun{
#' 
#' SSL("MMSE",0.15,2,4,0.7,0.95)
#'
#' SSL("SMA",0.15,2,4,0.7,0.95)
#'
#' SSL("ES",0.15,2,4,0.7,0.95) 
#' }
#' 
#' @name SSL-deprecated
#' @usage SSL(method, phi, L, p, alpha, SL)
#' @seealso \code{\link{pkgSCperf-deprecated}}
#' @keywords internal
NULL

#' @rdname pkgSCperf-deprecated
#' @details 
#' The \code{SSL} function has been deprecated and will be made defunct; use 
#' the bullwhipgame package.
#'
#' @export
#'
SSL=function(method=c("MMSE","SMA","ES"),phi,L,p,alpha,SL)
{ .Deprecated("bullwhipgame", package = "pkgSCperf")
  z<- qnorm(SL, mean = 0, sd = 1)
  if (method=="MMSE"){ #Calculating the variance during the LT
    arma1<-ARMAtoMA(ar=phi, ma=0,L);
    arma2<-c(1,arma1);
    
    totalLT <- 0
    for (i in 1:L)
    { valsumLT<-  (sum(arma2[1:i]))^2;
    totalLT<- totalLT + valsumLT;
    }
    
    VarLT<-totalLT;
    
    return(z*(VarLT^0.5))
  }
  
  else{ if (method=="SMA"){var_SMA<-(1/(1-phi^2))*(L*(1+phi)/(1-phi)-2*phi*(1-phi^L)/(1-phi)^2+((L/p)^2)*(p*(1+phi)/(1-phi)-2*phi*(1-phi^p)/(1-phi)^2)-2*(L/p)*(phi*(1-phi^L)*(1-phi^p)/(1-phi)^2))
  return(z*(var_SMA^0.5))
  }
    
    else{ if (method=="ES"){var_ES<-(1/(1-phi^2))*(L*(1+phi)/(1-phi)-2*phi*(1-phi^L)/(1-phi)^2+((alpha*L)^2)*(1+(1-alpha)*phi)/(alpha*(2-alpha)*(1-(1-alpha)*phi))-2*alpha*L*(phi-phi^(L+1))/((1-phi)*(1-(1-alpha)*phi)))
    
    return(z*(var_ES^0.5))
    }
      
      else {return("error")
      }
      
    }
  }
}
