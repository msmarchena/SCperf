#' @title Supply Chain Performance
#' @description
#' Computes the bullwhip effect for an stationary ARMA(p,q) demand process.
#'
#' @param phi     A vector of autoregressive parameters
#' @param theta   A vector of moving-average parameters
#' @param L       A positive lead-time
#' @param SL      Service level, (default:0.95)
#'
#' @return   Analytical measure for the bullwhip effect
#' 
#' @details 
#' The \code{SCperf} function has been deprecated and will be made defunct; use 
#' the bullwhipgame package.
#'
#' @references
#' - Zhang, X. (2004b). Evolution of ARMA demand in supply
#'   chains. Manufacturing and Services Operations Management, 6 (2), 195-198.
#'
#' - Silva Marchena, M. (2010) Measuring and implementing the bullwhip effect under
#'   a generalized demand process. \url{ http://arxiv.org/abs/1009.3977}
#'
#' @examples
#' 
#' \dontrun{
#' 
#' #ARMA(1,1) case
#'
#' SCperf(phi=0.95,theta=0.1,L=2,SL=0.99)
#'
#' #AR(2) case
#'
#' SCperf(phi=c(0.8,-0.2),theta=0,L=1)
#' }
#'
#' @name SCperf-deprecated
#' @usage SCperf(phi, theta, L, SL)
#' @seealso \code{\link{pkgSCperf-deprecated}}
#' @keywords internal
NULL


#' @rdname pkgSCperf-deprecated
#' 
#' @details 
#' The \code{SCperf} function has been deprecated and will be made defunct; use 
#' the bullwhipgame package.
#' 
#' @export
#'
SCperf <-
  function(phi,theta,L=L,SL=0.95)
  {  .Deprecated("bullwhipgame", package="pkgSCperf")
    if (L==0){cat("L is at least the review period which is one, ...\n")
    }
    else
    {
      #Calculating the variance of the demand using ARMAtoMA
      
      arma<-ARMAtoMA(ar=phi, ma=theta, 1000);
      VarD<-sum(arma^2)+1;
      
      #Calculating the bullwhip effect
      
      values<-ARMAtoMA(ar=phi, ma=theta, L);
      
      total = 0      #Calculating de duble sum in the formula of BE
      for (i in 1:L)
      {    valsum<-  sum (values[i:L]);
      if( i==1)  {total <- valsum;}
      else  {total <- total + values[i-1] * valsum;}
      }
      be<-1+2*total/VarD;
      
      t=ifelse(phi==0,1,be)
      
      #Calculating the variance during the LT
      arma1<-ARMAtoMA(ar=phi, ma=theta,L);
      arma2<-c(1,arma1);
      
      totalLT <- 0
      for (i in 1:L)
      { valsumLT<-  (sum(arma2[1:i]))^2;
      totalLT<- totalLT + valsumLT;
      }
      
      VarLT<-totalLT;
      
      #Calculating de SS=z*sigma*L^0.5 and SSLT=z*sigmaLT
      z<- qnorm(p=SL, mean = 0, sd = 1)
      SS<-z*(VarD*L)^0.5
      SSLT<-z*(VarLT^0.5)
    }
    f<-c(M=t[1],VarD=VarD,VarDL=VarLT,SS=SS,SSL=SSLT,z=z)
    return(f)
  }