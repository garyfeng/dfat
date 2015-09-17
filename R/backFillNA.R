
#' A function to back fill NAs in a vector with the last non-NA value
#' 
#' @param x A vector with some NAs
#' @return A vector with NAs back filled
#' @details see https://gist.github.com/garyfeng/27e7f8e406192a8cb33a
#' @note  
#'   \code{x<-c("A","B",NA,NA,"C",NA,NA,NA,NA,"D",NA,NA);}
#'   \code{backFillNA(x)}
#'   \code{[1] "A" "B" "B" "B" "C" "C" "C" "C" "C" "D" "D" "D" }
#' @export
#' 
backFillNA<- function (x) {
  # input checking
  if(!is.atomic(x)) stop("backFillNA only takes a atomic vector as input.")
  if(!is.null(dim(x))) stop("backFillNA cannot take a matrix or array or data.frame as input.")
  
  # return if there is no NA
  if(!any(is.na(x))) return(x)
  
  # now let's work
  nas<- which(is.na(x))
  # trick from http://stackoverflow.com/questions/24837401/find-consecutive-values-in-vector-in-r
  naList<-split(nas, cumsum(c(1, diff(nas) != 1)))

  # get the backfill values
  valueList<-lapply(naList, function(nalist) {
    prev<- nalist[1]-1
    #print(nalist)
    if (prev<=0) prev <- 1
    x[nalist]<-x[prev]
    return(x[nalist])
  })
  # now back fill
  x[unlist(naList)] <- unlist(valueList)
  return (x)
}
