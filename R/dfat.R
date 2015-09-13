
#########################
# To create an operator to extract a named member of a list that is in a list. 
# This may sound confusing, but 

#' An operator  to extract a named member of a list that is in a list and returns a vector or a list. 
#' 
#' The name 'dfat' comes from 'df' for data frames, and 'AT' for the operator , following the XPath convension. 
#' This overwrites the \code{slotOP} http://www.inside-r.org/r-doc/base/slotOp, which is only for S4 objects.
#' 
#' @param x A list of lists, where the sub-lists should have named members. 
#' @param key Either a string that is the name of the property to extract, or an integer index of the property to extract. 
#'   
#' @return A vector if all properties can be converted to the same atomic class; or a list in the case that 
#'   the values cannot be fit into a vector.
#' 
#' @details Imagine you have a data.frame where a variable/column is a list of lists,
#'   and the lists have named members, e.g.,
#'     df$itinary <- list(
#'         list(from="NYC", to="LA", price=1200, via="train"), 
#'         list(from="LA", to="SF", price="unknown"), 
#'     ...)
#'  You want to get the "from" value of itinary as a vector. You can do
#'     {df$itinaryAT"from\"} or {`AT`(df$itinary, "via")}
#'  It is slightly more than syntactic sugar for \code{sapply(x, function(m) {m[["from"]]})}. 
#'  We extended the idea in several ways:
#'    1). We define a in-fix operator `qqq` that does so in a way that is syntactically more natural 
#'    2). We added error handling, in the case of bad indecies, etc.
#'    
#' @examples 
#' library(daft)
#'   
#' df$itinaryAT"from"  
#' `AT`(df$itinary, "via")
#' 
#'   
#' @export
#' 
# %@@% is the permissive operator that returns a vector or, in the case of non-atomic properties, a list
`%@@%` <- function(x, key) {
  # makesure the input vars are valid
  force(x); force(key); 
  if(missing(key)) stop()
  if(!is.list(x)) stop("First parameter must be a list")
  
  sapply(x, function(c) {
    result <- tryCatch(
      # test the following code
      {`[[`(c, key);}
      , warning = function(war) {print(war)}
      , error = function(err) {print(err); result<-NA}
      #, finally = {stop("Error `qqq`: will reach this step no matter what.")}
    )
    if (is.null(result)) result<-NA
    result
  })
}

#' An operator  to extract a named member of a list that is in a list and returns a vector. 
#' 
#' The name 'dfat' comes from 'df' for data frames, and 'AT' for the operator , following the XPath convension. 
#' This overwrites the \code{slotOP} http://www.inside-r.org/r-doc/base/slotOp, which is only for S4 objects.
#' 
#' @param x A list of lists, where the sub-lists should have named members. 
#' @param key Either a string that is the name of the property to extract, or an integer index of the property to extract. 
#'   
#' @return A vector of the property, in the same class as the property, with unsuccessful conversions filled by NAs.
#' 
#' @details Imagine you have a data.frame where a variable/column is a list of lists,
#'   and the lists have named members, e.g.,
#'     df$itinary <- list(
#'         list(from="NYC", to="LA", price=1200, via="train"), 
#'         list(from="LA", to="SF", price="unknown"), 
#'     ...)
#'  You want to get the "from" value of itinary as a vector. You can do
#'     {df$itinaryAT"from\"} or {`AT`(df$itinary, "via")}
#'  It is slightly more than syntactic sugar for \code{sapply(x, function(m) {m[["from"]]})}. 
#'  We extended the idea in several ways:
#'    1). We define a in-fix operator `qqq` that does so in a way that is syntactically more natural 
#'    2). We added error handling, in the case of bad indecies, etc.
#'    
#' @examples 
#' library(daft)
#'   
#' df$itinaryAT"from"  
#' `AT`(df$itinary, "via")
#' 
#'   
#' @export
#' 
# @ and %@% are strict versions of @@ that only returns a vector of atomic values, replacing everything else with NA
`@` <- `%@%` <- function(x, key) {
  result <-x%@@%key
  if(is.list(result)) {
    sapply(result, function(x){
      if(is.atomic(x)) x else NA
    })
  } else {
    result
  }
}



