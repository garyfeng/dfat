% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/dfat.R
\name{\%@@\%}
\alias{\%@@\%}
\title{An operator  to extract a named member of a list that is in a list and returns a vector or a list.}
\usage{
x \%@@\% key
}
\arguments{
\item{x}{A list of lists, where the sub-lists should have named members.}

\item{key}{Either a string that is the name of the attribute to extract, or an integer index of the attribute to extract.}
}
\value{
A vector if all attributes can be converted to the same atomic class; or a list in the case that
  the values cannot be fit into a vector.
}
\description{
The name 'dfat' comes from 'df' for data frames, and '%AT%' for the operator , following the XPath convension.
This overwrites the \code{slotOP} http://www.inside-r.org/r-doc/base/slotOp, which is only for S4 objects.
}
\details{
Imagine you have a data.frame where a variable/column is a list of lists,
  and the lists have named members, e.g.,

  \code{
    df$itinary <- list(
        list(from="NYC", to="LA", price=1200, via="train"),
        list(from="LA", to="SF", price="unknown"),
    ...)
  }

 You want to get the "from" value of itinary as a vector. You can do

    \code{
      df$itinary %AT% "from"
    } or \code{
      %AT%(df$itinary, "via")
   }

 It is slightly more than syntactic sugar for

 \code{
   sapply(x, function(m) {
     m[["from"]]
   })
 }


 We extended the idea in several ways:

   1). We define a in-fix operator %AT% that does so in a way that is syntactically more natural

   2). We added error handling, in the case of bad indecies, etc.
}
\examples{
library(daft)

df$itinary\%AT\%"from"
\%AT\%(df$itinary, "via")
}

