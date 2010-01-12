caterpoints <- function(x, parnames, ...){
  if (!missing(parnames))
      x <- x[parnames]
  points(x, rev(seq(along=x)), ...)
  }
