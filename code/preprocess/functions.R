# remove nan
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

is.finite.data.frame <- function(x)
  do.call(cbind, lapply(x, is.finite))

