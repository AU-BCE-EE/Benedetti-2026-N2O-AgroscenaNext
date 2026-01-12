# Integration of interval-based rate measurements

mintegrate <- function(x, y, method = 'midpoint', lwr = min(x,na.RM=TRUE), upr = max(x), ylwr = y[which.min(x)], value = 'all') {

  method <- substr(tolower(method), 1, 1)

  if (length(x) != length(y)) {
    stop('Lengths of x and y are not equal.')
  }

  # Remove NaN
  non_na <- complete.cases(x, y)
  x2 <- x[non_na]
  y2 <- y[non_na]
  out <- rep(NaN,length(y))
  
  # Sort
  ord <- (1:length(x2))[order(x2)]
  y3 <- y2[order(x2)]
  x3 <- x2[order(x2)]

 

  if (method == 'l') {
    a <- cumsum(y * diff(c(lwr, x)))
  }

  if (method == 'r') {
    a <- cumsum(y * diff(c(x, upr)))
  }

  if (method == 'm') {
    a <- cumsum(c(0, y3[-length(y3)] * diff(x3)) / 2 +  y3 * diff(c(lwr, x3)) / 2)
  }

  if (method == 't') {
    x <- c(lwr, x)
    y <- c(ylwr, y)
    a <- cumsum((y[-length(y)] + diff(y) / 2) * diff(x)) 
    x <- x[-1]
    y <- y[-1]
  }
  
  out[non_na] <- a

  if (value == 'all') {
    return(out)
  } else if (value == 'total') {
    return(out[which.max(x)])
  }

}
