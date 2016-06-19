#'@export
cosabscossim <- function(x, y, labels) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  dot <- x %*% y
  z <- norm(x,type="2")*norm(y,type="2")
  if (!is.na(labels[1]) & !is.na(labels[2]) & (labels[1] == labels[2])) {
    return(dot/z)
  } else {
    return(abs(dot/z))
  }
}

#'@export
cossim <- function(x, y) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  dot <- x %*% y
  z <- norm(x,type="2")*norm(y,type="2")
  return(dot/z)
}

cos2sim <- function(x, y) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  dot <- x %*% y
  z <- norm(x,type="2")*norm(y,type="2")
  return((dot/z)^2)
}

#'@export
abscossim <- function(x, y) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  dot <- x %*% y
  z <- norm(x,type="2")*norm(y,type="2")
  return(abs(dot/z))  
}

abscossim_v <- function(x, y) {
  absdot <- abs(x %*% t(y))
  xz <- apply(x^2, 1, sum)
  yz <- apply(y^2, 1, sum)
  result <- absdot/xz
  result <- t(t(result)/yz)
  return(result)
}


abscosmagdist <- function(x, y) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  dot <- x %*% y
  normx <- norm(x,type="2")
  normy <- norm(y,type="2")
  z <- normx*normy
  abscos <- max(0, 1. - abs(dot/z))
  absmagdiff <- abs(normx - normy)
  return(abscos*absmagdiff)
}

minusabscosmagsim <- function(x, y) {
  return(-abscosmagdist(x, y))
}

eucldist <- function(x, y, minus=F) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  result <- norm(x - y, type="2")
  if (minus) {
    result <- -result
  }
  return(result)
}

minuseucldist <- function(x, y) {
  return(eucldist(x,y,minus=T))
}
