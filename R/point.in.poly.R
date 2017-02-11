

# util function to check if point is in polygon. angle sum idea borrowed from the SDMTools package
point.in.polygon <- function(x, y, poly.x, poly.y, threshold=1e-7) {
  if(length(x) != length(y)) stop("Length of x and y must be equal")
  if(length(x) == 0) return(logical(0))
  if(length(poly.x) != length(poly.y)) stop("Length of poly.x and poly.y must be equal")
  if(length(poly.x) < 2) stop("Too few points in polygon")
  n <- length(poly.x)

  # first and last point cannot be the same for the angle sum method
  if(poly.x[1] == poly.x[n] && poly.y[1] == poly.y[n]) {
    poly.x <- poly.x[2:n]
    poly.y <- poly.y[2:n]
  }
  p1x <- poly.x
  p1y <- poly.y
  p2x <- c(p1x[2:n], p1x[1])
  p2y <- c(p1y[2:n], p1y[1])

  sapply(1:length(x), function(i) {
    angsum <- sum(angle(x[i], y[i], p1x, p1y, p2x, p2y, threshold))
    return((abs(angsum - 2*pi) < threshold))
  })
}

# vectorized such that p1 and p2 can be vectors
# important to keep track of directions so that the function works in
# concave polygons
angle <- function(x, y, p1x, p1y, p2x, p2y, threshold) {
  if(is.na(x) || is.na(y)) {
    return(NA)
  }
  v1.X <- p1x-x
  v1.Y <- p1y-y
  v2.X <- p2x-x
  v2.Y <- p2y-y
  norms <- sqrt(v1.X^2 + v1.Y^2) * sqrt(v2.X^2 + v2.Y^2)
  # check of points are on a vertex
  if(any(norms < threshold)) {
    return(2*pi)
  }

  cprod <- (v1.X*v2.Y) - (v1.Y*v2.X)
  dprod <- (v1.X*v2.X) + (v1.Y*v2.Y)

  # check if point is on a line segment
  distances <- (p2x-p1x)^2 + (p2y-p1y)^2
  bacaprod <- ((p2x-p1x) * (x-p1x)) + ((p2y-p1y) * (y-p1y))
  if(any((abs(cprod) < threshold) & (bacaprod > -threshold) & ((distances - bacaprod) > -threshold))) {
    return(2*pi)
  }

  directions <- ifelse(cprod == 0, 1, round(cprod / abs(cprod)))
  angles <- suppressWarnings(acos(dprod/norms) * directions)

  asum <- sum(angles, na.rm = TRUE)
  if(asum < 0) {
    angles <- -angles
  }

  return(angles)
}
