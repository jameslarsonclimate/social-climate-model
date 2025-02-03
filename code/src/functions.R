compute_regression_stats <- function(x, y) {
  # Check that x and y are either both vectors or both matrices
  if (is.null(dim(x)) && is.null(dim(y))) {
    # Both are vectors
    if (length(x) != length(y))
      stop("x and y must have the same length.")
    
    n <- length(x)
    xmean <- mean(x, na.rm = TRUE)
    ymean <- mean(y, na.rm = TRUE)
    # Population standard deviation (ddof=0)
    xstd <- sqrt(sum((x - xmean)^2, na.rm = TRUE) / n)
    ystd <- sqrt(sum((y - ymean)^2, na.rm = TRUE) / n)
    # Covariance computed with denominator n
    cov_xy <- sum((x - xmean) * (y - ymean), na.rm = TRUE) / n
    
  } else if (!is.null(dim(x)) && !is.null(dim(y))) {
    # Both are matrices; ensure dimensions agree
    if (!all(dim(x) == dim(y)))
      stop("x and y must have the same dimensions.")
    
    n <- nrow(x)
    # Compute column means (ignoring NA's)
    xmean <- colMeans(x, na.rm = TRUE)
    ymean <- colMeans(y, na.rm = TRUE)
    # Compute "population" standard deviations for each column:
    xstd <- apply(x, 2, function(col) sqrt(sum((col - mean(col, na.rm = TRUE))^2, na.rm = TRUE) / n))
    ystd <- apply(y, 2, function(col) sqrt(sum((col - mean(col, na.rm = TRUE))^2, na.rm = TRUE) / n))
    # Compute covariance column by column
    cov_xy <- sapply(seq_len(ncol(x)), function(j) {
      sum((x[, j] - xmean[j]) * (y[, j] - ymean[j]), na.rm = TRUE) / n
    })
    
  } else {
    stop("x and y must both be either vectors or matrices.")
  }
  
  # Compute correlation as covariance divided by the product of the standard deviations
  cor_xy <- cov_xy / (xstd * ystd)
  
  # Compute regression slope and intercept
  slope <- cov_xy / (xstd^2)
  intercept <- ymean - xmean * slope
  
  # Return the computed values as a list
  return(list(n = n, 
              xmean = xmean, 
              ymean = ymean, 
              xstd = xstd, 
              ystd = ystd, 
              covariance = cov_xy, 
              correlation = cor_xy, 
              slope = slope, 
              intercept = intercept))
}
