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


# Function to identify exactly seq_length consecutive positive/negative anomalies
identify_anomaly_sequences <- function(anomaly_vector, seq_length, sign_filter = "both") {
  # Initialize result vector
  result <- rep(FALSE, length(anomaly_vector))
  
  # We need a vector with length at least seq_length
  if (length(anomaly_vector) < seq_length) return(result)
  
  # Validate sign_filter parameter
  sign_filter <- match.arg(sign_filter, c("both", "positive", "negative"))
  
  # Count consecutive values of the same sign
  count <- 1
  sign_value <- sign(anomaly_vector[1])
  
  # Loop through the vector starting from the second value
  for (i in 2:length(anomaly_vector)) {
    current_sign <- sign(anomaly_vector[i])
    
    # Check if current sign matches our filter criteria
    valid_sign <- (sign_filter == "both") || 
                  (sign_filter == "positive" && current_sign > 0) ||
                  (sign_filter == "negative" && current_sign < 0)
    
    # If the sign is the same, non-zero, and matches our filter
    if (current_sign == sign_value && current_sign != 0 && valid_sign) {
      count <- count + 1
      
      # Mark only exactly the seq_length-th consecutive value
      if (count == seq_length) {
        result[i] <- TRUE
      }
    } else {
      # Reset counter and update sign when sequence breaks
      count <- 1
      sign_value <- current_sign
    }
  }
  
  return(result)
}
