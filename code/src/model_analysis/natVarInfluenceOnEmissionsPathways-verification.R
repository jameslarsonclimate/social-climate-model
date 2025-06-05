# Install the ‘sn’ package if it’s not already installed
if (!require(sn)) {
  install.packages("sn")
  library(sn)
} else {
  library(sn)
}

set.seed(123)  # for reproducibility

# Parameters for the skew-normal
# xi: location (center of the distribution)
# omega: scale (controls spread)
# alpha: shape (positive → right skew, negative → left skew)
xi    <- 0.37
omega <- 0.2
alpha <- 2.8

N <- 100000
frac_supp_0 <- numeric(N)

# Draw frac_supp_0 from a skew-normal until they lie between 0.3 and 0.8
i <- 1
while (i <= N) {
  x <- rsn(1, xi = xi, omega = omega, alpha = alpha)
  if (x >= 0.30 && x <= 0.80) {
    frac_supp_0[i] <- x    
    i <- i + 1
  }
}

frac_opp_0 <- numeric(N)
for (i in seq_len(N)) {
  attempts <- 0
  repeat {
    attempts <- attempts + 1
    x <- rsn(1, xi = 0.3, omega = 0.2, alpha = 1)
    # print x every time i is a multiple of 100
    if (i %% 100 == 0) {
      print(paste0("i = ", i, " | x = ", signif(x, 4)))
    }
    # print x every 100 iterations of the repeat loop
    if (attempts %% 100 == 0) {
      print(paste0("i = ", i, " | attempts = ", attempts, " | x = ", signif(x, 4)))
    }
    upper <- 1 - frac_supp_0[i]
    if (x >= 0.1 && x <= upper) {
      frac_opp_0[i] <- x
      break
    }
  }
}

# # remainder goes to neutral
frac_neut_0 <- 1 - (frac_supp_0 + frac_opp_0)
 
# ---- Plot skew‐normal frac_supp_0 and the other fractions ----
hist(frac_supp_0,
     breaks      = 100,
     probability = TRUE,
     col         = "lightblue",
     border      = "white",
     xlim        = c(0, 0.80),
     ylim   = c(0, 5),
     main        = "Skew‐Normal Samples of frac_supp_0, frac_opp_0, frac_neut_0",
     xlab        = "Fraction",
     ylab        = "Density")

# density curves
dens_sup   <- density(frac_supp_0)
dens_opp   <- density(frac_opp_0)
dens_neut  <- density(frac_neut_0)

# plot densities
lines(dens_sup,   lwd = 2, col = "darkblue",    lty = 1)
lines(dens_opp,   lwd = 2, col = "forestgreen", lty = 2)
lines(dens_neut,  lwd = 2, col = "purple",      lty = 4)

# median of support
abline(v = median(frac_supp_0), col = "red", lwd = 2, lty = 3)

# legend
legend("topright",
       legend = c("frac_supp_0", "frac_opp_0", "frac_neut_0", "median(frac_supp_0)"),
       col    = c("darkblue",    "forestgreen",   "purple",       "red"),
       lwd    = c(2,             2,               2,              2),
       lty    = c(1,             2,               4,              3),
       bty    = "n")