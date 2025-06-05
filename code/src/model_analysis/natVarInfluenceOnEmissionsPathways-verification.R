# # Install the ‘sn’ package if it’s not already installed
# if (!require(sn)) {
#   install.packages("sn")
#   library(sn)
# } else {
#   library(sn)
# }

# set.seed(123)  # for reproducibility

# # Parameters for the skew-normal
# # xi: location (center of the distribution)
# # omega: scale (controls spread)
# # alpha: shape (positive → right skew, negative → left skew)
# xi    <- 0.37
# omega <- 0.2
# alpha <- 2.8

# N <- 100000
# frac_supp_0 <- numeric(N)

# # Draw frac_supp_0 from a skew-normal until they lie between 0.3 and 0.8
# i <- 1
# while (i <= N) {
#   x <- rsn(1, xi = xi, omega = omega, alpha = alpha)
#   if (x >= 0.30 && x <= 0.80) {
#     frac_supp_0[i] <- x    
#     i <- i + 1
#   }
# }

# frac_opp_0 <- numeric(N)
# for (i in seq_len(N)) {
#   attempts <- 0
#   repeat {
#     attempts <- attempts + 1
#     x <- rsn(1, xi = 0.3, omega = 0.2, alpha = 1)
#     # print x every time i is a multiple of 100
#     if (i %% 100 == 0) {
#       print(paste0("i = ", i, " | x = ", signif(x, 4)))
#     }
#     # print x every 100 iterations of the repeat loop
#     if (attempts %% 100 == 0) {
#       print(paste0("i = ", i, " | attempts = ", attempts, " | x = ", signif(x, 4)))
#     }
#     upper <- 1 - frac_supp_0[i]
#     if (x >= 0.1 && x <= upper) {
#       frac_opp_0[i] <- x
#       break
#     }
#   }
# }

# # # remainder goes to neutral
# frac_neut_0 <- 1 - (frac_supp_0 + frac_opp_0)

library(data.table)
library(ggplot2)


data_dir           <- "../results/MC Runs/MC Runs_TunedParams/"
fig_suffix = '_initClimSupportNormalDistribution'
title_suffix       <- fig_suffix
params_file        <- paste0(data_dir, "params",   fig_suffix, ".csv")
params_dt <- fread(params_file)            # 100000 × 22

frac_neut_0 <- params_dt$frac_neut_01
frac_opp_0 <- params_dt$frac_opp_0
frac_supp_0 <- 1- (frac_neut_01 + frac_opp_0)


# ---- Plot and save skew‐normal fractions to file (density only, no histogram) ----
out_file <- "../results/verification/opinion_fraction_distributions.png"
dir.create(dirname(out_file), showWarnings = FALSE, recursive = TRUE)

png(filename = out_file, width = 800, height = 600, res = 100)
# compute densities once
d_sup  <- density(frac_supp_0)
d_opp  <- density(frac_opp_0)
d_neut <- density(frac_neut_0)

# plot support density
plot(d_sup,
     xlim = c(0, 0.80),
     ylim = c(0, max(d_sup$y, d_opp$y, d_neut$y)),
     col   = "darkblue", lwd = 2,
     main  = "Density of Initial Opinion Fractions",
     xlab  = "Fraction", ylab = "Density")

# add opposition and neutral densities
lines(d_opp,  col = "forestgreen", lwd = 2)
lines(d_neut, col = "purple",      lwd = 2)

# add vertical median lines for each group
m_sup  <- median(frac_supp_0)
m_opp  <- median(frac_opp_0)
m_neut <- median(frac_neut_0)
abline(v = m_sup,  col = "darkblue",    lwd = 2, lty = 3)
abline(v = m_opp,  col = "forestgreen", lwd = 2, lty = 3)
abline(v = m_neut, col = "purple",      lwd = 2, lty = 3)

# label median values
y_pos <- max(d_sup$y, d_opp$y, d_neut$y) * 0.85
text(m_sup,  y_pos, labels = sprintf("M=%.2f", m_sup),  col = "darkblue",    pos = 3)
text(m_opp,  y_pos, labels = sprintf("M=%.2f", m_opp),  col = "forestgreen", pos = 3)
text(m_neut, y_pos, labels = sprintf("M=%.2f", m_neut), col = "purple",      pos = 3)

# legend
legend("topright",
       legend = c("Support", "Opposition", "Neutral"),
       col    = c("darkblue", "forestgreen", "purple"),
       lwd    = c(2,           2,            2),
       lty    = c(1,           1,            1),
       bty    = "n")

dev.off()