
# Project 1 – Part B
# Transformation, binning, and pure error ANOVA

library(alr3)   # for pureErrorAnova (and related regression tools)

# -------------------------------------------------------------------
# 1. Load data
# -------------------------------------------------------------------

# Assume a single CSV with columns x and y, stored in data/
data <- read.csv("data/partB.csv", header = TRUE)

str(data)

# -------------------------------------------------------------------
# 2. Transform response
# -------------------------------------------------------------------
# Apply y^(-2/3) transformation to stabilize variance and linearize
# the relationship between x and y.

data_trans <- data.frame(
  xtrans = data$x,
  ytrans = data$y^(-2/3)
)

# -------------------------------------------------------------------
# 3. Bin x into groups for pure error calculation
# -------------------------------------------------------------------
# We create roughly 0.3-wide bins across the range of x. The extra
# -Inf/Inf at the ends catch any values outside the interior sequence.

breaks <- c(
  -Inf,
  seq(min(data_trans$xtrans), max(data_trans$xtrans), by = 0.3),
  Inf
)

groups <- cut(data_trans$xtrans, breaks = breaks)

# Check bin counts
table(groups)

# -------------------------------------------------------------------
# 4. Compute group means for x and build binned dataset
# -------------------------------------------------------------------
# ave() computes the mean of xtrans within each group, returning a
# vector aligned with data_trans$xtrans.

x_group_mean <- ave(data_trans$xtrans, groups)

data_bin <- data.frame(
  x = x_group_mean,
  y = data_trans$ytrans
)

# -------------------------------------------------------------------
# 5. Fit linear model on binned data
# -------------------------------------------------------------------

fit_b <- lm(y ~ x, data = data_bin)

summary(fit_b)

# -------------------------------------------------------------------
# 6. Pure error ANOVA (lack-of-fit test)
# -------------------------------------------------------------------
# pureErrorAnova decomposes the residual sum of squares into pure error
# and lack-of-fit components, allowing a formal test for lack of fit.

pureErrorAnova(fit_b)
