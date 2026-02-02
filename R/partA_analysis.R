# Project 1 – Part A
# Linear regression with multiple imputation using mice

library(mice)   # multiple imputation
library(knitr)  # pretty tables (optional, mainly for Rmd/console)

# -------------------------------------------------------------------
# 1. Load and merge data
# -------------------------------------------------------------------

# NOTE: assume working directory is the repo root.
# CSVs are stored under data/
PartA_IV <- read.csv("data/partA_IV.csv", header = TRUE)
PartA_DV <- read.csv("data/partA_DV.csv", header = TRUE)

# Merge by subject ID
PartA <- merge(PartA_IV, PartA_DV, by = "ID")

# Quick structure check
str(PartA)

# -------------------------------------------------------------------
# 2. Explore missingness
# -------------------------------------------------------------------

# Check missingness in IV and DV columns (assume names IV and DV)
any(is.na(PartA$IV))
any(is.na(PartA$DV))

# Visualize missing data pattern
md.pattern(PartA)

# -------------------------------------------------------------------
# 3. Prepare data for imputation and run mice
# -------------------------------------------------------------------

# Keep rows where at least one of IV or DV is observed
PartA_imp <- PartA[!is.na(PartA$IV) | !is.na(PartA$DV), ]

# Run multiple imputation with normal model + bootstrap
imp <- mice(PartA_imp, method = "norm.boot", printFlag = FALSE)

# Extract one completed dataset (or pool models later if you want)
PartA_complete <- complete(imp)

# Check missing-data pattern after imputation
md.pattern(PartA_complete)

# -------------------------------------------------------------------
# 4. Fit linear model
# -------------------------------------------------------------------

M <- lm(DV ~ IV, data = PartA_complete)

summary(M)           # regression coefficients, R^2, etc.
kable(anova(M), caption = "ANOVA Table")  # ANOVA table for the model

# -------------------------------------------------------------------
# 5. Plot regression line
# -------------------------------------------------------------------

plot(
  PartA_complete$IV,
  PartA_complete$DV,
  main = "Scatterplot: DV ~ IV",
  xlab = "IV",
  ylab = "DV",
  pch = 20
)

abline(M, col = "red", lty = 3, lwd = 2)

legend(
  "topleft",
  legend = "Estimated Regression Line",
  lty = 3,
  lwd = 2,
  col = "red"
)

# -------------------------------------------------------------------
# 6. Confidence intervals for coefficients
# -------------------------------------------------------------------

confint(M, level = 0.95)
confint(M, level = 0.99)
