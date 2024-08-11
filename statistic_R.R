# ancova_analysis

# Example dataset
set.seed(123)
data <- data.frame(
  group = factor(rep(c("A", "B", "C"), each = 10)),
  covariate = rnorm(30, mean = 50, sd = 10),
  value = c(rnorm(10, mean = 10), rnorm(10, mean = 15), rnorm(10, mean = 20)) + rnorm(30, sd = 2)
)

# Perform ANCOVA
ancova_model <- lm(value ~ covariate + group, data = data)
summary(ancova_model)

# Check for interaction between covariate and group
interaction_model <- lm(value ~ covariate * group, data = data)
summary(interaction_model)

# Post-hoc comparisons if needed
library(emmeans)
posthoc <- emmeans(ancova_model, pairwise ~ group)
summary(posthoc)
