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

#######################################################
## LME
library(afex)
roi_table = data.frame(site = data$site, subject = data$subject, value = data[[roi]])
# method S: the Satterthwaite approximation for degrees of freedom
# methods PB and LRT will make a pvalue same as the matlab lme function.
mod <- afex::lmer_alt(value ~ site + (1|subject), data=roi_table, method="PB")
mod_result = summary(mod)
# main effect: F test
F_test = anova(mod)
stat_Ftest[1, roi] = F_test$`F value`
stat_Ftest[2, roi] = F_test$`Pr(>F)`
# estimate value: beta
stat_beta[, roi] = mod_result$coefficients[, 'Estimate']
# statistic value: t value
stat_t[, roi] = mod_result$coefficients[, 't value']
# p value
stat_p[, roi] = mod_result$coefficients[, 'Pr(>|t|)']

#######################################################
## LME + post_hoc
mod <- afex::lmer_alt(value ~ 1+ site + type + site*type + (1|subject), data=df_melt, method="PB")
anova(mod) # main effect: F test
summary(mod)
# Post-hoc comparisons if needed
library(emmeans)
emm <- emmeans(mod, ~ site)
pairs(emm)

