############################################################################
### PACKAGES AND DATA
library(car)
library(tidyverse)

data_stems <- read_csv("data/data_stems.csv") %>%
                mutate(plot_id = as.factor(plot_id),
                       size_class = as.factor(size_class),
                       harvested = as.factor(harvested),
                       vegetation_type = as.factor(vegetation_type),
                       milpa = as.factor(milpa))
data_plots <- read_csv("data/data_plots.csv") %>%
                mutate(plot_id = as.factor(plot_id),
                       harvested = as.factor(harvested),
                       vegetation_type = as.factor(vegetation_type),
                       milpa = as.factor(milpa))
############################################################################

############################################################################
### 1A: BASAL AREA (ALL SIZE CLASSES EXCEPT SEEDLINGS AND SAPLINGS)

### Figures

# Histogram of total basal area for all plots, harvested plots, unharvested plots
data_plots %>%
  ggplot() +
    geom_histogram(aes(x = ba_total), bins = 10, fill = "gray90", col = "black") +
    theme_classic() + ylim(c(0,20))

data_plots %>%
  ggplot() +
  geom_histogram(aes(x = ba_total), bins = 5, fill = "gray90", col = "black") +
  facet_wrap(~harvested) +
  theme_classic() + ylim(c(0,20))

# Five-number summary of total basal area for all plots, harvested plots, unharvested plots
summary(data_plots$ba_total)
summary(data_plots[data_plots$harvested == "yes",]$ba_total)
summary(data_plots[data_plots$harvested == "no",]$ba_total)

# Box plot of Total BA (m2/ha) by harvested
data_plots %>%
  ggplot() +
    geom_boxplot(aes(x = harvested, y = ba_total), fill = "gray90") +
    theme_classic()

# Box plot of Total BA (m2/ha) by vegetation type
data_plots %>%
  ggplot() +
  geom_boxplot(aes(x = vegetation_type, y = ba_total), fill = "gray90") +
  theme_classic()

# Box plot of Total BA (m2/ha) by milpa
data_plots %>%
  ggplot() +
  geom_boxplot(aes(x = milpa, y = ba_total), fill = "gray90") +
  theme_classic()

# Interaction plots
with(data_plots, {interaction.plot(vegetation_type, milpa, ba_total)
                  interaction.plot(milpa, harvested, ba_total)
                  interaction.plot(vegetation_type, harvested, ba_total)})



### Model

# Fit full model
mod.1a <- lm(ba_total ~ harvested + vegetation_type + milpa +
              harvested:vegetation_type + harvested:milpa, data = data_plots)

# Check constant variance and normality assumptions
plot(mod.1a, which = 1); plot(mod.1a, which = 2)

# Residuals vs. fitted looks pretty good, but we can try Box-Cox just to be safe
MASS::boxcox(mod.1a)

# Box-Cox suggests log transformation; re-fit model with log response
mod.1a.transform <- lm(log(ba_total) ~ harvested + vegetation_type + milpa +
                        harvested:vegetation_type + harvested:milpa, data = data_plots)

# Check constant variance and normality assumptions
plot(mod.1a.transform, which = 1); plot(mod.1a.transform, which = 2)

# Assumptions look about the same; for the sake of interpretability, we decide to go with the untransformed model
# Check for outliers
outlierTest(mod.1a)

# No significant outliers; look at model summary
summary(mod.1a)
# R^2 is relatively low

# Construct ANOVA table (use type 2 SS since data are unbalanced)
Anova(mod.1a, type = 2)

# We find that no predictors are significant

# Confirm lack of significance of harvested using Tukey HSD
TukeyHSD(aov(mod.1a), "harvested", conf.level = 0.95)
# Unharvested plots have higher BA than harvested plots, but difference is not significant
############################################################################
