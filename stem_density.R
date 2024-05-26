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
### 1B: STEM DENSITY (ALL SIZE CLASSES EXCEPT SEEDLINGS AND SAPLINGS)

### Figures

# Histogram of # total trees per ha for all plots, harvested plots, unharvested plots
data_plots %>%
  ggplot() +
  geom_histogram(aes(x = stemden_totaltrees), bins = 10, fill = "gray90", col = "black") +
  theme_classic() + ylim(c(0,20))

data_plots %>%
  ggplot() +
  geom_histogram(aes(x = stemden_totaltrees), bins = 10, fill = "gray90", col = "black") +
  facet_wrap(~harvested) +
  theme_classic() + ylim(c(0,20))

# Five-number summary of # total trees per ha for all plots, harvested plots, unharvested plots
summary(data_plots$stemden_totaltrees)
summary(data_plots[data_plots$harvested == "yes",]$stemden_totaltrees)
summary(data_plots[data_plots$harvested == "no",]$stemden_totaltrees)

# Box plot of # total trees per ha by harvested
data_plots %>%
  ggplot() +
  geom_boxplot(aes(x = harvested, y = stemden_totaltrees), fill = "gray90") +
  theme_classic()

# Box plot of # total trees per ha by vegetation type
data_plots %>%
  ggplot() +
  geom_boxplot(aes(x = vegetation_type, y = stemden_totaltrees), fill = "gray90") +
  theme_classic()

# Box plot of # total trees per ha by milpa
data_plots %>%
  ggplot() +
  geom_boxplot(aes(x = milpa, y = stemden_totaltrees), fill = "gray90") +
  theme_classic()



### Model

# Fit full model
mod.1b = lm(stemden_totaltrees ~ harvested + vegetation_type + milpa +
              harvested:vegetation_type + harvested:milpa, data = data_plots)

# Check constant variance and normality assumptions
plot(mod.1b, which = 1); plot(mod.1b, which = 2)

# There is a megaphone shape on the residuals vs. fitted; try Box-Cox
MASS::boxcox(mod.1b)

# Box-Cox suggests log transformation; re-fit model with logged response
mod.1b.transform = lm(log(stemden_totaltrees) ~ harvested + vegetation_type + milpa +
                        harvested:vegetation_type + harvested:milpa, data = data_plots)

# Check constant variance and normality assumptions again
plot(mod.1b.transform, which = 1); plot(mod.1b.transform, which = 2)

# The assumptions appear to be satisfied now, so we will proceed with the transformed model

# Note that we could also fix the assumption violations by removing a significant outlier (plot 19) from the untransformed model
# However, we decide to use the log-transform since our sample size is already quite small; if we removed the outlier, it would get even smaller

# Check for outliers
outlierTest(mod.1b.transform)

# No significant outliers; look at model summary
summary(mod.1b.transform)
# Adjusted R^2 is higher than in section 1A, but still not great

# Construct ANOVA table (use type 2 SS since data are unbalanced)
Anova(mod.1b.transform, type = 2)

# We find that vegetation type is significant, but harvested, milpa, and the interaction terms are not

# Pairwise comparison for vegetation type using Tukey HSD
TukeyHSD(aov(mod.1b.transform), "vegetation_type", conf.level = 0.95)
# Keelenche' has a significantly higher stem density then Nuku'uch che'

# Confirm lack of significance of harvested using Tukey HSD
TukeyHSD(aov(mod.1b.transform), "harvested", conf.level = 0.95)
# Unharvested plots have higher stem density than harvested plots, but difference is not significant
############################################################################
