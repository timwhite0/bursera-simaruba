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

plot_theme <- theme(plot.background = element_rect(fill = "white"),
                    plot.title = element_blank(), 
                    plot.subtitle = element_text(family="sans", face="plain"), 
                    axis.title.x = element_text(family="sans", face="bold"),
                    axis.title.y = element_text(family="sans", face="bold"),
                    axis.text.x = element_text(family="sans", face="plain"),
                    axis.text.y = element_text(family="sans", face="plain"),
                    panel.background = element_rect(fill="white"),
                    panel.grid.major.x = element_blank(),
                    panel.grid.major.y = element_line(color="gainsboro"),
                    panel.grid.minor = element_blank(),
                    axis.ticks = element_blank(),
                    legend.background = element_rect(color="black", fill = "white"),
                    legend.position = c(0.9, 0.85),
                    legend.title = element_text(color = "black", face = "bold", hjust = 0.5),
                    legend.text = element_text(color = "black"))
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
data_plots %>% filter(harvested == "yes") %>% pull(stemden_totaltrees) %>% summary()
data_plots %>% filter(harvested == "no") %>% pull(stemden_totaltrees) %>% summary()

# Box plot of log(# total trees per ha) by harvested
data_plots %>%
  mutate(harvested = fct_recode(harvested,
                                "Harvested" = "yes", "Unharvested" = "no")) %>%
  ggplot() +
  geom_boxplot(aes(x = fct_relevel(harvested, "Harvested", "Unharvested"),
                   y = log(stemden_totaltrees)),
               fill = "mistyrose2", outlier.shape = 21,
               outlier.color = "gray20", outlier.fill = "gray80") +
  plot_theme +
  coord_cartesian(ylim = c(5, 8)) +
  labs(x = "Plot status", y = "Total trees per hectare (log scale)")

# Box plot of log(# total trees per ha) by vegetation type
data_plots %>%
  mutate(vegetation_type = fct_recode(vegetation_type,
                                      "Ju'uche'" = "juuche",
                                      "Keelenche'" = "keelenche",
                                      "Nuku'uch che'" = "nukuuchche")) %>%
  ggplot() +
  geom_boxplot(aes(x = vegetation_type, y = log(stemden_totaltrees)),
               fill = "mistyrose2", outlier.shape = 21,
               outlier.color = "gray20", outlier.fill = "gray80") +
  plot_theme +
  coord_cartesian(ylim = c(5, 8)) +
  labs(x = "Vegetation type", y = "Total trees per hectare (log scale)")

# Box plot of log(# total trees per ha) by milpa
data_plots %>%
  mutate(milpa = fct_recode(milpa,
                            "Yes" = "yes", "No" = "no")) %>%
  ggplot() +
  geom_boxplot(aes(x = fct_relevel(milpa, "Yes", "No"), y = log(stemden_totaltrees)),
               fill = "mistyrose2", outlier.shape = 21,
               outlier.color = "gray20", outlier.fill = "gray80") +
  plot_theme +
  coord_cartesian(ylim = c(5, 8)) +
  labs(x = "Milpa exposure", y = "Total trees per hectare (log scale)")

# Interaction plots
with(data_plots, {interaction.plot(vegetation_type, milpa, stemden_totaltrees)
                  interaction.plot(milpa, harvested, stemden_totaltrees)
                  interaction.plot(vegetation_type, harvested, stemden_totaltrees)})



### Model

# Fit full model
mod.1b <- lm(stemden_totaltrees ~ harvested + vegetation_type + milpa +
              harvested:vegetation_type + harvested:milpa, data = data_plots)

# Check constant variance and normality assumptions
plot(mod.1b, which = 1); plot(mod.1b, which = 2)

# There is a megaphone shape on the residuals vs. fitted; try Box-Cox
MASS::boxcox(mod.1b)

# Box-Cox suggests log transformation; re-fit model with logged response
mod.1b.transform <- lm(log(stemden_totaltrees) ~ harvested + vegetation_type + milpa +
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
