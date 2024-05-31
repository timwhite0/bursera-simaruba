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
### 1A: BASAL AREA (ALL SIZE CLASSES EXCEPT SEEDLINGS AND SAPLINGS)

### Figures

# Histogram of total basal area for all plots, harvested plots, unharvested plots
data_plots %>%
  ggplot() +
    geom_histogram(aes(x = ba_totaltrees), bins = 10, fill = "gray90", col = "black") +
    theme_classic() + ylim(c(0,20))

data_plots %>%
  ggplot() +
  geom_histogram(aes(x = ba_totaltrees), bins = 5, fill = "gray90", col = "black") +
  facet_wrap(~harvested) +
  theme_classic() + ylim(c(0,20))

# Five-number summary of total basal area for all plots, harvested plots, unharvested plots
summary(data_plots$ba_totaltrees)
data_plots %>% filter(harvested == "yes") %>% pull(ba_totaltrees) %>% summary()
data_plots %>% filter(harvested == "no") %>% pull(ba_totaltrees) %>% summary()

# Box plot of Total BA (m2/ha) by harvested
data_plots %>%
  mutate(harvested = fct_recode(harvested,
                                "Harvested" = "yes", "Unharvested" = "no")) %>%
  ggplot() +
    geom_boxplot(aes(x = fct_relevel(harvested, "Harvested", "Unharvested"),
                     y = ba_totaltrees),
                 fill = "mistyrose2", outlier.shape = 21,
                 outlier.color = "gray20", outlier.fill = "gray80") +
    plot_theme +
    coord_cartesian(ylim = c(0, 15)) +
    labs(x = "Plot status", y = "Total basal area of trees (m2 per hectare)")

# Box plot of Total BA (m2/ha) by vegetation type
data_plots %>%
  mutate(vegetation_type = fct_recode(vegetation_type,
                                      "Ju'uche'" = "juuche",
                                      "Keelenche'" = "keelenche",
                                      "Nuku'uch che'" = "nukuuchche")) %>%
  ggplot() +
  geom_boxplot(aes(x = vegetation_type, y = ba_totaltrees),
               fill = "mistyrose2", outlier.shape = 21,
               outlier.color = "gray20", outlier.fill = "gray80") +
  plot_theme +
  coord_cartesian(ylim = c(0, 15)) +
  labs(x = "Vegetation type", y = "Total basal area of trees (m2 per hectare)")

# Box plot of Total BA (m2/ha) by milpa
data_plots %>%
  mutate(milpa = fct_recode(milpa,
                            "Yes" = "yes", "No" = "no")) %>%
  ggplot() +
  geom_boxplot(aes(x = fct_relevel(milpa, "Yes", "No"), y = ba_totaltrees),
               fill = "mistyrose2", outlier.shape = 21,
               outlier.color = "gray20", outlier.fill = "gray80") +
  plot_theme +
  coord_cartesian(ylim = c(0, 15)) +
  labs(x = "Milpa exposure", y = "Total basal area of trees (m2 per hectare)")

# Interaction plots
with(data_plots, {interaction.plot(vegetation_type, milpa, ba_totaltrees)
                  interaction.plot(milpa, harvested, ba_totaltrees)
                  interaction.plot(vegetation_type, harvested, ba_totaltrees)})



### Model

# Fit full model
mod.1a <- lm(ba_totaltrees ~ harvested + vegetation_type + milpa +
              harvested:vegetation_type + harvested:milpa, data = data_plots)

# Check constant variance and normality assumptions
plot(mod.1a, which = 1); plot(mod.1a, which = 2)

# Residuals vs. fitted looks pretty good, but we can try Box-Cox just to be safe
MASS::boxcox(mod.1a)

# Box-Cox suggests log transformation; re-fit model with log response
mod.1a.transform <- lm(log(ba_totaltrees) ~ harvested + vegetation_type + milpa +
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
