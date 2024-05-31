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
### 2C: BASAL AREA BY SIZE CLASS (FIGURES)

### Average basal area by size class

compute_bootstrap_interval <- function(var, statistic = mean, num_iters = 50000, 
                                       percentiles = c(0.025, 0.975)) {
  boot_samples <- numeric(num_iters)
  
  for (i in 1:num_iters) {
    boot_samples[i] <- statistic(sample(var, size = length(var), replace = TRUE))
  }
  
  return(quantile(boot_samples, percentiles))
}

harvested <- data_plots %>%
              filter(harvested == "yes") %>%
              select(ba_saplings, starts_with("ba_trees"))
unharvested <- data_plots %>%
                filter(harvested == "no") %>%
                select(ba_saplings, starts_with("ba_trees"))

set.seed(1)
boot_intervals <- bind_rows(compute_bootstrap_interval(unharvested$ba_saplings),
                            compute_bootstrap_interval(unharvested$ba_trees05to09),
                            compute_bootstrap_interval(unharvested$ba_trees10to14),
                            compute_bootstrap_interval(unharvested$ba_trees15to19),
                            compute_bootstrap_interval(unharvested$ba_trees20plus),
                            compute_bootstrap_interval(harvested$ba_saplings),
                            compute_bootstrap_interval(harvested$ba_trees05to09),
                            compute_bootstrap_interval(harvested$ba_trees10to14),
                            compute_bootstrap_interval(harvested$ba_trees15to19),
                            compute_bootstrap_interval(harvested$ba_trees20plus))

ba_mean_data <- data_plots %>%
                  select(harvested, ba_saplings, starts_with("ba_trees")) %>%
                  group_by(harvested) %>%
                  summarize_all(mean) %>%
                  pivot_longer(cols = starts_with("ba"), names_prefix = "ba_",
                               names_to = "size_class", values_to = "mean") %>%
                  bind_cols(boot_intervals) %>%
                  select(harvested, size_class, lower = `2.5%`, mean, upper = `97.5%`) %>%
                  mutate(size_class = as.factor(size_class)) %>%
                  mutate(harvested = fct_recode(harvested,
                                                "Harvested" = "yes", "Unharvested" = "no")) %>%
                  mutate(size_class = fct_recode(size_class,
                                                 "Saplings (0-4 cm DBH)" = "saplings",
                                                 "Trees (5-9 cm DBH)" = "trees05to09",
                                                 "Trees (10-14 cm DBH)" = "trees10to14",
                                                 "Trees (15-19 cm DBH)" = "trees15to19",
                                                 "Trees (20+ cm DBH)" = "trees20plus"))

# Point/line plot (all size classes except seedlings)
ba_mean_data %>%
  ggplot() +
  geom_pointrange(aes(x = size_class, col = fct_relevel(harvested, "Harvested", "Unharvested"),
                      ymin = lower, y = mean, ymax = upper),
                  position = position_dodge(width = 0.5), size = 0.5, linewidth = 1) +
  plot_theme +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Size class", y = "Average basal area (m2 per hectare)") +
  guides(col=guide_legend(title = "Status"))

# Bar plot (all size classes except seedlings)
ba_mean_data %>%
  ggplot(aes(x = size_class,
             fill = fct_relevel(harvested, "Harvested", "Unharvested"))) +
  geom_col(aes(y = mean),
           position = "dodge") +
  geom_errorbar(aes(x = size_class, ymin = lower, ymax = upper), col = "gray20",
                position = position_dodge(width = 0.9), width = 0.2, show.legend = FALSE) +
  plot_theme +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Size class", y = "Average basal area (m2 per hectare)") +
  guides(fill=guide_legend(title = "Status"))



### Distribution of basal area by size class

# Box plot (all size classes except seedlings)
ba_dist_data <- data_plots %>%
                  select(harvested, ba_saplings, starts_with("ba_trees")) %>%
                  pivot_longer(cols = starts_with("ba"),
                               names_prefix = "ba_",
                               names_to = "size_class",
                               values_to = "ba") %>%
                  mutate(size_class = as.factor(size_class)) %>%
                  mutate(harvested = fct_recode(harvested,
                                                "Harvested" = "yes",
                                                "Unharvested" = "no")) %>%
                  mutate(size_class = fct_recode(size_class,
                                                 "Saplings (0-4 cm DBH)" = "saplings",
                                                 "Trees (5-9 cm DBH)" = "trees05to09",
                                                 "Trees (10-14 cm DBH)" = "trees10to14",
                                                 "Trees (15-19 cm DBH)" = "trees15to19",
                                                 "Trees (20+ cm DBH)" = "trees20plus"))

# Violin plot (all size classes except seedlings)
ba_dist_data %>%
  ggplot() +
  geom_violin(aes(x = size_class, y = ba,
                  fill = fct_relevel(harvested, "Harvested", "Unharvested")),
              scale = "area", width = 2, position = position_dodge(width = 0.7),
              lwd = 0.75, kernel = "gaussian", adjust = 1.25) +
  plot_theme +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Size class", y = "Basal area (m2 per hectare)") +
  guides(fill = guide_legend(title = "Status"))

# Box plot (all size classes except seedlings)
ba_dist_data %>%
  ggplot() +
  geom_boxplot(aes(x = size_class, y = ba,
                   fill = fct_relevel(harvested, "Harvested", "Unharvested")),
               outlier.shape = 21, outlier.color = "gray20",
               outlier.fill = "gray80") +
  plot_theme +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Size class", y = "Basal area (m2 per hectare)") +
  guides(fill = guide_legend(title = "Status"))
############################################################################

############################################################################
### 2D: BASAL AREA BY SIZE CLASS (MODELS)

### Figures

# Histogram of BA (m2/ha) by size class for all plots, harvested plots, unharvested plots
data_plots %>%
  ggplot() + geom_histogram(aes(x = ba_saplings), bins = 20,
                            col = "black", fill = "gray80") + plot_theme
data_plots %>%
  ggplot() + geom_histogram(aes(x = ba_trees05to09), bins = 20,
                            col = "black", fill = "gray80") + plot_theme
data_plots %>%
  ggplot() + geom_histogram(aes(x = ba_trees10to14), bins = 20,
                            col = "black", fill = "gray80") + plot_theme
data_plots %>%
  ggplot() + geom_histogram(aes(x = ba_trees15to19), bins = 10,
                            col = "black", fill = "gray80") + plot_theme
data_plots %>%
  ggplot() + geom_histogram(aes(x = ba_trees20plus), bins = 5,
                            col = "black", fill = "gray80") + plot_theme

# Five-number summary of BA (m2/ha) by size class for all plots, harvested plots, unharvested plots
summary(data_plots$ba_saplings)
data_plots %>% filter(harvested == "yes") %>% pull(ba_saplings) %>% summary()
data_plots %>% filter(harvested == "no") %>% pull(ba_saplings) %>% summary()

summary(data_plots$ba_trees05to09)
data_plots %>% filter(harvested == "yes") %>% pull(ba_trees05to09) %>% summary()
data_plots %>% filter(harvested == "no") %>% pull(ba_trees05to09) %>% summary()

summary(data_plots$ba_trees10to14)
data_plots %>% filter(harvested == "yes") %>% pull(ba_trees10to14) %>% summary()
data_plots %>% filter(harvested == "no") %>% pull(ba_trees10to14) %>% summary()

summary(data_plots$ba_trees15to19)
data_plots %>% filter(harvested == "yes") %>% pull(ba_trees15to19) %>% summary()
data_plots %>% filter(harvested == "no") %>% pull(ba_trees15to19) %>% summary()

summary(data_plots$ba_trees20plus)
data_plots %>% filter(harvested == "yes") %>% pull(ba_trees20plus) %>% summary()
data_plots %>% filter(harvested == "no") %>% pull(ba_trees20plus) %>% summary()

# Box plot of BA (m2/ha) by harvested for each size class
data_plots %>% ggplot() +
  geom_boxplot(aes(x = harvested, y = ba_saplings,
                   fill = harvested)) + plot_theme
data_plots %>% ggplot() +
  geom_boxplot(aes(x = harvested, y = ba_trees05to09,
                   fill = harvested)) + plot_theme
data_plots %>% ggplot() +
  geom_boxplot(aes(x = harvested, y = ba_trees10to14,
                   fill = harvested)) + plot_theme
data_plots %>% ggplot() +
  geom_boxplot(aes(x = harvested, y = ba_trees15to19,
                   fill = harvested)) + plot_theme
data_plots %>% ggplot() +
  geom_boxplot(aes(x = harvested, y = ba_trees20plus,
                   fill = harvested)) + plot_theme

# Box plot of BA (m2/ha) by vegetation type for each size class
data_plots %>% ggplot() +
  geom_boxplot(aes(x = vegetation_type, y = ba_saplings,
                   fill = vegetation_type)) + plot_theme
data_plots %>% ggplot() +
  geom_boxplot(aes(x = vegetation_type, y = ba_trees05to09,
                   fill = vegetation_type)) + plot_theme
data_plots %>% ggplot() +
  geom_boxplot(aes(x = vegetation_type, y = ba_trees10to14,
                   fill = vegetation_type)) + plot_theme
data_plots %>% ggplot() +
  geom_boxplot(aes(x = vegetation_type, y = ba_trees15to19,
                   fill = vegetation_type)) + plot_theme
data_plots %>% ggplot() +
  geom_boxplot(aes(x = vegetation_type, y = ba_trees20plus,
                   fill = vegetation_type)) + plot_theme

# Box plot of BA (m2/ha) by milpa for each size class
data_plots %>% ggplot() +
  geom_boxplot(aes(x = milpa, y = ba_saplings,
                   fill = milpa)) + plot_theme
data_plots %>% ggplot() +
  geom_boxplot(aes(x = milpa, y = ba_trees05to09,
                   fill = milpa)) + plot_theme
data_plots %>% ggplot() +
  geom_boxplot(aes(x = milpa, y = ba_trees10to14,
                   fill = milpa)) + plot_theme
data_plots %>% ggplot() +
  geom_boxplot(aes(x = milpa, y = ba_trees15to19,
                   fill = milpa)) + plot_theme
data_plots %>% ggplot() +
  geom_boxplot(aes(x = milpa, y = ba_trees20plus,
                   fill = milpa)) + plot_theme

# Interaction plots for each size class
with(data_plots, {interaction.plot(vegetation_type, milpa, ba_saplings)
                  interaction.plot(milpa, harvested, ba_saplings)
                  interaction.plot(vegetation_type, harvested, ba_saplings)})
with(data_plots, {interaction.plot(vegetation_type, milpa, ba_trees05to09)
                  interaction.plot(milpa, harvested, ba_trees05to09)
                  interaction.plot(vegetation_type, harvested, ba_trees05to09)})
with(data_plots, {interaction.plot(vegetation_type, milpa, ba_trees10to14)
                  interaction.plot(milpa, harvested, ba_trees10to14)
                  interaction.plot(vegetation_type, harvested, ba_trees10to14)})
with(data_plots, {interaction.plot(vegetation_type, milpa, ba_trees15to19)
                  interaction.plot(milpa, harvested, ba_trees15to19)
                  interaction.plot(vegetation_type, harvested, ba_trees15to19)})
with(data_plots, {interaction.plot(vegetation_type, milpa, ba_trees20plus)
                  interaction.plot(milpa, harvested, ba_trees20plus)
                  interaction.plot(vegetation_type, harvested, ba_trees20plus)})



### Models

## Seedlings (seedling BA = 0 by definition, so there is no BA model for seedlings)



## Saplings

# Fit full model
mod.2d.saplings <- lm(ba_saplings ~ harvested + vegetation_type + milpa +
                        harvested:vegetation_type + harvested:vegetation_type, data = data_plots)

# Check constant variance and normality assumptions
plot(mod.2d.saplings, which = 1); plot(mod.2d.saplings, which = 2)

# Very distinct megaphone shape on the residuals vs. fitted; try Box-Cox
MASS::boxcox(mod.2d.saplings)

# Box-Cox strongly suggests log transformation; re-fit model with logged response
mod.2d.saplings.transform <- lm(log(ba_saplings) ~ harvested + vegetation_type + milpa +
                                  harvested:vegetation_type + harvested:vegetation_type, data = data_plots)

# Check constant variance assumption again
plot(mod.2d.saplings.transform, which = 1); plot(mod.2d.saplings.transform, which = 2)

# Assumptions look significantly better, so we will proceed with the transformed model
# Check for outliers
outlierTest(mod.2d.saplings.transform)

# No significant outliers; look at model summary
summary(mod.2d.saplings.transform)

# Construct ANOVA table (use type 2 SS since data are unbalanced)
Anova(mod.2d.saplings.transform, type = 2)

# We find that vegetation_type is significant, but harvested, milpa, and the interaction terms are not

# Pairwise comparison for vegetation_type using Tukey HSD
TukeyHSD(aov(mod.2d.saplings.transform), "vegetation_type", conv.level = 0.95)
# Nuku'uch che' has a significantly lower sapling BA than Ju'uche' and Keelenche'

# Confirm lack of significance of harvested using Tukey HSD
TukeyHSD(aov(mod.2d.saplings.transform), "harvested", conv.level = 0.95)



## 5-9 cm trees

# Fit full model
mod.2d.05to09 <- lm(ba_trees05to09 ~ harvested + vegetation_type + milpa +
                    harvested:vegetation_type + harvested:milpa, data = data_plots)

# Check constant variance and normality assumptions
plot(mod.2d.05to09, which = 1); plot(mod.2d.05to09, which = 2)

# Very distinct megaphone shape on the residuals vs. fitted; try Box-Cox
MASS::boxcox(mod.2d.05to09)

# Box-Cox strongly suggests log transformation; re-fit model with logged response
mod.2d.05to09.transform <- lm(log(ba_trees05to09) ~ harvested + vegetation_type + milpa +
                                harvested:vegetation_type + harvested:milpa, data = data_plots)

# Check constant variance assumption again
plot(mod.2d.05to09.transform, which = 1); plot(mod.2d.05to09.transform, which = 2)

# Assumptions look significantly better, so we will proceed with the transformed model
# Check for outliers
outlierTest(mod.2d.05to09.transform)

# No significant outliers; look at model summary
summary(mod.2d.05to09.transform)

# Construct ANOVA table (use type 2 SS since data are unbalanced)
Anova(mod.2d.05to09.transform, type = 2)

# We find that vegetation_type is significant, but harvested, milpa, and the interaction terms are not

# Pairwise comparison for vegetation_type using Tukey HSD
TukeyHSD(aov(mod.2d.05to09.transform), "vegetation_type", conf.level = 0.95)
# Keelenche' has a significantly higher 5-9 cm tree BA than Nuku'uch che'

# Confirm lack of significance of harvested using Tukey HSD
TukeyHSD(aov(mod.2d.05to09.transform), "harvested", conf.level = 0.95)



## 10-14 cm trees

# Fit full model
mod.2d.10to14 <- lm(ba_trees10to14 ~ harvested + vegetation_type + milpa +
                      harvested:vegetation_type + harvested:milpa, data = data_plots)

# Check constant variance and normality assumptions
plot(mod.2d.10to14, which = 1); plot(mod.2d.10to14, which = 2)

# Assumptions appear to be satisfied
# Check for outliers
outlierTest(mod.2d.10to14)

# No significant outliers; look at model summary
summary(mod.2d.10to14)

# Construct ANOVA table (use type 2 SS since data are unbalanced)
Anova(mod.2d.10to14, type = 2)

# We find that vegetation_type is significant, but harvested, milpa, and the interaction terms are not

# Pairwise comparison for vegetation_type using Tukey HSD
TukeyHSD(aov(mod.2d.10to14), "vegetation_type", conf.level = 0.95)
# Keelenche' has a significantly higher 10-14 cm tree BA than Nuku'uch che'

# Confirm lack of significance of harvested using Tukey HSD
TukeyHSD(aov(mod.2d.10to14), "harvested", conf.level = 0.95)



## 15-19 cm trees

# Fit full model
mod.2d.15to19 <- lm(ba_trees15to19 ~ harvested + vegetation_type + milpa +
                      harvested:vegetation_type + harvested:milpa, data = data_plots)

# Check constant variance and normality assumptions
plot(mod.2d.15to19, which = 1); plot(mod.2d.15to19, which = 2)

# Assumptions appear to be satisfied
# Check for outliers
outlierTest(mod.2d.15to19)

# No significant outliers; look at model summary
summary(mod.2d.15to19)
# R^2 is low

# Construct ANOVA table (use type 2 SS since data are unbalanced)
Anova(mod.2d.15to19, type = 2)

# We find that no predictors are significant

# Confirm lack of significance of harvested using Tukey HSD
TukeyHSD(aov(mod.2d.15to19), "harvested", conf.level = 0.95)



## 20+ cm trees

# Fit full model
mod.2d.20plus <- lm(ba_trees20plus ~ harvested + vegetation_type + milpa +
                      harvested:vegetation_type + harvested:milpa, data = data_plots)

# Check constant variance and normality assumptions
plot(mod.2d.20plus, which = 1); plot(mod.2d.20plus, which = 2)

# There is a slight megaphone shape on the residuals vs. fitted. We can't do a log transformation since the response has zeros.
# Try square root transformation
mod.2d.20plus.transform <- lm(sqrt(ba_trees20plus) ~ harvested + vegetation_type + milpa +
                                harvested:vegetation_type + harvested:milpa, data = data_plots)

# Check constant variance assumption again
plot(mod.2d.20plus.transform, which = 1); plot(mod.2d.20plus.transform, which = 2)

# No substantial improvement in assumptions, so we will proceed with the untransformed model
# Check for outliers
outlierTest(mod.2d.20plus)

# No significant outliers; look at model summary
summary(mod.2d.20plus)
# R^2 is relatively low

# Construct ANOVA table (use type 2 SS since data are unbalanced)
Anova(mod.2d.20plus, type = 2)

# We find that vegetation_type and milpa are significant, but harvested and the interaction terms are not

# Pairwise comparison for vegetation_type using Tukey HSD
TukeyHSD(aov(mod.2d.20plus), "vegetation_type", conf.level = 0.95)
# Nuku'uch che' has significantly higher 20+ cm tree BA than ju'uche' and keelenche';
# no significant difference between the latter two

# Pairwise comparison for milpa using Tukey HSD
TukeyHSD(aov(mod.2d.20plus), "milpa", conf.level = 0.95)
# Plots that have been milpa have significantly higher 20+ cm tree BA

# Confirm lack of significance of harvested using Tukey HSD
TukeyHSD(aov(mod.2d.20plus), "harvested", conf.level = 0.95)
############################################################################
