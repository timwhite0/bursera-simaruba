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
### 2A: STEM DENSITY BY SIZE CLASS (FIGURES)

### Average stem density by size class

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
              select(stemden_seedlings, stemden_saplings,
                     starts_with("stemden_trees"))
unharvested <- data_plots %>%
                filter(harvested == "no") %>%
                select(stemden_seedlings, stemden_saplings,
                       starts_with("stemden_trees"))

set.seed(1)
boot_intervals <- bind_rows(compute_bootstrap_interval(unharvested$stemden_seedlings),
                            compute_bootstrap_interval(unharvested$stemden_saplings),
                            compute_bootstrap_interval(unharvested$stemden_trees05to09),
                            compute_bootstrap_interval(unharvested$stemden_trees10to14),
                            compute_bootstrap_interval(unharvested$stemden_trees15to19),
                            compute_bootstrap_interval(unharvested$stemden_trees20plus),
                            compute_bootstrap_interval(harvested$stemden_seedlings),
                            compute_bootstrap_interval(harvested$stemden_saplings),
                            compute_bootstrap_interval(harvested$stemden_trees05to09),
                            compute_bootstrap_interval(harvested$stemden_trees10to14),
                            compute_bootstrap_interval(harvested$stemden_trees15to19),
                            compute_bootstrap_interval(harvested$stemden_trees20plus))

stemden_mean_data <- data_plots %>%
                      select(harvested, stemden_seedlings,
                             stemden_saplings, starts_with("stemden_trees")) %>%
                      group_by(harvested) %>%
                      summarize_all(mean) %>%
                      pivot_longer(cols = starts_with("stemden"), names_prefix = "stemden_",
                                   names_to = "size_class", values_to = "mean") %>%
                      bind_cols(boot_intervals) %>%
                      select(harvested, size_class, lower = `2.5%`, mean, upper = `97.5%`) %>%
                      mutate(size_class = as.factor(size_class)) %>%
                      mutate(harvested = fct_recode(harvested,
                                                    "Harvested" = "yes", "Unharvested" = "no"))
stemden_mean_data_noseedlings <- stemden_mean_data %>%
                                  filter(size_class != "seedlings") %>%
                                  mutate(size_class = fct_recode(size_class,
                                                                 "Saplings (0-4 cm DBH)" = "saplings",
                                                                 "Trees (5-9 cm DBH)" = "trees05to09",
                                                                 "Trees (10-14 cm DBH)" = "trees10to14",
                                                                 "Trees (15-19 cm DBH)" = "trees15to19",
                                                                 "Trees (20+ cm DBH)" = "trees20plus"))
stemden_mean_data_seedlings <- stemden_mean_data %>%
                                filter(size_class == "seedlings") %>%
                                mutate(size_class = fct_recode(size_class,
                                                               "Seedlings (0 cm DBH)" = "seedlings"))
  
# Point/line plot (all size classes except seedlings)
stemden_mean_data_noseedlings %>%
  ggplot() +
    geom_pointrange(aes(x = size_class, col = fct_relevel(harvested, "Harvested", "Unharvested"),
                        ymin = lower, y = mean, ymax = upper),
                    position = position_dodge(width = 0.5), size = 0.5, linewidth = 1) +
    plot_theme + theme(legend.position.inside = c(0.85, 0.85),
                       legend.title = element_text(hjust = 0.5)) +
    ylim(0, 800) +
    scale_color_brewer(palette = "Dark2") +
    labs(x = "Size class", y = "Average number of stems per hectare") +
    guides(col=guide_legend(title = "Plot status"))

# Point/line plot (seedlings only)
stemden_mean_data_seedlings %>%
  ggplot() +
  geom_pointrange(aes(x = size_class, col = fct_relevel(harvested, "Harvested", "Unharvested"),
                      ymin = lower, y = mean, ymax = upper),
                  position = position_dodge(width = 0.5), size = 0.5, linewidth = 1) +
  plot_theme + theme(legend.position.inside = c(0.85, 0.85),
                     legend.title = element_text(hjust = 0.5)) +
  ylim(0, 10000) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Size class", y = "Average number of stems per hectare") +
  guides(col=guide_legend(title = "Plot status"))

# Bar plot (all size classes except seedlings)
stemden_mean_data_noseedlings %>%
  ggplot(aes(x = size_class,
             fill = fct_relevel(harvested, "Harvested", "Unharvested"))) +
    geom_col(aes(y = mean),
             position = "dodge") +
    geom_errorbar(aes(x = size_class, ymin = lower, ymax = upper), col = "gray20",
                  position = position_dodge(width = 0.9), width = 0.2, show.legend = FALSE) +
    plot_theme + theme(legend.position.inside = c(0.85, 0.85),
                       legend.title = element_text(hjust = 0.5)) +
    ylim(0, 800) +
    scale_fill_brewer(palette = "Dark2") +
    labs(x = "Size class", y = "Average number of stems per hectare") +
    guides(fill=guide_legend(title = "Plot status"))

# Bar plot (seedlings only)
stemden_mean_data_seedlings %>%
  ggplot(aes(x = size_class,
             fill = fct_relevel(harvested, "Harvested", "Unharvested"))) +
  geom_col(aes(y = mean),
           position = "dodge") +
  geom_errorbar(aes(x = size_class, ymin = lower, ymax = upper), col = "gray20",
                position = position_dodge(width = 0.9), width = 0.2, show.legend = FALSE) +
  plot_theme + theme(legend.position.inside = c(0.85, 0.85),
                     legend.title = element_text(hjust = 0.5)) +
  ylim(0, 10000) +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Size class", y = "Average number of stems per hectare") +
  guides(fill=guide_legend(title = "Plot status"))



### Distribution of stem densities by size class

# Box plot (all size classes except seedlings)
stemden_dist_data <- data_plots %>%
                      select(harvested, stemden_seedlings,
                             stemden_saplings, starts_with("stemden_trees")) %>%
                      pivot_longer(cols = starts_with("stemden"),
                                   names_prefix = "stemden_",
                                   names_to = "size_class",
                                   values_to = "num_stems") %>%
                      mutate(size_class = as.factor(size_class)) %>%
                      mutate(harvested = fct_recode(harvested,
                                                    "Harvested" = "yes",
                                                    "Unharvested" = "no"))
stemden_dist_data_noseedlings <- stemden_dist_data %>%
                                    filter(size_class != "seedlings") %>%
                                    mutate(size_class = fct_recode(size_class,
                                                                   "Saplings (0-4 cm DBH)" = "saplings",
                                                                   "Trees (5-9 cm DBH)" = "trees05to09",
                                                                   "Trees (10-14 cm DBH)" = "trees10to14",
                                                                   "Trees (15-19 cm DBH)" = "trees15to19",
                                                                   "Trees (20+ cm DBH)" = "trees20plus"))
stemden_dist_data_seedlings <- stemden_dist_data %>%
                                filter(size_class == "seedlings") %>%
                                mutate(size_class = fct_recode(size_class,
                                                               "Seedlings (0 cm DBH)" = "seedlings"))


# Violin plot (all size classes except seedlings)
stemden_dist_data_noseedlings %>%
  ggplot() +
    geom_violin(aes(x = size_class, y = num_stems,
                    fill = fct_relevel(harvested, "Harvested", "Unharvested")),
                scale = "area", width = 2, position = position_dodge(width = 0.7),
                lwd = 0.75, kernel = "gaussian", adjust = 1.25) +
    plot_theme +
    scale_fill_brewer(palette = "Dark2") +
    coord_cartesian(ylim = c(0, 1500)) + # 2 outliers not shown
    labs(x = "Size class", y = "Number of stems per hectare") +
    guides(fill = guide_legend(title = "Plot status"))

# Violin plot (seedlings only)
stemden_dist_data_seedlings %>%
  ggplot() +
    geom_violin(aes(x = size_class, y = num_stems,
                    fill = fct_relevel(harvested, "Harvested", "Unharvested")),
                scale = "area", width = 0.5, position = position_dodge(width = 0.7),
                lwd = 0.75, kernel = "gaussian", adjust = 1.25) +
    plot_theme +
    scale_fill_brewer(palette = "Dark2") +
    labs(x = "Size class", y = "Number of stems per hectare") +
    guides(fill=guide_legend(title = "Plot status"))

# Box plot (all size classes except seedlings)
stemden_dist_data_noseedlings %>%
  ggplot() +
    geom_boxplot(aes(x = size_class, y = num_stems,
                     fill = fct_relevel(harvested, "Harvested", "Unharvested")),
                 outlier.shape = 21, outlier.color = "gray20",
                 outlier.fill = "gray80") +
    plot_theme +
    scale_fill_brewer(palette = "Dark2") +
    coord_cartesian(ylim = c(0, 1500)) + # 2 outliers not shown
    labs(x = "Size class", y = "Number of stems per hectare") +
    guides(fill = guide_legend(title = "Plot status"))

# Box plot (seedlings only)
stemden_dist_data_seedlings %>%
  ggplot() +
  geom_boxplot(aes(x = size_class, y = num_stems,
                   fill = fct_relevel(harvested, "Harvested", "Unharvested")),
               outlier.shape = 21, outlier.color = "gray20",
               outlier.fill = "gray80") +
  plot_theme +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Size class", y = "Number of stems per hectare") +
  guides(fill = guide_legend(title = "Plot status"))
############################################################################

############################################################################
### 2B: STEM DENSITY BY SIZE CLASS (MODELS)

# Histogram of # stems per ha by size class for all plots, harvested plots, unharvested plots
data_plots %>%
  ggplot() + geom_histogram(aes(x = stemden_seedlings), bins = 20,
                            col = "black", fill = "gray80") + plot_theme
data_plots %>%
  ggplot() + geom_histogram(aes(x = stemden_saplings), bins = 20,
                            col = "black", fill = "gray80") + plot_theme
data_plots %>%
  ggplot() + geom_histogram(aes(x = stemden_trees05to09), bins = 20,
                            col = "black", fill = "gray80") + plot_theme
data_plots %>%
  ggplot() + geom_histogram(aes(x = stemden_trees10to14), bins = 20,
                            col = "black", fill = "gray80") + plot_theme
data_plots %>%
  ggplot() + geom_histogram(aes(x = stemden_trees15to19), bins = 10,
                            col = "black", fill = "gray80") + plot_theme
data_plots %>%
  ggplot() + geom_histogram(aes(x = stemden_trees20plus), bins = 5,
                            col = "black", fill = "gray80") + plot_theme

# Five-number summary of # stems per ha by size class for all plots, harvested plots, unharvested plots
summary(data_plots$stemden_seedlings)
data_plots %>% filter(harvested == "yes") %>% pull(stemden_seedlings) %>% summary()
data_plots %>% filter(harvested == "no") %>% pull(stemden_seedlings) %>% summary()

summary(data_plots$stemden_saplings)
data_plots %>% filter(harvested == "yes") %>% pull(stemden_saplings) %>% summary()
data_plots %>% filter(harvested == "no") %>% pull(stemden_saplings) %>% summary()

summary(data_plots$stemden_trees05to09)
data_plots %>% filter(harvested == "yes") %>% pull(stemden_trees05to09) %>% summary()
data_plots %>% filter(harvested == "no") %>% pull(stemden_trees05to09) %>% summary()

summary(data_plots$stemden_trees10to14)
data_plots %>% filter(harvested == "yes") %>% pull(stemden_trees10to14) %>% summary()
data_plots %>% filter(harvested == "no") %>% pull(stemden_trees10to14) %>% summary()

summary(data_plots$stemden_trees15to19)
data_plots %>% filter(harvested == "yes") %>% pull(stemden_trees15to19) %>% summary()
data_plots %>% filter(harvested == "no") %>% pull(stemden_trees15to19) %>% summary()

summary(data_plots$stemden_trees20plus)
data_plots %>% filter(harvested == "yes") %>% pull(stemden_trees20plus) %>% summary()
data_plots %>% filter(harvested == "no") %>% pull(stemden_trees20plus) %>% summary()

# Box plot of # stems per ha by harvested for each size class
data_plots %>% ggplot() +
  geom_boxplot(aes(x = harvested, y = stemden_seedlings,
                   fill = harvested)) + plot_theme
data_plots %>% ggplot() +
  geom_boxplot(aes(x = harvested, y = stemden_saplings,
                   fill = harvested)) + plot_theme
data_plots %>% ggplot() +
  geom_boxplot(aes(x = harvested, y = stemden_trees05to09,
                   fill = harvested)) + plot_theme
data_plots %>% ggplot() +
  geom_boxplot(aes(x = harvested, y = stemden_trees10to14,
                   fill = harvested)) + plot_theme
data_plots %>% ggplot() +
  geom_boxplot(aes(x = harvested, y = stemden_trees15to19,
                   fill = harvested)) + plot_theme
data_plots %>% ggplot() +
  geom_boxplot(aes(x = harvested, y = stemden_trees20plus,
                   fill = harvested)) + plot_theme

# Box plot of # stems per ha by vegetation type for each size class
data_plots %>% ggplot() +
    geom_boxplot(aes(x = vegetation_type, y = stemden_seedlings,
                     fill = vegetation_type)) + plot_theme
data_plots %>% ggplot() +
  geom_boxplot(aes(x = vegetation_type, y = stemden_saplings,
                   fill = vegetation_type)) + plot_theme
data_plots %>% ggplot() +
  geom_boxplot(aes(x = vegetation_type, y = stemden_trees05to09,
                   fill = vegetation_type)) + plot_theme
data_plots %>% ggplot() +
  geom_boxplot(aes(x = vegetation_type, y = stemden_trees10to14,
                   fill = vegetation_type)) + plot_theme
data_plots %>% ggplot() +
  geom_boxplot(aes(x = vegetation_type, y = stemden_trees15to19,
                   fill = vegetation_type)) + plot_theme
data_plots %>% ggplot() +
  geom_boxplot(aes(x = vegetation_type, y = stemden_trees20plus,
                   fill = vegetation_type)) + plot_theme

# Box plot of # stems per ha by milpa for each size class
data_plots %>% ggplot() +
  geom_boxplot(aes(x = milpa, y = stemden_seedlings,
                   fill = milpa)) + plot_theme
data_plots %>% ggplot() +
  geom_boxplot(aes(x = milpa, y = stemden_saplings,
                   fill = milpa)) + plot_theme
data_plots %>% ggplot() +
  geom_boxplot(aes(x = milpa, y = stemden_trees05to09,
                   fill = milpa)) + plot_theme
data_plots %>% ggplot() +
  geom_boxplot(aes(x = milpa, y = stemden_trees10to14,
                   fill = milpa)) + plot_theme
data_plots %>% ggplot() +
  geom_boxplot(aes(x = milpa, y = stemden_trees15to19,
                   fill = milpa)) + plot_theme
data_plots %>% ggplot() +
  geom_boxplot(aes(x = milpa, y = stemden_trees20plus,
                   fill = milpa)) + plot_theme

# Interaction plots for each size class
with(data_plots, {interaction.plot(vegetation_type, milpa, stemden_seedlings)
                  interaction.plot(milpa, harvested, stemden_seedlings)
                  interaction.plot(vegetation_type, harvested, stemden_seedlings)})
with(data_plots, {interaction.plot(vegetation_type, milpa, stemden_saplings)
                  interaction.plot(milpa, harvested, stemden_saplings)
                  interaction.plot(vegetation_type, harvested, stemden_saplings)})
with(data_plots, {interaction.plot(vegetation_type, milpa, stemden_trees05to09)
                  interaction.plot(milpa, harvested, stemden_trees05to09)
                  interaction.plot(vegetation_type, harvested, stemden_trees05to09)})
with(data_plots, {interaction.plot(vegetation_type, milpa, stemden_trees10to14)
                  interaction.plot(milpa, harvested, stemden_trees10to14)
                  interaction.plot(vegetation_type, harvested, stemden_trees10to14)})
with(data_plots, {interaction.plot(vegetation_type, milpa, stemden_trees15to19)
                  interaction.plot(milpa, harvested, stemden_trees15to19)
                  interaction.plot(vegetation_type, harvested, stemden_trees15to19)})
with(data_plots, {interaction.plot(vegetation_type, milpa, stemden_trees20plus)
                  interaction.plot(milpa, harvested, stemden_trees20plus)
                  interaction.plot(vegetation_type, harvested, stemden_trees20plus)})



### Models

## Seedlings

# Fit full model
mod.2b.seedlings <- lm(stemden_seedlings ~ harvested + vegetation_type + milpa +
                         harvested:vegetation_type + harvested:milpa, data = data_plots)

# Check constant variance and normality assumptions
plot(mod.2b.seedlings, which = 1); plot(mod.2b.seedlings, which = 2)

# There is a megaphone shape on the residuals vs. fitted; try Box-Cox
MASS::boxcox(mod.2b.seedlings)

# Box-Cox suggests log transformation; re-fit with logged response
mod.2b.seedlings.transform <- lm(log(stemden_seedlings) ~ harvested + vegetation_type + milpa +
                                   harvested:vegetation_type + harvested:milpa, data = data_plots)

# Check constant variance assumption again
plot(mod.2b.seedlings.transform, which = 1); plot(mod.2b.seedlings.transform, which = 2)

# Constant variance and normality assumptions appear to be satisfied now; we proceed with the transformed model
# Check for outliers
outlierTest(mod.2b.seedlings.transform)

# No significant outliers; look at model summary
summary(mod.2b.seedlings.transform)
# R^2 is low, which suggests poor model fit

# Construct ANOVA table (use type 2 SS since data are unbalanced)
Anova(mod.2b.seedlings.transform, type = 2)

# We find that vegetation_type is significant, but harvested, milpa, and the interactions terms are not

# Pairwise comparison for vegetation_type using Tukey HSD
TukeyHSD(aov(mod.2b.seedlings.transform), "vegetation_type", conf.level = 0.95)
# Same conclusion as section 1B
# Keelenche' has a significantly higher seedling density then Nuku'uch che'

# Confirm lack of significance of harvested using Tukey HSD
TukeyHSD(aov(mod.2b.seedlings.transform), "harvested", conf.level = 0.95)



## Saplings

# Fit full model
mod.2b.saplings <- lm(stemden_saplings ~ harvested + vegetation_type + milpa +
                        harvested:vegetation_type + harvested:milpa, data = data_plots)

# Check constant variance and normality assumptions
plot(mod.2b.saplings, which = 1); plot(mod.2b.saplings, which = 2)

# Very distinct megaphone shape on the residuals vs. fitted; try Box-Cox
MASS::boxcox(mod.2b.saplings)

# Box-Cox strongly suggests log transformation
mod.2b.saplings.transform <- lm(log(stemden_saplings) ~ harvested + vegetation_type + milpa +
                                  harvested:vegetation_type + harvested:milpa, data = data_plots)

# Check constant variance assumption again
plot(mod.2b.saplings.transform, which = 1); plot(mod.2b.saplings.transform, which = 2)

# Assumptions look significantly better, so we will proceed with the transformed model
# Check for outliers
outlierTest(mod.2b.saplings.transform)

# No significant outliers; look at model summary
summary(mod.2b.saplings.transform)
# R^2 is relatively low

# Construct ANOVA table (use type 2 SS since data are unbalanced)
Anova(mod.2b.saplings.transform, type = 2)

# We find that vegetation_type and milpa are significant, but harvested and the interaction terms are not

# Pairwise comparison for vegetation_type using Tukey HSD
TukeyHSD(aov(mod.2b.saplings.transform), "vegetation_type", conf.level = 0.95)
# Nuku'uch che' has significantly lower sapling density than Ju'uche' and Keelenche'

# Pairwise comparison for milpa using Tukey HSD
TukeyHSD(aov(mod.2b.saplings.transform), "milpa", conf.level = 0.95)
# Plots that have been milpa have higher sapling density (note that the sign flips bc of the log transformation)

# Confirm lack of significance of harvested using Tukey HSD
TukeyHSD(aov(mod.2b.saplings.transform), "harvested", conf.level = 0.95)



## 5-9 cm trees

# Fit full model
mod.2b.05to09 <- lm(stemden_trees05to09 ~ harvested + vegetation_type + milpa +
                    harvested:vegetation_type + harvested:milpa, data = data_plots)

# Check constant variance and normality assumptions
plot(mod.2b.05to09, which = 1); plot(mod.2b.05to09, which = 2)

# Very distinct megaphone shape on the residuals vs. fitted; try Box-Cox
MASS::boxcox(mod.2b.05to09)

# Box-Cox suggests log transformation; re-fit model with logged response
mod.2b.05to09.transform <- lm(log(stemden_trees05to09) ~ harvested + vegetation_type + milpa +
                                harvested:vegetation_type + harvested:milpa, data = data_plots)

# Check constant variance assumption again
plot(mod.2b.05to09.transform, which = 1); plot(mod.2b.05to09.transform, which = 2)

# Assumptions look significantly better, so we will proceed with the transformed model
# Check for outliers
outlierTest(mod.2b.05to09.transform)

# No significant outliers; look at model summary
summary(mod.2b.05to09.transform)
# R^2 is low

# Construct ANOVA table (use type 2 SS since data are unbalanced)
Anova(mod.2b.05to09.transform, type = 2)

# We find that vegetation_type is significant, but harvested, milpa, and the interaction terms are not

# Pairwise comparison for vegetation_type using Tukey HSD
TukeyHSD(aov(mod.2b.05to09.transform), "vegetation_type", conf.level = 0.95)
# Keelenche' has a significantly higher 5-9 cm tree density then Nuku'uch che'

# Confirm lack of significance of Harvested using Tukey HSD
TukeyHSD(aov(mod.2b.05to09.transform), "harvested", conf.level = 0.95)



## 10-14 cm trees

# Fit full model
mod.2b.10to14 <- lm(stemden_trees10to14 ~ harvested + vegetation_type + milpa +
                      harvested:vegetation_type + harvested:milpa, data = data_plots)

# Check constant variance and normality assumptions
plot(mod.2b.10to14, which = 1); plot(mod.2b.10to14, which = 2)

# Assumptions appear to be satisfied
# Check for outliers
outlierTest(mod.2b.10to14)

# No significant outliers; look at model summary
summary(mod.2b.10to14)
# R^2 is low

# Construct ANOVA table (use type 2 SS since data are unbalanced)
Anova(mod.2b.10to14, type = 2)

# We find that vegetation_type is significant, but harvested, milpa, and the interaction terms are not

# Pairwise comparison for vegetation_type using Tukey HSD
TukeyHSD(aov(mod.2b.10to14), "vegetation_type", conf.level = 0.95)
# Keelenche' has a significantly higher 10-14 cm tree density then Nuku'uch che'

# Confirm lack of significance of harvested using Tukey HSD
TukeyHSD(aov(mod.2b.10to14), "harvested", conf.level = 0.95)



## 15-19 cm trees

# Fit full model
mod.2b.15to19 <- lm(stemden_trees15to19 ~ harvested + vegetation_type + milpa +
                      harvested:vegetation_type + harvested:milpa, data = data_plots)

# Check constant variance and normality assumptions
plot(mod.2b.15to19, which = 1); plot(mod.2b.15to19, which = 2)

# Constant variance and normality assumptions appear to be satisfied

# Look at model summary
summary(mod.2b.15to19)
# R^2 is low

# Construct ANOVA table (use type 2 SS since data are unbalanced)
Anova(mod.2b.15to19, type = 2)

# We find that no predictors are significant

# Confirm lack of significance of Harvested using Tukey HSD
TukeyHSD(aov(mod.2b.15to19), "harvested", conf.level = 0.95)



## 20+ cm trees

# Fit full model
mod.2b.20plus <- lm(stemden_trees20plus ~ harvested + vegetation_type + milpa +
                      harvested:vegetation_type + harvested:milpa, data = data_plots)

# Check constant variance and normality assumptions
plot(mod.2b.20plus, which = 1); plot(mod.2b.20plus, which = 2)

# There is a little bit of a megaphone shape on the residuals vs. fitted.
# We can't do a log transformation since the response has zeros.
# Try square root transformation
mod.2b.20plus.transform <- lm(sqrt(stemden_trees20plus) ~ harvested + vegetation_type + milpa +
                                harvested:vegetation_type + harvested:milpa, data = data_plots)

# Check constant variance assumption again
plot(mod.2b.20plus.transform, which = 1); plot(mod.2b.20plus.transform, which = 2)

# No substantial improvement in assumptions, so we will proceed with the untransformed model

# Look at model summary
summary(mod.2b.20plus)
# R^2 is relatively low

# Construct ANOVA table (use type 2 SS since data are unbalanced)
Anova(mod.2b.20plus, type = 2)

# We find that vegetation_type and milpa are significant, but Harvested and the interaction terms are not

# Pairwise comparison for vegetation_type using Tukey HSD
TukeyHSD(aov(mod.2b.20plus), "vegetation_type", conf.level = 0.95)
# Nuku'uch che' has significantly HIGHER 20+ cm tree density than ju'uche' and keelenche';
# no significant difference between the latter two
# This is different from previous significant vegetation_type results; for total stems (1B)
# and seedlings (2B), nuku'uch'che' had a significantly lower stem density than the other levels

# Pairwise comparison for milpa using Tukey HSD
TukeyHSD(aov(mod.2b.20plus), "milpa", conf.level = 0.95)
# Plots that have been milpa have a significantly higher 20+ cm tree density than plots that have not been milpa
# This is consistent with all earlier results involving Milpa

# Confirm lack of significance of harvested using Tukey HSD
TukeyHSD(aov(mod.2b.20plus), "harvested", conf.level = 0.95)
############################################################################
