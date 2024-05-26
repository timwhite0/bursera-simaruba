############################################################################
### PACKAGES AND DATA
library(readxl)
library(car)
library(tidyverse)

orig_all_data <- read_excel("data/data_raw.xlsx", sheet = "All_data_Categories")
orig_plots_data <- read_excel("data/data_raw.xlsx", sheet = "Plots_data")
orig_seedlings_data <- read_excel("data/data_raw.xlsx", sheet = "Seedlings_cut_sprouting_togethe")
############################################################################

############################################################################
### STEM-LEVEL DATA
data_stems <- orig_all_data %>%
                # Remove unnecessary columns from orig_all_data
                # Note: Forest'sAge is removed because it is the same as VegetationType
                # Note: Artesanos is removed since we are not using it at all,
                #       and SoilType is removed per Florencia's request
                select(plot_id = `Plot #`,
                       dbh = `DBH (cm)`,
                       ba_per_stem = `BasalArea per tree m2 ha-1`,
                       harvested = Harvested,
                       vegetation_type = VegetationType,
                       milpa = `Milpa(has it been milpa)`) %>%
                # Add column for size class
                mutate(size_class = case_when(
                  dbh <= 4 ~ "sapling",
                  dbh <= 9 ~ "tree_05to09",
                  dbh <= 14 ~ "tree_10to14",
                  dbh <= 19 ~ "tree_15to19",
                  dbh >= 20 ~ "tree_20plus"
                )) %>%
                # Convert variables to factors
                mutate(plot_id = as.factor(plot_id),
                       harvested = as.factor(harvested),
                       vegetation_type = as.factor(vegetation_type),
                       milpa = as.factor(milpa),
                       size_class = as.factor(size_class)) %>%
                # Clean up factor levels
                mutate(harvested = fct_recode(harvested, "yes" = "Yes", "no" = "No"),
                       vegetation_type = fct_recode(vegetation_type,
                                                    "juuche" = "Ju'uche'",
                                                    "keelenche" = "Keelenche'",
                                                    "nukuuchche" = "Nuku'uch che'")) %>%
                # Change the 167 "Maybe" cases for milpa to "No"
                mutate(milpa = fct_collapse(milpa,
                                            yes = c("Yes"),
                                            no = c("Maybe", "No"))) %>%
                # Reorder columns
                select(plot_id, dbh, ba_per_stem, size_class, everything())

# Write to csv
write_csv(data_stems, "data/data_stems.csv")
############################################################################

############################################################################
### PLOT-LEVEL DATA
data_plots <- orig_plots_data %>%
                # Remove unnecessary columns from orig_plots_data
                # Note: Forest'sAge is removed because it is the same as VegetationType
                # Note: Artesanos is removed since we are not using it at all,
                #       and SoilType is removed per Florencia's request
                select(plot_id = `Plot #ID`,
                       ba_total = `Basal Area (m2/ha-1)`,
                       harvested = Harvested,
                       vegetation_type = VegetationType,
                       milpa = `Milpa(has it been milpa)`) %>%
                # Convert variables to factors
                mutate(plot_id = as.factor(plot_id),
                       harvested = as.factor(harvested),
                       vegetation_type = as.factor(vegetation_type),
                       milpa = as.factor(milpa)) %>%
                # Clean up factor levels
                mutate(harvested = fct_recode(harvested, "yes" = "Yes", "no" = "No"),
                       vegetation_type = fct_recode(vegetation_type,
                                                    "juuche" = "Ju'uche'",
                                                    "keelenche" = "Keelenche'",
                                                    "nukuuchche" = "Nuku'uch che'")) %>%
                # Change the 167 "Maybe" cases for milpa to "No"
                mutate(milpa = fct_collapse(milpa,
                                            yes = c("Yes"),
                                            no = c("Maybe", "No")))

# Create variables to store basal area (m2/ha) and stem density (stems per ha) for each size class
ba_by_size_class <- data_stems %>%
                      group_by(plot_id, size_class) %>%
                      summarize(ba = sum(ba_per_stem), .groups = "drop") %>%
                      pivot_wider(names_from = size_class,
                                  values_from = ba, values_fill = 0) %>%
                      mutate(ba_seedling = 0) %>%
                      select(plot_id, ba_seedling,
                             ba_sapling = sapling,
                             ba_tree_05to09 = tree_05to09,
                             ba_tree_10to14 = tree_10to14,
                             ba_tree_15to19 = tree_15to19,
                             ba_tree_20plus = tree_20plus)

stemden_by_size_class <- data_stems %>%
                          count(plot_id, size_class) %>%
                          # For all size classes except seedlings (which is already per ha),
                          # we multiply by 10000/(100*pi) to convert from (# of stems per
                          # plot) to (# of stems per ha)
                          mutate(n = (10000/(100*pi))*n) %>%
                          pivot_wider(names_from = size_class,
                                      values_from = n, values_fill = 0) %>%
                          mutate(stemden_seedlings = orig_seedlings_data$`Seedlings per Ha`,
                                 stemden_totaltrees = tree_05to09 + tree_10to14 +
                                                        tree_15to19 + tree_20plus) %>%
                          select(plot_id, stemden_seedlings, stemden_totaltrees,
                                 stemden_saplings = sapling,
                                 stemden_trees05to09 = tree_05to09,
                                 stemden_trees10to14 = tree_10to14,
                                 stemden_trees15to19 = tree_15to19,
                                 stemden_trees20plus = tree_20plus)

data_plots <- data_plots %>%
                left_join(ba_by_size_class, by = "plot_id") %>%
                left_join(stemden_by_size_class, by = "plot_id") %>%
                # Total BA for each plot should not include sapling BA
                mutate(ba_total = ba_total - ba_sapling) %>%
                select(plot_id, harvested, vegetation_type, milpa,
                       starts_with("ba"), starts_with("stemden"))


# Check to make sure Milpa is consistent between data_stems and data_plots
rbind(table(data_stems$milpa, data_stems$plot_id),
      table(data_plots$milpa, data_plots$plot_id))

# Check to make sure that for each plot, total BA = sum of BA of all size
# classes except seedlings and saplings
# FALSE if no issues; TRUE if there is at least one issue
any(abs(round(data_plots$ba_total -
                data_plots$ba_tree_05to09 -
                data_plots$ba_tree_10to14 - 
                data_plots$ba_tree_15to19 -
                data_plots$ba_tree_20plus, 3)) > 0)

# Check to make sure that for each plot, # total stems = sum of # of stems
# of all size classes except seedlings and saplings
# FALSE if no issues; TRUE if there is at least one issue
any(abs(round(data_plots$stemden_totaltrees -
                data_plots$stemden_trees05to09 -
                data_plots$stemden_trees10to14 - 
                data_plots$stemden_trees15to19 -
                data_plots$stemden_trees20plus, 3)) > 0)

# Write to csv
write_csv(data_plots, "data/data_plots.csv")
############################################################################
