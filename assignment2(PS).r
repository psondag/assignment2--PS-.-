###################Load the Data###################
library(readr)
penguins <- read_csv("~/Desktop/QAC380/penguins.csv")
View(penguins)

###################Descriptive Statistic 1#################
# Load dplyr for data manipulation
library(dplyr)

# Define a function to calculate mode (most frequent value)
get_mode <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}

# Group by species and calculate mean, median, and mode of body_mass_g
body_mass_summary <- penguins %>%
  group_by(species) %>%
  summarise(
    mean_body_mass = mean(body_mass_g, na.rm = TRUE),
    median_body_mass = median(body_mass_g, na.rm = TRUE),
    mode_body_mass = get_mode(body_mass_g)
  )

# View the summary table in RStudio
View(body_mass_summary)

# Save results to CSV
write_csv(body_mass_summary, "~/Desktop/QAC380/body_mass_summary_by_species.csv")

###################Descriptive Statistic 2###################
# Load dplyr for data manipulation
library(dplyr)

# Define a function to calculate mode (most frequent value)
get_mode <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}

# Group by species and calculate mean, median, and mode of bill_length_mm
bill_length_summary <- penguins %>%
  group_by(species) %>%
  summarise(
    mean_bill_length_mm = mean(bill_length_mm, na.rm = TRUE),
    median_bill_length_mm = median(bill_length_mm, na.rm = TRUE),
    mode_bill_length_mm = get_mode(bill_length_mm)
  )

# View the summary table in RStudio
View(bill_length_summary)

# Save results to CSV
write_csv(bill_length_summary, "~/Desktop/QAC380/body_mass_summary_by_species.csv")

