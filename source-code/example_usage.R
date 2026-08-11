# Install and load ctmm package
remotes::install_github("ctmm-initiative/ctmm")
library(ctmm)

# Run source-code scripts.
# Use base function source() by pointing to the file location of the downloaded R scripts 
# or manually run through each line in all three scripts instead.
source("main.R")
source("TS_class.R")
source("window_intervals.R")

# Download example data
data(buffalo)

# Arguments
# This is chosen depending on the scale of the trends and data. 
# Choose a timestep and window width that best captures the target trend while smoothing.
min_time_step <- as.difftime(10, units = "days")
window <- as.difftime(30, units = "days")

# Individual example dataset
individual_data <- buffalo$Cilla
individual_GUESS <- ctmm.guess(individual_data, interactive = FALSE)
  # Create TS of individual window estimates
individual_TS <- slide(data = individual_data, CTMM = individual_GUESS, window = window, dt.min = min_time_step, recycle = TRUE, variable = "velocity", max_windows = 6)
  # Plot results
plot(individual_TS)

# Population example dataset
population_data <- buffalo
population_GUESS <- lapply(population_data, function(population_data) ctmm.guess(population_data, interactive = FALSE))
  # Create TS of population window estimates
population_TS <- slide(data = population_data, CTMM = population_GUESS, window = window, dt.min = min_time_step, recycle = TRUE, max_windows = 6)
  # Plot results
plot(population_TS)

# Examine trends since release/since tagging
population_TS_release <- slide(data = population_data, CTMM = population_GUESS, window = window, dt.min = min_time_step, recycle = TRUE, max_windows = 6, release = TRUE)
plot(population_TS_release)
