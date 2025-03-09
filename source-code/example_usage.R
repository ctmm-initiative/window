library(ctmm)

# Download example data
data(buffalo)

# Arguments
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
population_data <- buffalo[c(1,3,6)]
population_GUESS <- lapply(population_data, function(population_data) ctmm.guess(population_data, interactive = FALSE))
  # Create TS of population window estimates
population_TS <- slide(data = population_data, CTMM = population_GUESS, window = window, dt.min = min_time_step, recycle = TRUE, max_windows = 6)
  # Plot results
plot(population_TS)
