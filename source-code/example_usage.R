# Individual example dataset
data(buffalo)
individual_data <- buffalo$Cilla
single_data_GUESS <- ctmm.guess(individual_data, interactive = FALSE)

# Population example dataset
population_data <- buffalo[c(1, 3, 6)]
population_GUESS <- lapply(population_data, function(population_data) ctmm.guess(population_data, interactive = FALSE))

# Arguments
min_time_step <- as.difftime(10, units = "days")
window <- as.difftime(30, units = "days")

# Create TS of individual window estimates
individual_TS <- sliding_window(data = individual_data, CTMM = single_data_GUESS, window = window, dt.min = min_time_step, recycle = TRUE)

# Create TS of population window estimates
population_TS <- sliding_window(data = population_data, CTMM = population_GUESS, window = window, dt.min = min_time_step, recycle = TRUE)

# plot results
plot(population_TS)
plot(individual_TS)
