library(ctmm)

# Download example data
data(buffalo)

# Individual example dataset
individual_data <- buffalo$Cilla
individual_GUESS <- ctmm.guess(individual_data, interactive = FALSE)

# Population example dataset
population_data <- buffalo[c(1, 3, 6)]
population_GUESS <- lapply(population_data, function(population_data) ctmm.guess(population_data, interactive = FALSE))

# Arguments
min_time_step <- as.difftime(10, units = "days")
window <- as.difftime(30, units = "days")

# Create TS of individual window estimates
individual_TS <- slide(data = individual_data, CTMM = individual_GUESS, window = window, dt.min = min_time_step, recycle = TRUE)

# Create TS of population window estimates
population_TS <- slide(data = population_data, CTMM = population_GUESS, window = window, dt.min = min_time_step, recycle = TRUE)

 # plot results
plot(population_TS)
plot(individual_TS)
