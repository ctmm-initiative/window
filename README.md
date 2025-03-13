# window
This project introduces a sliding window analysis function for the `ctmm` R package.

## Features
- Custom time-series object class for sliding window analysis of animal tracking data
- Extracts point estimates and confidence intervals for selected variable estimates
- Customizable window size and time step options
- Flexible functionality for individual and population level estimates
- Compatible with the `ctmm` package's existing framework
- Plot point estimats, confidence intervals and (optional) covariates to visualize correlation

## Getting Started

### Prerequisites
- R
- The `ctmm` package installed.

### Installation
Clone the repository and set up the environment:
```bash
git clone https://github.com/ctmm-initiative/window.git
cd window
```
### Arguments
- data: tracking data of ctmm class
- CTMM: Guess model used as a starting point for the model fitting process. Use ctmm.guess(data)
- variable: calculates parameter estimate : "area", "diffusion", "speed, "velocity", "position"
- dt.min: minimum time step between Time series windows as difftime object
- window: window size as difftime object
- max_windows: Option for setting a limit to window estimates calculated for the time series (TS)
- select: More rigorous method for selecting movement model by default. For faster modeling fitting make select = FALSE
- recycle: Option to use the previous model fit as a starting point for the next model fitting process in the timeseries
- Guassian: Option when estimating speed parameter (Default set to FALSE)
- covariate: Option for including covariate in the TS object class and plotting along parameter estimate (name of column)

### Usage
Create an object of class TS using the animal tracking data, and then use plot() to visualize the TS object
```r
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

```
## Acknowledgments
- This project was developed by Michael Garan under the guidance of Dr. Christen Fleming.

## Contact
For questions or feedback, contact:
Michael Garan  
Email: michael.d.garan@gmail.com
