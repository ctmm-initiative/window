# window
This project introduces a sliding window analysis function for the `ctmm` R package.

## Features
- Flexible sliding window analysis for time-series data.
- Extracts point estimates and uncertainty measures for selected variables.
- Customizable window size and time step options.
- Visualization tools for intuitive time-series plots.
- Compatible with the `ctmm` package's existing framework.

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
- data (tracking data of ctmm class)
- variable (calculates parameter estimate : "area", "diffusion", "velocity")
- dt.min (minimum time step between Time series windows as difftime object)
- window (window size as difftime object)
- select (More rigorous method for selecting movement model by default. For faster modeling fitting make select = FALSE)
- recycle (Option to use the previous model fit as a starting point for the next model fitting process in the timeseries)

### Usage
Create an object of class TS using the animal tracking data, and then use plot() to visualize the TS object
```r
library(ctmm)
  # Download Data
data(buffalo)
Data <- buffalo$Cilla

  # Create TS (Timeseries) object using sliding_window()
min_time_step <- as.difftime(10, units = "days")
window <- as.difftime(10, units = "days")
results <- sliding_window(data = Data, window = window, dt.min = min_time_step, recycle = TRUE)
  #plot results
plot(results)
```
## Acknowledgments
- This project was developed by Michael Garan under the guidance of Dr. Christen Fleming.

## Contact
For questions or feedback, contact:
Michael Garan  
Email: michael.d.garan@gmail.com
