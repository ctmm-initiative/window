# Define S4 class
setClass(
  "TS",
  slots = list(
    dataframe = "data.frame",  # Slot for storing the data
    variable = "character"     # Slot for the variable name
  )
)

# Define the plot method for the TS class
setMethod(
  "plot", 
  "TS",
  function(x, col = "black", ylab = "", ...) {
    # Extract the data and variable from the TS object
    data <- x@dataframe
    variable <- x@variable
    
    # Handle X-axis (time)
    t <- data$timestamp
    
    # Handle Y-axis
    CI <- c(data$point_estimate, data$CI_low, data$CI_high)  # Combine columns for dimfig
    dimfig_output <- dimfig(data = CI, dimension = variable)  # Perform unit conversion
    units_label <- dimfig_output$units[2]  # Use the short unit label for units
    ylab <- paste(variable, "(", units_label, ")", sep = "")
    
    # Update dataframe values to converted units
    data$point_estimate <- dimfig_output$data[1:nrow(data)]
    data$CI_low <- dimfig_output$data[(nrow(data) + 1):(2 * nrow(data))]
    data$CI_high <- dimfig_output$data[(2 * nrow(data) + 1):(3 * nrow(data))]
    
    # Extract the columns for plotting
    CI <- data[, c("point_estimate", "CI_low", "CI_high")]
    MAX <- max(CI$point_estimate, na.rm = TRUE)
    MIN <- min(CI$point_estimate, na.rm = TRUE)
    DIFF <- MAX - MIN
    ylim <- c(max(MIN - DIFF, min(CI$CI_low, na.rm = TRUE)),
              min(MAX + DIFF, max(CI$CI_high, na.rm = TRUE)))
    
    # Plot the central estimate and Confidence Intervals
    graphics::plot(t, data$point_estimate, type = "l", col = col, ylim = ylim,
                   xlab = "Time", ylab = ylab, ...)
    graphics::lines(t, data$CI_low, col = adjustcolor(col, alpha.f = 0.3), lty = 2, ...)
    graphics::lines(t, data$CI_high, col = adjustcolor(col, alpha.f = 0.3), lty = 2, ...)
  }
)
