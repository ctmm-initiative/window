# Define S4 class with dimensional map and optional covariate slot
setClass(
  "TS",
  slots = list(
    dataframe = "data.frame",  # Main data frame for tracking data
    variable = "character",    # Variable of interest (e.g., "speed")
    dimension_map = "list",    # Mapping for dimension conversion (time, lat, lon, etc.)
    covariate = "numeric"      # New slot for storing covariate (e.g., temperature)
  ),
  prototype = list(
    dimension_map = list(
      "area" = "area",
      "speed" = "speed",          
      "position" = "position",
      "diffusion" = "diffusion",
      "velocity" = "velocity",
      "tau position" = "time",
      "tau velocity" = "time"
    ),
    covariate = NA_real_  # Default empty covariate (temperature, etc.)
  )
)

# Define the plot method for the TS class with covariate
setMethod(
  "plot", 
  "TS",
  function(x, col = "black", cov_col = "red", ylab = "", ...) {
    # Extract the data and variable from the TS object
    data <- x@dataframe
    variable <- x@variable
    covariate <- x@covariate
    
    # Handle X-axis (time)
    t <- data$timestamp
    
    # Ensure valid dimension mapping
    dimension_type <- x@dimension_map[[variable]]
    if (is.null(dimension_type)) {
      message(paste("Warning: No dimension mapping found for", variable, "- Defaulting to 'length'"))
      dimension_type <- "length"
    }
    
    # Handle Y-axis for movement variable
    CI <- c(data$point_estimate, data$CI_low, data$CI_high)
    dimfig_output <- dimfig(data = CI, dimension = dimension_type)
    units_label <- dimfig_output$units[2]
    ylab <- paste0(toupper(substr(variable, 1, 1)), substr(variable, 2, nchar(variable)), " (", units_label, ")")
    
    # Update values to converted units
    data$point_estimate <- dimfig_output$data[1:nrow(data)]
    data$CI_low <- dimfig_output$data[(nrow(data) + 1):(2 * nrow(data))]
    data$CI_high <- dimfig_output$data[(2 * nrow(data) + 1):(3 * nrow(data))]
    
    # Plot range
    CI_vals <- data[, c("point_estimate", "CI_low", "CI_high")]
    MAX <- max(CI_vals$point_estimate, na.rm = TRUE)
    MIN <- min(CI_vals$point_estimate, na.rm = TRUE)
    DIFF <- MAX - MIN
    ylim <- c(max(MIN - DIFF, min(CI_vals$CI_low, na.rm = TRUE)),
              min(MAX + DIFF, max(CI_vals$CI_high, na.rm = TRUE)))
    
    # Generate better x-axis ticks using pretty()
    x_ticks <- pretty(t, n = 8)
    
    # Main plot without default x-axis
    graphics::plot(t, data$point_estimate, type = "l", col = col, ylim = ylim,
                   xlab = "Time", ylab = ylab, xaxt = "n", ...)
    
    # Add custom x-axis
    axis(1, at = x_ticks, labels = format(x_ticks, "%b %d"), las = 2)
    
    # Confidence interval lines
    graphics::lines(t, data$CI_low, col = adjustcolor(col, alpha.f = 0.3), lty = 2, ...)
    graphics::lines(t, data$CI_high, col = adjustcolor(col, alpha.f = 0.3), lty = 2, ...)
    
    # Plot covariate (if present)
    if (!all(is.na(covariate))) {
      cov_max <- max(covariate, na.rm = TRUE)
      cov_min <- min(covariate, na.rm = TRUE)
      cov_diff <- cov_max - cov_min
      cov_ylimits <- c(cov_min - 0.1 * cov_diff, cov_max + 0.1 * cov_diff)
      
      par(new = TRUE)
      plot(t, covariate, type = "l", col = cov_col, axes = FALSE, ylim = cov_ylimits, 
           xlab = "", ylab = "", ...)
      
      axis(4, at = seq(cov_min, cov_max, length.out = 5), col = cov_col, col.axis = cov_col)
      mtext("Covariate (e.g., Temperature)", side = 4, line = 3, col = cov_col)
    }
  }
)
