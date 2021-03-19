#' @title run_analysis
#' @author Marcos Alves \email{mppalves@gmail.com}
#' @description Run Analysis take as input two sets of values (input and output
#'  data) and run a series of analysis outputing statistics and graphs to check
#'  how well the two datasets fit each other.
#' @param output_data Model predictions
#' @param input_data Original data
#' @param title String used to name the plots and the exported files
#' @param cor Hexadecimal or color name used to color details in plots
#' @param unit unit measure for the output
#' @param comment Comment to printed in the .txt statistical file results
#' @export run_analysis
#' @import stringr
#'
#' @import utils
#' @import ggplot2
#' @export

run_analysis <- function(output_data, input_data, title, cor, unit, comment = NULL) {

  # comparing results
  ttest <- t.test(input_data, output_data, paired = F, var.equal = T)
  correlation <- cor(input_data, output_data)
  Residuals <- output_data - input_data
  variances <- var.test(output_data, input_data)

  # Overlaid histograms
  a <- data.frame(input_data)
  b <- data.frame(output_data)
  a$Distribution <- "Original"
  b$Distribution <- "Predicted"
  colnames(a)[1] <- "Output"
  colnames(b)[1] <- "Output"
  dat <- rbind(a, b)

  size <- max(input_data) * 0.02

  # Initiating magritte variables
  Output <- NULL
  Distribution <- NULL

  histo1 <- ggplot(dat, aes(x = Output, fill = Distribution)) +
    geom_histogram(binwidth = size, alpha = .5, colour = "#595959", position = "identity") +
    # facet_grid(Distribution ~ .) +
    theme(text = element_text(size = 18)) +
    # coord_cartesian(xlim = c(0,125000)) +
    xlab("Output") +
    ylab("Density")

  print(histo1)

  ggsave(paste0(title, "_histogram_", str_remove(str_remove(Sys.time(), ":"), ":"), ".pdf"),
    width = 22,
    height = 15,
    units = "cm",
    limitsize = TRUE
  )

  histo2 <- ggplot(as.data.frame(Residuals), aes(x = Residuals)) +
    geom_histogram(binwidth = size, alpha = .5, colour = "#595959") +
    theme(text = element_text(size = 18)) +
    xlab("Residuals") +
    ylab("Density")

  print(histo2)

  ggsave(paste0(title, "_residuals_", str_remove(str_remove(Sys.time(), ":"), ":"), ".pdf"),
    width = 15,
    height = 15,
    units = "cm",
    limitsize = TRUE
  )

  # plotting with ggplot and adding a density function
  ggplot_dataset <- data.frame(input_data, output_data)
  index_sample <- sample(nrow(ggplot_dataset), nrow(ggplot_dataset) * 0.3)
  ggplot_dataset <- ggplot_dataset[index_sample, ]
  scatter <- ggplot() +
    geom_point(aes(x = ggplot_dataset[,1], y = ggplot_dataset[,2]), size = 0.1, stroke = 0, shape = 16, alpha = 0.7) +
    ylab(paste0("Predicted output", unit)) +
    xlab(paste0("Original output", unit)) +
    ggtitle(paste0(title), subtitle = "Output correlation between Orginal and Predicted") +
    geom_density_2d(alpha = 0.7, colour = cor) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
    # coord_cartesian(xlim = c(0,125000), ylim = c(0,125000)) +
    theme(text = element_text(size = 18))


  print(scatter)

  ggsave(paste0(title, "_dispersion_", str_remove(str_remove(Sys.time(), ":"), ":"), ".pdf"),
    width = 15,
    height = 15,
    units = "cm",
    limitsize = F
  )


  if (!is.null(comment)) {
    x <- list("t test" = ttest, "correlation" = correlation, "Variances" = variances, "function" = comment)
    y <- capture.output(x)
    writeLines(y, con = file(paste0(title, "_statistical_analysis", str_remove(str_remove(Sys.time(), ":"), ":"), ".txt")))
    return(x)
  } else {
    x <- list("t test" = ttest, "correlation" = correlation, "Variances" = variances)
    y <- capture.output(x)
    writeLines(y, con = file(paste0(title, "_statistical_analysis_", str_remove(str_remove(Sys.time(), ":"), ":"), ".txt")))

    return(x)
  }
}
