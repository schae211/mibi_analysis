plot_performance <- function(improvements, name, low = 0.7, metric = "R2") {
  gains <- improvements %>%
    dplyr::arrange(target) %>%
    dplyr::filter(measure == paste0("gain.", metric))
  
  plot <- improvements %>%
    dplyr::filter(measure %in% c(paste0("intra.", metric), paste0("multi.", metric))) %>%
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(x=target, y=value, col=measure)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) 
  
  y = rep(rep(c(1.01, 1.03, 1.05, 1.08, 1.11), 1000)[1:(length(gains$target))],2)
  
  if (metric == "R2") {
    return(
      plot +
        ggplot2::geom_label(ggplot2::aes(x=rep(gains$target,2), y=y), 
                   label = rep(round(gains$value, 2),2),
                   label.size = .1,
                   label.padding = unit(0.1, "lines"),
                   size=3) +
        ggplot2::labs(title = name) +
        ggplot2::theme(plot.title = element_text(hjust = 0.5)) +
        ggplot2::scale_y_continuous(limits=c(low, 1.11)) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) 
    )
  } else {
    return(plot)
  }
}


# removed the clean option
plot_interaction_heatmap <- function(misty.results, view, cutoff = 1,
                                     trim = -Inf, trim.measure = "gain.R2") {
  assertthat::assert_that(("importances.aggregated" %in% names(misty.results)),
                          msg = "The provided result list is malformed. Consider using collect_results()."
  )
  
  assertthat::assert_that(("improvements.stats" %in% names(misty.results)),
                          msg = "The provided result list is malformed. Consider using collect_results()."
  )
  
  assertthat::assert_that((view %in%
                             (misty.results$importances.aggregated %>% dplyr::pull(.data$view))),
                          msg = "The selected view cannot be found in the results table."
  )
  
  inv <- sign((stringr::str_detect(trim.measure, "gain") |
                 stringr::str_detect(trim.measure, "RMSE", negate = TRUE)) - 0.5)
  
  targets <- misty.results$improvements.stats %>%
    dplyr::filter(.data$measure == trim.measure, 
                  inv * .data$mean >= inv * trim) %>%
    dplyr::pull(.data$target)
  
  
  plot.data <- misty.results$importances.aggregated %>%
    dplyr::filter(.data$view == !!view, .data$Target %in% targets)
  
  set2.blue <- "#8DA0CB"
  
  results.plot <- ggplot2::ggplot(
    plot.data,
    ggplot2::aes(
      x = .data$Predictor,
      y = .data$Target
    )
  ) +
    ggplot2::geom_tile(ggplot2::aes(fill = .data$Importance)) +
    ggplot2::scale_fill_gradient2(
      low = "white",
      mid = "white",
      high = set2.blue,
      midpoint = cutoff
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
    ggplot2::coord_equal() +
    ggplot2::ggtitle(view)
}

plot_view_contributions <- function(misty.results, trim = -Inf,
                                    trim.measure = "gain.R2") {
  assertthat::assert_that(("contributions.stats" %in% names(misty.results)),
                          msg = "The provided result list is malformed. Consider using collect_results()."
  )
  
  assertthat::assert_that(("improvements.stats" %in% names(misty.results)),
                          msg = "The provided result list is malformed. Consider using collect_results()."
  )
  
  inv <- sign((stringr::str_detect(trim.measure, "gain") |
                 stringr::str_detect(trim.measure, "RMSE", negate = TRUE)) - 0.5)
  
  targets <- misty.results$improvements.stats %>%
    dplyr::filter(.data$measure == trim.measure, 
                  inv * .data$mean >= inv * trim) %>%
    dplyr::pull(.data$target)
  
  assertthat::assert_that(assertthat::not_empty(targets),
                          msg = "Invalid selection of trim measure and/or value."
  )
  
  plot.data <- misty.results$contributions.stats %>%
    dplyr::filter(.data$target %in% targets)
  
  results.plot <- ggplot2::ggplot(plot.data, ggplot2::aes(x = .data$target, y = .data$fraction)) +
    ggplot2::geom_col(ggplot2::aes(group = .data$view, fill = .data$view)) +
    ggplot2::scale_fill_brewer(palette = "Set2") +
    ggplot2::theme_classic() +
    ggplot2::ylab("Contribution") +
    ggplot2::xlab("Target") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
}

plot_diagnostics <- function(misty.results, name, low = .7) {
  plot1 <- list(plot_performance(misty.results$improvements, name, metric = "R2", low = low))
  plot2 <- list(plot_performance(misty.results$improvements, name, metric = "RMSE"))
  plot3 <- list(plot_view_contributions(misty.results))
  views <- misty.results$importances$view %>% unique
  plots <- map(views, function(view) {
    plot_interaction_heatmap(misty.results, view)
  })
  result.plots <- c(plot1, plot2, plot3, plots)
  do.call(gridExtra::grid.arrange, args = c(plot1, plot2, plot3, plots, ncol=3))
}