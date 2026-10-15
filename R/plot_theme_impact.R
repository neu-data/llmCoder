#' Plot how a metric shifts when each qualitative theme is present
#'
#' Draws one row per theme showing the difference in the metric mean (theme
#' present minus absent) with its confidence interval. Point size shows how
#' many units carry the theme; colour marks themes that remain significant
#' after multiple-comparison adjustment.
#'
#' @param cross_tab_data A tibble returned by [cross_tab_themes()].
#' @param metric_label Axis label for the metric. Defaults to the metric name
#'   recorded by [cross_tab_themes()].
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' df <- mock_mixed_data(80, seed = 42)
#' coded <- llm_auto_code(df, user_id, transcript_feedback,
#'                        codebook = c("UI_Confusion", "Positive_Experience",
#'                                     "Feature_Request"),
#'                        coder = mock_coder())
#' plot_theme_impact(cross_tab_themes(coded, session_duration_sec))
plot_theme_impact <- function(cross_tab_data, metric_label = NULL) {
  required <- c("theme", "n_present", "difference", "conf_low", "conf_high", "significant")
  if (!is.data.frame(cross_tab_data) || !all(required %in% names(cross_tab_data))) {
    cli::cli_abort("{.arg cross_tab_data} must be the output of {.fn cross_tab_themes}.")
  }
  metric_label <- metric_label %||% attr(cross_tab_data, "metric") %||% "metric"

  plot_df <- cross_tab_data[!is.na(cross_tab_data$difference), , drop = FALSE]
  if (nrow(plot_df) == 0) {
    cli::cli_abort("No theme has enough present and absent units to plot.")
  }
  plot_df$label <- paste0(gsub("_", " ", plot_df$theme), " (n=", plot_df$n_present, ")")
  plot_df$label <- stats::reorder(plot_df$label, plot_df$difference)
  plot_df$status <- factor(
    ifelse(plot_df$significant, "Significant (adjusted)", "Not significant"),
    levels = c("Significant (adjusted)", "Not significant")
  )

  ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$difference, y = .data$label)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey55") +
    ggplot2::geom_linerange(
      ggplot2::aes(xmin = .data$conf_low, xmax = .data$conf_high, colour = .data$status),
      linewidth = 1
    ) +
    ggplot2::geom_point(ggplot2::aes(size = .data$n_present, colour = .data$status)) +
    ggplot2::scale_colour_manual(
      values = c("Significant (adjusted)" = "#c0392b", "Not significant" = "#5d6d7e"),
      drop = FALSE
    ) +
    ggplot2::scale_size_area(max_size = 8) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      legend.position = "bottom",
      plot.title = ggplot2::element_text(face = "bold")
    ) +
    ggplot2::labs(
      title = "Theme impact on a quantitative metric",
      subtitle = "Difference in mean when the theme is present vs absent, with 95% CI",
      x = paste("Difference in", metric_label),
      y = NULL,
      colour = NULL,
      size = "Units with theme"
    )
}

#' @importFrom rlang .data
NULL
