benchmarks <- readRDS("benchmarks.rds")

benchmarksDT <- data.table::as.data.table(data.table::copy(benchmarks))
benchmarksDT[, c("Tool", "Algorithm", "Flag Method") := data.table::tstrsplit(expr, "__")]

g <-
  ggplot2::ggplot(
      data =
        benchmarksDT[
          ,
          .(y = median(time/1e9), lwr = quantile(time/1e9, prob = 0.25), upr = quantile(time/1e9, prob = 0.75)),
          by = .(Tool, Algorithm, `Flag Method`)
        ]
    ) +
    ggplot2::aes(x = Algorithm, y = y, ymin = lwr, ymax = upr, fill = `Flag Method`) +
    ggplot2::geom_col(position = ggplot2::position_dodge2()) +
    ggplot2::geom_errorbar(position = ggplot2::position_dodge2()) +
    ggplot2::facet_wrap( ~ Tool, nrow = 1, strip.position = "top") +
    ggplot2::scale_fill_manual(
      name = "Flag Method",
      values = c("current" = "#6F263D", "cumulative" = "#4F8FCB")
    ) +
    #ggplot2::scale_y_log10() +
    ggplot2::labs(x = "Comorbidity Algorithm", y = "Time (seconds)") +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom",
      axis.text.x = ggplot2::element_text(angle = 25, hjust = 1)
    )

saveRDS(benchmarksDT, "figure3data.rds")
ggplot2::ggsave(plot = g, filename = "figure3.svg", width = 6, height = 4)
ggplot2::ggsave(plot = g, filename = "figure3.pdf", width = 6, height = 4)

