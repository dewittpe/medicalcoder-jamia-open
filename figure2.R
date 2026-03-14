objs <- readRDS("objs.rds")

ttc <-
  lapply(objs, attr, "tictoc") |>
  lapply(as.numeric) |>
  data.table::as.data.table() |>
  data.table::melt(measure.vars = names(objs), variable.factor = FALSE)
ttc[, c("Tool", "Algorithm", "Flag Method") := data.table::tstrsplit(variable, "_")]
ttc[, variable := NULL]
ttc <- ttc[!(Tool != "medicalcoder" & `Flag Method` == "cumulative")]

g <-
  ggplot2::ggplot(ttc) +
  ggplot2::aes(x = Algorithm, y = value, fill = `Flag Method`) +
  ggplot2::geom_col(
    position = ggplot2::position_dodge2(width = 0.8, preserve = "single"),
    width = 0.7,
    color = "white",
    linewidth = 0.3
    ) +
  ggplot2::facet_wrap(~ Tool, nrow = 1, strip.position = "top") +
  ggplot2::scale_fill_manual(
    name = "Flag Method",
    values = c("current" = "#6F263D", "cumulative" = "#4F8FCB")
  ) +
  ggplot2::scale_y_continuous(breaks = seq(0, max(ttc$value), by = 15)) +
  ggplot2::labs(x = "Comorbidity Algorithm", y = "Time (seconds)") +
  ggplot2::theme_bw(base_size = 12) +
  ggplot2::theme(
    panel.grid.minor = ggplot2::element_blank(),
    legend.position = "bottom",
    axis.text.x = ggplot2::element_text(angle = 25, hjust = 1)
  )

ggplot2::ggsave(plot = g, filename = "figure2.svg", width = 6, height = 4)
