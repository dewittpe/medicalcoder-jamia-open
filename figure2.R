objs <- readRDS("objs.rds")

# build the needed data sets for plotting
flg_charlson <-
  data.table::rbindlist(
    list(
      current    = objs$medicalcoder_charlson_current,
      cumulative = objs$medicalcoder_charlson_cumulative
    ),
    idcol = "flag.method"
  )
flg_elixhauser <-
  data.table::rbindlist(
    list(
      current    = objs$medicalcoder_elixhauser_current,
      cumulative = objs$medicalcoder_elixhauser_cumulative
    ),
    idcol = "flag.method"
  )
flg_pcccv3.1 <-
  data.table::rbindlist(
    list(
      current    = objs$medicalcoder_pcccv3.1_current,
      cumulative = objs$medicalcoder_pcccv3.1_cumulative
    ),
    idcol = "flag.method"
  )

# remove some rows which are not of interest for this graphic
flg_charlson[, cci := NULL]
flg_charlson[, age_score := NULL]
flg_charlson[, num_cmrb := NULL]

flg_elixhauser[, readmission_index := NULL]
flg_elixhauser[, mortality_index := NULL]
flg_elixhauser[, num_cmrb := NULL]

for(j in grep("_dxpr_and_tech|_dxpr_only|_tech_only", names(flg_pcccv3.1), value = TRUE)) {
  data.table::set(flg_pcccv3.1, j = j, value = NULL)
}
flg_pcccv3.1[, num_cmrb := NULL]

# melt all the data sets
flg_charlson <-
  data.table::melt(
    data = flg_charlson,
    id.vars = c("flag.method", "subject_id", "enc_seq")
  )
flg_elixhauser <-
  data.table::melt(
    data = flg_elixhauser,
    id.vars = c("flag.method", "subject_id", "enc_seq")
  )
flg_pcccv3.1 <-
  data.table::melt(
    data = flg_pcccv3.1,
    id.vars = c("flag.method", "subject_id", "enc_seq")
  )


# aggregate
flg_charlson   <- flg_charlson[, .(N = sum(value)), by = .(flag.method, variable)]
flg_elixhauser <- flg_elixhauser[, .(N = sum(value)), by = .(flag.method, variable)]
flg_pcccv3.1   <- flg_pcccv3.1[, .(N = sum(value)), by = .(flag.method, variable)]

# extract cmrb_flag
cmrb_flags <-
  data.table::rbindlist(
    list(
      charlson   = flg_charlson[variable == "cmrb_flag"],
      elixhauser = flg_elixhauser[variable == "cmrb_flag"],
      pcccv3.1   = flg_pcccv3.1[variable == "cmrb_flag"]
    ),
    idcol = "method"
  ) |> droplevels()
data.table::setnames(cmrb_flags, old = "method", new = "condition")
cmrb_flags[, condition := factor(condition, levels = c("charlson", "elixhauser", "pcccv3.1"))]

flg_charlson   <- flg_charlson[variable != "cmrb_flag"]
flg_elixhauser <- flg_elixhauser[variable != "cmrb_flag"]
flg_pcccv3.1   <- flg_pcccv3.1[variable != "cmrb_flag"]

# merge on useful descriptions for the graphic
flg_charlson <-
  merge(
    x = flg_charlson,
    y = medicalcoder::get_charlson_index_scores()[c("condition", "condition_description")],
    all.x = TRUE,
    by.x = "variable",
    by.y = "condition"
  )

data.table::setnames(flg_charlson, old = "condition_description", new = "condition")
#flg_charlson[condition == "Diabetes with chronic complications",    condition := "DM with\nchronic complications"]
#flg_charlson[condition == "Diabetes without chronic complications", condition := "DM without\nchronic complications"]
flg_charlson[, condition := factor(condition, levels = rev(sort(unique(condition))))]

data.table::setnames(flg_elixhauser, old = "variable", new = "condition")
flg_elixhauser[condition == "CARDIAC_ARRHYTHMIAS", condition := "CARDIAC\nARRHYTHMIAS"]
flg_elixhauser[, condition := factor(condition, levels = rev(sort(unique(as.character(condition)))))]

flg_pcccv3.1[, variable := sub("_dxpr_or_tech", "", variable)]

flg_pcccv3.1 <-
  merge(
    x = flg_pcccv3.1,
    y = unique(medicalcoder::get_pccc_conditions()[c("condition", "condition_label")]),
    all.x = TRUE,
    by.x = "variable",
    by.y = "condition"
  )

flg_pcccv3.1[, condition := data.table::fcase(
  variable == "any_tech_dep", "Technology Dependence",
  variable == "any_transplant", "Transplant",
  startsWith(condition_label, "Miscellaneous"), "Miscellaneous",
  default = condition_label
  )
]
flg_pcccv3.1[, condition := factor(condition, levels = rev(sort(unique(condition))))]


# build graphic
g <- function(DT) {
  ggplot2::ggplot(data = DT) +
    ggplot2::aes(x = condition, y = N, fill = flag.method) +
    ggplot2::geom_col(position = ggplot2::position_dodge()) +
    ggplot2::scale_fill_manual(
      name = "Flag Method",
      values = c("current" = "#6F263D", "cumulative" = "#4F8FCB")
    ) +
    ggplot2::scale_y_continuous(
      name = "Encounters",
      label = scales::label_comma()
    ) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      legend.position = "bottom",
    ) +
    ggplot2::coord_flip()
}

out <-
  ggpubr::ggarrange(
    ggpubr::ggarrange(
      g(flg_elixhauser) + ggplot2::facet_wrap(~ "Elixhauser (Quan 2005)"),
      ggpubr::ggarrange(
        g(flg_charlson)   + ggplot2::facet_wrap(~ "Charslon (Quan 2005)"),
        g(flg_pcccv3.1)   + ggplot2::facet_wrap(~ "PCCC v3.1 (Feinstein 2024)"),
        labels = c("(B)", "(C)"),
        legend = "none",
        ncol = 1
      ),
    labels = c("(A)", ""),
    legend = FALSE,
    ncol = 2
  ),
  g(cmrb_flags) + ggplot2::facet_wrap(~ "Any Comorbidity"),
  labels = c("", "(D)"),
  common.legend = TRUE,
  ncol = 1,
  heights = c(5,1)
)

ggplot2::ggsave(
  filename = "figure2.svg",
  plot = out,
  width = 12,
  height = 9
)

################################################################################
#                                 End of File                                  #
################################################################################
