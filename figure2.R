objs <- readRDS("objs.rds")

flgmthddeltas <- function(current, cumulative) {
  stopifnot(
    inherits(current, "data.table"),
    inherits(cumulative, "data.table")
  )

  DT <-
    data.table::rbindlist(
      list(current = current, cumulative = cumulative),
      idcol = "flag.method"
    )

  # get the unique rows without considering the flag method
  DT <- unique(DT, by = names(DT)[-1])

  # if there is a "current" row and no "cumulative" row then there was nothing
  # different between the two flag methods
  DT <-
    data.table::dcast(
      data = DT,
      formula = subject_id + enc_seq ~ flag.method,
      value.var = list("num_cmrb", "cmrb_flag")
    )
  DT[, .N, keyby = .(cmrb_flag_current, cmrb_flag_cumulative)]
  DT <- DT[!is.na(cmrb_flag_cumulative)]
  DT[, .N, keyby = .(cmrb_flag_current, cmrb_flag_cumulative)]

  # sanity check
  stopifnot(
    DT[, all(enc_seq > 1)],
    DT[cmrb_flag_current == 1 & cmrb_flag_cumulative == 1 & (num_cmrb_current > num_cmrb_cumulative), .N == 0L]
  )

  DT <-
    rbind(
      # case 1: different risk profile
      DT[, .(case = 1, subjects = data.table::uniqueN(subject_id), encounters = .N)]
    ,
      # case 2: flag at least one comorbidit longitudinally when encounter level says zero
      DT[cmrb_flag_current == 0 & cmrb_flag_cumulative == 1, .(case = 2, subjects = data.table::uniqueN(subject_id), encounters = .N)]
    ,
      # case 3: Severity delta only
      DT[cmrb_flag_current == 1 & cmrb_flag_cumulative == 1 & (num_cmrb_current == num_cmrb_cumulative), .(case = 3, subjects = data.table::uniqueN(subject_id), encounters = .N)]
    ,
      # case 4: severity and/or more comorbidities
      DT[cmrb_flag_current == 1 & cmrb_flag_cumulative == 1 & (num_cmrb_current <= num_cmrb_cumulative), .(case = 4, subjects = data.table::uniqueN(subject_id), encounters = .N)]
    )[,
      `:=`(
        subjects_with_a_cmrb_flag   = (cumulative[enc_seq > 1 & cmrb_flag == 1L, data.table::uniqueN(subject_id)]),
        encounters_with_a_cmrb_flag = (cumulative[enc_seq > 1 & cmrb_flag == 1L, data.table::uniqueN(.SD, by = c("subject_id", "enc_seq"))])
      )
    ][
      ,
      `:=`(
        sp = subjects / subjects_with_a_cmrb_flag,
        ep = encounters / encounters_with_a_cmrb_flag
      )
    ]
  DT
}

plotdata <-
  data.table::rbindlist(
    list(
      "Charlson"   = flgmthddeltas(current = objs$medicalcoder_charlson_current,   cumulative = objs$medicalcoder_charlson_cumulative),
      "Elixhauser" = flgmthddeltas(current = objs$medicalcoder_elixhauser_current, cumulative = objs$medicalcoder_elixhauser_cumulative),
      "PCCC v3.1"  = flgmthddeltas(current = objs$medicalcoder_pcccv3.1_current,   cumulative = objs$medicalcoder_pcccv3.1_cumulative)
    ),
    idcol = "method"
  )

objs$medicalcoder_charlson_cumulative[cmrb_flag == 1, data.table::uniqueN(.SD, by = c("subject_id"))]
objs$medicalcoder_charlson_cumulative[cmrb_flag == 1, data.table::uniqueN(.SD, by = c("subject_id", "enc_seq"))]

plotdata[, case := factor(
  x = case,
  levels = c(1, 2, 3, 4),
  labels =
    c(
      "Different\nRisk Profile",
      "Zero Encounter-level Comorbidity\n≥ 1 Longitudinal Comorbidities",
      "Only Differences in Classification",
      "Differences in Classification\nand/or\nAdditional Comorbidities"
    )
  )
]

plotdata <- data.table::melt(data = plotdata, measure.vars = c("sp", "ep"))

data.table::setkey(plotdata, method, case, variable)

g <- function(DT, N = "subjects_with_a_cmrb_flag") {
  ggplot2::ggplot(data = DT) +
    ggplot2::aes(
      x = sprintf("%s\n(%s %s)", method, formatC(DT[[N]], format = "d", big.mark = ","), ifelse(startsWith(N, "subject"), "Subjects", "Encounters")),
      y = value,
      fill = case) +
    ggplot2::geom_col(
      position = ggplot2::position_dodge()
    ) +
    ggplot2::scale_y_continuous(label = scales::label_percent()) +
    ggplot2::scale_fill_brewer(type = "qual", palette = "Dark2") +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      legend.title = ggplot2::element_blank(),
      legend.key.height = grid::unit(1/10, "npc")
    )
}

a <- g(plotdata[variable == "sp"], N = "subjects_with_a_cmrb_flag")# + ggplot2::xlab("Algorithm (Subjects with at least one encounter with at least one comorbidity)")
b <- g(plotdata[variable == "ep"], N = "encounters_with_a_cmrb_flag")# + ggplot2::xlab("Algorithm (Encounters with at least one comorbidity)")

out <-
  ggpubr::ggarrange(
    a, b,
    ncol = 1,
    labels = c("(A)", "(B)"),
    label.x = 0.06,
    label.y = 0.93,
    common.legend = TRUE,
    legend = "right"
  )

ggplot2::ggsave(filename = "figure2.svg", plot = out, width = 8, height = 5)
ggplot2::ggsave(filename = "figure2.pdf", plot = out, width = 8, height = 5)

saveRDS(plotdata, file = "figure2data.rds")

################################################################################
#                                 End of File                                  #
################################################################################
