library(data.table)
library(medicalcoder)
library(ggplot2)

codes <- c("H49.811", "J84.111", "Z96.41")
permutations <-
  data.table::data.table(
    permutation = rep(1:6, each = 7),
    encounter_id = rep(1:7, times = 6),
    code =
      codes[c(NA, 1, NA, 2, NA, 3, NA,
              NA, 1, NA, 3, NA, 2, NA,
              NA, 2, NA, 1, NA, 3, NA,
              NA, 2, NA, 3, NA, 1, NA,
              NA, 3, NA, 1, NA, 2, NA,
              NA, 3, NA, 2, NA, 1, NA)]
  )
permutations[, plabel := paste(na.omit(code), collapse = ", "), by = .(permutation)]
permutations[, plabel := paste0("Permutation ", permutation, ": ", plabel)]

rtn <-
  comorbidities(
    data = permutations,
    icd.codes = "code",
    id.vars = c("permutation", "plabel", "encounter_id"),
    icdv = 10L,
    compact.codes = FALSE,
    method = "pccc_v3.1",
    flag.method = "cumulative",
    poa = 1
  )
rtn_long <-
  melt(
    rtn,
    id.vars = c("permutation", "plabel", "encounter_id"),
    measure.vars =
      c(
        "metabolic_dxpr_or_tech", "metabolic_dxpr_only",
        "metabolic_tech_only", "metabolic_dxpr_and_tech",
        "respiratory_dxpr_or_tech", "respiratory_dxpr_only",
        "respiratory_tech_only", "respiratory_dxpr_and_tech",
        "cmrb_flag", "num_cmrb"
      )
  )
rtn_long[, encounter_id := factor(encounter_id, rev(sort(unique(encounter_id))))]

rtn_long[value == 1 & variable != "num_cmrb", faicon := factor(value, 0:1, c("Not Flagged", "Flagged"))]
rtn_long[, hicon := ""]
rtn_long[value == 0 & variable == "num_cmrb", hicon := "0"]
rtn_long[value == 1 & variable == "num_cmrb", hicon := "1"]
rtn_long[value == 2 & variable == "num_cmrb", hicon := "2"]

rtn_long[, cmrb := fcase(startsWith(as.character(variable), "metabolic"), "metabolic",
                         startsWith(as.character(variable), "respiratory"), "respiratory",
                         default = "")]

g <-
  ggplot2::ggplot(data = rtn_long) +
  ggplot2::aes(x = variable, y = encounter_id) +
  ggplot2::geom_point(
    mapping = ggplot2::aes(shape = faicon),
    size = 2
  ) +
  scale_shape_manual(values = c("Not Flagged" = NULL, "Flagged" = 13)) +
  ggplot2::geom_text(
    data = subset(rtn_long, is.na(faicon)),
    mapping = ggplot2::aes(label = hicon),
    size = 4
  ) +
  ggplot2::facet_wrap( ~ plabel) +
  ggplot2::ylab("Encounter") +
  ggplot2::theme(
    legend.position = "none",
    axis.text.x = ggplot2::element_text(angle = 70, hjust = 1),
    axis.title.x = ggplot2::element_blank()
  )

ggsave(plot = g, filename = "figure2.png", width = 9, height = 5)
ggsave(plot = g, filename = "figure2.svg", width = 9, height = 5)

saveRDS(permutations, file = "figure2permutations.rds")

################################################################################
#                                 End of File                                  #
################################################################################
