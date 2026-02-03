################################################################################
# file: figure1-flag-method-example.R
#
# objective:
#
#   build a table/figure showing when a condition is flagged based on
#   present on admission codes and comorbidity algorithm
#
# inputs:
#
#   none (uses a hard-coded example record)
#
# outputs:
#
#   figure1.png, figure1.svg, figure1.Rdata (record + args)
#
# dependencies:
#
#  data.table, medicalcoder, ggplot2, ggh4x
#
# Notes:
#   run from repo root so outputs land alongside manuscript assets
#
################################################################################
library(data.table)
library(medicalcoder)
library(ggplot2)
library(ggh4x)

# assume we have a patient record for six encounters.  We use ICD-10 diagnostic
# codes C78.4 and I50.40 which maps to a cancer and heart failure
# (cardiovascular disease) comorbidity respectively for PCCC, Charlson, and
# Elixhauser. For demonstration, we also flag POA with the second report of
# I50.40 intentionally marked as not present on admission.
# POA (present on admission) is varied to show algorithm differences.

lookup_icd_codes(c("C78.4", "I50.40"))

codes <- c("C78.4", "I50.40")
cols  <- c("icdv", "dx", "code", "full_code", "condition")

subset(
  get_pccc_codes(),
  subset = full_code %in% codes,
  select = c(cols, "pccc_v3.1")
)

subset(
  get_charlson_codes(),
  subset = full_code %in% codes,
  select = c(cols, "charlson_quan2005")
)

subset(
  get_elixhauser_codes(),
  subset = full_code %in% codes & elixhauser_ahrq2025 == 1L,
  select = c(cols, "elixhauser_ahrq2025")
)

record <-
  structure(
    list(
      # 6 encounters for 1 patient; NA code/POA rows represent no code that visit
      patid = c("A", "A", "A", "A", "A", "A", "A"),
      encid = c(1L, 2L, 3L, 4L, 5L, 5L, 6L),
      code = c(NA, "C78.4", "I50.40", NA, "C78.4", "I50.40", NA),
      poa = c(NA, 0L, 1L, NA, 1L, 0L, NA)),
    row.names = c(NA, -7L),
    class = "data.frame"
  )
data.table::setDT(record)

# We will call `comorbidities()` for the three methods using static POA flags and
# dynamic POA flags, and both flag methods.  Results are shown in the following
# table.

args <-
  data.table::CJ(
    data = list(record),
    icd.codes = "code",
    id.vars = list(c("patid", "encid")),
    icdv = 10L,
    dx = 1L,
    poa = c(0L, 1L, NA_integer_),
    method = c("charlson_quan2005", "elixhauser_ahrq2025", "pccc_v3.1"),
    flag.method = c("current", "cumulative"),
    sorted = FALSE
  )
# NA poa => use poa.var; non-NA poa => use static POA flag
args[, poa.var := fifelse(is.na(poa), "poa", NA_character_)]
args[!startsWith(method, "pccc"), primarydx := 0L]

build_args <-
  function(x) {
    # remove NA args so poa.var is passed only when needed
    i <- which(is.na(x))
    x[i] <- NULL
    x
  }

args[, args := apply(.SD, 1, build_args)]

args[, rtn := lapply(args, do.call, what = comorbidities)]

tab <- list()
for (i in seq_len(nrow(args))) {
  m <- args[["rtn"]][[i]]
  # normalize method-specific outputs to CVD/CANCER for plotting
  if (startsWith(args[["method"]][[i]], "pccc")) {
    m <- m[, .(patid, encid, CVD = cvd_dxpr_or_tech, CANCER = malignancy_dxpr_or_tech)]
  } else if (startsWith(args[["method"]][[i]], "charlson")) {
    m <- m[, .(patid, encid, CVD = chf, CANCER = mst)]
  } else {
    m <- m[, .(patid, encid, CVD = HF, CANCER = CANCER_METS)]
  }
  m[, poa := fifelse(is.na(args[["poa"]][[i]]), "v", as.character(args[["poa"]][[i]]))]
  m[, flag.method := args[["flag.method"]][[i]]]
  m[, method := args[["method"]][[i]]]
  # annotate encounter codes; * indicates POA = 1 for that code
  m[, codes := c("", "C78.4", "I50.40*", "", paste0("C78.4*\nI50.40"), "")]
  tab[[i]] <- m
}
tab <- rbindlist(tab)

tab <-
  melt(tab,
    measure.vars = c("CVD", "CANCER"),
    variable.factor = FALSE
  )

tab[, value := factor(value, 0:1, c("Not Flagged", "Flagged"))]
tab[, flag.method := factor(flag.method, c("current", "cumulative"))]

tab[, encid  := factor(encid, levels = rev(1:6))]
tab[, poa    := factor(poa, c("0", "1", "v"), labels = c("poa = 0", "poa = 1", "poa.var = 'poa'"))]
tab[, method := factor(method, c("charlson_quan2005", "elixhauser_ahrq2025", "pccc_v3.1"), c("Charlson\n(Quan 2005)", "Elixhauser\n(AHRQ 2025)", "PCCC\nv3.1"))]

g <-
  ggplot(tab) +
  theme_bw() +
  aes(x = flag.method, y = encid) +
  geom_point(mapping = aes(shape = value), size = 2) +
  scale_shape_manual(values = c("Not Flagged" = NULL, "Flagged" = 13)) +
  geom_text(
    data  = tab[, unique(.SD), .SDcols = c("encid", "codes")],
    mapping = aes(x = 1.5, y = encid, label = codes),
    size = 2
  ) +
  # facet by method, POA handling, and condition
  facet_nested(method ~ poa + variable) +
  ylab("Encounter") +
  xlab("Flag Method") +
  labs(
    caption = "* Present on Admission\nC78.4 does not need to be POA to be flagged by Elixhauser\nI50.40 does need to be POA to be flagged by Elixhauser"
  ) +
  theme(
    legend.position = "none"
  )

# figure output sizes aligned with manuscript layout
ggsave(plot = g, filename = "figure1.png", width = 8, height = 4.5)
ggsave(plot = g, filename = "figure1.svg", width = 8, height = 4.5)

# save intermediate data for reproducibility/debugging
save(record, args, file = "figure1.Rdata")

################################################################################
#                                 End of File                                  #
################################################################################
