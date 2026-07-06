loadNamespace("data.table")
qwraps2::lazyload_cache_labels(
  labels = c(
    "medicalcoder-charlson-current",
    "medicalcoder-charlson-cumulative",
    "medicalcoder-elixhauser-current",
    "medicalcoder-elixhauser-cumulative",
    "medicalcoder-pcccv3.1-current",
    "medicalcoder-pcccv3.1-cumulative"),
  path = "mimiciv-data-analysis_cache/pdf/"
)

################################################################################
#                                 End of File                                  #
################################################################################
