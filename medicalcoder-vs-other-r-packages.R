################################################################################
# required namespaces
nmsps <- c("data.table", "medicalcoder", "icdcomorbid", "multimorbidity", "comorbidity")

for (nmsp in nmsps) {
  if (!requireNamespace(nmsp, quietly = TRUE)) {
    msg <- sprintf("Namespace %s and its dependencies will be installed.", nmsp)
    message(msg)
    install.packages(nmsp,
      repos = c(
        CRAN = "https://cran.rstudio.com",
        BIOC = "https://bioconductor.org/packages/release/bioc/"
      )
    )
  }
}

library(data.table)
library(medicalcoder)
library(icdcomorbid)
library(multimorbidity)
library(comorbidity)

mdcrDT <- data.table::copy(mdcr)
data.table::setDT(mdcrDT)
data.table::setkey(mdcrDT, patid)

################################################################################
# apply charlson_quan2005, charlson_quan2011, and elixhauser_quan2005 to the
# mdcrDT data set via medicalcoder::comorbidity()

tic <- Sys.time()
medicalcoder_charlson_quan2005 <-
  medicalcoder::comorbidities(
    data = mdcrDT,
    id.vars = "patid",
    icd.codes = "code",
    dx.var = "dx",
    icdv.var = "icdv",
    poa = 1L, # assume all codes are present on admission
    primarydx = 0L, # assume all codes are secondary diagnoses
    method = "charlson_quan2005"
  )
toc <- Sys.time()

attr(medicalcoder_charlson_quan2005, "tictoc") <-
  as.numeric(difftime(toc, tic, units = "secs"))


tic <- Sys.time()
medicalcoder_charlson_quan2011 <-
  medicalcoder::comorbidities(
    data = mdcrDT,
    id.vars = "patid",
    icd.codes = "code",
    dx.var = "dx",
    icdv.var = "icdv",
    poa = 1L, # assume all codes are present on admission
    primarydx = 0L, # assume all codes are secondary diagnoses
    method = "charlson_quan2011"
  )
toc <- Sys.time()

attr(medicalcoder_charlson_quan2011, "tictoc") <-
  as.numeric(difftime(toc, tic, units = "secs"))

tic <- Sys.time()
medicalcoder_elixhauser_quan2005 <-
  medicalcoder::comorbidities(
    data = mdcrDT,
    id.vars = "patid",
    icd.codes = "code",
    dx.var = "dx",
    icdv.var = "icdv",
    poa = 1L, # assume all codes are present on admission
    primarydx = 0L, # assume all codes are secondary diagnoses
    method = "elixhauser_quan2005"
  )
toc <- Sys.time()

attr(medicalcoder_elixhauser_quan2005, "tictoc") <-
  as.numeric(difftime(toc, tic, units = "secs"))

################################################################################
# Charlson 2011 and Elixhauser Quan 2005 via comorbidity
mdcr_icd9dx  <- subset(mdcrDT, icdv ==  9L & dx == 1L)
mdcr_icd10dx <- subset(mdcrDT, icdv == 10L & dx == 1L)

# make sure we have a fair comparison, we will add rows without a code for each
# patid not in the subsets.
mdcr_icd9dx <-
  rbind(
    mdcr_icd9dx,
    data.table(patid = setdiff(mdcr$patid, mdcr_icd9dx$patid), icdv = 9L, code = "", dx = 1L)
  )

mdcr_icd10dx <-
  rbind(
    mdcr_icd10dx,
    data.frame(patid = setdiff(mdcr$patid, mdcr_icd10dx$patid), icdv = 10L, code = "", dx = 1L)
  )

tic <- Sys.time()
comorbidity_charlson_icd9_results <-
  comorbidity::comorbidity(
    x = mdcr_icd9dx,
    id = "patid",
    code = "code",
    map = "charlson_icd9_quan",
    assign0 = TRUE # set less severe comorbidities flags to 0 when more severe comorbidities is also flagged
  )

comorbidity_charlson_icd10_results <-
  comorbidity::comorbidity(
    x = mdcr_icd10dx,
    id = "patid",
    code = "code",
    map = "charlson_icd10_quan",
    assign0 = TRUE
  )

# combine the ICD-9 and ICD-10 results into one set
comorbidity_charlson_results <-
  rbind(comorbidity_charlson_icd9_results, comorbidity_charlson_icd10_results)

comorbidity_charlson_results <-
  aggregate(. ~ patid, data = comorbidity_charlson_results, FUN = max)

# add the attributes to the combine set
attributes(comorbidity_charlson_results)[c("class", "variable.labels", "map")] <-
  attributes(comorbidity_charlson_icd9_results)[c("class", "variable.labels", "map")]

comorbidity_charlson_results[["score"]] <-
  comorbidity::score(
    x = comorbidity_charlson_results,
    weights = "quan",
    assign0 = TRUE
  )

# the score return is a numeric value, setting to integer to match the storage
# mode of medicalcoder
comorbidity_charlson_results[["score"]] <-
  as.integer(comorbidity_charlson_results[["score"]])
toc <- Sys.time()

attr(comorbidity_charlson_results, "tictoc") <-
  as.numeric(difftime(toc, tic, units = "secs"))



tic <- Sys.time()
comorbidity_elixhauser_icd9_results <-
  comorbidity::comorbidity(
    x = mdcr_icd9dx,
    id = "patid",
    code = "code",
    map = "elixhauser_icd9_quan",
    assign0 = TRUE # set less severe comorbidities flags to 0 when more severe comorbidities is also flagged
  )

comorbidity_elixhauser_icd10_results <-
  comorbidity::comorbidity(
    x = mdcr_icd10dx,
    id = "patid",
    code = "code",
    map = "elixhauser_icd10_quan",
    assign0 = TRUE
  )

# combine the ICD-9 and ICD-10 results into one set
comorbidity_elixhauser_results <-
  rbind(comorbidity_elixhauser_icd9_results, comorbidity_elixhauser_icd10_results)

comorbidity_elixhauser_results <-
  aggregate(. ~ patid, data = comorbidity_elixhauser_results, FUN = max)
toc <- Sys.time()

attr(comorbidity_elixhauser_results, "tictoc") <-
  as.numeric(difftime(toc, tic, units = "secs"))

# check that medicalcoder::comorbidities() and comorbidity::comorbidity()
# results are the same
charlson_delta <-
  merge(
    x = comorbidity_charlson_results,
    y = medicalcoder_charlson_quan2011,
    all = TRUE,
    by = "patid"
  )

charlson_columns <- data.table::fread(text = "
Condition                          | comorbidity | medicalcoder
AIDS                               | aids        | aidshiv
Cancer (Any malignancy)            | canc        | mal
Cancer (Metastatic solid tumor)    | metacanc    | mst
Cerebrovascular disease            | cevd        | cebvd
Chronic pulmonary disease          | cpd         | copd
Congestive heart failure           | chf         | chf
Dementia                           | dementia    | dem
Hemiplegia or paraplegia           | hp          | hp
Myocardial infarction              | mi          | mi
Peripheral vascular disease        | pvd         | pvd
Peptic ulcer disease               | pud         | pud
Rheumatic disease                  | rheumd      | rhd
Diabetes (uncomplicated)           | diab        | dm
Diabetes (complicated)             | diabwc      | dmc
Renal disease                      | rend        | rnd
Liver disease (mild)               | mld         | mld
Liver disease (moderate or severe) | msld        | msld
Score (based on Quan 2011)         | score       | cci
  ")
data.table::setDF(charlson_columns)

for (i in seq_len(nrow(charlson_columns))) {
  x <- charlson_columns[["comorbidity"]][i]
  y <- charlson_columns[["medicalcoder"]][i]
  if (x == y) {
    x <- paste0(x, ".x")
    y <- paste0(y, ".y")
  }
  e <- base::substitute(identical(charlson_delta[[X]], charlson_delta[[Y]]), list(X = x, Y = y))
  print(e)
  r <- eval(e)
  print(r)
  stopifnot(r)
  charlson_delta[[x]] <- NULL
  charlson_delta[[y]] <- NULL
}

stopifnot(
  identical(
    names(charlson_delta),
    c("patid", "num_cmrb", "cmrb_flag", "age_score")
  )
)

elixhauser_delta <-
  merge(
    x = comorbidity_elixhauser_results,
    y = medicalcoder_elixhauser_quan2005,
    all = TRUE,
    by = "patid"
  )

elixhauser_columns <- data.table::fread(text = "
Condition                                     | comorbidity | medicalcoder
AIDS/HIV                                      | aids        | AIDS
Alcohol abuse                                 | alcohol     | ALCOHOL
Anemias (Blood loss anaemia)                  | blane       | BLDLOSS
Anemias (Deficiency anaemia)                  | dane        | ANEMDEF
Cardiac Arrhythmias                           | carit       | CARDIAC_ARRHYTHMIAS
Cancer (Solid tummor without metastasis)      | solidtum    | TUMOR
Cancer (Metastatic)                           | metacanc    | METS
Chronic pulmonary disease                     | cpd         | CHRNLUNG
Coagulopathy                                  | coag        | COAG
Congestive Heart Failure                      | chf         | CHF
Depression                                    | depre       | DEPRESS
Diabetes (Complicated)                        | diabc       | DMCX
Diabetes (Uncomplicated)                      | diabunc     | DM
Drug abuse                                    | drug        | DRUG
Fluid and electrolyte disorders               | fed         | LYTES
Hypertension (Complicated)                    | hypc        | HTN_CX
Hypertension (Uncomplicated)                  | hypunc      | HTN_UNCX
Hypothyroidism                                | hypothy     | HYPOTHY
Liver disease                                 | ld          | LIVER
Lymphoma                                      | lymph       | LYMPH
Obesity                                       | obes        | OBESE
Other neurological disease                    | ond         | NEURO
Paralysis                                     | para        | PARA
Peptic ulcer disease excluding bleeding       | pud         | ULCER
Peripheral Vascular Disorders                 | pvd         | PERIVASC
Psychoses                                     | psycho      | PSYCH
Pulmonary Circulation Disorders               | pcd         | PULMCIRC
Renal Failure                                 | rf          | RENLFAIL
Rheumatoid artritis/collaged vascular disease | rheumd      | ARTH
Valvular disease                              | valv        | VALVE
Weight loss                                   | wloss       | WGHTLOSS
  ")
data.table::setDF(elixhauser_columns)

# the HTN_C is only in the medicalcoder results
stopifnot(
  identical(
    medicalcoder_elixhauser_quan2005[["HTN_C"]],
    as.integer(
      medicalcoder_elixhauser_quan2005[["HTN_UNCX"]] |
        medicalcoder_elixhauser_quan2005[["HTN_CX"]]
    )
  )
)

for (i in seq_len(nrow(elixhauser_columns))) {
  x <- elixhauser_columns[["comorbidity"]][i]
  y <- elixhauser_columns[["medicalcoder"]][i]
  if (x == y) {
    x <- paste0(x, ".x")
    y <- paste0(y, ".y")
  }
  e <- base::substitute(identical(elixhauser_delta[[X]], elixhauser_delta[[Y]]), list(X = x, Y = y))
  print(e)
  r <- eval(e)
  print(r)
  stopifnot(r)
  elixhauser_delta[[x]] <- NULL
  elixhauser_delta[[y]] <- NULL
}

stopifnot(identical(names(elixhauser_delta), c("patid", "HTN_C", "num_cmrb", "cmrb_flag", "mortality_index", "readmission_index")))

################################################################################
# icdcomorbid

#  mdcr_icd9dx_wide <-
#    icdcomorbid::long_to_wide(
#      df = mdcr_icd9dx,
#      idx = "patid",
#      icd_cols = "code"
#    )
#
# killed process after 15 seconds

mdcr_icd9dx[, DX := paste0("DX", seq_along(code)), by = .(patid)]
mdcr_icd10dx[, DX := paste0("DX", seq_along(code)), by = .(patid)]

mdcr_icd9dx_wide <-
  data.table::dcast(mdcr_icd9dx, patid ~ DX, value.var = "code")

mdcr_icd10dx_wide <-
  data.table::dcast(mdcr_icd10dx, patid ~ DX, value.var = "code")

tic <- Sys.time()
icdcomorbid_icd9_results <-
  icdcomorbid::icd9_to_comorbid(
    df = mdcr_icd9dx_wide[1:10000, ],
    idx = "patid",
    icd_cols = grep("^DX", names(mdcr_icd9dx_wide), value = TRUE),
    mapping = "charlson9"
    #, batch_size = 10000  # same time default or 10000
  )
toc <- Sys.time()

attr(icdcomorbid_icd9_results, "tictoc") <-
  as.numeric(difftime(toc, tic, units = "secs"))

################################################################################
# multimorbidity

tic <- Sys.time()
multimorbidity_charlson_results <-
  multimorbidity::charlson(
    dat = mdcrDT,
    id  = patid,
    version = 19, # default - both ICD-9 and ICD-10 data
    version_var = icdv
  )
toc <- Sys.time()

attr(multimorbidity_charlson_results, "tictoc") <-
  as.numeric(difftime(toc, tic, units = "secs"))

# false negatives:
deltas <-
  merge(
    x = medicalcoder_charlson_quan2005,
    y = multimorbidity_charlson_results,
    all = TRUE,
    by.x = "patid",
    by.y = "id"
  )
setDT(deltas)


missingcodes <-
  merge(
    x = subset(mdcr, patid %in% deltas[rhd != charlson_rheum, patid]),
    y = subset(medicalcoder::get_charlson_codes(), charlson_quan2005 == 1L),
    by = c("code", "icdv", "dx")
  )
missingcodes <- subset(missingcodes, condition == "rhd")
missingcodes <- unique(missingcodes[, c("icdv", "dx", "code", "full_code")])
setDT(missingcodes)
missingcodes[, codeid := 1:.N]

medicalcoder::comorbidities(
  data = missingcodes,
  id.vars = "codeid",
  icd.codes = "code",
  icdv.var = "icdv",
  dx.var = "dx",
  method = "charlson_quan2005",
  poa = 1L,
  primarydx = 0L
)$rhd

multimorbidity::charlson(
  dat = missingcodes,
  id = codeid,
  version = 19,
  version_var = icdv
)$charlson_rheum

################################################################################
# save
save(
  medicalcoder_charlson_quan2005,
  medicalcoder_charlson_quan2011,
  medicalcoder_elixhauser_quan2005,
  comorbidity_charlson_results,
  comorbidity_elixhauser_results,
  icdcomorbid_icd9_results,
  multimorbidity_charlson_results,
  file = "medicalcoder-vs.Rdata"
)

################################################################################
#                                 End of File                                  #
################################################################################
