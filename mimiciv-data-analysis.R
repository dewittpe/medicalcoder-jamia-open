## ---- verify-mimiciv-version ----
# verify that you are looking at version 3.1 of the MIMIC-IV data
stopifnot(
  identical(
    scan(
      file = file.path(Sys.getenv("MIMICIVDATA"), "CHANGELOG.txt"),
      what = "character", sep = "\n", quiet = TRUE)[1],
    "This is the change log for MIMIC-IV v3.1."
  )
)

## ---- icd-src-years ----
aggregate(cbind(known_start, known_end) ~ src + icdv + dx,
  data = medicalcoder::get_icd_codes(),
  FUN = range
  )

cms9 <-
  with(subset(medicalcoder::get_icd_codes(), icdv == 9 & src == "cms"),
    paste(min(known_start), max(known_end), sep = " - "))
cdc9 <-
  with(subset(medicalcoder::get_icd_codes(), icdv == 9 & src == "cdc"),
    paste(min(known_start), max(known_end), sep = " - "))

cdc10 <-
  with(subset(medicalcoder::get_icd_codes(), icdv == 10 & src == "cdc"),
    paste(min(known_start), max(known_end), sep = " - "))

cms10 <-
  with(subset(medicalcoder::get_icd_codes(), icdv == 10 & src == "cms"),
    paste(min(known_start), max(known_end), sep = " - "))

who10 <-
  with(subset(medicalcoder::get_icd_codes(), icdv == 10 & src == "who"),
    paste(min(known_start), max(known_end), sep = " - "))

## ---- tbl-comorbidity-methods ----
cm <- data.table::fread(text = "
Comorbidity Method        | Notes
charlson_beyrer2021       | U.S. extension of Quan et al. (2005) by Beyrer et al. (2021)
charlson_cdmf2019         | ICD-9 and ICD-10 defined in [@glasheen2019]
charlson_deyo1992         | ICD-9 codes defined in Table 1 of Quan et al. (2005)
charlson_mimicivcode      | MIMIC-IV Charlson SQL from [`mimic-code`](https://github.com/MIT-LCP/mimic-code)
charlson_quan2005         | ICD-9 and ICD-10 defined in Table 1 of Quan et al. (2005); index scoring as reported in Table 2 of Quan et al. (2011)
charlson_quan2011         | ICD-9 and ICD-10 defined in Table 1 of Quan et al. (2005); index scoring as reported in Table 2 of Quan et al. (2011)
elixhauser_elixhauser1988 | ICD-9 codes defined in Table 2 of Quan et al. (2005)
elixhauser_ahrq_web       | ICD-9 codes defined in Table 2 of Quan et al. (2005)
elixhauser_quan2005       | ICD-9 and ICD-10 defined in Table 2 of Quan et al. (2005)
elixhauser_ahrq2022       | ICD-10 codes from AHRQ for fiscal year 2022
elixhauser_ahrq2023       | ICD-10 codes from AHRQ for fiscal year 2023
elixhauser_ahrq2024       | ICD-10 codes from AHRQ for fiscal year 2024
elixhauser_ahrq2025       | ICD-10 codes from AHRQ for fiscal year 2025
elixhauser_ahrq2026       | ICD-10 codes from AHRQ for fiscal year 2026
elixhauser_ahrq_icd10     | Any ICD-10 code from all elixhauser_ahrqYYYY
pccc_v2.0                 | ICD-9 and ICD-10 diagnostic and procedure codes consistent with pccc::ccc()
pccc_v2.1                 | Extended set of pccc_v2.0 codes based on pccc::ccc() and supplements to Feudtner et al. (2014)
pccc_v3.0                 | ICD-9 and ICD-10 diagnostic and procedure codes consistent with SAS code from Children's Hospital Assoication
pccc_v3.1                 | Extended set of pccc_v3.0 codes based on the supplements to Feinstein et al. (2024)"
)
cm <- cm[, lapply(.SD, gsub, pattern = "_", replacement = "\\\\_")]

kableExtra::kbl(
  x = cm,
  format = "latex",
  booktabs = TRUE,
  escape = FALSE,
) |>
kableExtra::kable_styling(
  latex_options = c("striped", "scale_down", "HOLD_position")
) |>
kableExtra::pack_rows(group_label = "Charlson",   start_row =  1, end_row =  4) |>
kableExtra::pack_rows(group_label = "Elixhauser", start_row =  5, end_row = 13) |>
kableExtra::pack_rows(group_label = "PCCC",       start_row = 14, end_row = 17)

## ---- tbl-mimicivdata-checksums ----
data.table::fread(
  file = file.path(Sys.getenv("MIMICIVDATA"), "SHA256SUMS.txt"),
  header = FALSE
)[grepl("^hosp.(admissions|patients|.+_icd)\\.csv\\.gz$", V2)] |>
kableExtra::kbl(
  x = _,
  col.names = c("sha256sum", "file"),
  format = "latex",
  booktabs = TRUE
  ) |>
kableExtra::kable_styling(
  latex_options = c("striped", "scale_down", "HOLD_position")
)

## ---- sha256-verify ----
verify_sha256sums <- function(dir, sums_file = "SHA256SUMS.txt") {
  old <- setwd(dir)
  on.exit(setwd(old), add = TRUE)

  sys <- Sys.info()[["sysname"]]

  if (identical(sys, "Windows")) {
    # Use CertUtil (built-in)
    lines <- readLines(sums_file, warn = FALSE)
    ok <- logical(length(lines))
    for (i in seq_along(lines)) {
      line <- trimws(lines[[i]])
      if (!nzchar(line)) { ok[[i]] <- TRUE; next }
      # Expected format: "<hash>  <filename>" or "<hash> *<filename>"
      parts <- strsplit(line, "\\s+")[[1]]
      hash <- parts[[1]]
      file <- sub("^\\*", "", parts[[2]])
      out <- suppressWarnings(system2("certutil", c("-hashfile", shQuote(file), "SHA256"), stdout = TRUE))
      got <- tolower(gsub("\\s+", "", out[grepl("^[0-9A-Fa-f]+$", out)]))
      ok[[i]] <- length(got) == 1 && tolower(hash) == got
    }
    return(all(ok))
  }

  # macOS often has `shasum -a 256`, Linux typically has `sha256sum`
  if (nzchar(Sys.which("sha256sum"))) {
    out <- system2("sha256sum", c("-c", sums_file), stdout = TRUE, stderr = TRUE)
    return(all(grepl(": OK$", out)))
  }

  if (nzchar(Sys.which("shasum"))) {
    out <- system2("shasum", c("-a", "256", "-c", sums_file), stdout = TRUE, stderr = TRUE)
    return(all(grepl(": OK$", out)))
  }

  stop("No SHA-256 tool found (need sha256sum, shasum, or certutil).")
}
stopifnot(verify_sha256sums(Sys.getenv("MIMICIVDATA")))

## ---- import-mimiciv-data ----
mimicivdata <-
  list.files(
    path = file.path(Sys.getenv("MIMICIVDATA"), "hosp"),
    pattern = "(admissions|patients|.+_icd)\\.csv\\.gz",
    full.names = TRUE
  )
names(mimicivdata) <- sub("\\.csv\\.gz$", "", basename(mimicivdata))

mimicivdata <- lapply(mimicivdata, data.table::fread)

## ---- build-mimicivdata-ages ----
mimicivdata_admissions_age <-
  mimicivdata$admissions[, list(subject_id, hadm_id, admittime)]
mimicivdata_patients_age <-
  mimicivdata$patients[, list(subject_id, anchor_age, anchor_year)]

mimicivdata$ages <-
  merge(
    x = mimicivdata_admissions_age,
    y = mimicivdata_patients_age,
    all = FALSE,
    by = "subject_id"
  )
mimicivdata$ages[
  ,
  anchor_year := as.POSIXct(paste0(anchor_year, "-01-01"), format = "%Y-%m-%d")
]
mimicivdata$ages[
  ,
  age :=
    anchor_age +
    as.numeric(difftime(admittime, anchor_year, units = "days")) / 365.25
]

## ---- build-mimicivdata-icd
mimicivdata$icd <-
  data.table::rbindlist(
    list(dx = mimicivdata$diagnoses_icd, pr = mimicivdata$procedures_icd),
    idcol = "dx",
    use.names = TRUE,
    fill = TRUE
  )

# set the diagnoses indicator to an integer as expected for input to
# medicalcoder::comorbidities()
mimicivdata$icd[, dx := as.integer(dx == "dx")]

## ---- one-data-set ----
mimicivDT <-
  merge(
    x = mimicivdata$patients[, .(subject_id)],
    y = mimicivdata$ages[, .(subject_id, hadm_id, age)],
    all = TRUE,
    by = c("subject_id")
  )

mimicivDT <-
  merge(
    x = mimicivdata$icd,
    y = mimicivDT,
    all = TRUE,
    by = c("subject_id", "hadm_id")
  )

mimicivDT <-
  merge(
    x = mimicivDT,
    y = mimicivdata$admissions[, .(subject_id, hadm_id, admittime)],
    all = TRUE,
    by = c("subject_id", "hadm_id")
  )

## ---- build-enc-seq ----
data.table::setorder(mimicivDT, subject_id, admittime)
mimicivDT[, enc_seq := cumsum(!duplicated(hadm_id)), by = .(subject_id)]
data.table::setkey(mimicivDT, subject_id, enc_seq, hadm_id)

## ---- build-cid ----
mimicivDT[, cid := paste(subject_id, hadm_id, sep = "__")]

# Sanity checks:
# expect age to always be increasing as enc_seq increase
mimicivDT[
  ,
  unique(.SD),
  .SDcols = c("subject_id", "hadm_id", "admittime", "age", "enc_seq")
  ][
  ,
  c(NA_real_, diff(age)),
  by = .(subject_id)
  ][
  ,
  all(V1 >= 0, na.rm = TRUE)
  ] |>
print() |>
stopifnot()

## ---- mimicivDT-data-structure ----
str(mimicivDT)
summary(mimicivDT)

## ---- mimicivdata-icd-codes
unique_icdcodes <- mimicivDT[, unique(.SD), .SDcols = c("icd_code", "icd_version", "dx")]

unique_icdcodes[!is.na(icd_code) & icd_version ==  9 & dx == 0, iscms := medicalcoder::is_icd(x = icd_code, icdv =  9, dx = 0, src = "cms")]
unique_icdcodes[!is.na(icd_code) & icd_version ==  9 & dx == 1, iscms := medicalcoder::is_icd(x = icd_code, icdv =  9, dx = 1, src = "cms")]
unique_icdcodes[!is.na(icd_code) & icd_version == 10 & dx == 0, iscms := medicalcoder::is_icd(x = icd_code, icdv = 10, dx = 0, src = "cms")]
unique_icdcodes[!is.na(icd_code) & icd_version == 10 & dx == 1, iscms := medicalcoder::is_icd(x = icd_code, icdv = 10, dx = 1, src = "cms")]

unique_icdcodes[!is.na(icd_code) & icd_version ==  9 & dx == 0, iscmshdrok := medicalcoder::is_icd(x = icd_code, icdv =  9, dx = 0, src = "cms", headerok = TRUE)]
unique_icdcodes[!is.na(icd_code) & icd_version ==  9 & dx == 1, iscmshdrok := medicalcoder::is_icd(x = icd_code, icdv =  9, dx = 1, src = "cms", headerok = TRUE)]
unique_icdcodes[!is.na(icd_code) & icd_version == 10 & dx == 0, iscmshdrok := medicalcoder::is_icd(x = icd_code, icdv = 10, dx = 0, src = "cms", headerok = TRUE)]
unique_icdcodes[!is.na(icd_code) & icd_version == 10 & dx == 1, iscmshdrok := medicalcoder::is_icd(x = icd_code, icdv = 10, dx = 1, src = "cms", headerok = TRUE)]

unique_icdcodes[, .N, keyby = .(iscms, iscmshdrok)]

subset(medicalcoder::get_icd_codes(with.description = TRUE), startsWith(code, "S73109"))
subset(medicalcoder::get_icd_codes(with.description = TRUE), code == "Z31")

## ---- charlson-variants ----
grep(
  pattern = "^charlson",
  x = names(medicalcoder::get_charlson_codes()),
  value = TRUE
)

## ---- medicalcoder-charlson-quan2011 ----
medicalcoder_charlson_quan2011 <-
  medicalcoder::comorbidities(
    data        = mimicivDT,  # object inheriting data.frame
    icd.codes   = "icd_code", # character string name of icd codes in data
    id.vars     = c("subject_id", "hadm_id"),
    icdv.var    = "icd_version",
    dx.var      = "dx",
    poa         = 1L, # consider all codes present on admission
    primarydx   = 0L, # consider all diagnosis codes secondary diagnoses.
    method      = "charlson_quan2011",
    flag.method = "current" # default
  )
data.table::setkey(medicalcoder_charlson_quan2011, subject_id, hadm_id)

## ---- medicalcoder-charlson-mimiciv ----
medicalcoder_charlson_mimiciv <-
  medicalcoder::comorbidities(
    data        = mimicivDT,  # object inheriting data.frame
    icd.codes   = "icd_code", # character string name of icd codes in data
    id.vars     = c("subject_id", "hadm_id"),
    icdv.var    = "icd_version",
    dx.var      = "dx",
    age.var     = "age",
    poa         = 1L,    # consider all codes present on admission
    primarydx   = 0L,    # consider all diagnosis codes secondary diagnoses.
    method      = "charlson_mimicivcode",
    flag.method = "current" # default
  )
data.table::setkey(medicalcoder_charlson_mimiciv, subject_id, hadm_id)

## ---- elixhauser-variants ----
grep(
  pattern = "^elixhauser",
  x = names(medicalcoder::get_elixhauser_codes()),
  value = TRUE
)

## ---- medicalcoder-elixhauser-quan2005 ----
medicalcoder_elixhauser_quan2005 <-
  medicalcoder::comorbidities(
    data        = mimicivDT,
    icd.codes   = "icd_code",
    id.vars     = c("subject_id", "hadm_id"),
    icdv.var    = "icd_version",
    dx.var      = "dx",
    poa         = 1L,
    primarydx   = 0L,
    method      = "elixhauser_quan2005"
  )
data.table::setkey(medicalcoder_elixhauser_quan2005, subject_id, hadm_id)

## ---- medicalcoder-pccc-v2.0 ----
medicalcoder_pccc_v2.0 <-
  medicalcoder::comorbidities(
    data        = mimicivDT,
    icd.codes   = "icd_code",
    id.vars     = c("subject_id", "hadm_id"),
    icdv.var    = "icd_version",
    dx.var      = "dx",
    poa         = 1L,
    method      = "pccc_v2.0"
  )
data.table::setkey(medicalcoder_pccc_v2.0, subject_id, hadm_id)

## ---- checksum-mimiciv-code ----
mimiciv_charlson_sql <-
  file.path(Sys.getenv("MIMICIVCODE"), "concepts", "comorbidity", "charlson.sql")

# checksum
stopifnot(
  substr(
    system(
      sprintf("sha256sum %s", mimiciv_charlson_sql),
      intern = TRUE
    ),
  start = 1L,
  stop = 64L
  ) == "5b797b673ace4dcbc6f686848d24371382f49ac61fcc39101f09c3b42969d801"
)

## ---- mimic-code-edits ----
# scan in the sql code
mimiciv_charlson_sql <-
  scan(
    file = mimiciv_charlson_sql,
    what = "character",
    sep = "\n",
    quiet = TRUE
  )

# modify the query to work in SQLite
# replace Google Big Query table names with table names to be using in the local
# RSQLite in memory database.
mimiciv_charlson_sql <-
  gsub(
    pattern = "physionet-data.mimiciv_hosp.admissions",
    replacement = "admissions",
    x = mimiciv_charlson_sql,
    fixed = TRUE
  )

mimiciv_charlson_sql <-
  gsub(
    pattern = "physionet-data.mimiciv_hosp.diagnoses_icd",
    replacement = "diagnoses",
    x = mimiciv_charlson_sql,
    fixed = TRUE
  )

mimiciv_charlson_sql <-
  gsub(
    pattern = "physionet-data.mimiciv_derived.age",
    replacement = "ages",
    x = mimiciv_charlson_sql,
    fixed = TRUE
  )

# Replace the Google Big Query SQL function GREATEST with MAX
mimiciv_charlson_sql <-
  gsub(
    pattern = "GREATEST",
    replacement = "MAX",
    x = mimiciv_charlson_sql,
    fixed = TRUE
  )

mimiciv_charlson_sql <- paste(mimiciv_charlson_sql, collapse = "\n")

## ---- mimiciv-charlson-results ----
con <- odbc::dbConnect(drv = RSQLite::SQLite(), dbname = ":memory:")

# add data to the data base
odbc::dbWriteTable(conn = con, name = "diagnoses",  value = mimicivdata$diagnoses)
odbc::dbWriteTable(conn = con, name = "admissions", value = mimicivdata$admissions)
odbc::dbWriteTable(conn = con, name = "patients",   value = mimicivdata$patients)
odbc::dbWriteTable(conn = con, name = "ages",       value = mimicivdata$ages)

# Run the query and record the time required to do so
mimiciv_charlson <- odbc::dbGetQuery(con, mimiciv_charlson_sql)
# close DB connection
odbc::dbDisconnect(conn = con)
data.table::setDT(mimiciv_charlson)
data.table::setkey(mimiciv_charlson, subject_id, hadm_id)

## ---- comorbidity-charlson ----
comorbidity_charlson_icd9 <-
  comorbidity::comorbidity(
    x       = subset(mimicivDT, icd_version == 9 & dx == 1L),
    id      = "cid",
    code    = "icd_code",
    map     = "charlson_icd9_quan",
    assign0 = TRUE # set less severe comorbidities flags to 0 when more severe
                   # comorbidities is also flagged
  )

comorbidity_charlson_icd10 <-
  comorbidity::comorbidity(
    x       = subset(mimicivDT, icd_version == 10 & dx == 1L),
    id      = "cid",
    code    = "icd_code",
    map     = "charlson_icd10_quan",
    assign0 = TRUE
  )

comorbidity_charlson <-
  rbind(
    comorbidity_charlson_icd9,
    comorbidity_charlson_icd10
  )

# aggregate between the icd versions
comorbidity_charlson <-
  aggregate(. ~ cid, data = comorbidity_charlson, FUN = max)

# apply the scoring, to use comorbidity::score the object needs to have certain
# attributes which were lost when aggregating over the icd versions.  Replace
# the attributes
attributes(comorbidity_charlson)[c("class", "variable.labels", "map")] <-
  attributes(comorbidity_charlson_icd9)[c("class", "variable.labels", "map")]

comorbidity_charlson[["score"]] <-
  comorbidity::score(
    x = comorbidity_charlson,
    weights = "quan",
    assign0 = TRUE
  )

# expand the id columns
data.table::setDT(comorbidity_charlson)
comorbidity_charlson[
  ,
  c("subject_id", "hadm_id") :=
    lapply(data.table::tstrsplit(cid, "__"), as.integer)
  ]
data.table::set(comorbidity_charlson, j = "cid", value = NULL)
data.table::setkey(comorbidity_charlson, subject_id, hadm_id)

## ---- comorbidity-elixhauser ----
comorbidity_elixhauser_icd9 <-
  comorbidity::comorbidity(
    x       = subset(mimicivDT, icd_version == 9 & dx == 1L),
    id      = "cid",
    code    = "icd_code",
    map     = "elixhauser_icd9_quan",
    assign0 = TRUE
  )

comorbidity_elixhauser_icd10 <-
  comorbidity::comorbidity(
    x       = subset(mimicivDT, icd_version == 10 & dx == 1L),
    id      = "cid",
    code    = "icd_code",
    map     = "elixhauser_icd10_quan",
    assign0 = TRUE
  )

comorbidity_elixhauser <-
  rbind(
    comorbidity_elixhauser_icd9,
    comorbidity_elixhauser_icd10
  )

# aggregate between the icd versions
comorbidity_elixhauser <-
  aggregate(. ~ cid, data = comorbidity_elixhauser, FUN = max)

# expand the id columns
data.table::setDT(comorbidity_elixhauser)
comorbidity_elixhauser[
  ,
  c("subject_id", "hadm_id") :=
    lapply(data.table::tstrsplit(cid, "__"), as.integer)
  ]
data.table::set(comorbidity_elixhauser, j = "cid", value = NULL)
data.table::setkey(comorbidity_elixhauser, subject_id, hadm_id)

## ---- pccc-pcccv2.0 ----
mimicivDT_for_pccc <-
  split(mimicivDT, by = "icd_version") |>
  lapply(
    data.table::dcast,
    formula = cid ~ ifelse(dx == 1, "DX", "PR") + seq_num,
    value.var = "icd_code"
  )

pccc_pcccv2.0 <-
  rbind(
    pccc::ccc(
      data = mimicivDT_for_pccc[["9"]],
      id = cid,
      dx_cols = grep("^DX", names(mimicivDT_for_pccc[["9"]]), value = TRUE),
      pc_cols = grep("^PR", names(mimicivDT_for_pccc[["9"]]), value = TRUE),
      icdv = 9
    )
    ,
    pccc::ccc(
      data = mimicivDT_for_pccc[["10"]],
      id = cid,
      dx_cols = grep("^DX", names(mimicivDT_for_pccc[["10"]]), value = TRUE),
      pc_cols = grep("^PR", names(mimicivDT_for_pccc[["10"]]), value = TRUE),
      icdv = 10
    )
  )

# aggregate over ICD version
pccc_pcccv2.0 <- aggregate(. ~ cid, data = pccc_pcccv2.0, FUN = max)

# expand the id columns
data.table::setDT(pccc_pcccv2.0)
pccc_pcccv2.0[
  ,
  c("subject_id", "hadm_id") :=
    lapply(data.table::tstrsplit(cid, "__"), as.integer)
  ]
data.table::set(pccc_pcccv2.0, j = "cid", value = NULL)
data.table::setkey(pccc_pcccv2.0, subject_id, hadm_id)

# ---- nrows-medicalcoder-vs-comorbidity ----
nrow(medicalcoder_charlson_quan2011)
nrow(comorbidity_charlson)

# ---- no-icd-no-comorbibity-result ----
# subject/hadm_id not in comorbibity::comorbibity() results all have no
# diagnostic codes.
mimicivDT[!comorbidity_charlson][, any(dx == 1 & !is.na(icd_code))]
medicalcoder_charlson_quan2011[!comorbidity_charlson][, all(cmrb_flag == 0L)]

## ---- medicalcoder-vs-comorbidity-charlson ----
mdcr_v_cmrb <-
  merge(
    x = medicalcoder_charlson_quan2011,
    y = comorbidity_charlson,
    all = FALSE,
    by = c("subject_id", "hadm_id"),
    suffixes = c("_mdcr", "_cmrb")
  )

## ---- mdcr-v-cmrb-charlson-column-map ----
mvc_columns <-
  data.table::fread(text = "
    medicalcoder   | comorbidity
    aidshiv        | aids
    cebvd          | cevd
    chf_mdcr       | chf_cmrb
    hp_mdcr        | hp_cmrb
    copd           | cpd
    dem            | dementia
    dm             | diab
    dmc            | diabwc
    mal            | canc
    mi_mdcr        | mi_cmrb
    mld_mdcr       | mld_cmrb
    msld_mdcr      | msld_cmrb
    mst            | metacanc
    pud_mdcr       | pud_cmrb
    pvd_mdcr       | pvd_cmrb
    rhd            | rheumd
    rnd            | rend
    cci            | score
  ")

## ---- column-by-column-mdcr-v-cmrb ----
data.table::set(
  x = mdcr_v_cmrb,
  j = "score",
  value = as.integer(mdcr_v_cmrb[["score"]])
)

for (i in seq_len(nrow(mvc_columns))) {
  x <- mvc_columns[["medicalcoder"]][i]
  y <- mvc_columns[["comorbidity"]][i]
  z <- identical(mdcr_v_cmrb[[x]], mdcr_v_cmrb[[y]])
  if (z) {
    message(sprintf("`%s` and `%s` are identical.", x, y))
    data.table::set(mdcr_v_cmrb, j = x, value = NULL)
    data.table::set(mdcr_v_cmrb, j = y, value = NULL)
  } else {
    stop(sprintf("`%s` and `%s` are not identical.", x, y))
  }
}

## ---- verify-only-remaining-mdcr-v-cmrb-columns ----
stopifnot(
  identical(
    names(mdcr_v_cmrb),
    c("subject_id", "hadm_id", "num_cmrb", "cmrb_flag", "age_score")
  )
)

## ---- remaining-mdcr_v_cmrb ----
str(mdcr_v_cmrb)

## ---- medicalcoder-vs-mimiciv-code-nrows ----
nrow(medicalcoder_charlson_mimiciv)
nrow(mimiciv_charlson)

## ---- no-comorb-no-row-in-mimic-code ----
mimicivDT[!mimiciv_charlson, on = c("hadm_id", "hadm_id")][, all(is.na(hadm_id))]

## ---- define-mdcr_v_mmcc ----
mdcr_v_mmcc <-
  merge(
    x = medicalcoder_charlson_mimiciv,
    y = mimiciv_charlson,
    all = FALSE,
    by = c("subject_id", "hadm_id"),
    suffixes = c("_mdcr", "_mimiciv")
  )

## ---- set-zeros-by-severity ----
# Set the MIMIC-IV code DM without cc to 0 when cc exist
mdcr_v_mmcc[
  diabetes_without_cc == 1L & diabetes_with_cc == 1L,
  diabetes_without_cc := 0L
]

# Set mild liver disease to 0 when sever liver disease exits for MIMIC-IV code
# results
mdcr_v_mmcc[
  mild_liver_disease == 1L & severe_liver_disease == 1L,
  mild_liver_disease := 0L
]

# set malignant_cancer to zero when metastatic_solid_tumor exits for MIMIC-IV
# code results
mdcr_v_mmcc[
  malignant_cancer == 1L & metastatic_solid_tumor == 1L,
  malignant_cancer := 0L
]

## ---- mdcr-v-mimic-column-mapping ----
mvm_columns <-
  data.table::fread(text = "
    medicalcoder   | mimicivcode
    aidshiv        | aids
    cebvd          | cerebrovascular_disease
    chf            | congestive_heart_failure
    copd           | chronic_pulmonary_disease
    dem            | dementia
    dm             | diabetes_without_cc
    dmc            | diabetes_with_cc
    hp             | paraplegia
    mal            | malignant_cancer
    mi             | myocardial_infarct
    mld            | mild_liver_disease
    msld           | severe_liver_disease
    mst            | metastatic_solid_tumor
    pud            | peptic_ulcer_disease
    pvd            | peripheral_vascular_disease
    rhd            | rheumatic_disease
    rnd            | renal_disease
    age_score_mdcr | age_score_mimiciv
    cci            | charlson_comorbidity_index
  ")

## ---- check-columns-mdcr-v-mmcc ----
for (i in seq_len(nrow(mvm_columns))) {
  x <- mvm_columns[["medicalcoder"]][i]
  y <- mvm_columns[["mimicivcode"]][i]
  z <- identical(mdcr_v_mmcc[[x]], mdcr_v_mmcc[[y]])
  if (z) {
    message(sprintf("`%s` and `%s` are identical.", x, y))
    data.table::set(mdcr_v_mmcc, j = x, value = NULL)
    data.table::set(mdcr_v_mmcc, j = y, value = NULL)
  } else {
    stop(sprintf("`%s` and `%s` are not identical.", x, y))
  }
}

## ---- sanity-check-mdcr-v-mmcc-remaining-columns ----
stopifnot(
  identical(
    names(mdcr_v_mmcc),
    c("subject_id", "hadm_id", "num_cmrb", "cmrb_flag")
  )
)

## ---- str-remaining-mdcr-v-mmcc ----
str(mdcr_v_mmcc)

## ---- medicalcoder-vs-comorbidity-elixhauser ----
mdcr_v_cmrb_elix <-
  merge(
    x = medicalcoder_elixhauser_quan2005,
    y = comorbidity_elixhauser,
    all = FALSE,
    by = c("subject_id", "hadm_id"),
    suffixes = c("_mdcr", "_cmrb")
  )

## ---- mdcr-v-cmrb-elixhauser-column-map ----
mvc_elix_columns <-
  data.table::fread(text = "
    medicalcoder        | comorbidity
    AIDS                | aids
    ALCOHOL             | alcohol
    ANEMDEF             | dane
    ARTH                | rheumd
    BLDLOSS             | blane
    CARDIAC_ARRHYTHMIAS | carit
    CHF                 | chf
    CHRNLUNG            | cpd
    COAG                | coag
    DEPRESS             | depre
    DM                  | diabunc
    DMCX                | diabc
    DRUG                | drug
    HTN_CX              | hypc
    HTN_UNCX            | hypunc
    HYPOTHY             | hypothy
    LIVER               | ld
    LYMPH               | lymph
    LYTES               | fed
    METS                | metacanc
    NEURO               | ond
    OBESE               | obes
    PARA                | para
    PERIVASC            | pvd
    PSYCH               | psycho
    PULMCIRC            | pcd
    RENLFAIL            | rf
    TUMOR               | solidtum
    ULCER               | pud
    VALVE               | valv
    WGHTLOSS            | wloss
  ")

## ---- column-by-column-mdcr-v-cmrb-elix ----
for (i in seq_len(nrow(mvc_elix_columns))) {
  x <- mvc_elix_columns[["medicalcoder"]][i]
  y <- mvc_elix_columns[["comorbidity"]][i]
  z <- identical(mdcr_v_cmrb_elix[[x]], mdcr_v_cmrb_elix[[y]])
  if (z) {
    message(sprintf("`%s` and `%s` are identical.", x, y))
    data.table::set(mdcr_v_cmrb_elix, j = x, value = NULL)
    data.table::set(mdcr_v_cmrb_elix, j = y, value = NULL)
  } else {
    stop(sprintf("`%s` and `%s` are not identical.", x, y))
  }
}

## ---- verify-only-remaining-mdcr-v-cmrb-columns-elixhauser ----
stopifnot(
  identical(
    names(mdcr_v_cmrb_elix),
    c("subject_id", "hadm_id", "HTN_C", "num_cmrb", "cmrb_flag", "mortality_index", "readmission_index")
  )
)

## ---- remaining-mdcr_v_cmrb_elix ----
str(mdcr_v_cmrb_elix)

## ---- nrow-medicalcoder-vs-pccc ----
nrow(medicalcoder_pccc_v2.0)
nrow(pccc_pcccv2.0)

## ---- define-mdcr_v_pccc ----
mdcr_v_pccc <-
  merge(
    x = medicalcoder_pccc_v2.0,
    y = pccc_pcccv2.0,
    all = FALSE,
    by = c("subject_id", "hadm_id"),
    suffixes = c("_mdcr", "_pccc")
  )

## ---- mvp-columns ----
mvp_columns <- data.table::fread(text = "
  medicalcoder | pccc
  congeni_genetic_mdcr | congeni_genetic_pccc
  cvd_mdcr | cvd_pccc
  gi_mdcr | gi_pccc
  hemato_immu_mdcr | hemato_immu_pccc
  malignancy_mdcr | malignancy_pccc
  metabolic_mdcr | metabolic_pccc
  neonatal_mdcr | neonatal_pccc
  neuromusc_mdcr | neuromusc_pccc
  renal_mdcr | renal_pccc
  respiratory_mdcr | respiratory_pccc
  cmrb_flag | ccc_flag"
)

## ---- check-identical-columns-mdcr-v-pccc ----
for (i in seq_len(nrow(mvp_columns))) {
  x <- mvp_columns[["medicalcoder"]][i]
  y <- mvp_columns[["pccc"]][i]
  z <- identical(mdcr_v_pccc[[x]], mdcr_v_pccc[[y]])
  if (z) {
    message(sprintf("`%s` and `%s` are identical.", x, y))
    data.table::set(mdcr_v_pccc, j = x, value = NULL)
    data.table::set(mdcr_v_pccc, j = y, value = NULL)
  } else {
    stop(sprintf("`%s` and `%s` are not identical.", x, y))
  }
}

## ---- structre-remaining-mdcr-v-pccc ----
str(mdcr_v_pccc)

## ---- any-tech-dep-and-transplant ----
mdcr_v_pccc[, .N, keyby = .(medicalcoder = any_tech_dep, pccc = tech_dep)]
mdcr_v_pccc[, .N, keyby = .(medicalcoder = any_transplant, pccc = transplant)]

## ---- medicalcoder-charlson-current ----
medicalcoder_charlson_current <-
  medicalcoder::comorbidities(
    data        = mimicivDT,
    icd.codes   = "icd_code",
    id.vars     = c("subject_id", "enc_seq"),
    icdv.var    = "icd_version",
    dx.var      = "dx",
    poa         = 1L,
    primarydx   = 0L,
    method      = "charlson_quan2011",
    flag.method = "current"
  )

## ---- medicalcoder-charlson-cumulative ----
medicalcoder_charlson_cumulative <-
  medicalcoder::comorbidities(
    data        = mimicivDT,
    icd.codes   = "icd_code",
    id.vars     = c("subject_id", "enc_seq"),
    icdv.var    = "icd_version",
    dx.var      = "dx",
    poa         = 1L,
    primarydx   = 0L,
    method      = "charlson_quan2011",
    flag.method = "cumulative"
  )

## ---- medicalcoder-elixhauser-current ----
medicalcoder_elixhauser_current <-
  medicalcoder::comorbidities(
    data        = mimicivDT,
    icd.codes   = "icd_code",
    id.vars     = c("subject_id", "enc_seq"),
    icdv.var    = "icd_version",
    dx.var      = "dx",
    poa         = 1L,
    primarydx   = 0L,
    method      = "elixhauser_quan2005",
    flag.method = "current"
  )

## ---- medicalcoder-elixhauser-cumulative ----
medicalcoder_elixhauser_cumulative <-
  medicalcoder::comorbidities(
    data        = mimicivDT,
    icd.codes   = "icd_code",
    id.vars     = c("subject_id", "enc_seq"),
    icdv.var    = "icd_version",
    dx.var      = "dx",
    poa         = 1L,
    primarydx   = 0L,
    method      = "elixhauser_quan2005",
    flag.method = "cumulative"
  )

## ---- medicalcoder-pcccv2.0-current ----
medicalcoder_pcccv2.0_current <-
  medicalcoder::comorbidities(
    data        = mimicivDT,
    icd.codes   = "icd_code",
    id.vars     = c("subject_id", "enc_seq"),
    icdv.var    = "icd_version",
    dx.var      = "dx",
    poa         = 1L,
    method      = "pccc_v2.0",
    flag.method = "current"
  )

## ---- medicalcoder-pcccv2.0-cumulative ----
medicalcoder_pcccv2.0_cumulative <-
  medicalcoder::comorbidities(
    data        = mimicivDT,
    icd.codes   = "icd_code",
    id.vars     = c("subject_id", "enc_seq"),
    icdv.var    = "icd_version",
    dx.var      = "dx",
    poa         = 1L,
    method      = "pccc_v2.0",
    flag.method = "cumulative"
  )

## ---- medicalcoder-pcccv3.1-current ----
medicalcoder_pcccv3.1_current <-
  medicalcoder::comorbidities(
    data        = mimicivDT,
    icd.codes   = "icd_code",
    id.vars     = c("subject_id", "enc_seq"),
    icdv.var    = "icd_version",
    dx.var      = "dx",
    poa         = 1L,
    method      = "pccc_v3.1",
    flag.method = "current"
  )

## ---- medicalcoder-pcccv3.1-cumulative ----
medicalcoder_pcccv3.1_cumulative <-
  medicalcoder::comorbidities(
    data        = mimicivDT,
    icd.codes   = "icd_code",
    id.vars     = c("subject_id", "enc_seq"),
    icdv.var    = "icd_version",
    dx.var      = "dx",
    poa         = 1L,
    method      = "pccc_v3.1",
    flag.method = "cumulative"
  )

## ---- build-flagmethod-delta ----
flagmethod_delta <-
  data.table::rbindlist(
    list(
      medicalcoder_charlson_current = medicalcoder_charlson_current,
      medicalcoder_charlson_cumulative = medicalcoder_charlson_cumulative,
      medicalcoder_elixhauser_current = medicalcoder_elixhauser_current,
      medicalcoder_elixhauser_cumulative = medicalcoder_elixhauser_cumulative,
      medicalcoder_pcccv2.0_current = medicalcoder_pcccv2.0_current,
      medicalcoder_pcccv2.0_cumulative = medicalcoder_pcccv2.0_cumulative,
      medicalcoder_pcccv3.1_current = medicalcoder_pcccv3.1_current,
      medicalcoder_pcccv3.1_cumulative = medicalcoder_pcccv3.1_cumulative
    ),
    use.names = TRUE,
    fill = TRUE,
    idcol = "obj"
  )

flagmethod_delta[
  ,
  c("algo", "flag.method") := data.table::tstrsplit(obj, "_", keep = 2:3)
]

flagmethod_delta[, obj := NULL]

flagmethod_delta <-
  data.table::melt(
    data = flagmethod_delta,
    id.vars = c("subject_id", "enc_seq", "algo", "flag.method"),
    variable.factor = FALSE,
    variable.name = "condition",
    na.rm = TRUE
  )

flagmethod_delta <-
  flagmethod_delta[
   , .(N = sum(value), p = mean(value)),
   by = .(algo, flag.method, condition)
 ]

flagmethod_delta[,
  flag.method := factor(flag.method, levels = c("current", "cumulative"))
]

flagmethod_delta <-
  flagmethod_delta[
    !(condition %in% c("readmission_index", "mortality_index",
        "cci", "num_cmrb", "age_score"))
  ]

flagmethod_delta[condition == "cmrb_flag"]

## ---- define-flagmethod-plot ----
flagmethod_plot <- function(data) {
  ggplot2::ggplot(data = data) +
    ggplot2::theme_bw() +
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
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      legend.position = "bottom",
    ) +
    ggplot2::coord_flip()
}

## ---- fig-flagmethod-delta-charlson ----
flagmethod_plot(flagmethod_delta[algo == "charlson"])

## ---- fig-flagmethod-delta-elixhauser ----
flagmethod_plot(flagmethod_delta[algo == "elixhauser"])

## ---- fig-flagmethod-delta-pcccv2.0 ----
flagmethod_plot(flagmethod_delta[algo == "pcccv2.0"])

## ---- fig-flagmethod-delta-pcccv3.1 ----
flagmethod_plot(flagmethod_delta[algo == "pcccv3.1"])

## ---- build-dmseverity ----
dmseverity <-
  merge(
    x = medicalcoder_charlson_current,
    y = medicalcoder_charlson_cumulative,
    by = c("subject_id", "enc_seq"),
    suffix = c("_current", "_cumulative")
  )

## ---- s10009326 ----
dmseverity[,
  .(
    V1 = mean(dm_current + dmc_current),
    V2 = mean(dm_cumulative + dmc_cumulative),
    .N
   ),
 by = .(subject_id)
 ][, d := V2 - V1][d > 0 & N > 5]

s10009326 <-
  dmseverity[
    subject_id == 10009326,
    .SD,
    .SDcols = patterns("enc_seq|^dm")
  ]

## ---- s19855614 ----
s19855614 <-
  merge(
    x = medicalcoder_pcccv3.1_current[subject_id == 19855614],
    y = medicalcoder_pcccv3.1_cumulative[subject_id == 19855614],
    all = TRUE,
    by = c("subject_id", "enc_seq"),
    suffixes = c("_current", "_cumulative")
 )
Filter(f = function(x) sum(x) > 0, s19855614)
# need gi, malignancy, metabolic, neuromusc, respriatory,

## ---- s10728333 ----
s10728333 <-
  merge(
    x = medicalcoder_pcccv3.1_current[subject_id == 10728333],
    y = medicalcoder_pcccv3.1_cumulative[subject_id == 10728333],
    all = TRUE,
    by = c("subject_id", "enc_seq"),
    suffixes = c("_current", "_cumulative")
 )
Filter(f = function(x) sum(x) > 0, s10728333)
# metabolic, respriatory, any_tech, num_cmrb, misc,

## ---- tbl-s10728333 ----
# NOTE: not using the quarto yaml for the table crossref so I can use short
# captions
DT0 <- s10728333[, .SD, .SDcols = patterns("enc_seq|^(met|resp|misc|num_|any_tech)")]
DT1 <-
  DT0[
    ,
    .(enc_seq,
      metabolic__current = data.table::fcase(
        metabolic_dxpr_only_current == 1, "DxPr",
        metabolic_tech_only_current == 1, "Tech",
        metabolic_dxpr_and_tech_current == 1, "DxPrTech",
        default = ""),
      metabolic__cumulative = data.table::fcase(
        metabolic_dxpr_only_cumulative == 1, "DxPr",
        metabolic_tech_only_cumulative == 1, "Tech",
        metabolic_dxpr_and_tech_cumulative == 1, "DxPrTech",
        default = ""),
      misc__current = data.table::fcase(
        misc_dxpr_only_current == 1, "DxPr",
        misc_tech_only_current == 1, "Tech",
        misc_dxpr_and_tech_current == 1, "DxPrTech",
        default = ""),
      misc__cumulative = data.table::fcase(
        misc_dxpr_only_cumulative == 1, "DxPr",
        misc_tech_only_cumulative == 1, "Tech",
        misc_dxpr_and_tech_cumulative == 1, "DxPrTech",
        default = ""),
      respiratory__current = data.table::fcase(
        respiratory_dxpr_only_current == 1, "DxPr",
        respiratory_tech_only_current == 1, "Tech",
        respiratory_dxpr_and_tech_current == 1, "DxPrTech",
        default = ""),
      respiratory__cumulative = data.table::fcase(
        respiratory_dxpr_only_cumulative == 1, "DxPr",
        respiratory_tech_only_cumulative == 1, "Tech",
        respiratory_dxpr_and_tech_cumulative == 1, "DxPrTech",
        default = ""),
      any_tech_dep_current = data.table::fifelse(any_tech_dep_current == 1, "1", ""),
      any_tech_dep_cumulative = data.table::fifelse(any_tech_dep_cumulative == 1, "1", ""),
      num_cmrb_current = as.character(num_cmrb_current),
      num_cmrb_cumulative = as.character(num_cmrb_cumulative)
    )
  ]

ftm1 <- "*"#kableExtra::footnote_marker_symbol(1, format = "latex")
ftm2 <- "\\dag"#kableExtra::footnote_marker_symbol(2, format = "latex")
DT1[enc_seq == 1, `:=`(respiratory__current = ftm1, respiratory__cumulative = ftm1)]
DT1[enc_seq %in% 2:3,`:=`(respiratory__cumulative = ftm2)]
DT1[enc_seq == 3, `:=`(misc__current = ftm1, misc__cumulative = ftm1)]

kableExtra::kbl(
  x = DT1,
  col.names = c("Encounter", rep(c("Current", "Cumulative"), 5)),
  format = "latex",
  booktabs = TRUE,
  row.names = FALSE,
  align = rep("c", ncol(DT1)),
  escape = FALSE,
  caption.short = "Flagging of comorbidities under PCCC v3 for MIMIC-IV subject 10728333.",
  caption = "Flagging of comorbidities under PCCC v3 for MIMIC-IV subject 10728333. Cells marked with DxPr denote that the comorbidity was flagged due to a ICD code that is not technology-dependent.  Tech denotes flagging of a comorbidity base on the presence of at least one technology-dependent ICD code and the presence of at least one non-technology-dependent code for another comorbidity. DxPrTech denotes flagging due to both technology-dependent and non-technology-dependent ICD codes.  Under the Any Tech columns we mark the encounters where some tech dependence is identified. For this patient, on encounter 1, a technology-dependent ICD code for a respiratory comorbidity was found in the subject's record.  Because no non-technology-dependent code was observed on encounter 1, under PCCC v3 this subject has no comorbidities for encounter 1.  This persists for encounters 2 and 3 with respect to respiratory and on encounter 3 for miscellaneous as well.  On encounter 4 a non-technology-dependent code for a metabolic comorbidity was reported.  Under a cumulative flagging paradigm the miscellaneous and respiratory comorbidities are also flagged as the reported codes from encounters 3 and 1 have been carried forward respectively.  Under the 'current' flagging method miscellaneous is never flagged as the technology-dependent code never occurs on the same encounter as the non-technology-dependent code."
  ) |>
kableExtra::kable_styling(
  latex_options = c("striped", "scale_down", "HOLD_position")
) |>
kableExtra::add_header_above(
  header = c("", "Metabolic" = 2, "Miscellaneous" = 2, "Respiratory" = 2, "Any Tech" = 2, "Number of Comorbiditys" = 2)
) |>
kableExtra::add_footnote(
  label = c("A technology-dependent code was reported on this encounter.  Since no non-technology-dependent was reported no comorbidities are flagged.",
            "A technology-dependent code has been carried forward in the record.  Since no non-technology-dependent was reported on this, or a prior, enconter, no comorbidities are flagged."),
  notation = "symbol",
  threeparttable = TRUE
)

## ---- tbl-dm-s10009326 ----
s10009326[
  ,
  lapply(.SD, function(x) data.table::fifelse(x == 1, "X", "")),
  .SDcols = patterns("^dm"),
  by = .(enc_seq)
  ] |>
kableExtra::kbl(
  x = _,
  format = "latex",
  booktabs = TRUE,
  align = "ccccc",
  row.names = FALSE,
  col.names = c("Encounter", "Complicated", "Uncomplicated", "Complicated", "Uncomplicated")
  ) |>
kableExtra::kable_styling(
  latex_options = c("striped", "scale_down", "HOLD_position")
) |>
kableExtra::add_header_above(
  header = c("", "Current" = 2, "Cumulative" = 2)
)

## ---- ICD-10-categories-dm-quan ----
subset(
  x = medicalcoder::get_charlson_codes(),
  subset = charlson_quan2005 == 1L & condition %in% c("dm", "dmc") & icdv == 10,
)[["code"]] |>
substring(1, 3) |>
unique() |>
sort()

## ---- minimum-two-encounters-at-least-one-with-dm ----
sids <-
  medicalcoder_charlson_current[
    ,
    .(encounters = length(unique(enc_seq)), any_dm = sum(dm + dmc)),
    by = .(subject_id)
  ][
    encounters > 1 & any_dm > 0,
    subject_id
  ]

dms <-
  merge(
    x = medicalcoder_charlson_current[
          subject_id %in% sids,
          .(subject_id, enc_seq, dm_current = dm, dmc_current = dmc)
        ],
    y = medicalcoder_charlson_cumulative[
          subject_id %in% sids,
          .(subject_id, enc_seq, dm_cumulative = dm, dmc_cumulative = dmc)
        ],
    all = TRUE,
    by = c("subject_id", "enc_seq")
  )
data.table::setDT(dms)
dms[, current := (dm_current + dmc_current)]
dms[, cumulative := (dm_cumulative + dmc_cumulative)]
dms[, total_encs := max(enc_seq), by = .(subject_id)]
dms

## ---- Type-1-DM ----
type1subjects <-
  mimicivDT[subject_id %in% sids & startsWith(icd_code, "E10") & icd_version == 10L, subject_id]

subset(
  x      = medicalcoder::get_icd_codes(with.description = TRUE),
  subset = full_code == "E10.65" & src == "cms"
)
# Subject  10023239
dms[subject_id %in% type1subjects][cumulative_between_current_flags == 1]
dms[subject_id == 10252385]

## ---- cumulative-flags-between-and-after-current-flag ----
dms[, current_flags_before := as.integer((cumsum(current) - current) > 0), by = .(subject_id)]
dms[, current_flags_after  := as.integer(rev(cumsum(rev(current)) - current) > 0), by = .(subject_id)]

dms[, cumulative_between_current_flags   := as.integer((current != cumulative) & current_flags_before == 1L & current_flags_after == 1L)]
dms[, cumulative_only_after_current_flag := as.integer((current != cumulative) & current_flags_before == 1L & current_flags_after == 0L)]

# Number of Encounters:
dms[, .(Encounters = .N), keyby = .(cumulative_between_current_flags, cumulative_only_after_current_flag)]

## ---- cumulative-flags-between-and-after-current-flag-subject-level ----
# counts of subjects with encounters flagged
dms[
    ,
    .(
      between = any(cumulative_between_current_flags),
      after = any(cumulative_only_after_current_flag)
    ),
    by = .(subject_id)
  ][
    ,
    .(Subjects = .N),
    keyby = .(between, after)
  ]

## ---- ahrq-icd-10-dm-codes ----
ahrq_icd10_dm_codes <-
  subset(
    x = medicalcoder::get_elixhauser_codes(),
    subset = elixhauser_ahrq_icd10 == 1 & condition %in% c("DIAB_CX", "DIAB_UNCX"),
    select = c("code", "condition")
  )
data.table::setDT(ahrq_icd10_dm_codes)
ahrq_icd10_dm_codes[, category := substring(code, 1, 3)]

ahrq_icd10_dm_codes[, .N, keyby = .(category, condition)] |>
  data.table::dcast(category ~ condition, value.var = "N", fill = 0L)

## ---- gestational-and-non-gestational-dm ----
DT <- mimicivDT[icd_code %in% ahrq_icd10_dm_codes[condition == "DIAB_UNCX", code]]
DT[, Gestational := any(startsWith(icd_code, "O")), by = .(subject_id)]
DT[, `Non-Gestational` := any(startsWith(icd_code, "E")), by = .(subject_id)]
DT[
  , unique(.SD), .SDcols = c("subject_id", "Gestational", "Non-Gestational")
  ][
  , .(Subjects = .N), keyby = .(Gestational, `Non-Gestational`)
  ]

## ---- history-of-malignant ----
personal_history_of_malignant <-
  subset(
    x = medicalcoder::get_icd_codes(with.description = TRUE),
    subset = src == "cms" & grepl("personal history of malignant", desc, ignore.case = TRUE),
    select = c("code", "icdv", "dx")
  )
personal_history_of_malignant[["history_of"]] <- 1L

charlson_quan2011_mal <-
  subset(
    x = medicalcoder::get_charlson_codes(),
    subset = condition == "mal" & charlson_quan2011 == 1L,
    select = c("code", "icdv", "dx")
  )
charlson_quan2011_mal[["observed_mal"]] <- 1L

mal <-
  merge(
    x = mimicivDT[!is.na(icd_code), .(subject_id, enc_seq, icd_code, icd_version, dx)],
    y = personal_history_of_malignant,
    all.x = TRUE,
    by.x = c("icd_code", "icd_version", "dx"),
    by.y = c("code", "icdv", "dx")
  )

mal <-
  merge(
    x = mal,
    y = charlson_quan2011_mal,
    all.x = TRUE,
    by.x = c("icd_code", "icd_version", "dx"),
    by.y = c("code", "icdv", "dx")
  )
data.table::setkey(mal, subject_id, enc_seq)

# set flag for any history or observation of a malignancy
mal <-
  mal[
    ,
    .(
      enc_history_of   = as.integer(any(history_of, na.rm = TRUE)),
      enc_observed_mal = as.integer(any(observed_mal, na.rm = TRUE))
    ),
    by = .(subject_id, enc_seq)
  ]
mal[, subject_history_of   := as.integer(sum(enc_history_of) > 0), by = .(subject_id)]
mal[, subject_observed_mal := as.integer(sum(enc_observed_mal) > 0), by = .(subject_id)]

mal <- mal[subject_history_of + subject_observed_mal > 0]

# join on the cumulative flags from medicalcoder for both malignancy and
# metastatic solid tumors.  An encounter with both a malignancy and a metastatic
# solid tumor will have the malignancy flag set to zero and retain only the more
# severe condition.
mal <-
  merge(
    x = mal,
    y = medicalcoder_charlson_cumulative[, .(subject_id, enc_seq, medicalcoder_cummulative_mal = mal, medicalcoder_cummulative_mst = mst)],
    all.x = TRUE,
    by = c("subject_id", "enc_seq")
  )

mal[, .(Encounters = .N), keyby = .(enc_observed_mal, enc_history_of)]

## ---- mal-subject-level-variables ----
subject_history <-
  mal[
    ,
    lapply(lapply(.SD, any), as.integer),
    .SDcols = c("subject_history_of", "subject_observed_mal"),
    by = .(subject_id)
  ]
subject_history[
    ,
    .(subjects = .N), keyby = .(subject_history_of, subject_observed_mal)
  ]

## ---- ahrq-longitudinial-example ----
common_args <-
  list(
    data = mimicivDT,
    id.vars = c("subject_id", "enc_seq"),
    icd.codes = "icd_code",
    icdv.var = "icd_version",
    dx.var = "dx",
    poa = 0L,
    primarydx = 0L,
    method = "elixhauser_ahrq_icd10"
  )

flgcurrent <-
  do.call(
    medicalcoder::comorbidities,
    c(common_args, list(flag.method = "current"))
  )

flgcumulative <-
  do.call(
    medicalcoder::comorbidities,
    c(common_args, list(flag.method = "cumulative"))
  )

## ---- s19997538 ----
mimicivDT[subject_id == 19997538 & icd_code == "K760"]

## ---- K760 ----
subset(medicalcoder::get_icd_codes(with.description = TRUE), full_code == "K76.0")
subset(medicalcoder::get_elixhauser_codes(), full_code == "K76.0")
subset(medicalcoder::get_elixhauser_poa(), condition == "LIVER_MLD")

## ---- s19997538-current-v-cumulative ----
merge(
  x = flgcurrent,
  y = flgcumulative,
  by = c("subject_id", "enc_seq"),
  suffixes = c("_current", "_cumulative")
)[
  subject_id == 19997538,
  .SD,
  .SDcols = patterns("^(subject_id|enc_seq|LIVER_MLD_c)")
]


## ---- figure2 ----
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

figure2data <-
  data.table::rbindlist(
    list(
      "Charlson"   = flgmthddeltas(current = medicalcoder_charlson_current,   cumulative = medicalcoder_charlson_cumulative),
      "Elixhauser" = flgmthddeltas(current = medicalcoder_elixhauser_current, cumulative = medicalcoder_elixhauser_cumulative),
      "PCCC v3.1"  = flgmthddeltas(current = medicalcoder_pcccv3.1_current,   cumulative = medicalcoder_pcccv3.1_cumulative)
    ),
    idcol = "method"
  )

medicalcoder_charlson_cumulative[cmrb_flag == 1, data.table::uniqueN(.SD, by = c("subject_id"))]
medicalcoder_charlson_cumulative[cmrb_flag == 1, data.table::uniqueN(.SD, by = c("subject_id", "enc_seq"))]

figure2data[, case := factor(
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

figure2data <- data.table::melt(data = figure2data, measure.vars = c("sp", "ep"))

data.table::setkey(figure2data, method, case, variable)

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

a <- g(figure2data[variable == "sp"], N = "subjects_with_a_cmrb_flag")# + ggplot2::xlab("Algorithm (Subjects with at least one encounter with at least one comorbidity)")
b <- g(figure2data[variable == "ep"], N = "encounters_with_a_cmrb_flag")# + ggplot2::xlab("Algorithm (Encounters with at least one comorbidity)")

figure2 <-
  ggpubr::ggarrange(
    a, b,
    ncol = 1,
    labels = c("(A)", "(B)"),
    label.x = 0.06,
    label.y = 0.93,
    common.legend = TRUE,
    legend = "right"
  )

## ---- save-figure2 ----
ggplot2::ggsave(
  filename = "figure2.pdf",
  plot = figure2,
  width = 8,
  height = 5,
  device = grDevices::cairo_pdf
)

## ---- figure2-pros ----
fmtn <- function(x) formatC(x, format = "d", big.mark = ",")
figure2pros <- list()
figure2pros[["N"]]  <- figure2data[method == "Elixhauser"][1, fmtn(subjects_with_a_cmrb_flag)]
figure2pros[["cs"]] <-
  figure2data[
    method == "Elixhauser" & variable == "sp",
    sprintf("%s (%s%%)", fmtn(subjects), formatC(100 * value, format = "f", digits = 1))
  ]
figure2pros[["es"]] <-
  figure2data[
    method == "Elixhauser" & variable == "ep",
    sprintf("%s (%s%%)", fmtn(subjects), formatC(100 * value, format = "f", digits = 1))
  ]

## ---- s10728333-manuscript ----
s10728333 <-
  merge(
    x = medicalcoder_pcccv3.1_current[subject_id == 10728333],
    y = medicalcoder_pcccv3.1_cumulative[subject_id == 10728333],
    all = TRUE,
    by = c("subject_id", "enc_seq"),
    suffixes = c("_current", "_cumulative")
 )
DT0 <- s10728333[subject_id == 10728333, .SD, .SDcols = patterns("enc_seq|^(met|resp|misc|num_|any_tech)")]
DT1 <-
  DT0[
    ,
    .(enc_seq,
      metabolic__current = data.table::fcase(
        metabolic_dxpr_only_current == 1, "DxPr",
        metabolic_tech_only_current == 1, "Tech",
        metabolic_dxpr_and_tech_current == 1, "DxPrTech",
        default = ""),
      metabolic__cumulative = data.table::fcase(
        metabolic_dxpr_only_cumulative == 1, "DxPr",
        metabolic_tech_only_cumulative == 1, "Tech",
        metabolic_dxpr_and_tech_cumulative == 1, "DxPrTech",
        default = ""),
      misc__current = data.table::fcase(
        misc_dxpr_only_current == 1, "DxPr",
        misc_tech_only_current == 1, "Tech",
        misc_dxpr_and_tech_current == 1, "DxPrTech",
        default = ""),
      misc__cumulative = data.table::fcase(
        misc_dxpr_only_cumulative == 1, "DxPr",
        misc_tech_only_cumulative == 1, "Tech",
        misc_dxpr_and_tech_cumulative == 1, "DxPrTech",
        default = ""),
      respiratory__current = data.table::fcase(
        respiratory_dxpr_only_current == 1, "DxPr",
        respiratory_tech_only_current == 1, "Tech",
        respiratory_dxpr_and_tech_current == 1, "DxPrTech",
        default = ""),
      respiratory__cumulative = data.table::fcase(
        respiratory_dxpr_only_cumulative == 1, "DxPr",
        respiratory_tech_only_cumulative == 1, "Tech",
        respiratory_dxpr_and_tech_cumulative == 1, "DxPrTech",
        default = ""),
      any_tech_dep_current = data.table::fifelse(any_tech_dep_current == 1, "1", ""),
      any_tech_dep_cumulative = data.table::fifelse(any_tech_dep_cumulative == 1, "1", ""),
      num_cmrb_current = as.character(num_cmrb_current),
      num_cmrb_cumulative = as.character(num_cmrb_cumulative)
    )
  ]

ftm1 <- "*"
ftm2 <- "†"
DT1[enc_seq == 1, `:=`(respiratory__current = ftm1, respiratory__cumulative = ftm1)]
DT1[enc_seq %in% 2:3,`:=`(respiratory__cumulative = ftm2)]
DT1[enc_seq == 3, `:=`(misc__current = ftm1, misc__cumulative = ftm1)]

DT1 |>
  gt::gt(caption = "") |>
  gt::cols_align(align = "center") |>
  gt::tab_spanner("Metabolic", columns = c(metabolic__current, metabolic__cumulative)) |>
  gt::tab_spanner("Miscellaneous", columns = c(misc__current, misc__cumulative)) |>
  gt::tab_spanner("Respiratory", columns = c(respiratory__current, respiratory__cumulative)) |>
  gt::tab_spanner("Any Tech", columns = c(any_tech_dep_current, any_tech_dep_cumulative)) |>
  gt::tab_spanner("Comorbidity Count", columns = c(num_cmrb_current, num_cmrb_cumulative)) |>
  gt::cols_label(
    enc_seq = "Encounter",
    metabolic__current = "Current", metabolic__cumulative = "Cumulative",
    misc__current = "Current", misc__cumulative = "Cumulative",
    respiratory__current = "Current", respiratory__cumulative = "Cumulative",
    any_tech_dep_current = "Current", any_tech_dep_cumulative = "Cumulative",
    num_cmrb_current = "Current", num_cmrb_cumulative = "Cumulative"
  ) |>
  gt::tab_footnote(footnote = "* A technology-dependent code was reported on this encounter.  Since no non-technology-dependent code was reported no comorbidities are flagged.") |>
  gt::tab_footnote(footnote = "† A technology-dependent code has been carried forward in the record.  Since no non-technology-dependent code was reported on this, or a prior, encounter, no comorbidities are flagged.") |>
  #gt::opt_table_font(size = 19)
  #gt::tab_options(table.font.size = gt::px(10))
  gt::tab_style(
    style = gt::cell_text(size = "8pt"),
    locations = gt::cells_body()
  ) |>
  gt::tab_style(
    style = gt::cell_text(size = "8pt", weight = "bold"),
    locations = gt::cells_column_labels()
  )

## ---- benchmarking ----
lines <- readLines("mimiciv-data-analysis.R")

chunk_pattern <- "^## ---- .* ----"
marker_lines  <- which(grepl(chunk_pattern, lines))
marker_labels <- lines[marker_lines]

get_chunk_by_label <- function(label) {
  idx <- which(grepl(label, marker_labels, fixed = TRUE))
  start_line <- marker_lines[idx] + 1L
  end_line   <- marker_lines[idx+1L] - 1L
  parse(text = lines[start_line:end_line])
}

benchmarks <-
  microbenchmark::microbenchmark(
    medicalcoder__charlson__current      = eval(get_chunk_by_label("medicalcoder-charlson-current")),
    medicalcoder__charlson__cumulative   = eval(get_chunk_by_label("medicalcoder-charlson-cumulative")),
    medicalcoder__elixhauser__current    = eval(get_chunk_by_label("medicalcoder-elixhauser-current")),
    medicalcoder__elixhauser__cumulative = eval(get_chunk_by_label("medicalcoder-elixhauser-cumulative")),
    medicalcoder__pcccv2.0__current      = eval(get_chunk_by_label("medicalcoder-pcccv2.0-current")),
    medicalcoder__pcccv2.0__cumulative   = eval(get_chunk_by_label("medicalcoder-pcccv2.0-cumulative")),
    medicalcoder__pcccv3.1__current      = eval(get_chunk_by_label("medicalcoder-pcccv3.1-current")),
    medicalcoder__pcccv3.1__cumulative   = eval(get_chunk_by_label("medicalcoder-pcccv3.1-cumulative")),
    mimiciv__charlson__current           = eval(get_chunk_by_label("mimiciv-charlson-results")),
    comorbidity__charlson__current       = eval(get_chunk_by_label("comorbidity-charlson")),
    comorbidity__elixhauser__current     = eval(get_chunk_by_label("comorbidity-elixhauser")),
    pccc__pcccv2.0__current              = eval(get_chunk_by_label("pccc-pcccv2.0")),
    times = 10L
  )

benchmarksDT <- data.table::as.data.table(data.table::copy(benchmarks))
benchmarksDT[, c("Tool", "Algorithm", "Flag Method") := data.table::tstrsplit(expr, "__")]

benchmark_summary_figure <-
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

benchmark_time_to_compute <-
  benchmarksDT[, .(value = qwraps2::median_iqr(time/1e9)), by = .(Tool, Algorithm, `Flag Method`)]
benchmark_time_to_compute <-
  data.table::dcast(benchmark_time_to_compute, Algorithm + `Flag Method` ~ Tool, value.var = "value")
benchmark_time_to_compute[, `Flag Method` := factor(`Flag Method`, levels = c("current", "cumulative"))]
data.table::setcolorder(
  benchmark_time_to_compute,
  c("Algorithm", "Flag Method", "medicalcoder", "comorbidity", "mimiciv", "pccc")
)

data.table::setnames(
  benchmark_time_to_compute,
  old = c("medicalcoder", "comorbidity", "mimiciv", "pccc"),
  new = c("medicalcoder::comorbidities()", "comorbidity::comorbidity()", "MIMIC-IV Code", "pccc::ccc()")
  )

data.table::setkey(benchmark_time_to_compute, Algorithm, `Flag Method`)

## ---- fig-benchmark-summary ----
benchmark_summary_figure
ggplot2::ggsave(
  plot = benchmark_summary_figure,
  filename = "figure3.pdf"
)

## ---- for-figure3-alt-text ----
mdcr_f3_range <- benchmarksDT[Tool == "medicalcoder", round(range(time / 1e9), digits = 1)]
cmrb_f3_char_median <- benchmarksDT[Tool == "comorbidity" & Algorithm == "charlson", round(median(time / 1e9), digits = 1)]
cmrb_f3_elix_median <- benchmarksDT[Tool == "comorbidity" & Algorithm == "elixhauser", round(median(time / 1e9), digits = 1)]
mmic_f3_median <- benchmarksDT[Tool == "mimiciv", round(median(time / 1e9), digits = 1)]
pccc_f3_median <- benchmarksDT[Tool == "pccc", round(median(time / 1e9), digits = 1)]

## ---- tbl-time-to-compute ----
kableExtra::kbl(
  x = benchmark_time_to_compute[, 2:6],
  format = "latex",
  booktabs = TRUE,
  row.names = FALSE,
  col.names = c("", names(benchmark_time_to_compute)[3:6]),
  align = "lrrrr"
  ) |>
kableExtra::kable_styling(
  latex_options = c("striped", "scale_down", "HOLD_position")
) |>
kableExtra::pack_rows(
  index = table(benchmark_time_to_compute$Algorithm)
)

## ----  structure-of-summaries ----
str(summary(medicalcoder_charlson_current), max.level = 1)
str(summary(medicalcoder_elixhauser_current), max.level = 1)
str(summary(medicalcoder_pcccv2.0_current), max.level = 1)
str(summary(medicalcoder_pcccv3.1_current), max.level = 1)

str(summary(medicalcoder_charlson_cumulative), max.level = 0)

## ---- charlson-summary ----
cc <- summary(medicalcoder_charlson_current)$conditions
data.table::setDT(cc)
cc[, condition := NULL]
cc[, count := qwraps2::frmt(as.integer(count))]
cc[, percent := qwraps2::frmt(percent)]
cc[, rgp := data.table::fifelse(grepl(">=", condition_description), "Total Conditions", "Condition")]
cc[, condition_description := sub(">=", "≥", condition_description)]
data.table::setnames(cc, old = c("count", "percent"), new = c("Encounters", "Percent"))

gt::gt(
  data = cc,
  rowname_col = "condition_description",
  groupname_col = "rgp"
)

## ---- tbl-medicalcoder-pcccv2.0-summary ----
kableExtra::kbl(
  x = summary(medicalcoder_pcccv2.0_current)[, 2:4],
  format = "latex",
  booktabs = TRUE,
  col.names = c("", "Encounters", "Percent"),
  digits = 2
) |>
kableExtra::kable_styling(
  latex_options = c("striped", "scale_down", "HOLD_position")
) |>
kableExtra::pack_rows("Conditions", start_row =  1, end_row = 13) |>
kableExtra::pack_rows("Total Conditions", start_row = 14, end_row = 24)

## ---- mem-total-helper ----
mem_total_bytes <- function() {
  sys <- Sys.info()[["sysname"]]
  if (is.na(sys)) sys <- .Platform$OS.type

  if (identical(sys, "Windows")) {
    # memory.size() returns MB
    return(utils::memory.size(max = TRUE) * 1024^2)
  }

  if (identical(sys, "Darwin")) {
    out <- suppressWarnings(system("sysctl -n hw.memsize", intern = TRUE))
    if (length(out) == 1 && nchar(out)) return(as.numeric(out))
  }

  if (identical(sys, "Linux")) {
    if (file.exists("/proc/meminfo")) {
      lines <- readLines("/proc/meminfo", warn = FALSE)
      mem <- sub("^MemTotal:\\s+([0-9]+)\\s+kB.*$", "\\1",
                 lines[grepl("^MemTotal:", lines)])
      if (length(mem) == 1 && nchar(mem)) return(as.numeric(mem) * 1024)
    }
  }

  NA_real_
}

format_gb <- function(bytes) {
  if (is.na(bytes)) return(NA_character_)
  sprintf("%.1f GB", bytes / 1024^3)
}

mem_total_gb <- format_gb(mem_total_bytes())

## ---- get-cpu-brand ----
cpu_brand <- if (.Platform$OS.type == "unix") {
  if (Sys.info()["sysname"] == "Darwin") {
    # macOS
    system("sysctl -n machdep.cpu.brand_string", intern = TRUE)
  } else {
    # Linux
    system("grep -m 1 'model name' /proc/cpuinfo | awk -F: '{print $2}'", intern = TRUE)
  }
} else {
  # Windows
  system("wmic cpu get name", intern = TRUE)[2]
}

CPU <- trimws(cpu_brand)

## ---- sessioninfo-with-memory-cpu ----
si <- sessioninfo::session_info()
si$platform$RAM <- mem_total_gb
si$platform$CPU <- CPU

si$platformDT <-
  data.table::data.table(
    V1 = names(si$platform),
    V2 = do.call(c, si$platform)
  )

si$platformDT <-
  si$platformDT[
    V1 %in% c("version", "os", "system", "CPU", "RAM", "date", "pandoc", "quarto")
  ][c(1:3, 8, 7, 4:6)]

## ---- tbl-session-info-platform ----
kableExtra::kbl(
  x = si$platformDT,
  format = "latex",
  row.names = FALSE,
  col.names = c("Setting", "Value"),
  booktabs = TRUE
) |>
kableExtra::kable_styling(
  latex_options = c("striped", "HOLD_position")
)

## ---- tbl-session-info-packages ----
pkgs <- as.data.frame(si$packages)[c("package", "loadedversion")]
blks <- 4
n <- ceiling(nrow(pkgs) / blks)
mat <- matrix("", nrow = n, ncol = 2 * blks)
for (i in seq_len(nrow(pkgs))) {
  rw  <- (i - 1) %% n + 1
  blk <- (i - 1) %/% n
  cl  <-  blk * 2 + 1
  mat[rw, cl:(cl+1)] <- unlist(pkgs[i, ])
}

kableExtra::kbl(
  x = mat,
  col.names = NA, #rep("", 8),
  format = "latex",
  booktabs = TRUE
) |>
kableExtra::kable_styling(
  latex_options = c("striped", "HOLD_position")
) |>
kableExtra::add_header_above(rep(c("Package Version" = 2), 4))

## ---- end-of-file ----
