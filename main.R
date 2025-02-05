# unzip(zipfile = "data/RAND-LAB-dataset.zip", exdir = "data")

main_dt <- data.table::fread("data/K3anon_FullDataset_for_VfM.csv")

sir_dt <- data.table::fread("data/SIR_table_for_VfM_linked.csv")

# Inner-join on ID-JY
dt <-
  data.table::merge.data.table(main_dt, sir_dt, by = c("client_random_id", "n_jy"))

# Since the SIR table doesn't specify a drug group
# check that ID-JY pairs not in the opiate group have been included
assertthat::assert_that(unique(dt[["drug_grp"]]) == "Opiate")

# Date cols for conversoin
cols <- c("submoddt", "disd", "triaged")

# Update these columns in place using lapply over .SD
dt[, (cols) := lapply(.SD, lubridate::as_date), .SDcols = cols]

dt[, year := lubridate::year(submoddt)]

dt[, disrsn := data.table::fifelse(is.na(disd), "Retained", disrsn)]

dt[, disrsn := data.table::fifelse(disrsn %chin% c("Exit reason inconsistent",
                                                    "Incomplete/other",
                                                    "Moved away, referred on or transferred not in custody"),
                                    "Other", disrsn), ]

yrs <- unique(dt[["year"]])

yrs <- yrs[order(unique(dt[["year"]]))]

dates_vector <-
  vector(mode = "character", length = length(yrs))

dates_vector <- lubridate::as_date(dates_vector)


for (i in seq_along(yrs)) {
  dates_vector[i] <-
    max(dt[["submoddt"]]) - lubridate::years(i)
}

btwn_dates <-
  data.frame("index" = seq_along(yrs),
             "enddate" = dates_vector + lubridate::years(1),
             "startdate" = dates_vector)


# Index `1` means this is for the latest year (2024)
# In this context the 2024 figures are defined as between
# 2024-10-01 and 2023-10-01.

date_range <- c(btwn_dates[1, "startdate"], btwn_dates[1, "enddate"])

dt2 <-
  dt[, .(client_random_id, n_jy, submoddt, disd, disrsn, phbudi_any)]

dt3 <-
  dt2[submoddt %between% date_range]


dt3 <-
  dt3[, .(submoddt = max(submoddt), phbudi_any = sum(phbudi_any)), by = .(client_random_id, n_jy, disrsn)]


dt3[, phbudi_any := data.table::fifelse(phbudi_any >= 1,
                                        "depot_bupe",
                                        "no_depot_bupe")]


dt4 <-
  dt3[, .N, by = .(disrsn, phbudi_any)]



dt5 <-
    data.table::dcast.data.table(dt4,
                                 disrsn ~ phbudi_any,
                                 value.var = "N",
                                 fill = 0)



dt5[, depot_total := sum(dt5[["depot_bupe"]])]

dt5[, no_depot_total := sum(dt5[["no_depot_bupe"]])]

dt5[, depot_rate := depot_bupe / depot_total]

dt5[, no_depot_rate := no_depot_bupe / no_depot_total]

dt5 <-
  dt5[, .(disrsn, depot_bupe, no_depot_bupe, depot_rate, no_depot_rate)]



dt5 |>
  janitor::adorn_totals()
