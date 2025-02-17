library(data.table)

main_dt <-
  data.table::fread("data/K3anon_FullDataset_for_VfM.csv")

main_dt[age == "", ]

sir_dt <-
  data.table::fread("data/SIR_table_for_VfM_linked.csv")

dt <-
  data.table::merge.data.table(main_dt, sir_dt, by = c("client_random_id", "n_jy"))

sum_characteristics <- function(dt, characteristic) {

  df <- unique(dt[, .(client_random_id,
                      characteristic = get(characteristic),
                      phbudi_any)])
  df <- df[characteristic != "", ]

  df <- data.table::dcast.data.table(
    df[, .N, by = .(phbudi_any, characteristic)],
    characteristic ~ phbudi_any,
    value.var = "N")

  data.table::setnames(df, c("characteristic", "t0", "t1"))

  df[, t0_p := t0 / sum(df[["t0"]], na.rm = TRUE)]
  df[, t1_p := t1 / sum(df[["t1"]], na.rm = TRUE)]
  df[, n := t0 + t1]

  df <- df[, .(characteristic, t0, t0_p, t1, t1_p, n)]

  return(df)
}

characteristics <- colnames(main_dt)[3:36]

characteristics  <- grep("triage", characteristics, value = TRUE, invert = TRUE)

mapply(FUN = sum_characteristics,
       characteristic = characteristics,
       MoreArgs = list(dt = dt),
       SIMPLIFY = FALSE)
