
prepare_main_data  <-
  function(main_data) {
    # Filter to opiate clients
    main_dt <- main_dt[drug_grp == "Opiate", ]

    # Convert date columns to compatible data class
    main_dt[, `:=`(
      triaged = lubridate::as_date(triaged),
      disd = lubridate::as_date(disd)
    )]

    # Recode disrsn to have fewer categories
    main_dt[, disrsn := fifelse(
      is.na(disd), "Retained",
      fifelse(
        disrsn %chin% c(
          "Exit reason inconsistent",
          "Incomplete/other",
          "Moved away, referred on or transferred not in custody"
        ),
        "Other", disrsn
      )
    )]
 }
