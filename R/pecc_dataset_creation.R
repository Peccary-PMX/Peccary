#' Dataset creation for Monolix and NONMEM
#' @export
pecc_dataset_creation <- function(df, admin = NULL, time = NULL, obs = "C_ng_mL", dose = NULL, dose_fct = 1000, evid2 = NULL, LOQ = NULL ){



  if(is.null(time)){

    grep_time <-  grep("TIME", toupper(names(df)))
    time <- names(df)[[grep_time[[1]]]]
    warning(paste0("Column \"", time, "\" used as time, please mention time = \"your_time_column\" as argument if needed"))
  }

  if(is.null(dose)){

    grep_dose <-  grep("DOSE", toupper(names(df)))
    dose <- names(df)[[grep_dose]]
    warning(paste0("Column \"", dose, "\" used as dose, please mention dose = \"your_dose_column\" as argument if needed"))
  }

  names(df)[names(df) == time] <- "time"
  names(df)[names(df) == obs] <- "obs"
  names(df)[names(df) == dose] <- "dose"



  df %>%
    group_by_("ID", if_else(is.null(admin), "ID", names(admin)[[1]])) %>%
    nest() -> tempp

  if(!is.null(admin)){
    tempp <- tempp %>%
      left_join(admin)
  }else{

    tempp <- tempp %>% mutate(admin = -5/pi)
  }

  tempp %>%
    # filter(ID == 61) %>%
    # {.[[1, "data"]]} -> data
    {.[[1, "admin"]]} -> admin
    mutate(data2 = map2(data, admin, function(data, admin){

      if(admin[[1]] == -5/pi){

        administrations <- NULL

      }else{


        administrations <-  data %>%
          slice(1) %>%
          select(- time) %>%
          crossing(time = admin) %>%
          mutate(MDV = 1) %>%
          mutate(EVID = 1) %>%
          mutate(AMT = dose * dose_fct) %>%
          mutate(ADM = 1) %>%
          mutate(obs = 0)


      }


      administrations %>%
        bind_rows(


          data %>%
            ungroup() %>%
            mutate(MDV = 0) %>%
            mutate(EVID = 0) %>%
            mutate(AMT = 0) %>%
            mutate(ADM = 0)

        )  -> ind_df


      if(!is.null(evid2)){


        ind_df <- ind_df %>%
          bind_rows(


            data %>%
              slice(1) %>%
              select(- time) %>%
              crossing(time = evid2[!(evid2 %in% unique(ind_df$time))]) %>%
              mutate(MDV = 1) %>%
              mutate(EVID = 2) %>%
              mutate(AMT = 0) %>%
              mutate(ADM = 0) %>%
              mutate(obs = 0)

          )

      }


      if(!is.null(LOQ)){

        ind_df <- ind_df %>%
          mutate(obs = if_else(MDV == 0 & obs < LOQ, LOQ, obs)) %>%
          mutate(CENS = if_else(MDV == 0 & obs == LOQ, 1, 0))

      }

  if(length(admin) >1){
      ind_df <- ind_df  %>%
        arrange(time, desc(MDV)) %>%
        left_join(
      tibble(time = admin) %>%
        rownames_to_column("OCC"), by = "time"
        ) %>%
        fill(OCC)
  }

      return(ind_df %>%
               arrange(time, desc(MDV)))

    })) %>%
    select(-data) %>%
    unnest(data2) %>%
    # select(-contains("admin")) %>%
    select(ID, time, obs, dose, everything())-> temp



  names(temp)[names(temp) == "time"] <- time
  names(temp)[names(temp) == "obs"] <- obs
  names(temp)[names(temp) == "dose"] <- dose

  return(temp)

}
