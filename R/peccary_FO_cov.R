#' @export
setGeneric("peccary_FO_COV",
           function(object, cov = "" ){standardGeneric("peccary_FO_COV")}
)

#' Objective Function Comparison through covariate
#' @export
setMethod(f = "peccary_FO_COV",
          signature = "folder",
          definition = function(object, cov){



  n <- object@lastSelected[1:2]
  n1 <- select_run(object,n[1])@name
  n2 <- select_run(object,n[2])@name

  if(object@software == "NONMEM"){

    temp <- tibble(n_run = n) %>%
      mutate(run = map(n_run, function(x)readTablePerso(paste0(select_run(object,x)@path,".phi"), skip = 1, header = T) %>% select(ID, OBJ) %>%
                         mutate(ID = factor(ID)) %>%
                         mutate(run = select_run(object,x)@name) %>%
                         left_join(select_run(object,x)@cov))) %>%
      unnest

    label <- "Difference Individual Objective Function"

  }else{

    temp <- suppressMessages(tibble(n_run = n) %>%
                               mutate(run = map(n_run, function(x) readTablePerso(paste0(select_run(object,x)@path,"/individualContributionToLL.txt"),  header = T)  %>%
                                                  rename(ID = Subject) %>%
                                                  rename(OBJ = Linearization) %>%
                                                  mutate(ID = factor(ID)) %>%
                                                  left_join(select_run(object,x)@cov) %>%
                                                  mutate(run = select_run(object,x)@name))) %>%
                               unnest) %>%
      mutate(OBJ = -2 * OBJ)

    label <- "Difference Individual Objective Function"

  }

  if(cov == F){

   names_cov <-  tibble(cov = names(temp)[ !(names(temp) %in% c("n_run", "ID", "OBJ","run"))])

  }

  temp %>%
    select(- run) %>%
    spread(key = "n_run",  value = "OBJ") %>%
    # Compute the difference
    mutate_(diff = paste0("`",n[[1]],"` - `",n[[2]],"`")) %>%
    # summarise(sum(diff))
    group_by_(cov) %>%
    # 1 if second run is the best, 0 either
    mutate(testbest = if_else(diff > 0, 1, 0)) %>%
    #best = for how many patient the second run is the best
    summarise_(diff = "sum(diff)",
               on = "length(unique(ID))",
               best_second = "sum(testbest)") %>%
    mutate_(Best_OF = paste0("if_else(diff < 0, \"",n1 ,"\",\"", n2, "\")")) %>%
    arrange(Best_OF, desc(abs(diff))) %>%
    # mutate(if_else())
    # mutate(n1on = n1/on) %>%
    # mutate(n2on = n2/on) %>%
    # mutate_(Best_nInd = paste0("case_when(best < on/2 ~ \"",n2 ,"\", best == on/2 ~ \"-\", T ~\"", n1, "\")")) %>%
    mutate(best = if_else(diff <0, on- best_second , best_second))%>%
    map_df(function(x){

      if(is.numeric(x)){
        x <- c(x, sum(x))
      }else{

        x <- c(x,"")
      }
      x
    }) %>%
    mutate(nID = paste0(best,"/",on)) %>%
    mutate(pctID = paste0(round(best/on * 100), "%"))-> temp2

  temp2[[nrow(temp2),1]] <- "All"

  temp2[[nrow(temp2), "Best_OF"]] <- if_else(temp2[[nrow(temp2), "diff"]] > 0, n2, n1 )

  temp2 %>%
    select_(cov, "Best_OF", "diff","pctID", "nID")

})
