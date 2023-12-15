
#   ############################## Model building #######################
observeEvent(input$mb_delete_plot,{

  hot_to_r(isolate(input$mb_plot_stat)) %>%
    filter(delete == T) %>%
    pull(Plot) -> number_to_remove

  number_to_remove <- number_to_remove[number_to_remove != 1]

  if(length(number_to_remove) > 0){
    ## udpate display2
    output$mb_display2 <- renderRHandsontable({


      outputplot <- hot_to_r(isolate(input$mb_display2)) %>%
        filter(!(Plot %in% number_to_remove))

      rhandsontable(outputplot, width = 500, height = 200, rowHeaders = NULL)

    })

    ## udpate plot stat
    output$mb_plot_stat <- renderRHandsontable({


      outputplot2 <- hot_to_r(isolate(input$mb_plot_stat)) %>%
        filter(!(Plot %in% number_to_remove))

      rhandsontable(outputplot2, width = 400, height = 200, rowHeaders = NULL)

    })
  }
})


observeEvent(input$mb_add_plot,{

  ## udpate display2
  table_mb_prev <- hot_to_r(isolate(input$mb_display2)) %>%
    mutate(Plot = as.integer(Plot))


  outputplot <- table_mb_prev %>%
    bind_rows(table_mb_prev %>%
                filter(Plot == 1) %>%
                mutate(Plot = max(table_mb_prev$Plot)+1L) %>%
                mutate(Check = F, Point = F))

  # print(outputplot)
  output$mb_display2 <- renderRHandsontable({


    rhandsontable(outputplot, width = 500, height = 200, rowHeaders = NULL)

  })

  ## udpate mb_plot_stat

  table_mb_prev2 <- hot_to_r(isolate(input$mb_plot_stat)) %>%
    mutate(Plot = as.integer(Plot))


  outputplot2 <- table_mb_prev2 %>%
    bind_rows(table_mb_prev2 %>%
                filter(Plot == 1) %>%
                mutate(Plot = max(table_mb_prev2$Plot)+1L))

  # print(outputplot)
  output$mb_plot_stat <- renderRHandsontable({


    rhandsontable(outputplot2, width = 400, height = 200, rowHeaders = NULL)

  })





})


observeEvent(input$path_models,{
  # print("reading_model_files")
  models <-  try(read.table(isolate(input$path_models), stringsAsFactors = F) %>% pull(name))
  #
  # print("heererareraeor")
  # print(isolate(input$path_models))
  # print(models)
  # print(class(models))
  if(class(models) !="try-error"){
    print("there")
    updateSelectInput(session, inputId = "names_model", choices = c(models[order(models)]), selected =  c(models[order(models)])[[min(length(models),2)]])
  }else{

    # print("here")
    updateSelectInput(session, inputId = "names_model", choices = c("No model"))

  }
  # print("end_reading_model_files")
})



# helper_writing_model ----------------------------------------------------
observeEvent(input$modelLibInput,{

  inputs <<- isolate(input$modelLibInput)
  for_update <- inputs
  prev_model <<- isolate(input$mb_model)


# print(inputs)
  # Read the csv file containing all model
  models <- read.csv(file.path(find.package("peccary"), "Librairies_model", "library_peccary.csv" ),stringsAsFactors = F,  header = T, sep = ";") %>%
    as_tibble
  names(models) <- c("name","nPK", "equation", "type", "return", "param_values", 	"IC")


 # remove the useless number displayed on the Shiny App
 inputs <-  inputs %>%
    gsub(pattern = "Pk\\(.\\)",replacement = "Pk" )


 # Analyse the inputs and make a join with csv file to get required equations
 tibble(name = inputs) %>%
   mutate(drug = map_chr(name, function(x){

    if(!grepl("_drug", x)) return(NA)
     paste0("drug", str_split(x,"_drug")[[1]][-1]) %>% paste0(collapse = "_") #stupidly complex
   })) %>%
   mutate(name = gsub("_drug.+", "", name)) %>%
   mutate(nPK = map_dbl(drug, function(x){

    if(is.na(x)) return(NA)
    str_split(x, pattern = "_")[[1]] %>% length()
   })) %>%
   left_join(models) -> modelstemp

 # If several PK, add number after every parameter
 if(sum(grepl("^Pk", inputs)) > 1){

   # first need to put extra space to facilitate the process

   modelstemp <- modelstemp %>%
     mutate(equation = map_chr(equation, ~ .x %>%
                            gsub(pattern = "\\(",replacement =  " ( " ) %>%
              gsub(pattern = "\\)",replacement =  " ) " ) %>%
              gsub(pattern = "/",replacement =  " / " ) %>%
              gsub(pattern = "\\+",replacement =  " + " ) %>%
              gsub(pattern = "-",replacement =  " - " ) %>%
                gsub(pattern = "< -",replacement =  " <- " ) %>%
              gsub(pattern = "\\*",replacement =  " * " )%>%
                gsub(pattern = "\\* *\\*",replacement =  " ** " )  ))


   modelstemp <-  modelstemp %>%
     rowid_to_column() %>%
     mutate(equation = pmap_chr(list(equation, rowid, type), function(x, nn,type){

       if(type == "PD") return(x)

       temp <- str_split(x, pattern = "\n")[[1]]

       str_split(temp, pattern = " ") %>%
         map_chr(function(y){

            map_chr(y, function(z){

              if(!is.na(as.double(z))) return(z)

              return(paste0(z, "_", nn))

            }) %>%
              gsub(pattern = "<-_.",replacement = "<-" ) %>%
             gsub(pattern = "<_.",replacement = "<" ) %>%
              gsub(pattern = "\\*_.",replacement = "*" ) %>%
              gsub(pattern = "\\)_.",replacement = ")" ) %>%
              gsub(pattern = "\\(_.",replacement = "(" ) %>%
              gsub(pattern = " log_.",replacement = " log" ) %>%
             gsub(pattern = " exp_.",replacement = " exp" ) %>%
             gsub(pattern = " sqrt_.",replacement = " sqrt" ) %>%

             gsub(pattern = "^ *_.",replacement = "" ) %>%
             gsub(pattern = "/_.",replacement = "/" ) %>%
             gsub(pattern = "-_.",replacement = "-" ) %>%
             gsub(pattern = "\\+_.",replacement = "+" ) %>%
             paste0(collapse = " ")

         }) %>%  paste0(collapse = "\n")
     }))

 }


 # Pull the names PK from the table
 pk <- modelstemp %>%
   filter(type == "PK") %>%
   pull(name)
 ### update possibilites
 models %>%
   filter((nPK <= length(pk) & nPK > if_else(length(pk) ==0, -1,0)) | is.na(nPK)) %>%
   select(name, nPK) %>%
   mutate(name = map2(name, nPK, function(name, nPK){

     if(is.na(nPK) ){

       return(name)


     }else if(nPK == 1){

       return(c(paste0(name,"_", paste0("drug", 1:length(pk)))))

     }else if(nPK == 2){

        return(c(paste0(name,"_", c(paste0("drug", 1:length(pk)), paste0(paste0("drug", 1:length(pk)), collapse = "_")))))
     }

   })) %>%
   unnest() %>%
   pull(name) -> nameall

 nameall <- gsub("^Pk\\.",paste0("Pk(", length(pk) +1, ").") , nameall)
# inputs[length(inputs)] <- paste0(name_new_input, "(",inputs[length(inputs)],")")
 # verify not an issue with pk number (afeter erase)

 verif_pk_number <- which(grepl("^Pk",for_update))

 if(length(verif_pk_number) > 0){

for(a in verif_pk_number){

  for_update[a] <-  gsub("Pk\\([[:digit:]]\\)", paste0("Pk(",which(verif_pk_number == a),")"), for_update[a])
}

 }


 nameall <- unique(c(nameall, for_update))


 updateSelectInput(session, "modelLibInput", choices = nameall, selected = for_update)

# Now compute the new model !
# For each line
 for(a in 1:nrow(modelstemp)){

# Extract the line
r <- modelstemp %>% slice(a)

# If PD row and several PK: replace the [PK] blocs
if(r$type == "PD" & r$nPK > 0 ) {

    drug <- as.double(gsub("drug", "", str_split(r$drug,"_")[[1]]))

    for(b in 1:length(drug)){

      drugtemp <- drug[[b]]
      if(length(pk) > 1){
        #ex Conc_1
        newcible <- paste0(modelstemp$return[modelstemp$type == "PK"][[drugtemp]],"_",drugtemp)
      }else{
        #ex Conc
        newcible <- modelstemp$return[modelstemp$type == "PK"][[drugtemp]]
      }
      modelstemp$equation[a]   <- gsub(paste0("\\[PK", if_else(r$nPK == 1, "",as.character(b)),"\\]"), newcible, modelstemp$equation[a])
    }
  }

}


 modelstemp %>%
   # handle _plot addition
   mutate(equation = map2(equation, return, function(x,y){

     temp <-  str_split(gsub("\n", "\n ",x), pattern = " ")[[1]]

     temp[grepl(paste0("^", y, "_?.*"), temp)][[1]] <- paste0(temp[grepl(paste0("^", y, "_?.*"), temp)][[1]], "_plot")
     temp %>% paste0(collapse = " ") %>% gsub(pattern = "\n *", replacement = "\n")

   })) %>%
   mutate(equation = paste0("##", name, "\n", equation,"\n")) %>%
   pull(equation) %>%
   paste0(collapse = "\n") -> resu

 resu <- gsub("  *", " ", resu)
 if(nrow(modelstemp) == 1){

  if(is.na(modelstemp$nPK)) resu <- gsub("Conc *<-", "Conc_output <-", resu)
 }

 # resu

#
#

prev_model <- gsub("##### Peccary Auto-Creation.*", "", prev_model)
prev_model <- gsub("(( )|(\n))*$", "", prev_model)
 paste0(prev_model,
   "\n\n\n##### Peccary Auto-Creation ###\n\n", resu) -> resu

updateTextAreaInput(session, "mb_model", value = resu)

})


# load model --------------------------------------------------------------


observeEvent(input$load_model,{

  print("start load model")


  model <- project$models[[isolate(input$names_model)]]

  print("start load model")
  updateTextAreaInput(session, "mb_model", value = model[["model"]])
  updateTextInput(session, "name_model", value = isolate(input$names_model))

  print("start load model")
  updateTextAreaInput(session, "filterrExplo", value =   model$otherinput[["filterpoint"]])
  updateSelectInput(session, "exploX", selected =  model$otherinput[["xpoint"]])
  updateSelectInput(session, "exploY",  selected =   model$otherinput[["ypoint"]])
  updateSelectInput(session, "nastringExplo", selected =   model$otherinput[["nastringExplo"]])
  updateSelectInput(session, "sepExplo",  selected =   model$otherinput[["sepExplo"]])
  updateSelectInput(session, "decExplo", selected =   model$otherinput[["decExplo"]])
  updateNumericInput(session, "mb_time_from", value = model$otherinput[["from"]])
  updateNumericInput(session, "mb_time_to", value = model$otherinput[["to"]])
  updateNumericInput(session, "mb_time_by", value = model$otherinput[["by"]])
  updateSelectInput(session, "matrix_sd_var",  selected =   model$otherinput[["matrix_sd_var"]])




  output$mb_state <- renderRHandsontable({rhandsontable(model$compartmet, rowHeaders = NULL)})
  output$mb_paramater <- renderRHandsontable({rhandsontable( model$parameter, rowHeaders = NULL)})
  output$mb_event <- renderRHandsontable({ rhandsontable(model$event, rowHeaders = NULL)})
  output$mb_plot_stat <- renderRHandsontable({rhandsontable( model$plotstat, rowHeaders = NULL)})
  output$mb_matrix <- renderRHandsontable({rhandsontable( model$matrix_eta, rowHeaders = NULL)})
  output$mb_output <- renderRHandsontable({ rhandsontable(model$mb_output, rowHeaders = NULL)})
  output$mb_display2 <- renderRHandsontable({ rhandsontable(model$todisplay, rowHeaders = NULL)})

  output$OD_sampling <- renderRHandsontable({

    tibbleoutput <-
      tibble(Output = factor(NA, levels = unique(todisplay_loaded$Todisplay)),
             Group = "1", Proto = "1", TimeSample = "c(1,5,10)", add = "0F", prop = "0.2F", nidgroup = 30, delete = F, cov = "")
    rhandsontable(tibbleoutput, width = 1000, height = 200, rowHeaders = NULL)
  })


  # if(model[["preloaded"]] != isolate(input$preloadeddataset) & model[["preloaded"]] != F){
  #
  #
  #   updateSelectInput(session, "preloadeddataset", selected = model$otherinput[["preloaded"]])
  #   updateSelectInput(session, "exploX", selected  = model$otherinput[["xpoint"]])
  #   updateSelectInput(session, "exploY", selected  = model$otherinput[["ypoint"]])
  #
  #   showNotification(paste("Dataset changed to ", model[["preloaded"]], ", x = ",  model[["xpoint"]], ", y = ",  model[["ypoint"]]), type = "message", duration = 5)
  #   noneed <- T
  # }


#
#   updateTextInput(session, "pathExplo", value =  model[["pathpoint"]])
#
#   separateur <- model[["sepExplo"]]
#   if(is.na(separateur)) separateur <- ""



  # explo <<- try(read.table( model[["pathpoint"]], header = T, na.strings =  model[["nastringExplo"]], sep = separateur , dec =  model[["decExplo"]]))


  print("end load model")
})


# omega siwth matrix diag -------------------------------------------------

observeEvent(input$matrix_diag,{

  if(input$matrix_diag == F){

    try({
    cmatrix <- hot_to_r(isolate(input$mb_matrix))

    # cmatrix <- tibble(ka = c("0.1","0"), cl = c("", "0.2"))
     #
    diag <- diag(cmatrix %>%  as.matrix())

    cmatrix <- cmatrix %>% slice(1)

    cmatrix[1, ] <- diag

    rownames(cmatrix) <- "diag"
    output$mb_matrix <- renderRHandsontable({rhandsontable(cmatrix) })
    })
  }else if(input$matrix_diag == T){

    try({
    cmatrix <- hot_to_r(isolate(input$mb_matrix))
    tempmatrix <- diag_to_pecc(cmatrix)

     output$mb_matrix <- renderRHandsontable({rhandsontable(tempmatrix) })
    })
  }


})


# Function to be use in the two next section (big model analyses + update NoVar)

matrix_fill <- function(tibble_param){


  namesparamatrix <- tibble_param %>%
    filter(Distrib != "NoVar") %>%
    pull(Param)


  # previous_matrix <- tibble() # for test
  previous_matrix <- hot_to_r(isolate(input$mb_matrix))

  namesprev <- names(previous_matrix)

  ## add bioav and tlag names ! will not be in desolvepcc
  grepp <- namesprev[grep("(BioAv)|(tlag)", namesprev)]
  if(length(grepp) > 0){
    namesparamatrix <- unique(c(namesparamatrix, grepp))
  }


  namesparamatrix <- sort(namesparamatrix)
  # create new matrix wt
  temp <- matrix(nrow = length(namesparamatrix), ncol = length(namesparamatrix), data = "0")
  diag(temp) <- "0.3"

  # replace all value above diag by "
  if(ncol(temp) >= 2){
    for(a in 1:(ncol(temp)-1)){

      temp[ 1:a, a + 1] <- ""

    }
  }

  # transform into dataframe

  temp <- as.data.frame(temp, stringsAsFactors = FALSE)
  rownames(temp) <- namesparamatrix
  colnames(temp) <- namesparamatrix

  # for each value, see if a previous one existed, and if yes do the replacement
  # previous_matrix <- temp; previous_matrix[[1]][[1]] <- 2; A = "IVX"; B = "IVX" # for developing only, to comment

  crossing(A = namesparamatrix, B = namesparamatrix) %>%
    mutate(C = map2(A,B, function(A, B){ # we don't care about C, its just the transformation that matter

      if(A %in% namesprev & B %in% namesprev){

        value <- try(previous_matrix[[B]][[which(rownames(previous_matrix) == A)]], silent = T)

        if(class(value) != "try-error") temp[[B]][[which(rownames(temp) == A)]] <<- value
      }


    }))

  temp

}

# launch model --------------------------------------------------------------





# model <- "file:///D:/these/TMM_models.txt"
# model <- "file:///D:/these/Pecc_test/3_Models/1_Models/000_21_01_11_5ytype/cov_analsysis/Ref_without_cov_no_growth_estimElim_IL7onExpanHillIL7_10HigherEff4_IL750free2.mlxtran"
# model <- "file:///D:/Peccary/Exemple_demo/Simeoni/closeIV.mlxtran"
observeEvent(input$mb_load_model,{
  try({

    print("start launch model")

    if(isolate(input$mb_load_cov) == F){

      model <- "dX_0 <- IVX \n dX <- - ke * X" # to test
      model <- isolate(input$mb_model)

    }else{

      model <- paste(isolate(input$mb_model_cov), isolate(input$mb_model), sep = "\n")

    }

    if(is.double(as.double(model)) & !is.na(as.double(model))){


      model <- dossier(as.double(model))@path_source


    }



    ## if the model is NOT directly usable -> transformation into deSolve syntax
    needupdate <- F # to know if input$mb_model should be import


    model_import <- pecc_import_model(model)

    if(!is.null(model_import)){

      model <- model_import$model
      needupdate <- T
    }


    if(needupdate == T){

      updateTextAreaInput(session, inputId = "mb_model", value = model)

    }

    #### End tranformation into deSolve syntax



    desolvepcc <- try(deSolve_pecc(model))

    # desolvepcc <-  deSolve_pecc(model) # to test

    if(class(desolvepcc) != "try-error"){


      # updateTextAreaInput(session, inputId = "mb_state2", value = desolvepcc[["state"]]  %>% gsub(pattern = ", ?", replacement = "\n") )
      # updateTextAreaInput(session, inputId = "mb_paramater2", value = desolvepcc[["parameter"]] %>% gsub(pattern = ", ?", replacement = "\n") )


      ####### Initial Values
      print("start mb_state / initial value")
      output$mb_state <- renderRHandsontable({

        # table_mb_prev <- tibble(Cmt = character(), t0 = character()) # to test when on previous tab
        # table_mb_prev <- tibble(Cmt = "X", t0 = "3") # to test left_join
        table_mb_prev <- hot_to_r(isolate(input$mb_state))


        if(!is.null(model_import$initial_values)) table_mb_prev <- model_import$initial_values


        # Create a tibble with only cmt colunm (one row per compartment)
        tibble_state <-  tibble(Cmt =desolvepcc[["state"]]) %>%
          left_join(table_mb_prev) # and left_join with previous values

        # Add 0 as default value when value is missing
        tibble_state$t0[tibble_state$t0 == "" | is.na(tibble_state$t0)] <- 0

        # Add initial conditions !
        if(nrow(desolvepcc$initialCond)> 0){
          for(a in 1:nrow(desolvepcc$initialCond)){

            tibble_state$t0[tibble_state$Cmt == gsub("^d|(_0$)", "",desolvepcc$initialCond[[a, 1]] )] <- desolvepcc$initialCond[[a, 2]]
          }
        }

        rhandsontable(tibble_state, width = 200, height = 200, rowHeaders = NULL)

      })

      #### valeur des parametres
      print("start mb_parameter / initial parameter value")



      # table_mb_prev <- table_param() # for test
      table_mb_prev <- hot_to_r(isolate(input$mb_paramater))

      if(!is.null(model_import$values)) table_mb_prev <- model_import$values

      # create a tibble with only parameter column
      tibble_param <-  tibble(Param =desolvepcc[["parameter"]]) %>%
        left_join(table_mb_prev) # and join previous table

      # then use table_param to make sure everything is okay
      tibble_param <- table_param(tibble_param)%>% arrange(Param)

      output$mb_paramater <- renderRHandsontable({

        rhandsontable( tibble_param , width = 200, height = 200, rowHeaders = NULL)

      })

      #### evenement
      print("start event")
      output$mb_event <- renderRHandsontable({

        # table_event_prev <-  table_input(var = "aze") %>% slice(0) # for test

        table_event_prev <- table_input(hot_to_r(isolate(input$mb_event)))

        # if no row, add at least one (with NA as Var)
        if(nrow(table_event_prev) ==0) table_event_prev <- table_input(var = NA)

        if(!is.null(model_import$input)) table_event_prev <- model_import$input



        # Remove the previous cmt that no longer exist and update the possible factors
        table_event_prev$var <- if_else(! table_event_prev$var %in% desolvepcc[["state"]], NA_character_, as.character(table_event_prev$var))
        table_event_prev$var <- factor(table_event_prev$var, levels = desolvepcc[["state"]]) #

        # no need to have too many NA rows... keep only one if it would create empty df, otherwise remove them
        if(sum(is.na(table_event_prev$var)) == nrow(table_event_prev)){
          table_event_prev <- table_event_prev %>%
            slice(1)
        }else{
          table_event_prev <- table_event_prev %>%
            filter(!is.na(var))
        }


        try({ if(is.na(table_event_prev$var)[[1]]) table_event_prev$var[[1]] <-  desolvepcc[["state"]][[1]] })


        rhandsontable( table_event_prev  , width = 400, height = 200, rowHeaders = NULL)


      })

      ### mb_matrix


      print("start matrix")


      temp <- matrix_fill(tibble_param = tibble_param)

      output$mb_matrix <- renderRHandsontable({

        if(!is.null(model_import$matrix)) temp <- model_import$matrix

        rhandsontable(temp, width = 500, height = 200)

      })

      updateCheckboxInput(session, "matrix_diag", value = T)


      ### Ubdate des checkbox et autres

      # print("start checbock")
      c(desolvepcc[["state"]], desolvepcc[["output_manual"]], desolvepcc[["toplot"]]) -> choicescheck #%>%
      choicescheck <- unique(choicescheck[choicescheck != ""])

      if(desolvepcc[["output_manual"]] == ""){

        selectcheck <- desolvepcc[["state"]]

      }else{

        selectcheck <- desolvepcc[["output_manual"]]

      }

      selectcheck <- c(selectcheck, desolvepcc[["toplot"]] )

      if(needupdate == T)  selectcheck <- model_import$Todisplay


      #### output
      outputbase <- desolvepcc$output_manual
      if(outputbase[[1]] == "")outputbase <- NA
      output$mb_output <- renderRHandsontable({

        output_temp <-  tibble(output = factor(outputbase, levels = choicescheck),
                               YTYPE = NA_integer_, err_add = "0.1", err_prop = "0.3", export = T, rm = F)



        rhandsontable( output_temp  , width = 500, height = 200, rowHeaders = NULL)
      })

      ######## To display

      output$mb_display2 <- renderRHandsontable({

        # table_mb_prev <-  table_display(possiblevalues = NA_character_) %>% slice(0)


        table_mb_prev <- table_display(hot_to_r(isolate(input$mb_display2)))


        max <- max(table_mb_prev$Plot)

        if(max == -Inf) max <- 1
        # print("aeraze")
        tibble_display <-  crossing(Plot =  1L:max, tibble(Todisplay = choicescheck)) %>%
          left_join(table_mb_prev) %>%
          mutate(Check = case_when(is.na(Check) & Todisplay %in% selectcheck ~ T,
                                   is.na(Check) & !(Todisplay %in% selectcheck) ~ F,
                                   T ~ Check)) %>%
          mutate(Point = if_else(is.na(Point), F,Point))
        # print("aeraze")

        if(needupdate == T) tibble_display$Point[tibble_display$Todisplay %in% model_import$Todisplay$Todisplay]  <- TRUE

        rhandsontable( tibble_display  , width = 500, height = 200, rowHeaders = NULL)


      })


      updateCheckboxGroupInput(session, inputId = "mb_display", choices = choicescheck, selected = selectcheck )
      updateSelectInput(session, inputId = "ode_baselines", choices = c("None" = "",choicescheck) )

      ### Optimal design

      output$OD_sampling <- renderRHandsontable({

        table_mb_prev <- try(hot_to_r(isolate(input$OD_sampling)))

        if(class(table_mb_prev) %in% c("try-error", "NULL")){

          choices <- c(desolvepcc$state, desolvepcc$output_manual[ desolvepcc$output_manual != ""], desolvepcc$toplot[ desolvepcc$toplot != ""])
          prese <- if_else(length(choices) == 1, choices[[1]], NA_character_ )
          tibbleoutput <-
            tibble(Output = factor(prese,choices),
                   Group = "1", Proto = "1", TimeSample = "c(1,5,10,20,30)", add = "0F", prop = "0.2", nidgroup = 30, delete = F, cov = "")


        }else{

          tibbleoutput <- table_mb_prev %>%
            mutate(Output = factor(Output, levels = unique(choicescheck)))

        }

        return(rhandsontable(tibbleoutput, width = 1000, height = 200, rowHeaders = NULL))
      })


      # updateTextAreaInput(session, inputId = "mb_events", value = paste0("\"", desolvepcc[["state"]] %>% gsub(pattern = " *=.*", replacement =  ""), "\", 0, 0, \"add\"" ))
      #     ### udpate plot stat

      output$mb_plot_stat <- renderRHandsontable({

        table_mb_prev <- try(hot_to_r(isolate(input$mb_plot_stat))%>%
                               mutate(Plot = as.integer(Plot)))
        # table_mb_prev <-  tibble(Param =desolvepcc[["parameter"]] , Value = rep(3, length(desolvepcc[["parameter"]])))



        if(class(table_mb_prev) != "try-error"){


          max <- max(table_mb_prev$Plot)

          if(max == -Inf) max <- 1

          tibble_display <-  tibble(Plot =  1L:max) %>%
            left_join(table_mb_prev) %>%
            mutate(wrap = factor(wrap, levels = c("None", "Output", "Param","Event", "OP", "OE", "PE", "OPE", "O|P", "O|E", "P|E")))


        }else{


          tibble_display <- tibble(Plot = 1L, wrap = factor("None", levels = c("None", "Output", "Param","Event", "OP", "OE", "PE", "OPE", "O|P", "O|E", "P|E")),
                                   scalewrap = factor("free",levels= c("fixed", "free","free_x", "free_y")), ylog = F, xlog = F, delete = F)

        }


        rhandsontable( tibble_display  , width = 400, height = 200, rowHeaders = NULL)


      })



      # update list available values

      if(!is.null(model_import$res)){


        InfoModelPecc <<- model_import

        initial <- "Initial"
        print("quoi.")
        if("Pop" %in% names(model_import$res))  initial <- "Pop"

        updateSelectInput(session, inputId = "IDImpMod", choices = c(initial,   names(model_import$res)[-1]))

        updateCheckboxInput(session, inputId = "useImpData", value = T)

      }
      # output$mb_plot_stat <- renderRHandsontable({
      #
      #   temp_plot_stat <-
      #
      #   rhandsontable(temp_plot_stat, width = 200, height = 200, rowHeaders = NULL)
      #
      # })


    }
    print("end launch model")

  })
})



# When param set to NoVar -> update Matrix -----------------------------------


observeEvent(input$mb_paramater,{

# print("quoi")
parameters <<-  hot_to_r(input$mb_paramater)

matrix_temp <<-  hot_to_r(isolate(input$mb_matrix))



# Is there some matrix col to remove

names_parameters_No_Var <- parameters %>%
  filter(Distrib  == "NoVar") %>%
  pull(Param)

torem <- names_parameters_No_Var[names_parameters_No_Var %in% names(matrix_temp)]


# Is there a column to add

names_parameters_Var <- parameters %>%
  filter(Distrib  != "NoVar") %>%
  pull(Param)

toadd <- names_parameters_Var[! names_parameters_Var %in% names(matrix_temp)]


if(length(toadd) > 0 | length(torem) > 0){




  matrix_res <-   matrix_fill(parameters)

  output$mb_matrix <- renderRHandsontable(rhandsontable(matrix_res, width = 500, height = 200))


}

})


# add output --------------------------------------------------------------


observeEvent(input$mb_output_add,{


  output$mb_output <- renderRHandsontable({

    mb_output <-   hot_to_r(isolate(input$mb_output)) %>%
      bind_rows(tibble(output = NA, YTYPE = NA, err_add = "0.1", err_prop = "0.3", export = T,  rm = F))

    rhandsontable( mb_output  , width = 400, height = 200, rowHeaders = NULL)

  })


})

#
observeEvent(input$mb_output_remv,{


  output$mb_output <- renderRHandsontable({

    ## carefull a column as a name with F, so you can use  == F
    ## instead FALSE or 0
    if(   nrow(hot_to_r(isolate(input$mb_output)) %>%
               filter(rm == FALSE))>0  ){

      mb_output <-  hot_to_r(isolate(input$mb_output)) %>%
        filter(rm == FALSE)

    }else{

      tibble_event <- hot_to_r(isolate(input$mb_output)) %>%
        slice(1)

      showNotification("First line conserved", type = "warning", duration = 3)
    }



    rhandsontable( mb_output  , width = 400, height = 200, rowHeaders = NULL)

  })



})

# add evend ---------------------------------------------------------------



observeEvent(input$mb_add_event,{

  prev <- hot_to_r(isolate(input$mb_event))




  if(typeof(prev) == "try-error"){

    model <- isolate(input$mb_model)
    model <- deSolve_pecc(model)
    states <- model$state
    newtable <- table_input(var = factor(NA, states))
  }else{

    newtable <- prev %>%
      bind_rows( prev %>% slice(nrow(prev)) %>%
                   mutate(Proto = NA, var= NA, time = NA))

  }


  output$mb_event <- renderRHandsontable({



    rhandsontable( newtable  , width = 400, height = 200, rowHeaders = NULL)

  })


})


observeEvent(input$mb_delete_event,{


  output$mb_event <- renderRHandsontable({

    ## carefull a column as a name with F, so you can use  == F
    ## instead FALSE or 0
    if(   nrow(hot_to_r(isolate(input$mb_event)) %>%
          filter(delete == FALSE))>0  ){

      tibble_event <-  hot_to_r(isolate(input$mb_event)) %>%
        filter(delete == FALSE)

    }else{

      tibble_event <- hot_to_r(isolate(input$mb_event)) %>%
        slice(1)

      showNotification("First line conserved", type = "warning", duration = 3)
    }



    rhandsontable( tibble_event  , width = 400, height = 200, rowHeaders = NULL)

  })



})


# add tlag, biodispo and perfusion ----------------------------------------

# Goal: when the user click on the event table for adding a
# bioavailability, a tlag or a perfusion, we automatically
# add the parameters.

observeEvent(input$mb_event,{

try({
  # Get event and parameters tables
  events <- hot_to_r(input$mb_event)
  parameters <- hot_to_r(isolate(input$mb_paramater))
  matrixx <- hot_to_r(isolate(input$mb_matrix))
  ### Adding fake ADM if needed
if(nrow(events)>0){
  if(length(unique(events$ADM)) == 1 & events$ADM[[1]] == "" & length(unique(events$var[events$var != ""])) > 1){

    for(a in 1:length(unique(events$var[events$var != ""]))){

      vartemp <- unique(events$var)[[a]]
      events$ADM[events$var == vartemp] <- a
    }
  }

  ### Adding biodisponibility
  biodisp <-
    events %>%
    group_by(ADM) %>% # only first row for each ADM counts
    slice(1) %>%
    filter(F == TRUE, use == TRUE) %>% # geting rows with F == T
    pull(ADM) #getting ADM requiring bioavailability

  for(a in biodisp){

  namepara <-  paste0("BioAv_", a) %>% # getting name parameters BioAv
      gsub(pattern = "_$", replacement = "")  # removing "_" if no ADM

  # if not included into parameters df, add it
  if( !namepara %in% parameters$Param ){

  parameters <-   parameters %>%
    add_row(Param = namepara, Distrib = "logN", E = "Esti")
  }

  # if not included into matrix, add it
  if( !namepara %in% names(matrixx) ){

    matrixx[[namepara]] <- NA_character_
     new <- matrixx %>%
      slice(1) %>%
      map_df(function(x){
        x <- "0"
        x
      })
      new[[namepara]] <- "0.3"

    matrixx  <- rbind(matrixx, new)
    rownames(matrixx) <- names(matrixx)
     }

  }




  ### Removing bioavailability
  previoAv <- parameters$Param[grep("^BioAv", parameters$Param)] %>%
      gsub(pattern = "BioAv_?", replacement = "")

  # for every previous availability not required anymore
  for(a in previoAv[! previoAv %in% biodisp]){

    namepara <- paste0("BioAv", ifelse(a == "", a,  paste0("_", a)))
    parameters <- parameters %>%
      filter(Param != namepara)

   toremove <-  which(names(matrixx) == namepara)
   matrixx <-  matrixx[- toremove, -toremove]

  }

  ### Adding tlag

  tlag <-
    events %>%
    group_by(ADM) %>% # only first row for each ADM counts
    slice(1) %>%
    filter(tlag == TRUE, use == TRUE) %>% # geting rows with F == T
    pull(ADM) #getting ADM requiring bioavailability


  for(a in tlag){

    namepara <-  paste0("tlag_", a) %>% # getting name parameters BioAv
      gsub(pattern = "_$", replacement = "")  # removing "_" if no ADM

    # if not included into parameters df, add it
    if( !namepara %in% parameters$Param ){

      parameters <-   parameters %>%
        add_row(Param = namepara, Distrib = "logN", E = "Esti")
    }

    # if not included into matrix, add it
    if( !namepara %in% names(matrixx) ){

      matrixx[[namepara]] <- NA_character_
      new <- matrixx %>%
        slice(1) %>%
        map_df(function(x){
          x <- "0"
          x
        })
      new[[namepara]] <- "0.3"

      matrixx  <- rbind(matrixx, new)
      rownames(matrixx) <- names(matrixx)
    }
  }

  ### Removing tlag
  previoAv <- parameters$Param[grep("^tlag", parameters$Param)] %>%
    gsub(pattern = "tlag_?", replacement = "")

  # for every previous availability not required anymore
  for(a in previoAv[! previoAv %in% tlag]){

    namepara <- paste0("tlag", ifelse(a == "", a,  paste0("_", a)))
    parameters <- parameters %>%
      filter(Param != namepara)


    toremove <-  which(names(matrixx) == namepara)
    matrixx <-  matrixx[- toremove, -toremove]
  }


  ### Perf
  if(sum(events$Perf != "None") >0){

    if(!"Perf_num" %in% names(events)) events <- events %>%
        mutate(Perf_num = 0.0)
    output$mb_event <- renderRHandsontable(rhandsontable(events, width = 400, height = 200, rowHeaders = NULL))

  }else{

    if("Perf_num" %in% names(events)) events <- events %>%
        select(- Perf_num)


    output$mb_event <- renderRHandsontable(rhandsontable(events, width = 400, height = 200, rowHeaders = NULL))

  }


  output$mb_paramater <- renderRHandsontable(rhandsontable(parameters, width = 200, height = 200, rowHeaders = NULL))
  output$mb_matrix <- renderRHandsontable(rhandsontable(matrixx, width = 500, height = 200))
  # print("here? ?? end")
}


})

})

# observeEvent(input$mb_event,{
#
#   print("yes!")
#
# temp <-   hot_to_r(input$mb_event) %>%
#     filter( !((is.na(var) | var = "") & (is.na(time) | time = "") & (is.na(value) | value = "") & (is.na(method) | method = "")))
#
# temp %>%
#   filter()
#   tibble(var = factor(x = "a"), time = "0", value = 0, method = factor("add", levels = c("add", "mult", "rep")))
#
#   tibble(var = factor(x = desolvepcc[["state"]][[1]], levels = desolvepcc[["state"]]), time = "0", value = 0, method = factor("add", levels = c("add", "mult", "rep"))) %>%
#     add_row(var = NA, time = NA, value = NA, method = NA)
# })
#


# # inmport model -----------------------------------------------------------
#
# observeEvent(input$launchImpMod,{
#
#   # print(as.double(isolate(input$pathImpMod)))
#
#   path <- isolate(input$pathImpMod)
#
#   # to give the number of a model and get the model
#   if(!is.na(as.double(path) )){
#
#     string <- try(dossier(as.double(isolate(input$pathImpMod)))@path_source)
#     # print("string")
#     # print(string)
#     if(!is.na(string) & string != "") {
#       updateTextInput(session, inputId = "pathImpMod", value = string)
#       path <- string
#     }
#
#   }
#
#
#
#   if(length(grep("(\\.cfl)|(\\.res)",path)) > 0 ){
#     InfoModelPecc <<- try(nonmem_to_desolve(path))
#
#   }else if(length(grep("(\\.nlmixr)",path)) > 0){
#
#     InfoModelPecc <<- try(nlmixr_to_desolve(path))
#
#   }else{
#     # print(path)
#     InfoModelPecc <<- try(monolix_to_desolve(path))
#     # print("here")
#   }
#
# # print("class(InfoModelPecc)")
# # print(class(InfoModelPecc))
#   if(class(InfoModelPecc) == "try-error"){
#
#     showNotification("Unable to translat the model into deSolve", type = "warning", duration = 10)
#
#   }else{
#
#
#     # load text model
#     updateTextAreaInput(session, "mb_model", value = InfoModelPecc$model)
#
#     # load initial condition
#     output$mb_state <- renderRHandsontable(rhandsontable(InfoModelPecc$initial_values, width = 200, height = 200, rowHeaders = NULL))
#
#     if(is.logical(InfoModelPecc$values$E)){
#
#       InfoModelPecc$values$E <- case_when(InfoModelPecc$values$E == T ~ "Esti",
#                                           is.na(InfoModelPecc$values$Distrib) ~ "Input",
#                                           T ~ "Fix")
#       InfoModelPecc$values$E <- factor(InfoModelPecc$values$E, levels = c("Esti","Fix", "Input"))
#     }
#
#     InfoModelPecc$values$Distrib <- factor(if_else(is.na(InfoModelPecc$values$Distrib), "NoVar", as.character(InfoModelPecc$values$Distrib)), levels = c("logN", "Norm", "NoVar"))
#
#
#     # load values
#     output$mb_paramater <- renderRHandsontable(rhandsontable(InfoModelPecc$values, width = 200, height = 200, rowHeaders = NULL))
#
#     #  load matrix
#     output$mb_matrix <- renderRHandsontable(rhandsontable(InfoModelPecc$matrix, width = 500, height = 200))
#
#
#
#
#
#     # update input after addition of tlag etc
#     if(! "ADM" %in% names(InfoModelPecc$input)) InfoModelPecc$input$ADM <- ""
#     if( "Fpar" %in% names(InfoModelPecc$input)) InfoModelPecc$input <-  InfoModelPecc$input %>% select(-Fpar)
#     if(! "tlag" %in% names(InfoModelPecc$input)) InfoModelPecc$input$tlag <- FALSE
#     if(! "Perf" %in% names(InfoModelPecc$input)) InfoModelPecc$input$Perf <- factor("None", levels = c("None", "rate", "time"))
#     if("F"  %in% names(InfoModelPecc$input) ){
#
#       if(sum(InfoModelPecc$input$F %in% c("TRUE", "FALSE")) == 0 ) InfoModelPecc$input$F <- F
#     }else{
#
#       InfoModelPecc$input$F <- F
#     }
#
#     InfoModelPecc$input <<- InfoModelPecc$input
#
#     output$mb_event <- renderRHandsontable(rhandsontable(InfoModelPecc$input, width = 400, height = 200, rowHeaders = NULL))
#
#     # update list available values
#     initial <- "Initial"
#     if("Pop" %in% names(InfoModelPecc$res))  initial <- "Pop"
#     updateSelectInput(session, inputId = "IDImpMod", choices = c(initial,   names(InfoModelPecc$res)[-1]))
#
#     # Load output selection
#     output$mb_display2 <- renderRHandsontable(rhandsontable(InfoModelPecc$Todisplay, width = 500, height = 200, rowHeaders = NULL))
#
#     # Plot stat
#     output$mb_plot_stat <- renderRHandsontable(rhandsontable(
#       tibble(Plot = 1, wrap =factor("Output", levels = c("None", "Output", "Param","Event", "OP", "OE", "PE", "OPE", "O|P", "O|E", "P|E")),
#              ylog = T, xlog = F, delete = F), width = 400, height = 200, rowHeaders = NULL))
#
#     # output
#     output$mb_output <- renderRHandsontable({
#
#
#       output_temp <-  tibble(output = factor(NA, levels = unique(  InfoModelPecc$Todisplay$Todisplay)),
#                              YTYPE = NA_integer_, err_add = "0.1", err_prop = "0.3", export = T, rm = F)
#
#       rhandsontable( output_temp  , width = 500, height = 200, rowHeaders = NULL)
#     })
#     # print(input$mb_model)
#
#   }
#
# })
#

# change Id ---------------------------------------------------------------

observeEvent(input$IDImpMod,{

  cat("Change ID modeling\n")
 idImpMod <- input$IDImpMod
 # idImpMod <- "id204_mode"
  # load values
 if(exists("InfoModelPecc")){
  try({

    InfoModelPecc$res[c("Param", idImpMod)] -> tempres
    names(tempres)[2] <- "Value"

    InfoModelPecc$values %>%
      # hot_to_r(isolate(input$mb_paramater)) %>%
      select(-Value) %>%
      left_join( tempres ) %>%
      mutate(Value = as.character(Value)) %>%
      select(Param, Value, Distrib, E)  -> tempres

    output$mb_paramater <- renderRHandsontable(rhandsontable(tempres, width = 200, height = 200, rowHeaders = NULL))

  })

  # load input
  if( ! idImpMod %in% c("Initial", "Pop")){

    reference <- gsub( "(id)|_|mode|mean", "", idImpMod)

    InfoModelPecc$input %>%
      group_by(var) %>%
      slice(1) %>%
      select(-time, -value) %>%
      left_join(

        InfoModelPecc$df %>%
          filter(eventidentifier == 1, identifier == reference) %>%
          select(time, administration, amount) %>%
          rename(ADM = administration) %>%
          mutate(ADM  = as.character(ADM))
      ) %>%
      select(Proto, var, time, amount, everything()) %>%
      rename(value= amount) %>%
      mutate(use= if_else(is.na(time)|is.na(value), FALSE, use)) %>%
      mutate(time = as.character(time)) %>%
      mutate(value = as.character(value))-> temp

    # print(temp)
    # load event
    output$mb_event <- renderRHandsontable(rhandsontable(temp, width = 400, height = 200, rowHeaders = NULL))

  }else{

    output$mb_event <- renderRHandsontable(rhandsontable(InfoModelPecc$input, width = 400, height = 200, rowHeaders = NULL))

  }


 }
 cat("Change ID end")
})


# # import model pdf --------------------------------------------------------
#
# observeEvent(input$pdfImpData,{
#
#   times      <- seq(isolate(input$mb_time_from), isolate(input$mb_time_to), by = isolate(input$mb_time_by))
#   print("debut pdf")
#
#
#   listPlot <-
#     tibble( ID = names(InfoModelPecc$res)[-(1:2)]) %>%
#     mutate(test = map_dbl(ID, ~ length(grep("_mean", .x)))) %>%
#     filter(test != 1) %>%
#     select(-test) %>%
#     mutate(Parameter = map(ID, function(a){
#
#       InfoModelPecc$res[c("Param", a)] -> temp
#
#       if(a == "Pop"){
#
#         return(temp)
#
#       }else{
#
#         temp %>%
#           left_join(InfoModelPecc$res[c("Param", "Pop")] %>%
#                       mutate(Pop = as.double(Pop))) -> temp
#
#         temp$fold <- as.double(temp[[2]])/temp[[3]]
#
#         temp %>%
#           filter(fold != 1) %>%
#           mutate(fold = if_else(fold > 1, paste0("x ", signif(fold, 2)), paste0("/ ", signif(1/fold, 2)))) %>%
#           select(-Pop) -> temp
#
#         temp[[2]] <- as.character(   signif(as.double(temp[[2]]),3))
#
#         return(temp)
#
#       }
#
#
#     })) %>%
#     mutate(event = map(ID, function(a){
#
#
#       ## events
#       if( ! a %in% c("Initial", "Pop")){
#
#         reference <- gsub( "(id)|_|mode|mean", "", a)
#
#         InfoModelPecc$input %>%
#           group_by(var) %>%
#           slice(1) %>%
#           select(-time, -value) %>%
#           left_join(
#
#             InfoModelPecc$df %>%
#               filter(eventidentifier == 1, identifier == reference) %>%
#               select(time, administration, amount) %>%
#               rename(ADM = administration) %>%
#               mutate(ADM  = as.character(ADM))
#           ) %>%
#           select(Proto, var, time, amount, everything()) %>%
#           rename(value= amount) %>%
#           mutate(use= if_else(is.na(time)|is.na(value), FALSE, use)) %>%
#           mutate(time = as.character(time)) %>%
#           mutate(value = as.character(value)) %>%
#           filter(use ==  T)-> event
#       }else{
#
#         InfoModelPecc$input -> event
#
#       }
#
#       return(event)
#
#     })) %>%
#     # {.[[1, 3]]}
#     mutate(lines = map2(ID,event, function(a, event) {
#
#       # print(paste0("a = ", a))
#       ## load parameter
#       InfoModelPecc$res[c("Param", a)] -> tempres
#       names(tempres)[2] <- "Value"
#       #
#       #       InfoModelPecc$values %>%
#       #         # hot_to_r(isolate(input$mb_paramater)) %>%
#       #         select(-Value) %>%
#       #         left_join( tempres ) %>%
#       #         mutate(Value = as.character(Value)) %>%
#       #         select(Param, Value, Distrib, E)
#       #
#
#
#       parameters <-  random_etas(n = 0,
#                                  parameter_df = tempres %>% mutate(Value = as.character(Value)),
#                                  matrix_eta = matrix(0),sd = T, returnExpr = T, matrixShiny = F)
#
#
#
#       # ## events
#       # if( ! a %in% c("Initial", "Pop")){
#       #
#       #   reference <- gsub( "(id)|_|mode|mean", "", a)
#       #
#       #   InfoModelPecc$input %>%
#       #     group_by(var) %>%
#       #     slice(1) %>%
#       #     select(-time, -value) %>%
#       #     left_join(
#       #
#       #       InfoModelPecc$df %>%
#       #         filter(eventidentifier == 1, identifier == reference) %>%
#       #         select(time, administration, amount) %>%
#       #         rename(ADM = administration) %>%
#       #         mutate(ADM  = as.character(ADM))
#       #     ) %>%
#       #     select(Proto, var, time, amount, everything()) %>%
#       #     rename(value= amount) %>%
#       #     mutate(use= if_else(is.na(time)|is.na(value), FALSE, use)) %>%
#       #     mutate(time = as.character(time)) %>%
#       #     mutate(value = as.character(value)) %>%
#       #     filter(use ==  T)-> event
#       # }else{
#       #
#       #   InfoModelPecc$input -> event
#       #
#       # }
#
#       simulations <-   make_simulations(parameter = parameters,
#                                         model = isolate(input$mb_model),
#                                         states =  hot_to_r(isolate(input$mb_state)),
#                                         events =  event,times = times, timesF = T,Progress = F)
#       plotpoint <- mtcars # crash without that and no point, don't understand why.
#       # print("Simulation ok")
#
#
#
#       explo <<-  InfoModelPecc$df
#       explotemp <<-  InfoModelPecc$df
#
#       if(! isolate(input$IDImpMod) %in% c("Initial", "Pop") ){
#
#         explotemp <- explotemp %>%
#           filter(identifier == gsub("(id)|(_mean)|(_mode)", "", isolate(input$IDImpMod)))
#
#         explo <-  explotemp
#       }
#
#       xpointt <- "time"
#       ypointt  <- "observation"
#       ytype_headerr <- "observationtype"
#
#
#       plot_simulations <-  plot_simulations(simulations = simulations,
#                                             plot_table = hot_to_r(isolate(input$mb_display2)),
#                                             plot_table_cov = hot_to_r(isolate(input$mb_plot_stat)),
#                                             xpoint = xpointt,ypoint = ypointt,
#                                             n = 0,
#                                             ytype_header = ytype_headerr)
#
#       # aaaag <<- list()
#
#
#
#       output_plot <-   plot_simulations %>%
#         map(function(x){
#
#           deparse(x, width.cutoff = 500) %>%
#             paste(collapse = "\n") %>%
#             gsub(pattern = "%>%", replacement = "%>%\n" ) %>%
#             # gsub(pattern = "\\+", replacement = "+\n" ) %>%
#             # gsub(pattern = "ysimysim + 1", replacement = "") %>%
#             gsub(pattern = "  *", replacement = " " ) %>%
#             gsub(pattern = "withProgress\\(message.+simmax <- nrow\\(crossing\\(parameters, events\\)\\)", replacement = "") %>%
#             gsub(pattern = "incProgress\\(1/simmax.+simmax\\)\\)", replacement = "") %>%
#             gsub(pattern = "ysim <<- ysim \\+\n? 1",replacement = "") %>%
#             gsub(pattern = "as.data.frame %>%\n as.tibble\\(\\)\n }\\)\\) %>%\n select\\(-parameter, -events\\) %>%\n unnest\n\\}\\)",
#                  replacement = "as.data.frame %>%\n as.tibble()\n })) %>%\n select(-parameter, -events) %>%\n unnest\n")
#
#         }) %>%
#         paste(collapse = "\n\n")%>%
#         gsub(pattern = "desolve %>%", replacement = "desolve <- desolve %>% map_dfr(function(x) {
#              if (length(unique(x[!is.na(x)])) == 1 & unique(x[!is.na(x)]) == 0)
#              x[x == 0] <- 0.001
#              x
#     })
#
#              \ndesolve %>%")
#
#
#       if(! a %in% c("Pop", "Initial")){
#
#         a <- gsub("(id)|(_mean)|(_mode)", "", a)
#         output_plot <- gsub("crossing\\(explo", paste0(" crossing(explo %>%
#                                                        filter(identifier == ", a, ")"), output_plot )
#       }
#
#       output_plot <- paste0("times <- seq(", isolate(input$mb_time_from), ",", isolate(input$mb_time_to), ",", isolate(input$mb_time_by), ")\n",
#                             output_plot)
#
#       output_plot <- output_plot %>%
#         gsub(pattern = "desolve %>%", replacement = "desolve <- desolve %>%
#              map_dfr(function(x){
#              if(length(unique(x)) ==  1 & unique(x) == 0 ) x <- rep(0.001, length(x))
#              x
#              })
#
#              \ndesolve %>%")
#
#       # print(a)
#       return(output_plot)
#
#
#       }))
#   #
#
#   # #
#   # listPlot %>%
#   #   slice(2) %>%
#   #
#   #   pull(lines) -> temp
#   # temp[[1]] %>% cat
#   # # #
#
#   # #
#   # eval(parse_expr(paste0("{",temp, "}")))
#   # # # # eval(parse_expr(paste0("{",temp, "}")))
#
#
#   #
#
#   })
#
# observeEvent(input$pdfImpData2,{
#
#
#
#   pdf("test2.pdf", width = 20,height = 15)
#   # listPlot[[which(listPlot$ID == "Pop" ),3]]%>% cat
#   # # # temp[[1]]
#   listPlot %>%
#     # mutate()
#     slice(c(1,3)) %>%
#     # {.[[1,3]]} -> event
#     mutate(test = pmap(list(Parameter, lines,event), function(x,y, event){
#
#       plot_grid(plot_grid(tableGrob(x), tableGrob(event %>% select(var, time, value)),ncol = 1, rel_heights =  c(5,1)),
#                 eval(parse_expr(paste0("{",y, "}"))), rel_widths = c(1,3))
#
#
#
#     })) %>% pull(test)
#   dev.off()
#   shell.exec("test2.pdf")
#
# })
#

# Optimal Design ----------------------------------------------------------

observeEvent(input$OptimDesign_add,{

  cat('Add optimal design')

  temp <- hot_to_r(isolate(input$OD_sampling))



  output$OD_sampling <- renderRHandsontable({


    rhandsontable(bind_rows(temp, temp %>% slice(1)), rowHeaders = NULL)

  })
})

observeEvent(input$OptimDesign_delete,{


  temp <- hot_to_r(isolate(input$OD_sampling))

  if(sum(temp$delete) == nrow(temp)) temp$delete[[1]] <- F

  temp <- temp %>%
    filter(delete == F)



  output$OD_sampling <- renderRHandsontable({


    rhandsontable(temp, rowHeaders = NULL)

  })
})




observeEvent(input$OptimDesign,{


  cat("Beggining optim design")
  #

  # poped.db <<- character()
  ### verify there is no too many parameter values

  ### Update Sampling by removing redondant information
# print("hereaaaaa")
  OD_input <- hot_to_r(input$OD_sampling)
  levelstemp <- levels(OD_input$Output)

  print("start modif OD_input")
  # print(OD_input)
  for(a in unique(OD_input$Output)){

    if(nrow(OD_input %>% filter(Output == a)) > 1){

      OD_input$add[OD_input$Output == a][-1] <- NA
      OD_input$prop[OD_input$Output == a][-1] <- NA
    }
  }



  print("first step")
  # print(OD_input)

  for(a in unique(OD_input$Group)){

    if(nrow(OD_input %>% filter(Group == a)) > 1){

      OD_input$nidgroup[OD_input$Group == a][-1] <- NA
      OD_input$Proto[OD_input$Group == a][-1] <- NA
    }
  }

  output$OD_sampling <- renderRHandsontable(rhandsontable(OD_input %>%
                                                            mutate(Output = factor(Output, levels = levelstemp)), rowHeaders = NULL))
  # print("hereaaaaa")
  ### okay let's go
  if(isolate(input$mb_load_cov) == F){

    model <- isolate(input$mb_model)

  }else{

    model <- paste(isolate(input$mb_model_cov), isolate(input$mb_model), sep = "\n")

  }

  states <- isolate(hot_to_r(input$mb_state))
  diagOmega <- isolate(hot_to_r(input$mb_matrix))
  events <- isolate(hot_to_r(input$mb_event))
  parameters <<- isolate(hot_to_r(input$mb_paramater))
  # handle when there are several values as input -> keep only the first value and let a message
  # first test if several values
  parameters %>%
    as.tibble() %>%
    mutate(test = map(Value, ~ eval(parse_expr(.x)))) %>%
    mutate(ntest = map_dbl(test, ~ length(.x)))-> testparam

  if(max(testparam$ntest) > 1){

    parameters <- testparam %>%
      mutate(Value = map_dbl(test, ~ .x[[1]])) %>%
      select(-test, -ntest)

    parameters %>%
      left_join(testparam, by = "Param") %>%
      filter(ntest > 1) %>%
      mutate(warning = paste0(Param, " = ", Value.x)) %>%
      pull(warning) %>%
      paste0(collapse = ", ")-> warnin

    warnin <- paste0("Attention, only following values were used for concerned parameters:\n", warnin)

    showNotification(warnin, type = "warning", duration = 8,closeButton = T)

  }
  #### ends handling if several parameters
  print("Use pecc_PopEd function")

output_OD <- pecc_PopEd(model ,OD_input  , states , events , parameters, diagOmega, outputExpr = T)

# output_OD <<- output_OD
# print("heddreaaaaa")
print("End use pecc_PopEd function")

updateTextAreaInput(session, "Code_Optimal_Design", value = output_OD %>%
                      map(~paste0(deparse(.x, width.cutoff = 500), collapse = "\n")) %>%
                      paste(collapse = "\n\n") %>%
                      # gsub(pattern = "<<-", replacement = "<-") %>%
                      gsub(pattern = "^\\{ *\n*", replacement = "") %>%
                      gsub(pattern = "\\}$", replacement = "")
                      )
print("Begin evaluation")
# print(output_OD)
testOD <- try(eval(output_OD,envir = globalenv()))

print("End evaluation")
#
if(class(testOD) == "try-error"){

  showNotification("PopED failed - try modify produced code", type = "error", duration = 3)

}else{

  # print results
  imap_chr(testOD$result_OD$rse, ~ paste0(.y, " = ", round(.x,1), "%")) %>%
    paste0(collapse = "\n") %>%
    {paste0("RSE percentage:\n--------------------\n", ., "\n--------------------\nofv: ", testOD$result_OD$ofv, "\n--------------------" )} -> res


  updateTextAreaInput(session, "Result_Optimal_Design", value = res )

  # show plot

  output$OptimPlot <- renderPlot(testOD$plot_OD)
}


})

# model plot --------------------------------------------------------------



observeEvent(input$mb_load_plot,{

  cat('Load plot')

  if(isolate(input$mb_load_cov) == F){

    model <- isolate(input$mb_model)

  }else{

    model <- paste(isolate(input$mb_model_cov), isolate(input$mb_model), sep = "\n")

  }


  matrixx <- hot_to_r(isolate(input$mb_matrix))
  if(input$matrix_diag == F)    matrixx <- diag_to_pecc(matrixx)

  sdd <- T
  if(isolate(input$matrix_sd_var) == "var") sdd <- F

  print("start")




  parameters <-  random_etas(n = isolate(input$mb_nsimul),
                             parameter_df = hot_to_r(isolate(input$mb_paramater)),
                             matrix_eta = matrixx,sd = sdd, returnExpr = T, matrixShiny = T)

  print("parameters okay")


if(isolate(input$mb_nsimul) > 0 & isolate(input$mb_add_error_plot)  == T){
  errortemp <- hot_to_r(isolate(input$mb_output))
}else{

  errortemp <- NA
}


  if(isolate(input$mb_ode_solver) == "deSolve"){

    # add 1E-20 and remove time = 0 during smulation (to have pretty IV)
  times      <- expr(sort(c(1E-20, seq(!!isolate(input$mb_time_from), !!isolate(input$mb_time_to), by = !!isolate(input$mb_time_by)))))


  simulations <-   make_simulations(parameter = parameters,
                                    model = model,
                                    error = errortemp,
                                    states =  hot_to_r(isolate(input$mb_state)),
                                    events =  hot_to_r(isolate(input$mb_event)),times = !!times, timesF = F)
  rmt0 <- T
   }else{

     library(RxODE)

    times      <- expr(seq(!!isolate(input$mb_time_from), !!isolate(input$mb_time_to), by = !!isolate(input$mb_time_by)))




    simulations <-   make_simulations_rxode(parameter = parameters,
                                      model = model,
                                      error = errortemp,
                                      states =  hot_to_r(isolate(input$mb_state)),
                                      events =  hot_to_r(isolate(input$mb_event)),times = !!times)
    rmt0 <- F
  }
  print("simulations okay")
  # print(simulations)

  if(isolate(input$useImpData) == F){
    # print("ici")
    explotemp <- expr(explo)


    xpointt <- isolate(input$exploX)
    ypointt  <- isolate(input$exploY)
    # print <- "pouet"
    ytype_headerr <- try(explo_headerevaled[explo_headerevaled == "YTYPE"])
    # print("iciend")
  } else{
    # explo <-  InfoModelPecc$df
   # print("la")
    explotemp <- expr(InfoModelPecc$df)

    if(! isolate(input$IDImpMod) %in% c("Initial", "Pop") ){


      explotemp <- expr(!!explotemp %>%
                          filter(identifier == !!gsub("(id)|(_mean)|(_mode)", "",  isolate(input$IDImpMod))))
      #print("laend")
      # explo <-  explotemp
    }

    xpointt <- "time"
    ypointt  <- "observation"
    ytype_headerr <- "observationtype"
  }


  print("simulations2 okay")
# print(explo)
# print(xpointt)
# print(ypointt)
  plot_table <- hot_to_r(isolate(input$mb_display2)) %>%
              filter(Check)

  if(sum(plot_table$Point) > 0 & (xpointt == ""| ypointt == "")){

    showNotification("Data can not be added - X or Y unknown", type = "warning", duration = 4, closeButton = T)
    plot_table <- plot_table %>%
      mutate(Point = F)
  }

  # Check if all filters are correct

  if(sum(plot_table$Point) > 0){
    plot_table$test <- F

    for(a in 1:nrow(plot_table)){

      if(plot_table$Check[[a]] & plot_table$Filter_of_dataset[[a]] != "" & !is.na(plot_table$Filter_of_dataset[[a]])){

        testfiltre <-  try(explo %>%
                             slice(1) %>%
                             filter(!!!parse_expr(plot_table$Filter_of_dataset[[a]])), silent = T)
        if(class(testfiltre) == "try-error"){
          plot_table$test[[a]] <- T
          plot_table$Point[[a]] <- F
        }
      }

    }

    if(sum(plot_table$test) > 0){

      showNotification(paste0("Following filter(s) does not work (Point set to false):", paste0(plot_table$Filter_of_dataset[plot_table$test], collapse = ", ")) , type = "warning", duration = 4, closeButton = T)


      output$mb_display2 <- renderRHandsontable({


        rhandsontable(plot_table %>% select(-test), width = 500, height = 200, rowHeaders = NULL)

      })


    }
  }





  plot_simulations <-  plot_simulations(simulations = simulations,ymindisplayed = isolate(input$mb_minvaluesimul),
                                        ymaxdisplayed =  isolate(input$mb_maxvaluesimul),
                                        plot_table = plot_table,
                                        plot_table_cov = hot_to_r(isolate(input$mb_plot_stat)),
                                        events =  hot_to_r(isolate(input$mb_event)),
                                        xpoint = xpointt,ypoint = ypointt,
                                        scalewrap =  paste0("\"",hot_to_r(isolate(input$mb_plot_stat))$scalewrap[[1]],"\""), # for now only the first value,
                                        #so not possible to have different scalewrap per plot....
                                        #but in practice making several plots is almost never used so...
                                        n = isolate(input$mb_nsimul),
                                        ytype_header = ytype_headerr,
                                        name_df = deparse(explotemp) %>% paste(collapse = " "), rmt0 = rmt0)
  print("plot okay")

  output_plot <-   map(plot_simulations, function(x){

      deparse(x, width.cutoff = 500) -> temp

    temp[temp != "        NA"  ] %>%
        paste(collapse = "\n") %>%
        gsub(pattern = "%>%", replacement = "%>%\n" ) %>%
        # gsub(pattern = "\\+", replacement = "+\n" ) %>%
        gsub(pattern = "  *", replacement = " " ) %>%
      gsub(pattern = "withProgress\\(message.+simmax <- nrow\\(crossing\\(parameters, events\\)\\)", replacement = "") %>%
      gsub(pattern = "withProgress.+simmax <- nrow\\(parameters_df\\) \\* nrow\\(events_df\\)", replacement = "") %>%
      gsub(pattern = "\n *incProgress\\(1/simmax.+simmax\\)\\) *\n", replacement = "") %>%
        gsub(pattern = "ysim <<- ysim \\+ 1",replacement = "") %>%
        gsub(pattern = "unnest\n?}\\)",
             replacement = "unnest\n")

    })%>%
    paste(collapse = "\n\n")



  updateTextAreaInput(session, "Code_plot_sim", value = output_plot)




  output$mb_plot <- renderPlot(eval(expr({!!!plot_simulations})))




})



# Try estimate initial value ----------------------------------------------

observeEvent(input$mb_estimatePopParam,{
# print()
#
# ### copy-past bloc from above, expeced time and parameters
  if(isolate(input$mb_load_cov) == F){

    model <- isolate(input$mb_model)

  }else{

    model <- paste(isolate(input$mb_model_cov), isolate(input$mb_model), sep = "\n")

  }

  # print(model)



  matrixx <<- hot_to_r(isolate(input$mb_matrix))
  if(input$matrix_diag == F)    matrixx <- diag_to_pecc(matrixx)

  sdd <- T
  if(isolate(input$matrix_sd_var) == "var") sdd <- F

  print("start")
  print("isolate(input$mb_paramater)")





  print("parameters okay")


  if(isolate(input$mb_nsimul) > 0 & isolate(input$mb_add_error_plot)  == T){
    errortemp <- hot_to_r(isolate(input$mb_output))
  }else{

    errortemp <- NA
  }

  ### copy-past bloc from above



explotemp <- explo
X <- isolate(input$exploX)
Y <- isolate(input$exploY)


### Apply
# print("here")
# explotemp <- explotemp[c(X, Y)] %>% as_tibble %>% distinct
# names(explotemp)[which(names(explotemp) == X)] <- "timePeccc"
# names(explotemp)[which(names(explotemp) == Y)] <- "OBSPeccc"
# print("here")
# names(explotemp) <- c("time", "OBS")





parameterrs <- hot_to_r(isolate(input$mb_paramater))
state <- hot_to_r(isolate(input$mb_state))
events <-  hot_to_r(isolate(input$mb_event)) %>%
  filter(use)

todisplay <-  hot_to_r(isolate(input$mb_display2)) %>%
  filter(Check & Point)

### gestion filter events
if(events %>%
   filter(use == T & filterPlot != "") %>%
   nrow > 1 ){
  events %>%
    filter(use == T & filterPlot != "") %>%
    mutate(Proto = paste0( filterPlot, " ~ \"Prot ", Proto,"\"" )) %>%
    pull(Proto) %>%
    parse_exprs %>%
    {expr(case_when(!!!.))} -> exprfilterPlotAdm
  # print(exprfilterPlotAdm)
  explotemp <- explotemp %>%
    mutate(Proto = !!exprfilterPlotAdm)

}else if( events %>%
          filter(use == T & filterPlot != "") %>%
          nrow == 1){

  events %>%
    filter(use == T & filterPlot != "") %>%
    mutate(Proto = paste0( filterPlot, " ~ \"\"" )) %>%
    pull(Proto) %>%
    parse_exprs %>%
    {expr(case_when(!!!.))} -> exprfilterPlotAdm


  explotemp <- explotemp %>%
    mutate(Proto = !!exprfilterPlotAdm)
}
### end gestion filter events

times      <- explotemp %>%
  distinct(!!parse_expr(X)) %>%
  arrange(!!parse_expr(X)) %>%
  pull(!!parse_expr(X))

# X <- "time"
# Y <- "OBS"
# x <- parameterrs$Value[parameterrs$E == "Esti"]
#
# model <<- model
# errortemp <<- errortemp
ncount <- 0
withProgress(message = 'Trying parameter optimization', value = 0, {
funcOptim <- function(x){

  ncount <<- ncount + 1
  incProgress(1/200, detail = paste( ncount, "/", 200, "(max)"))


  parameterrs$Value[parameterrs$E == "Esti"] <- x

  parameters <-  random_etas(n = 0,
                             parameter_df = parameterrs,
                             matrix_eta = matrixx,sd = sdd, returnExpr = T, matrixShiny = T)



  simulations <-   make_simulations(parameter = parameters,
                                    model = model,
                                    error = errortemp,
                                    states = state,
                                    events = events,times = times, timesF = T,  returnExpr = F, Progress = F)


 todisplay %>%
   filter(Check & Point) %>%
    mutate(OF = map2_dbl(Todisplay , Filter_of_dataset, function(xx,yy){


      names(simulations)[[which(names(simulations) == xx)]] <- "outputforpecc"
      names(simulations)[which(names(simulations) == "time")] <-X


      if(is.na(yy)) yy <- ""
      # print(yy)
      # print(explotemp)
      if(gsub(" *", "", yy) != "") explotemp <- explotemp %>% filter_(yy)

      explotemp %>%
        left_join( simulations %>%
                     select(!!parse_expr(X), outputforpecc, Proto)) %>%
        filter(!is.na(outputforpecc)) %>%
        filter(!!parse_expr(Y) !=0) %>%
        # mutate(obs = if_else(OBS == 0 , 0.001, OBS)) %>%
        mutate(residuals = (outputforpecc - !!parse_expr(Y)) ^2 /  !!parse_expr(Y)) %>% # abs(outputforpecc - !!parse_expr(Y))/ (!!parse_expr(Y))
        filter(!is.na(residuals)) %>%
        summarise(sum = sum(residuals)) %>%
        pull(sum)




    })) %>%
   pull(OF) %>%
   sum
}

resultOptim <- optim(par = parameterrs$Value[parameterrs$E == "Esti"], fn = funcOptim, control = list(maxit = 200))$par

}) # end withprogress
parameterrs$Value[parameterrs$E == "Esti"] <- resultOptim
output$mb_paramater <- renderRHandsontable(rhandsontable(parameterrs, width = 200, height = 200, rowHeaders = NULL))

#################### here it's just a copy past from model plot ######


if(isolate(input$mb_load_cov) == F){

  model <- isolate(input$mb_model)

}else{

  model <- paste(isolate(input$mb_model_cov), isolate(input$mb_model), sep = "\n")

}

# print(model)

times      <- seq(isolate(input$mb_time_from), isolate(input$mb_time_to), by = isolate(input$mb_time_by))

matrixx <- hot_to_r(isolate(input$mb_matrix))
if(input$matrix_diag == F)    matrixx <- diag_to_pecc(matrixx)

sdd <- T
if(isolate(input$matrix_sd_var) == "var") sdd <- F

print("start")
print("isolate(input$mb_paramater)")



parameters <-  random_etas(n = isolate(input$mb_nsimul),
                           parameter_df = parameterrs,
                           matrix_eta = matrixx,sd = sdd, returnExpr = T, matrixShiny = T)

print("parameters okay")


if(isolate(input$mb_nsimul) > 0 & isolate(input$mb_add_error_plot)  == T){
  errortemp <- hot_to_r(isolate(input$mb_output))
}else{

  errortemp <- NA
}


simulations <-   make_simulations(parameter = parameters,
                                  model = model,
                                  error = errortemp,
                                  states =  hot_to_r(isolate(input$mb_state)),
                                  events =  hot_to_r(isolate(input$mb_event)),times = times, timesF = T)
print("simulations okay")
# print(simulations)

if(isolate(input$useImpData) == F){
  # print("ici")
  explotemp <- expr(explo)
  xpointt <- isolate(input$exploX)
  ypointt  <- isolate(input$exploY)
  # print <- "pouet"
  ytype_headerr <- try(explo_headerevaled[explo_headerevaled == "YTYPE"])
  # print("iciend")
} else{
  # explo <-  InfoModelPecc$df
  # print("la")
  explotemp <- expr(InfoModelPecc$df)

  if(! isolate(input$IDImpMod) %in% c("Initial", "Pop") ){


    explotemp <- expr(!!explotemp %>%
                        filter(identifier == !!gsub("(id)|(_mean)|(_mode)", "",  isolate(input$IDImpMod))))
    #print("laend")
    # explo <-  explotemp
  }

  xpointt <- "time"
  ypointt  <- "observation"
  ytype_headerr <- "observationtype"
}


print("simulations2 okay")
# simulationstest <<- simulations

plot_simulations <-  plot_simulations(simulations = simulations,ymindisplayed = isolate(input$mb_minvaluesimul),
                                      ymaxdisplayed =  isolate(input$mb_maxvaluesimul),
                                      plot_table = hot_to_r(isolate(input$mb_display2)),
                                      plot_table_cov = hot_to_r(isolate(input$mb_plot_stat)),
                                      xpoint = xpointt,ypoint = ypointt,
                                      events =  hot_to_r(isolate(input$mb_event)),
                                      n = isolate(input$mb_nsimul),
                                      ytype_header = ytype_headerr,
                                      name_df = deparse(explotemp) %>% paste(collapse = " "))
print("plot okay")
# print(plot_simulations)
output_plot <-   plot_simulations %>%
  map(function(x){

    deparse(x, width.cutoff = 500) %>%
      paste(collapse = "\n") %>%
      gsub(pattern = "%>%", replacement = "%>%\n" ) %>%
      # gsub(pattern = "\\+", replacement = "+\n" ) %>%
      gsub(pattern = "  *", replacement = " " ) %>%
      gsub(pattern = "withProgress\\(message.+simmax <- nrow\\(crossing\\(parameters, events\\)\\)", replacement = "") %>%
      gsub(pattern = "withProgress.+simmax <- nrow\\(parameters_df\\) \\* nrow\\(events_df\\)", replacement = "") %>%
      gsub(pattern = "incProgress\\(1/simmax.+simmax\\)\\)", replacement = "") %>%
      gsub(pattern = "ysim <<- ysim \\+ 1",replacement = "") %>%
      gsub(pattern = "unnest\n?}\\)",
           replacement = "unnest\n")

  }) %>%
  paste(collapse = "\n\n")


output_plot <- paste0("times <- seq(", isolate(input$mb_time_from), ",", isolate(input$mb_time_to), ",", isolate(input$mb_time_by), ")\n",
                      output_plot)

updateTextAreaInput(session, "Code_plot_sim", value = output_plot)




output$mb_plot <- renderPlot(eval(expr({!!!plot_simulations})))


})


# Scheme ------------------------------------------------------------------


observeEvent(input$scheme_launch,{


  modell <- isolate(input$mb_model)
  print(modell)
# try(library(diagram))
test_scheme <-  try( pecc_scheme(model = modell, relsize = input$scheme_size))
print(test_scheme)
if(class(test_scheme) != "try-error"){

  output$scheme_plot <- renderPlot(pecc_scheme(model = modell, relsize = input$scheme_size))

}

})

# nlmixr translation ------------------------------------------------------------------


observeEvent(input$nlmixr_translate,{

  modell <<- isolate(input$mb_model)
  parameters <<- hot_to_r(isolate(input$mb_paramater))
  states <<-  hot_to_r(isolate(input$mb_state))
  events <<- eventsinput <- hot_to_r(isolate(input$mb_event)) %>%
    filter(Proto == 1 & use == T)
  # odebaseline <- isolate(input$ode_baselines)
  # display2 <-  hot_to_r(isolate(input$mb_display2))
 output <<-  mb_output <<-  hot_to_r(isolate(input$mb_output))
  diagOmega <<- omega <-  hot_to_r(isolate(input$mb_matrix))

nlmixrcode <-  try(deSolve_to_nlmixr(model = modell, states = states, events =  eventsinput,
                    parameters =  parameters,diagOmega = omega, output = mb_output, path_data = explo_path,
                    xcol = isolate(input$exploX), ycol = isolate(input$exploY) ))

if(class(nlmixrcode) != "try-error"){

updateTextAreaInput(session, "nlmixr_result", value = nlmixrcode)

}

})

# nlmixr launch ------------------------------------------------------------------
observeEvent(input$nlmixr_go,{

  if(length(grep("C:/Rtools/bin;", path)) == 0){
  path <- Sys.getenv("PATH")
  path <- c("C:/Rtools/bin", "C:/Rtools/mingw_64/bin", path)
  path <- paste(path,collapse=";")
  Sys.setenv(PATH=path)
  Sys.getenv("PATH")
  }

  try(library(nlmixr))

nlmixrcode <- isolate(input$nlmixr_result)

paste0("try({\n",nlmixrcode, "\n
saveRDS(object = fit.s, file =\"",  isolate(input$nlmixr_path),".nlmixr\")})") %>%
  # print()
  parse_expr() %>%
  eval

showNotification("Model nlmixr done", type = "message", duration = 5)

print("nlmixr Done !")
})

# ode translation Monolix---------------------------------------------------------

###  ode ode ode ode ode ode ode ode ode ode ode ode
observeEvent(input$ode_trigger_monolix,{


  modell <- isolate(input$mb_model)
  parameters <- hot_to_r(isolate(input$mb_paramater))
  states <-  hot_to_r(isolate(input$mb_state))
  eventsinput <- hot_to_r(isolate(input$mb_event)) %>%
    filter(Proto == 1 & use == T)
  odebaseline <- isolate(input$ode_baselines)
  display2 <-  hot_to_r(isolate(input$mb_display2))
  mb_output <-  hot_to_r(isolate(input$mb_output))
  # parametersbckp <- parameters
  # statesbckp <- states
  # eventsbckp <- events
  # states <- statesbckp

  deSolve_peccc <- deSolve_pecc(modell)

  test <- try(ode_monolix(mb_output = mb_output %>% filter(export == T),
                          depot = eventsinput, y = states,
                          times = times, func = eval(deSolve_peccc[[1]]),
                          outputcat = F, parms = parameters))
  # print(test)
  if(class(test)!="try-error") updateTextAreaInput(session, inputId = "ode_output", value = test )

  # }


})




# nonmem ------------------------------------------------------------------


observeEvent(input$ode_trigger_nonmem,{

  modell <- isolate(input$mb_model)
  parameters <- hot_to_r(isolate(input$mb_paramater))
  deSolve_peccc <<- deSolve_pecc(modell)
  omega <-  hot_to_r(isolate(input$mb_matrix))
  states <- hot_to_r(isolate(input$mb_state))
  mb_output <<- hot_to_r(isolate(input$mb_output)) %>%
    filter(export == T)

  # print("try nonmem")
  test <- try(
    ode_nonmem(omega = omega, mu_referencig = isolate(input$ode_mu), BLQ = isolate(input$ode_blq), add_param = addparam, mb_output = mb_output, y = states,
               times = times, func = eval(deSolve_peccc[[1]]),
               outputcat = F, parms = parameters)
  )

  if(class(test)!="try-error")  updateTextAreaInput(session, inputId = "ode_output_nonmem", value = test )


})





# launche monoilx -------------------------------------------------------

observeEvent(input$ode_modelsave_monolix,{

  # some usefull data
  namemodel <- gsub(" ", "_",isolate(input$names_model))
  filepath <- file.path(project_file, paste0("3_Models/0_model_files/", namemodel, ".txt"))
  deSolve_peccc <- deSolve_pecc(isolate(input$mb_model))


  explo_header_temp <-   explo_header %>%
    gsub(pattern = "\"ID\"", replacement = "\"identifier\"") %>%
    gsub(pattern = "\"OBS\"", replacement = "\"observation\"") %>%
    gsub(pattern = "\"BLQ\"", replacement = "\"censored\"") %>%
    gsub(pattern = "\"EVID\"", replacement = "\"eventidentifier\"") %>%
    gsub(pattern = "\"MDV\"", replacement = "\"missingdependentvariable\"") %>%
    gsub(pattern = "\"AMT\"", replacement = "\"amount\"") %>%
    gsub(pattern = "\"cov.cont\"", replacement = "\"covariate, type=continuous\"") %>%
    gsub(pattern = "\"cov.cat\"", replacement = "\"covariate, type=categorical\"") %>%
    gsub(pattern = "\"BLQ\"", replacement = "\"censored\"") %>%
    gsub(pattern = "\"YTYPE\"", replacement = "\"observationtype\"") %>%
    gsub(pattern = "\"ADM\"", replacement = "\"administration\"")



  # print("whaaat")
  headertemp <-  eval(parse_expr(explo_header_temp))



  ## prefer the time used for simulation
  headertemp[headertemp == "time"] <- "drop"
  headertemp[isolate(input$exploX)] <- "time"
  # print("whaaat2")
  ytypelabel <-   names(headertemp)[headertemp == "observationtype"]

  print("end loading usefull data")

  ###  ####1 create if needed a subset of the dataset ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###

  ### 1.1 composing the final filter
  # which filter is needed
  pathsubset <- "" # Initialisation
  fordataset <- hot_to_r(isolate(input$mb_display2)) %>% filter(Point == T) # Which lines had point?


  print("waow")
  # if there is no YTYPE (note: to remove in futur, lide added because YTYPE columns has been added lately)
  if(!"YTYPE" %in% names(fordataset))fordataset <- fordataset %>% mutate(YTYPE = "")

  ##F seach for line with a filter to use
  filtersearch <- fordataset %>%
    filter((YTYPE != "" | !is.na(YTYPE)) | (Filter_of_dataset != "" | !is.na(Filter_of_dataset)))
  ## if at least one filter has been used, then compute the final filter
  if(nrow(filtersearch) >0){

    filtersearch %>%
      mutate(filter = map2(YTYPE,Filter_of_dataset, function(YTYPE,Filter_of_dataset){

        YTYPE <- if_else(YTYPE =="", "", paste0(ytypelabel, " == " , YTYPE))

        temp <- c(YTYPE, Filter_of_dataset)
        temp <- temp[temp != ""]

        if(length(temp) > 0){

          return(paste0("(", paste0(temp, collapse = " & "),")"))

        }else{


          return("")
        }         } )) %>%
      pull(filter) %>%
      paste(collapse = "|") -> filtersearch
    print("waow2")
  }else{

    filtersearch -> ""

  }



  # filtering evid
  hot_to_r(isolate(input$mb_event)) %>%
    filter(use == T & Proto == "1") %>%
    pull(ADM) -> ADMtokeep
  # print(ADMtokeep)

  if(length(which(gsub(" ", "", ADMtokeep) == ""))>0)  ADMtokeep <- ADMtokeep[- which(gsub(" ", "", ADMtokeep) == "")]


  # print("waow3")
  # print(ADMtokeep)
  if(length(ADMtokeep) > 0){

    # ugly but I am out of time, to improve after
    eval(parse_expr(explo_header))[eval(parse_expr(explo_header)) == "ADM"] %>% names() -> admini

    filtersearch <- paste0("(", filtersearch, ") | ", admini , " %in% c(", ADMtokeep %>% paste(collapse = ","), ")")
    # filtersearch <- "TIME == 3 & YTYPE2"
  }else{

    eval(parse_expr(explo_header))[eval(parse_expr(explo_header)) == "EVID"] %>% names() -> evid
    filtersearch <- paste0("(", filtersearch, ") | ", evid , " == 1")

  }

  # if you wand to add additional filter:
  if(isolate(input$monolix_filter) != "")   filtersearch <- paste0("(", filtersearch, ") & ", "(", isolate(input$monolix_filter), ")")
  # print("waow4")
  ### 1.2 looking if the subset has already been created or create it
  if(filtersearch != ""){



    ## search for the folder containing every subsets or create it
    folder_path <- paste0(gsub("\\..{,5}", "", explo_path), "_subsets")
    file_path <-  file.path(folder_path, "metada.txt")

    if(!file.exists(folder_path)){

      dir.create(folder_path)
      write.table(data.frame(number = character(), filter = character()), file = file_path, row.names = F, sep = ",")

    }

    ## search if the filter has already be done in the metadatas file
    # print("waow5")

    read.table(file_path, header = T, sep = ",") ->metadatas
    metadatas %>%
      filter(filter == filtersearch) %>% pull(number) %>% as.numeric()-> number_subdataset

    # print("metadas")
    # print(number_subdataset)
    # if not create the new dataset
    if(length(number_subdataset) == 0){
      # print("rrrrr")
      # print(metadatas$number)

      n_newsub <-  if_else(max(metadatas$number) == -Inf, 1, as.double(max(metadatas$number))+1)
      # print(n_newsub)
      pathsubset <-  file.path(folder_path, paste0(gsub("(.+/)|(\\..{,5})", "", explo_path),"_sbd", n_newsub,".", gsub(".+\\.", "", explo_path) ))
      # print(pathsubset)



      # print(pathsubset)
      ## create the new dataset
      # print(filtersearch)
      # print()
      explo %>%
        filter(!!parse_expr(filtersearch)) %>%
        write.table(pathsubset, sep = explo_sep, dec = explo_dec, na = explo_na, quote = F, row.names = F)
      # print("waow6")

      # output$messages <- renderMenu({
      #
      #   dropdownMenu(type = "tasks", .list =   list(taskItem(text = paste0("New dataset created"), value = 100)))
      # })

      showNotification(paste0("New dataset created:",pathsubset), type = "message", duration = 10)
      #and store metadatas
      metadatas %>%
        add_row(number = n_newsub, filter = filtersearch) %>%
        write.table(file = file_path, row.names = F, sep = ",")

    }else{
      # print("waow7")

      pathsubset <-  file.path(folder_path, paste0(gsub("(.+/)|(\\..{,5})", "", explo_path),"_sbd", number_subdataset,".", gsub(".+\\.", "", explo_path) ))


    }
    # end if(filtersearch != "")
  }

  print("end subset handling")

  ####2 Make sure the code source is saved or create one ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###

  # print(filepath)
  if(1 == 1){ #!file.exists(filepath)

    print("file do not exist: creation ongoing...")
    parameters <- hot_to_r(isolate(input$mb_paramater)) %>%
      # parameters <- tibble(Param = c("ke","ka"), Value = 1:2)
      transmute(temp = paste0(Param, " = ", Value)) %>%
      pull(temp) %>%
      {paste0("c(",   reduce(., paste, sep = ", "), ")")}





    states <-   hot_to_r(isolate(input$mb_state)) %>%
      transmute(temp = paste0(Cmt, " = \"", t0, "\"")) %>%
      pull(temp) %>%
      {paste0("c(",   reduce(., paste, sep = ", "), ")")}




    eventsinput <- hot_to_r(isolate(input$mb_event)) %>%
      filter(Proto == 1 & use == T)


    ### Ajout automatique des output_
    if(is.na(deSolve_peccc[["output_manual"]]) | deSolve_peccc[["output_manual"]] == "" ){

      outputode <- ""
    }else{

      outputode <- str_split(deSolve_peccc[["output_manual"]], " ")[[1]]
    }


    #### Possibilité d'ajout de parametre baseline



    if(length(isolate(input$ode_baselines)) > 0){

      addparam <- paste0(isolate(input$ode_baselines), "_0")

    }else{

      addparam <- ""
    }



    outputcat <<- F
    # print("azezeazeza")
    add_param <- hot_to_r(isolate(input$mb_state)) %>%
      filter(Param != "None") %>%
      pull(Cmt)
    # print(add_param)
    if(length(add_param) == 0) add_param <- ""

    test <- try(ode_monolix(ytype = hot_to_r(isolate(input$mb_display2)) %>% filter(Point == T),
                            add_param = add_param, y = eval(parse_expr(states)),depot = eventsinput,
                            times = times, func = eval(deSolve_peccc[[1]]),
                            outputcat = F, parms = eval(parse_expr(parameters))))


    # print(test)
    if(class(test)!="try-error") updateTextAreaInput(session, inputId = "ode_output", value = test )

    if(!file.exists( file.path(project_file, "3_Models/0_model_files"))) dir.create( file.path(project_file, "3_Models/0_model_files"))
    # print("azezeazeza")
    fileConn<-file(file.path(project_file, paste0("3_Models/0_model_files/", namemodel, ".txt"))) #file.path(project_file, "3_Models/0_model_files/test.txt")
    # print("azezeazeza")
    writeLines( test, fileConn)
    close(fileConn)

    showNotification(paste0("Model file created:", file.path(project_file, paste0("3_Models/0_model_files/", namemodel, ".txt"))))
    # print("azezeazeza")
  }else{

    test <- readLines( file.path(project_file, paste0("3_Models/0_model_files/", namemodel, ".txt"))) %>%
      paste(collapse = "\n")
    updateTextAreaInput(session, inputId = "ode_output", value = test )

  }

  print("model_file okay")
  #### place the nmlxtran at the right place

  print("mlxtran creation")

  ####2  mlxtran creation  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###  ###

  delimiter <- case_when(explo_sep == "," ~ "comma",
                         explo_sep == ";" ~ "semicolon",
                         T ~ "comma")


  # explo_header_temp <-
  explo_header_temp <-   explo_header %>%
    gsub(pattern = "\"ID\"", replacement = "\"identifier\"") %>%
    gsub(pattern = "\"OBS\"", replacement = "\"observation\"") %>%
    gsub(pattern = "\"BLQ\"", replacement = "\"censored\"") %>%
    gsub(pattern = "\"EVID\"", replacement = "\"eventidentifier\"") %>%
    gsub(pattern = "\"MDV\"", replacement = "\"missingdependentvariable\"") %>%
    gsub(pattern = "\"AMT\"", replacement = "\"amount\"") %>%
    gsub(pattern = "\"cov.cont\"", replacement = "\"covariate, type=continuous\"") %>%
    gsub(pattern = "\"cov.cat\"", replacement = "\"covariate, type=categorical\"") %>%
    gsub(pattern = "\"BLQ\"", replacement = "\"censored\"") %>%
    gsub(pattern = "\"YTYPE\"", replacement = "\"observationtype\"")







  # hot_to_r(isolate(input$mb_display2)) %>%
  #   filter(Filter_of_dataset != "" & !is.na(Filter_of_dataset)) %>%
  #   mutate(Filter = map(Filter_of_dataset, ~ str_split(.x, "&")[[1]][grep("YTYPE", str_split(.x, "&")[[1]])] %>%
  #                         gsub(pattern = "(YTYPE * == *)| *",replacement = "") ))

  if(pathsubset != "")
    df_path <-pathsubset
  else
    df_path <- explo_path


  read.table(df_path, sep = explo_sep, nrows = 1, header = T) %>%
    names -> checknewcol

  checknewcol[!checknewcol%in% names(headertemp)] -> newcols
  # print(newcols)
  if(length(newcols) > 0){

    for(a in newcols) headertemp[a] <- "drop"

  }


  if(!file.exists( file.path(project_file, paste0("3_Models/1_models/", namemodel)))) dir.create( file.path(project_file,paste0("3_Models/1_models/", namemodel)))
  pathmeta <- file.path(project_file, paste0("3_Models/1_models/", namemodel,"/", "peccary_", namemodel,".txt"))
  if(!file.exists(pathmeta)){

    tibble(n = double(), description = character(), dataset = double(), filter = character(), model = character(), software = character()) %>%
      write.table(file = pathmeta, quote = F, row.names = F, sep = ";")
  }
  # reading metadata
  metaModel <- read.table(pathmeta, header = T, sep = ";")
  # what is the new number to apply !
  if(nrow(metaModel) == 0 ){newn <- 1}else{ newn <- max(metaModel$n) + 1}
  # updating metamodel
  metaModel %>%
    bind_rows(
      tibble(n = newn, description = isolate(input$monolix_description), dataset = isolate(input$preloadeddataset), filter = filtersearch, model = namemodel, software = "Monolix")
    )%>%
    write.table(file = pathmeta, quote = F, row.names = F, sep = ";")


  mlxtran <- mlxtran_creation(df_path = df_path,
                              event = hot_to_r(isolate(input$mb_event)),
                              ytype = fordataset,
                              exportpath= paste0(namemodel, "_", newn),
                              cmt = hot_to_r(isolate(input$mb_state)),
                              model_path = filepath,
                              omega = hot_to_r(isolate(input$mb_matrix)),
                              parameter = hot_to_r(isolate(input$mb_paramater)),
                              delimiter = delimiter,
                              output = gsub("^output_","", deSolve_peccc$output_manual),
                              header = headertemp)

  updateTextAreaInput(session, inputId = "nmlxtran_output", value = mlxtran)




  # creating the new file
  outputfile <- file.path(project_file, paste0("3_Models/1_models/", namemodel,"/", namemodel,"_",newn, ".mlxtran"))
  mxtranfile<-file(outputfile) #file.path(project_file, "3_Models/0_model_files/test.txt")
  writeLines(mlxtran, mxtranfile)
  close(mxtranfile)


  showNotification(paste0("monolix project created: ", paste0("3_Models/1_models/", namemodel,"/test.mlxtran")))
  print("mlxtran created")
  #### launch the model


  updateTextInput(session, "ode_projectpath_monolix", value = gsub("/", "\\\\", outputfile) )





  # print(file.path(project_file, paste0("3_Models/1_models/", namemodel,"/test.mlxtran")))
  # withProgress(message = 'Nonmem modeling', value = 0, {
  #     library(lixoftConnectors)
  #     initializeLixoftConnectors(software="monolix")
  #
  #
  #                       loadProject(projectFile = file.path(project_file, paste0("3_Models/1_models/", namemodel,"/test.mlxtran")))
  #
  #                       runPopulationParameterEstimation()
  # })

})



# ADAPT -------------------------------------------------------------------


observeEvent(input$ode_trigger_adapt,{

  modell <- isolate(input$mb_model)

  # print("try nonmem")
  test <- try(
    desolve_to_adapt(modell)
  )

  if(class(test)!="try-error")  updateTextAreaInput(session, inputId = "ode_output_adapt", value = test )


})


# nlmxtran ----------------------------------------------------------------

observeEvent(input$launch_monolix,{

  shell.exec("C:/Program Files/Monolix_Suite/lib/monolix.exe")
  # click(id = "nmlxtran_launch")
})

observeEvent(input$nmlxtran_launch,{

  namemodel <- gsub(" ", "_",isolate(input$names_model))
  filepath <- file.path(project_file, paste0("3_Models/0_model_files/", namemodel, ".txt"))
  deSolve_peccc <- deSolve_pecc(isolate(input$mb_model))

  delimiter <- case_when(explo_sep == "," ~ "comma",
                         explo_sep == ";" ~ "semicolon",
                         T ~ "comma")


  # explo_header_temp <-
  explo_header_temp <-   explo_header %>%
    gsub(pattern = "\"ID\"", replacement = "\"identifier\"") %>%
    gsub(pattern = "\"OBS\"", replacement = "\"observation\"") %>%
    gsub(pattern = "\"BLQ\"", replacement = "\"censored\"") %>%
    gsub(pattern = "\"EVID\"", replacement = "\"eventidentifier\"") %>%
    gsub(pattern = "\"MDV\"", replacement = "\"missingdependentvariable\"") %>%
    gsub(pattern = "\"AMT\"", replacement = "\"amount\"") %>%
    gsub(pattern = "\"cov.cont\"", replacement = "\"covariate, type=continuous\"") %>%
    gsub(pattern = "\"cov.cat\"", replacement = "\"covariate, type=categorical\"") %>%
    gsub(pattern = "\"BLQ\"", replacement = "\"censored\"") %>%
    gsub(pattern = "\"YTYPE\"", replacement = "\"observationtype\"") %>%
    gsub(pattern = "\"ADM\"", replacement = "\"administration\"")





  headertemp <-  eval(parse_expr(explo_header_temp))

  ## prefer the time used for simulation
  headertemp[headertemp == "time"] <- "drop"
  headertemp[isolate(input$exploX)] <- "time"

  ### if there is no header

  read.table(explo_path, sep = explo_sep, nrows = 1, header = T) %>%
    names -> checknewcol

  checknewcol[!checknewcol%in% names(headertemp)] -> newcols
  # print(newcols)
  if(length(newcols) > 0){

    for(a in newcols) headertemp[a] <- "drop"

  }
  # print(headertemp)


  # hot_to_r(isolate(input$mb_display2)) %>%
  #   filter(Filter_of_dataset != "" & !is.na(Filter_of_dataset)) %>%
  #   mutate(Filter = map(Filter_of_dataset, ~ str_split(.x, "&")[[1]][grep("YTYPE", str_split(.x, "&")[[1]])] %>%
  #                         gsub(pattern = "(YTYPE * == *)| *",replacement = "") ))

  event <- hot_to_r(isolate(input$mb_event)) %>%
                       filter(use == T)
  mb_output <- hot_to_r(isolate(input$mb_output)) %>%
                           filter(export == T)
  cmt <- hot_to_r(isolate(input$mb_state))
  omega <- hot_to_r(isolate(input$mb_matrix))
  parameter <- hot_to_r(isolate(input$mb_paramater))

  mlxtran <- mlxtran_creation(df_path = explo_path,
                              event = event,
                              mb_output = mb_output,
                              exportpath= namemodel,
                              cmt = cmt,
                              model_path = filepath,
                              omega = omega,
                              parameter = parameter,
                              delimiter = delimiter,
                              header = headertemp)
  # mlxtran <<- mlxtran
  # print("hooo")
  updateTextAreaInput(session, inputId = "nmlxtran_output", value = mlxtran)

})


# SAEMIX ------------------------------------------------------------------

observeEvent(input$saemix_go,{


  showNotification("Model initialisation", type = "warning", duration = 1)



  datar <- theo.saemix %>%
    as_tibble

  model <- "ypred <- Dose * ka/( V * (ka-k)) * (exp(-k * t)-exp(-ka * t))"


  exploX <- "Time"
  names(datar)[names(datar) == exploX] <- "t"
  exploX <- "t"
  regressors <- c("t","Dose")
  exploY <- "Concentration"
  exploID <- "Id"
  covname <- "Sex"
  outputmodel <- "ypred"

  predictors <- c(regressors, exploX)
  parametre <- deSolve_pecc(model)$parameter
  parametre <- parametre[! parametre %in% predictors]


  matrix_saemix <- c(ka = 0.1, V = 5, k = 0.3)
  saemix.data<-saemixData(name.data=datar,
                          name.group= exploID,name.predictors=predictors,
                          name.response=exploY)

  model_expr <- expr(model_saemix <- function(psi, id, xidep){

    !!!parse_exprs(paste0(predictors, "<- xidep[ ,",1:length(predictors), "]"))
    !!!parse_exprs(paste0(parametre, "<- psi[id,",1:length(parametre), "]"))
    !!!parse_exprs(str_split(model,"\n")[[1]])
    !!expr(return(!!parse_expr(outputmodel)))
  })
  eval(parse_expr(deparse(model_expr) %>% paste(collapse = "\n")))
  distribution_par <- rep(1, length(parametre))
  fixed_par <- rep(1, length(parametre))


  saemixmodelexpr <- expr(saemixmodel <- saemixModel(model=model_saemix,
                                  description="One-compartment model with first-order absorption",
                                  psi0=!!matrix_saemix,transform.par=!!distribution_par,
                                  fixed.estim=!!fixed_par,
                                  covariance.model=!!diag(rep(1, length(parametre))),
                                  omega.init=!!diag(rep(1, length(parametre))),error.model="constant")
  )

eval(saemixmodelexpr)

  library(future)

  plan(multiprocess)
  showNotification("Model launch in parallel. See run to follow it", type = "Message", duration = 5)

  saemix.fit <- future({
    map(1, function(x){

      saemix(saemixmodel,saemix.data,list(seed=632545,directory=paste0("D:/Peccary/Exemple_demo/saemix_test/",letters[.x]),
                                          save=TRUE,save.graphs=TRUE))

    return(3)
    })
    })
  # showNotification("You can go !!!", type = "warning", duration = 10)
  # value(saemix.fit)
  # test
  # test <- 3
  # test
  # file.exists()

  # showNotification("End", type = "warning", duration = 3)
  # eta(saemix.fit)
  #
  # datar %>%
  #   mutate(IPRED = fitted(saemix.fit)) %>%
  #   mutate(RES = resid.SaemixObject(saemix.fit))
  #
  # resid.Saemix(saemix.fit)
  # testnpde()
  # iwres(saemix.fit)
  # individual.fits(saemix.fit)
  # print(saemix.fit)
  # psi(saemix.fit)
  # plot(saemix.fit)
  #


})
# save model --------------------------------------------------------------



observeEvent(input$save_model,{

  elements <- list()

  # take the model
  elements[["model"]] <- isolate(input$mb_model)


  elements[["compartmet"]] <- hot_to_r(isolate(input$mb_state))

  # print(elements[["compartmet"]])
  # take parameter value

  elements[["parameter"]] <- hot_to_r(isolate(input$mb_paramater))
  # print(elements[["parameter"]])
  #
  # take events

  elements[["event"]] <- hot_to_r(isolate(input$mb_event))
  # tale to display

  # print(elements[["event"]])
  # print("test todisplay")
  # print(hot_to_r(isolate(input$mb_display2)))

  elements[["todisplay"]] <- hot_to_r(isolate(input$mb_display2))

  # %>%
    # mutate(Filter_of_dataset = gsub("\\\"", "\\\\\"",Filter_of_dataset))

  # print(elements[["todisplay"]])
  elements[["plotstat"]] <- hot_to_r(isolate(input$mb_plot_stat))
  # print(elements[["plotstat"]])

  elements[["matrix_eta"]] <- hot_to_r(isolate(input$mb_matrix))

  elements[["mb_output"]] <- hot_to_r(isolate(input$mb_output))

  # tale to displau
  # print(elements[["matrix_eta"]])
  # take checks
  # print(elements %>% as.data.frame())
  # print("test iciiiii")
  elements[["otherinput"]] <-


    tibble(
    name = isolate(input$name_model),
    filterpoint = isolate(input$filterrExplo),
   pathpoint = isolate(input$pathExplo),
   xpoint = isolate(input$exploX),
   ypoint = isolate(input$exploY),
   preloaded = isolate(input$preloadeddataset),
   nastringExplo= isolate(input$nastringExplo),
   decExplo= isolate(input$decExplo),
   sepExplo= isolate(input$sepExplo),
   from = isolate(input$mb_time_from),
   to = isolate(input$mb_time_to),
  by = isolate(input$mb_time_by),
  matrix_sd_var = isolate(input$matrix_sd_var)

  )

  # print(names(output))
  print('Ehding to save')
  project$models[[isolate(input$name_model)]] <- elements

  saveRDS(project, project$path)


  updateSelectInput(session, inputId = "names_model", choices = names(project$models), selected = isolate(input$name_model))

#
#   try({
#     models <-  try(read.table(file.path(project_file, "0_pecc_project","models.txt" ), stringsAsFactors = F) %>% pull(name))
#
#
#     if(class(models) !="try-error"){
#       updateSelectInput(session, inputId = "reportModelEq", choices = c(models[order(models)]))
#       updateSelectInput(session, inputId = "reportModelSimul", choices = c(models[order(models)]))
#     }
#   })

})

