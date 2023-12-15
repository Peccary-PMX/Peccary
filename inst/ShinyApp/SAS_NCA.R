

# Table NCA ---------------------------------------------------



output$tableNCA <- DT::renderDataTable(options = list(scrollX = TRUE),{


  Indiv_table <<- tibble()
  ### get the explo with filter if asked
  # print(explo)
  if(input$filterrExplo == "" | is.na(input$filterrExplo)){

    exrexplo <-  explotemp <- expr(explo)

  }else{

    explotemp <-  expr(explo %>%
      filter_(input$filterrExplo))

    exrexplo <- eval(explotemp)
  }


groupNCA <- c("ID", "cov")
covNCA <- ""
groupNCA <- input$groupNCA
covNCA <- input$covNCA

  if(nrow(eval(explotemp)) > 3 ){


    # now If there is the need to keep covariate or not
    if(is.na(input$covNCA) | input$covNCA == ""){

    groups <- parse_exprs(groupNCA)
    forstat <- ""
    }else{

      groups <- c(parse_expr(covNCA), parse_exprs(groupNCA))
      forstat <- input$covNCA
    }


codepeccind <- list()


if(input$pckgNCA == "Peccary"){

# If Peccary --------------------------------------------------------------



if(input$nca_admin == "IVbolus -> backextrapolation"){

bcketra <- T
obs0time0 <- F
}else if(input$nca_admin == "Other -> add observation = 0 at time = 0"){

  bcketra <- F
  obs0time0 <- T
}else{

  bcketra <- F
  obs0time0 <- F
}

#Blqcol

if(input$blqNCA == ""){

blqcol <- NULL
}else{

  blqcol <- input$blqNCA
}


if(gsub(" ", "",input$filterrExplo) == ""){
      filtre <- ""
  }else{
      filtre <-  parse_expr(input$filterrExplo)
}

# print("Step 1: Peccary dependent code")
  # 1) Reconstitute the peccary expression
    NCA_expr_pecc <- expr(peccary_NCA(outputExpr = T, dataset  = explo, timecol  = !!parse_expr(input$exploX), obscol  = !!parse_expr(input$exploY) , !!!groups, IVbackextr = !!bcketra, add_Obs0_Time0 = !!obs0time0,   auc_0_X =  !!input$auc_0_x, BLQcol = !!blqcol, filter = !!filtre ) )

    deparse(NCA_expr_pecc, width.cutoff = 500) %>% paste0(collapse = "\n") %>%
      gsub(pattern = "%>% *\n* *",replacement = "%>% \n")  %>%
      gsub(pattern = "outputExpr *= *T, *", replacement =  "")-> codpecc

    # print(NCA_expr_pecc)

  # 2) Compute the peccary independt code with previous code

# print("Step 2: Peccary independent code")
  NCA_comput <- eval(NCA_expr_pecc)

  deparse(NCA_comput, width.cutoff = 500) %>% paste0(collapse = "\n") %>%
    gsub(pattern = "%>% *\n* *",replacement = "%>% \n") -> codpeccindep

  # print(NCA_comput)

  # 3) Final evaluation and NCA computation
  # print("Step 3: Final NCA !")
    Indiv_table <<- eval(NCA_comput) %>%
      select(-data)

# print(Indiv_table)

# update the text
updateTextAreaInput(session,"nca_code_peccdep", value = paste0("# ", deparse(explo_expr, width.cutoff = 500), "\n\n", paste0("Indiv_table <- ", codpecc, "; Indiv_table")))
updateTextAreaInput(session,"nca_code_peccindep", value = paste0("# ", deparse(explo_expr, width.cutoff = 500), "\n\n",paste0("Indiv_table <- ", codpeccindep, "; Indiv_table")  ))

codepeccind <- c(deparse(explo_expr, width.cutoff = 500),paste0("Indiv_table <- ", codpeccindep, "; Indiv_table") )


if(forstat != "") if(length(unique(Indiv_table[[forstat]])) >7) forstat <- ""





}else{ # End Peccary NCA version, beginnig PKNCA


# If PKNCA -------------------------------------------------------------------


  rateduration <- isolate(input$pknca_rateduration)

  conc <- input$exploY
  Time <- input$exploX
  Subject <- groupNCA
  dose <- input$pknca_dose
  covgroup <- input$covNCA
  ADM <- input$pknca_ADM
  route <- input$pknca_route

  ratedig <- 0
  durationdig <- 0
  if(route == "IV perf (rate)"){

    route <- "intravascular"
    ratedig <- input$pknca_rateduration
  }else if(route == "IV perf (time perf)"){

    route <- "intravascular"

    durationdig <-  input$pknca_rateduration
  }else if(route == "IV bolus"){

    route <- "intravascular"

  }


  # if(is.na(dose)|dose == "") dose <- "." # to create "one-sided (missing left side)"

  print("beginoption")
  optionslist <- list()

  if(isolate(input$pknca_auc.method) != "lin up/log down")   optionslist$auc.method <- expr(!!isolate(input$pknca_auc.method))
  if(isolate(input$pknca_adj.r.squared.factor) != 0.0001) optionslist$adj.r.squared.factor <- expr(!!isolate(input$pknca_adj.r.squared.factor))
  if(isolate(input$pknca_max.missing)!= "drop") optionslist$max.missing <- expr(!!isolate(input$pknca_max.missing))
  if(isolate(input$pknca_conc.na)!= "drop") optionslist$conc.na <- expr(!!isolate(input$pknca_conc.na))
  if(isolate(input$pknca_first.tmax)!= T) optionslist$first.tmax <- expr(!!isolate(input$pknca_first.tmax))
  if(isolate(input$pknca_allow.tmax.in.half.life)!= F) optionslist$allow.tmax.in.half.life <- expr(!!isolate(input$pknca_allow.tmax.in.half.life))
  if(isolate(input$pknca_min.hl.points)!= 3) optionslist$min.hl.points <- expr(!!isolate(input$pknca_min.hl.points))
  if(isolate(input$pknca_min.span.ratio)!= 2) optionslist$min.span.ratio <- expr(!!isolate(input$pknca_min.span.ratio))
  if(isolate(input$pknca_max.aucinf.pext)!= 20) optionslist$max.aucinf.pext <- expr(!!isolate(input$pknca_max.aucinf.pext))
  if(isolate(input$pknca_min.hl.r.squared)!= 0.9) optionslist$min.hl.r.squared <- expr(!!isolate(input$pknca_min.hl.r.squared))

  optionslist<<-optionslist
  if(length(optionslist) > 0){
    optionslist <-  expr(list(!!!optionslist))

  }else{
    optionslist <- NA

  }

  print(optionslist)
  groupsplus <- paste0(map_chr(groups, ~deparse(.x)), collapse = " + ") %>% parse_expr()

 bothcode <-  expr(peccary_pknca(dataset = !!explotemp, Time = !!parse_expr(Time),conc = !!parse_expr(conc),
                     Subject = !!groupsplus, dose = !!parse_expr(dose),EVID = !!parse_expr(ADM),
                     AUC0_x =!!isolate(input$auc_0_x),route = !!route,rate = !!ratedig, duration = !!durationdig,  computeMedian = T, option = !!optionslist,   outputExpr = T)) %>%
   eval

 NCA_expr <- bothcode[[1]]
 testtry <- try(eval(NCA_expr))

  updateTextAreaInput(session = session, inputId = "nca_code_peccindep",value =
                        deparse(NCA_expr,width.cutoff = 500) %>%
                        paste0(collapse = "\n") %>%
                        gsub(pattern = "\\\n *", replacement = "\n"))

  if(class(testtry)[[1]] == "try-error"){



    showNotification("NCA not computed, be sure you have EVID column with at least one row equal to 0 and 1", type = "error", duration = 3, closeButton = T)

    updateTextAreaInput(session = session, inputId = "pknca_code",value =
                          deparse(NCA_expr,width.cutoff = 500) %>%
                          paste0(collapse = "\n") %>%
                          gsub(pattern = "\\\n *", replacement = "\n"))



  }else{
    Indiv_table_expr <-   bothcode[[2]]

    if(covgroup != ""){

      Indiv_table_expr <- expr(Indiv_table <- !!explotemp %>%
                                 select(!!!parse_exprs(Subject),!!parse_expr(covgroup)) %>%
                                 distinct() %>%
                                 full_join(!!Indiv_table_expr))

    }else{

      Indiv_table_expr <- expr(Indiv_table <- !!Indiv_table_expr)
    }

# NCA_eval <<- NCA_eval
    # print("eval Indiv_table_explr")
    eval(Indiv_table_expr)
    Indiv_table <<- Indiv_table
    #

 codepeccind <- c(paste0(deparse(NCA_expr)  %>% paste0(collapse = "\n")),deparse(Indiv_table_expr) %>% paste0(collapse = "\n"))

    updateTextAreaInput(session = session, inputId = "nca_code_peccindep",value = paste0(deparse(NCA_expr)  %>% paste0(collapse = "\n"), "\n", deparse(Indiv_table_expr) %>% paste0(collapse = "\n")))
  }



}


# Do table ----------------------------------------------------------------



  a <-  try({

    if(input$pckgNCA == "Peccary"){
      tocount <-  c("Cmax_cont", "Cmin_cont", "AUCTlast_cont","AUCTlastlog_cont", "Tmax_cont", "Tfirst_cont", "Tlast_cont")
    }else{
      tocount <-  c("auclast_cont", "aucinf.obs_cont", "cmax_cont","tmax_cont")

    }
    tocount <- gsub("_.+", "", tocount) # from inmade table1 to real table1 package

        if(input$auc_0_x > 0 & input$pckgNCA == "Peccary") tocount <- c(tocount, paste0("AUC", input$auc_0_x ), paste0("AUC", input$auc_0_x , "log_cont"))
    if(input$auc_0_x > 0 & input$pckgNCA != "Peccary") tocount <- c(tocount, paste0("AUC", input$auc_0_x ))

if(forstat != "") forstat <- paste0("|", forstat)

if(forstat != "") if(length(unique(Indiv_table[[forstat]])) >5) forstat <- ""

        exprtable1 <- expr(table1::table1(~ !!parse_expr(paste0(paste0(tocount, collapse = "+"), forstat)), data=Indiv_table))


      codepeccind <- c(codepeccind, paste0("### Stat table\n", deparse(exprtable1) %>% paste0(collapse = "")))

      output$tableNCA2 <- renderTable(options = list(scrollX = TRUE),{eval(exprtable1)})

      # table1(~ conc + uptake | Type, data=CO2)
    })

codepeccind <<- codepeccind

updateTextAreaInput(session = session, inputId = "nca_code_peccindep",value = paste0(codepeccind, collapse = "\n\n"))


  if(class(a)[[1]] == "try-error")   output$tableNCA2 <- DT::renderDataTable(options = list(scrollX = TRUE),{tibble()})


    return(Indiv_table %>% select(-contains("AUCblocs")))


  } # end if three nrow





})



# Plot NCA ----------------------------------------------------------------



output$plotNCAcov <- renderPlot({

  input$filterrExplo # just for update plot when doing filtering modification
  input$groupNCA
  input$covNCA
  input$blqNCA
  input$nca_admin
  input$pckgNCA
  input$exploY
  input$exploX
  input$pknca_dose
  input$covNCA
  input$pknca_ADM
  input$pknca_route

  xlabs <- if_else(input$nca_labelx != "" | is.na(input$nca_labelx), input$nca_labelx,input$exploX)
  ylabs <- if_else(input$nca_labely != "" | is.na(input$nca_labely), input$nca_labely,input$exploY)

  xlabs <- paste0(xlabs," (",input$nca_unitx,")") %>% gsub(pattern = "\\(\\)",replacement  = "")
  ylabs <- paste0(ylabs," (",input$nca_unity,")") %>% gsub(pattern = "\\(\\)",replacement  = "")


  inputcovNCA <- input$covNCA
  # inputcovNCA <- "doseCAT"
if(inputcovNCA == ""){

  group <- expr(no_grp)
  typeplot <- "box"

}else{

  group <- expr(!!parse_expr(inputcovNCA))
  # Indiv_table[[deparse(group)]] <- as.character(    Indiv_table[[deparse(group)]])

  ## test cont or cat

  ntest <- length(unique(Indiv_table[[inputcovNCA]]))
  typetest <- typeof(Indiv_table[[inputcovNCA]])
  if(typetest == "integer" & is.factor(Indiv_table[[inputcovNCA]])) typetest <- "factor"

  if(typetest %in% c("integer", "double") & input$nca_forcecat == F){

      typeplot <- "corr"
  }else{

    typeplot <- "box"

    }

}

 if(input$pckgNCA == "Peccary"){
 tocount <- exprs(Cmax, AUCTlast, Tmax, Tlast)
 }else{

   tocount <- exprs( auclast, aucinf.obs , cmax, tmax)
 }


  if(input$auc_0_x > 0 )tocount[[5]] <-  parse_expr(paste0("AUC", input$auc_0_x ))

   if("C0" %in% names(Indiv_table)) tocount[[6]] <- expr(C0)




  if(typeplot == "box"){


    exprbox <-
      plot_boxplot(df = Indiv_table %>% mutate(no_grp = "All IDs"), x = !!group, !!!tocount, statTest = input$nca_teststat, addPoints = input$nca_bxpltpoint, methodCompar = input$nca_method_box, ylog = input$nca_ylog, outputExpr = T)

    updateTextAreaInput(session = session, inputId = "nca_code_peccindep",value = paste0(c(codepeccind, paste0("### Boxplot\n\n", deparse(exprbox) %>% paste0(collapse = "\n") %>%
                                                                                                                         gsub(pattern = "%>% *\n*",replacement = " %>% \n") %>%
                                                                                                                         gsub(pattern = "\\+ *\n*",replacement = " + \n"))), collapse = "\n\n"))

  return(

  eval(exprbox)
  )







}else{

correxpr <-
    # mutate(dose= rnorm(12)) %>%
    plot_correlation(df =  Indiv_table %>% mutate(no_grp = "All IDs"),  outputExpr = T,  x = !!group, !!!tocount, cor.coef = input$nca_teststat,
                     caption = T, conf.int = input$nca_corCI, add = if_else(input$nca_corregline == T, "reg.line", "none"),cor.method = input$nca_method_corr, ylog = input$nca_ylog, xlog = input$nca_xlog )


  updateTextAreaInput(session = session, inputId = "nca_code_peccindep",value = paste0(c(codepeccind,  paste0("### Plot\n", deparse(correxpr) %>% paste0(collapse = "\n")%>%
                                                                                                                       gsub(pattern = "%>% *\n*",replacement = " %>% \n") %>%
                                                                                                                       gsub(pattern = "\\+ *\n*",replacement = " + \n"))), collapse = "\n\n"))

  return(

eval(correxpr)


  )


}


})
#

# save nca ----------------------------------------------------------------

observeEvent(input$nca_save,{
  # print("ncasave")
  # path_temp <- file.path(project_file,"0_pecc_project/NCA.txt")

  previous <- project$NCA #try(read.table(path_temp, header = T))

  tibble(
    Name = isolate(input$nca_name),
    preloadeddataset = isolate(input$preloadeddataset),
    Filter = isolate(input$filterrExplo),
    exploX = isolate(input$exploX),
    exploY = isolate(input$exploY),
    covNCA = isolate(input$covNCA),
    Nsignif = isolate(input$Nsignif),
    nca_unitx = isolate(input$nca_unitx),
    nca_unity = isolate(input$nca_unity),
    nca_labelx = isolate(input$nca_labelx),
    nca_tabletitle = isolate(input$nca_tabletitle),
    nca_labely = isolate(input$nca_labely)




  ) -> new_line

  if(class(previous) == "try-error"){


    outputt <- new_line

  }else{



    previous %>%
      filter(!(Name == isolate(input$nca_name))) %>%
      bind_rows(new_line) -> outputt

  }

  project$NCA <- outputt

  saveRDS(project, project$NCA)

  project <<- project
  # write.table(outputt, file = path_temp, row.names = F)
  #
  #
  output$NCASaved<- renderRHandsontable(rhandsontable(outputt %>%
                                                        filter(preloadeddataset == isolate(input$preloadeddataset)) %>%
                                                        select(Name, Filter) %>%
                                                        mutate(Load = F) %>%
                                                        mutate(pdf = F) %>%
                                                        select(Load, pdf, everything()),rowHeaders = NULL))


})




observeEvent(input$NCASaved,{
  hot_to_r(isolate(input$NCASaved)) %>%
    filter(Load == T) -> temp

  if(nrow(temp) > 0){

    path_temp <- file.path(project_file,"0_pecc_project/NCA.txt")

    temp <- read.table(path_temp, header = T, stringsAsFactors = F) %>%
      filter(Name == temp$Name, Filter == temp$Filter | is.na(Filter))

    updateTextInput(session, inputId = "nca_name",value =  temp$Name)
    updateTextInput(session, inputId = "filterrExplo", value = temp$Filter)
    updateSelectInput(session, inputId = "exploX", selected = temp$exploX)
    updateSelectInput(session, inputId = "exploY", selected = temp$exploY)
    updateSelectInput(session, inputId = "covNCA", selected = temp$covNCA)
    updateNumericInput(session, inputId = "Nsignif", value = temp$Nsignif)
    updateTextInput(session, inputId = "nca_unitx",value =  temp$nca_unitx)
    updateTextInput(session, inputId = "nca_unity",value =  temp$nca_unity)
    updateTextInput(session, inputId = "nca_labelx",value =  temp$nca_labelx)
    updateTextInput(session, inputId = "nca_tabletitle",value =  temp$nca_tabletitle)
    updateTextInput(session, inputId = "nca_labely",value =  temp$nca_labely)


    output$NCASaved <- renderRHandsontable(rhandsontable(hot_to_r(isolate(input$NCASaved)) %>%
                                                           mutate(Load = F) %>%
                                                           mutate(pdf = F) %>%
                                                           select(Load, pdf, everything()),rowHeaders = NULL))

  }



})



# PKNCA package -----------------------------------------------------------


# show/hide pknca specific blocs


observeEvent(input$pckgNCA,{



    sft <- input$pckgNCA

    if(sft == "Peccary"){

      shinyjs::hide(id = "pknca_dose")
      shinyjs::hide(id = "pknca_ADM")
      shinyjs::hide(id = "pknca_route")
      shinyjs::hide(id = "pknca_rateduration")
      shinyjs::hide(id = "boxpkncaparam") #why is it not workin?

    }else{
      shinyjs::show(id = "pknca_dose")
      shinyjs::show(id = "pknca_ADM")
      shinyjs::show(id = "pknca_route")
      shinyjs::show(id = "pknca_rateduration")
      shinyjs::show(id = "boxpkncaparam")
    }


})

# show/hide pknca_route

observeEvent(input$pknca_route,{

  print("beginoption")
try({
route <- input$pknca_route

if(route == "IV perf (rate)"){
  shinyjs::show(id = "pknca_rateduration")
  updateSelectInput(session, "pknca_rateduration", label = "Rate col")

}else if(route == "IV perf (time perf)"){
  shinyjs::show(id = "pknca_rateduration")
  updateSelectInput(session, "pknca_rateduration", label = "Time perf col")

}else{

  shinyjs::hide(id = "pknca_rateduration")
}

})
})

print("beginoption")
observeEvent(input$pknca_go,{

  })

