

# Table NCA ---------------------------------------------------



output$tableNCA <- DT::renderDataTable(options = list(scrollX = TRUE),{


  ### get the explo with filter if asked
  # print(explo)
  if(input$filterrExplo == "" | is.na(input$filterrExplo)){

    explotemp <- explo
  }else{

    explotemp <-  expr(explo %>%
      filter_(input$filterrExplo))
  }



  if(nrow(eval(explotemp)) > 3 ){


    if(is.na(input$covNCA) | input$covNCA == ""){

    groups <- exprs(!!parse_expr(input$groupNCA))
    forstat <- ""
    }else{

      groups <- exprs(!!parse_expr(input$covNCA), !!parse_expr(input$groupNCA))
      forstat <- input$covNCA
    }



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
    NCA_test <<- eval(NCA_comput) %>%
      select(-data)

# print(NCA_test)

# update the text
updateTextAreaInput(session,"nca_code_peccdep", value = paste0("# ", deparse(explo_expr, width.cutoff = 500), "\n\n",codpecc))
updateTextAreaInput(session,"nca_code_peccindep", value = paste0("# ", deparse(explo_expr, width.cutoff = 500), "\n\n",codpeccindep ))


if(forstat != ""){
if(length(unique(NCA_test[[forstat]])) >7) forstat <- ""
}
try({

 tocount <-  c("Cmax_cont", "Cmin_cont", "AUCTlast_cont","AUCTlastlog_cont", "Tmax_cont", "Tfirst_cont", "Tlast_cont")
 if(input$auc_0_x > 0 ) tocount <- c(tocount, paste0("AUC", input$auc_0_x, "_cont" ), paste0("AUC", input$auc_0_x , "log_cont"))


stat <- NCA_test %>%
  pecc_table1(rowl = tocount, coll = forstat)

output$tableNCA2 <- DT::renderDataTable(options = list(scrollX = TRUE),{stat})
})


    return(NCA_test %>% select(-AUCblocs))
  }

})



# Plot NCA ----------------------------------------------------------------



output$plotNCAcov <- renderPlot({


  input$filterrExplo # just for update plot when doing filtering modification
  input$groupNCA
  input$covNCA
  input$blqNCA
  input$nca_admin


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
  # NCA_test[[deparse(group)]] <- as.character(    NCA_test[[deparse(group)]])

  ## test cont or cat

  ntest <- length(unique(NCA_test[[inputcovNCA]]))
  typetest <- typeof(NCA_test[[inputcovNCA]])
  # print(ntest)
  # print(typetest)
  if(typetest == "integer" & is.factor(NCA_test[[inputcovNCA]])) typetest <- "factor"

  if(typetest %in% c("integer", "double") & input$nca_forcecat == F){

      typeplot <- "corr"
  }else{

    typeplot <- "box"

    }

}

  tocount <- exprs(Cmax, AUCTlast, Tmax, Tlast)
  if(input$auc_0_x > 0 )tocount[[5]] <-  parse_expr(paste0("AUC", input$auc_0_x ))

  if("C0" %in% names(NCA_test)) tocount[[6]] <- expr(C0)

if(typeplot == "box"){
  return(

    NCA_test %>%
      mutate(no_grp = "All IDs") %>%
      plot_boxplot(x = !!group, !!!tocount, statTest = input$nca_teststat, addPoints = input$nca_bxpltpoint, methodCompar = input$nca_method_box, ylog = input$nca_ylog)

  )
}else{

  return(


    NCA_test %>%
      # mutate(dose= rnorm(12)) %>%
      plot_correlation(x = !!group, !!!tocount, cor.coef = input$nca_teststat,
                       caption = T, conf.int = input$nca_corCI, add = if_else(input$nca_corregline == T, "reg.line", "none"),cor.method = input$nca_method_corr, ylog = input$nca_ylog, xlog = input$nca_xlog )

  )


}


})
#

# save nca ----------------------------------------------------------------

observeEvent(input$nca_save,{
  # print("ncasave")
  path_temp <- file.path(project_file,"0_pecc_project/NCA.txt")

  previous <- try(read.table(path_temp, header = T))

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


  write.table(outputt, file = path_temp, row.names = F)
  #
  #
  # print("okay")
  output$NCASaved<- renderRHandsontable(rhandsontable(outputt %>%
                                                        filter(preloadeddataset == isolate(input$preloadeddataset)) %>%
                                                        select(Name, Filter) %>%
                                                        mutate(Load = F) %>%
                                                        mutate(pdf = F) %>%
                                                        select(Load, pdf, everything()),rowHeaders = NULL))


})




observeEvent(input$NCASaved,{
  # print("NCAsaved")
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


  if(input$filterrExplo == "" | is.na(input$filterrExplo)){
       exrexplo <-  expr(explotemp <- explo)
      }else{

        exrexplo <- expr( explotemp <-explo %>%
          filter(!!parse_expr(input$filterrExplo)))
      }
# print(exrexplo)
eval(exrexplo)
 # checkboxGroupInput(inputId = "pknca_output",
 #                    label = "output",
 #                    choices = c("Indiv.", "Summary", "plot"),
 #                    selected = c("Indiv.", "Summary", "plot")
 #                    ,inline = T))


# explo <- read.table( "D:/these/Pecc_test/1_Data/3_final/ALEMTUZUMAB_20_04_07.csv", header = T, na.strings = ".",sep = ";",dec = ".") %>%
#   as_tibble
#
#
#   conc <- "DV"
#   Time = " TIME"
#   Subject = "ID"
#   dose <- "AMT"
#   covgroup <- "covBlast"
#   ADM <- "EVID"



  rateduration <- isolate(input$pknca_rateduration)

  conc <- isolate(input$exploY)
  Time <- isolate(input$exploX)
  Subject <- isolate(input$pknca_id)
  dose <- isolate(input$pknca_dose)
  covgroup <- isolate(input$pknca_cov)
  ADM <- isolate(input$pknca_ADM)
  route <- isolate(input$pknca_route)
  if(route == "IV perf (rate)"){
    rateduration <- "RATE"
    ratedurationwhich <- "rate"
    route <- "intravascular"
  }else if(route == "IV perf (time perf)"){
    rateduration <- "RATE"
    ratedurationwhich <- "duration"
    route <- "intravascular"
  }


  if(is.na(dose)|dose == "") dose <- "." # to create "one-sided (missing left side)"

  print("beginoption")
  optionslist <- list()

optionslist$auc.method <- expr(!!isolate(input$pknca_auc.method))
if(isolate(input$pknca_adj.r.squared.factor) != 0.0001) optionslist$adj.r.squared.factor <- expr(!!isolate(input$pknca_adj.r.squared.factor))
if(isolate(input$pknca_max.missing)!= "drop") optionslist$max.missing <- expr(!!isolate(input$pknca_max.missing))
if(isolate(input$pknca_conc.na)!= "drop") optionslist$conc.na <- expr(!!isolate(input$pknca_conc.na))
if(isolate(input$pknca_first.tmax)!= T) optionslist$first.tmax <- expr(!!isolate(input$pknca_first.tmax))
if(isolate(input$pknca_allow.tmax.in.half.life)!= F) optionslist$allow.tmax.in.half.life <- expr(!!isolate(input$pknca_allow.tmax.in.half.life))
if(isolate(input$pknca_min.hl.points)!= 3) optionslist$min.hl.points <- expr(!!isolate(input$pknca_min.hl.points))
if(isolate(input$pknca_min.span.ratio)!= 2) optionslist$min.span.ratio <- expr(!!isolate(input$pknca_min.span.ratio))
if(isolate(input$pknca_max.aucinf.pext)!= 20) optionslist$max.aucinf.pext <- expr(!!isolate(input$pknca_max.aucinf.pext))
if(isolate(input$pknca_min.hl.r.squared)!= 0.9) optionslist$min.hl.r.squared <- expr(!!isolate(input$pknca_min.hl.r.squared))

optionslist$single.dose.aucs <- expr(PKNCA.options()$single.dose.aucs  %>% mutate(end = Inf))
  optionslist<<-optionslist



print("begin conclist")
conclist <- list()
conclist$data <- expr(explotemp %>%
                        filter(!!parse_expr(ADM) == 0) %>%
                        group_by(!!parse_expr(Subject), !!parse_expr(Time)) %>%
                        summarise(!!parse_expr(conc) := median(!!parse_expr(conc))))
conclist$formula <- expr(!!parse_expr(conc)~!!parse_expr(Time)|!!parse_expr(Subject))

print("begin doselist")
doselist <- list()
doselist$data <- expr(explotemp %>%
                        filter(!!parse_expr(ADM) == 1))
doselist$formula <- expr( !!parse_expr(dose)~!!parse_expr(Time)|!!parse_expr(Subject))
doselist$route <- expr(!!route)
if(route == "intravascular") doselist[[ratedurationwhich]] <- expr(!!rateduration)


NCA_expr <- expr(NCA_eval <- {
  conc_obj <- PKNCAconc(!!!conclist)

  dose_obj <- PKNCAdose(!!!doselist)


  data_obj_automatic <-PKNCAdata(conc_obj, dose_obj,  options = list(!!!optionslist ))

  pk.nca(data_obj_automatic)

})

testtry <- try({
   eval(NCA_expr)
})

if(class(testtry) == "try-error"){



  showNotification("NCA not computed, be sure you have EVID column with at least one row equal to 0 and 1", type = "error", duration = 3, closeButton = T)

  updateTextAreaInput(session = session, inputId = "pknca_code",value =
                        deparse(NCA_expr,width.cutoff = 500) %>%
                        paste0(collapse = "\n") %>%
                        gsub(pattern = "\\\n *", replacement = "\n"))



}else{
print("ici")
   Indiv_table_expr <- expr( NCA_eval$result %>%
    select(!!parse_expr(Subject),PPTESTCD,  PPORRES) %>%
      distinct() %>% ## carefull with this distinct!
    spread(key = PPTESTCD, value = PPORRES))
   print("la")


if(covgroup != ""){

  Indiv_table_expr <- expr(Indiv_table <- explotemp %>%
         select(!!parse_expr(Subject),!!parse_expr(covgroup)) %>%
         distinct() %>%
         full_join(!!Indiv_table_expr))

}else{

  Indiv_table_expr <- expr(Indiv_table <- !!Indiv_table_expr)
}

   Indiv_table_expr <<- Indiv_table_expr
print("eval Indiv_table_explr")
eval(Indiv_table_expr)

# print("here")

if( "Indiv." %in% isolate(input$pknca_output_sel)){
  # print("no hide")
  shinyjs::show(id =  "pknca_output")
  output$pknca_output <- renderRHandsontable(rhandsontable(Indiv_table,rowHeaders = F, readOnly = T, search = T))
  # print("no hide done")
}else{
  print("hide")
  shinyjs::hide(id =  "pknca_output")
}

print("aaa")
  table1row <<- names(Indiv_table)[-1]
  table1row <- table1row[ ! c(table1row %in% covgroup)]
  print("bbb")
#
#   # table1row <- table1row[- (table1row %in% c("adj.r.squared", ))]
#
#   # print(covgroup)
#   # explo <<- resfinal
#   # rowl <<-   paste0(table1row, "_cont")
#   # coll <<- "Dose"
#   # print(resfinal)
  # Indiv_table <<- Indiv_table
  # covgroup <<- covgroup
  # rowl <<- paste0(table1row, "_cont")
  #
  # pecc_table1(dataset =  Indiv_table,rowl =  rowl, coll = covgroup)
  # print(paste0(table1row, "_cont"))
  # print(covgroup)
   table1 <- pecc_table1(dataset = Indiv_table,rowl =   paste0(table1row, "_cont"), coll = covgroup)
#
  output$pknca_output2 <- renderRHandsontable(rhandsontable(table1,rowHeaders = F, readOnly = T, search = T))
  print("ccc")
#   ### plot
# #
# #
#
  NCA_boxplot_expr <<-   expr(  NCA_boxplot  <-
           explo %>%
           select(!!parse_expr(Subject),!!parse_expr(covgroup)) %>%
           distinct() %>%
           full_join(NCA_eval$result) %>%
           filter(PPTESTCD %in% c("cmax", "auclast", "tlast", "half.life")) %>%
           ggplot()+
           geom_boxplot(aes(factor(!!parse_expr(covgroup)),PPORRES, fill = factor(!!parse_expr(covgroup))))+
           geom_point(aes(factor(!!parse_expr(covgroup)),PPORRES))+
           facet_wrap(~PPTESTCD, scales = "free"))

 eval(NCA_boxplot_expr)

  # plot_grid(
  #   plot_spagh(df = explotemp,filter = input$filterrExplo, facetscale = input$scaleExplo, loq = as.double(input$exploLOQ), x = input$exploX,median = input$exploMedian,  y = input$exploY, col = input$covNCA, facetwrap = input$exploWrap, facetgrid = input$exploGrid, group = input$exploID, point = input$exploPoint, standardisation = input$exploStand, ylog = input$exploYlog, xlog = input$exploXlog, line_alpha  = input$exploLine )#,
  #   # NCA_boxplot
  #   ) ->> finalplot
  # print("eee")
  # output$pknca_plot <- renderPlot({finalplot})

#  ## code
str0 <- deparse(exrexplo)

    deparse(NCA_expr,width.cutoff = 500) %>%
     paste0(collapse = "\n") %>%
         gsub(pattern = "\\\n *", replacement = "\n")  -> str1

    deparse(Indiv_table_expr,width.cutoff = 500) %>%
      gsub(pattern = "%>%", replacement = "%>%\n") -> str2
#
#
  str3 <-   deparse(expr(summary <- pecc_table1(explo = Indiv_table,rowl =   !!paste0(table1row, "_cont"), coll = !!covgroup))) %>%
      paste0(collapse = "")

  str4 <- deparse(NCA_boxplot_expr, width.cutoff = 500) %>%
    paste0(collapse = "")%>%
    gsub(pattern = "%>%", replacement = "%>%\n")
#
#
#
  paste(str0, str1, str2,str3,str4,   sep = "\n\n") -> finalcode
#

  updateTextAreaInput(session = session, inputId = "pknca_code",value = finalcode)
}

  })

