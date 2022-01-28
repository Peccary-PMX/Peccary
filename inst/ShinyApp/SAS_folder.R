# table update
tableUpdate <- function(subPath){

  # print("tableUpdatehere")
  # print(exists("dossier"))


  if(exists("dossier")){
  dossier(-1)@summary -> temp

  # remove the name of the run !
  temp$files <- temp$files %>%
    map(function(x){

      str_split(x, pattern = "/")[[1]] -> splitt

      paste0(splitt[-length(splitt)], collapse =  "/")
    })

  subPath <- input$subPath
  # subPath <- "1_LINEAR"
  if(subPath != "Root"){

    temp %>%
      mutate(test = map( files, ~ grep(paste0("^/",subPath ), .x))) %>%
      filter(test == 1) %>%
      select(-test) %>%
      mutate(files = gsub(paste0("^/",subPath ), "", files))-> temp

  }

  # to refresh rank
  temp %>%
    arrange(FOs ) %>%
    rowid_to_column("temp") %>%
    mutate(rank = temp) %>%
    select(-temp) %>%
    arrange(number) -> temp

  return(temp)
}
}

# Pre-existing folder
# If we open peccary with already a dossier
if(exists ("dossier")){
  # print("here")
  output$namecurrendfolder <-  renderText(expr = paste0("", dossier(folder = T)@racine))
  dossier(folder = T)@summary$files %>%
    map_chr(function(x){

      temp <- str_split(x, "/")[[1]]
      temp <- temp[temp != ""]
      if(length(temp)>0) temp <- temp[-length(temp)]
      # print(temp)
      if(length(temp)>0) return(temp[1])
      return("")
    }) -> subFolderPath

  updateSelectInput(session, inputId = "subPath", choices = c("Root",subFolderPath))

  subPathPrevious <<- ""

}

#create folder -----------------------------------------------------------


observeEvent(input$folderupdate, {

  print("Creation folder")

  # We update the folder
  dossier <<- try(createFolder(isolate(input$path2)))
  # path <<- input$path2
  print("Creation model2")

  # Update de subpath System
  if(class(dossier) == "try-error") updateSelectInput(session, inputId = "subPath", choices = c("Root"), selected = "Root")


  if(class(dossier) != "try-error"){

    dossier(folder = T)@summary$files %>%
      map_chr(function(x){

        temp <- str_split(x, "/")[[1]]
        temp <- temp[temp != ""]
        if(length(temp)>0) temp <- temp[-length(temp)]
        # print(temp)
        if(length(temp)>0) return(temp[1])
        return("")
      }) -> subFolderPath

    updateSelectInput(session, inputId = "subPath", choices = c("Root",subFolderPath))

    subPathPrevious <<- ""
  }
  # And Render the table
  # print("outputtable??")
  # print(tableUpdate())
  output$table <- renderTable(tableUpdate())

})



#



# er ----------------------------------------------------------------------
observeEvent(input$subPath, {
  try({

    inputt <- input$subPath
    files <- dossier(folder = T)@summary$files
    # if not going back


    if(! inputt %in% c(".", "..",  "Root")){
      new <- paste0(subPathPrevious, "/", inputt)

      test <-  grep(new, files)

      # to avoid infinit loop
      if(length(test) > 0){
        subPathPrevious <<- new

        files[grep(new,files )] %>%
          gsub(pattern = new, replacement = "") %>%
          map_chr(function(x){

            temp <- str_split(x, "/")[[1]]
            temp <- temp[temp != ""]
            if(length(temp)>0) temp <- temp[-length(temp)]
            # print(temp)
            if(length(temp)>0) return(temp[1])
            return("")
          }) -> subFolderPath

        subFolderPath <- subFolderPath[subFolderPath != ""]
        subFolderPath <- unique(subFolderPath)
        updateSelectInput(session, inputId = "subPath", choices = c(gsub("^/", "", subPathPrevious), ".", "..", subFolderPath), selected = gsub("^/", "", subPathPrevious))

      }}


    if(inputt == "."){

      new <- str_split(subPathPrevious, "/")[[1]]
      new <- paste0(new[-length(new)], collapse = "/")

      files[grep(new,files )] %>%
        gsub(pattern = new, replacement = "") %>%
        map_chr(function(x){

          temp <- str_split(x, "/")[[1]]
          temp <- temp[temp != ""]
          if(length(temp)>0) temp <- temp[-length(temp)]
          # print(temp)
          if(length(temp)>0) return(temp[1])
          return("")
        }) -> subFolderPath

      subFolderPath <- subFolderPath[subFolderPath != ""]
      subFolderPath <- unique(subFolderPath)
      subPathPrevious <<- new

      choicess <- c(gsub("^/", "", subPathPrevious), c(".", "..") , subFolderPath)

      if(subPathPrevious == "")   choicess <- c("Root", subFolderPath)

      updateSelectInput(session, inputId = "subPath", choices = choicess, selected = choicess[[1]])


    }

    if(inputt == ".."){

      new <- ""
      new <- ""
      files[grep(new,files )] %>%
        gsub(pattern = new, replacement = "") %>%
        map_chr(function(x){

          temp <- str_split(x, "/")[[1]]
          temp <- temp[temp != ""]
          if(length(temp)>0) temp <- temp[-length(temp)]
          # print(temp)
          if(length(temp)>0) return(temp[1])
          return("")
        }) -> subFolderPath

      subFolderPath <- subFolderPath[subFolderPath != ""]
      subFolderPath <- unique(subFolderPath)
      subPathPrevious <<- new

      if(subPathPrevious == "")   choicess <- c("Root", subFolderPath)

      updateSelectInput(session, inputId = "subPath", choices = choicess, selected = choicess[[1]])


    }

    liner <-  "/"
    if(subPathPrevious == "") liner <-  ""
    output$namecurrendfolder <-  renderText(expr = paste0("", dossier(folder = T)@racine, liner, subPathPrevious))

  })

})





output$table <- renderTable(tableUpdate())




# output$table2 <- renderTable({
#   dossier(-1)@summary %>%
#     arrange(files)
# })
#
# output$table3 <- renderTable({
#   dossier(-1)@summary %>%
#     arrange(files)
# })
#
# output$table4 <- renderTable({
#   dossier(-1)@summary %>%
#     arrange(files)
# })
#
#
# output$table6 <- renderTable({
#   dossier(-1)@summary %>%
#     arrange(files)
# })

# output$table7 <- renderTable({
#   dossier(-1)@summary %>%
#     arrange(files)
# })
#



# Input number ------------------------------------------------------------

observeEvent(input$number, {

  print("inputNumber")
  print(exists("dossier"))

  ### Update Numbers
  updateTextInput(session, "numberPred2", value =  input$number)
  updateTextInput(session, "nBoxPlots", value =  input$number)
  updateTextInput(session, "nmatrix", value =  input$number)
  updateTextInput(session, "niLL", value =  input$number)
  updateTextInput(session, "nGOF", value =  input$number)
  # updateTextInput(session, "nVPC", value =  input$number)


  ### update cov
  a <- try(names(dossier(stringToNumbers(input$number)[1])@cov)[-1])

  if(class(a) != "try-error"){
    updateRadioButtons(session, "variable", choices = c("none", a))
    updateRadioButtons(session, "variable2", choices = c("none", a))
    updateSelectInput(session, "covPrimBP", choices = c("None" = "none", a))
    updateSelectInput(session, "covPrimGOF", choices = c("None" = "", a))
    updateSelectInput(session, "covSecBP", choices = c("None" = "none", a))
    updateSelectInput(session, "colourPred", choices = c("None" = "none", a))
    updateSelectInput(session, "colourPred2", choices = c("None" = "none", a))
    updateSelectInput(session, "covPrimBP", choices = c("none", a), selected = input$variable)
    updateSelectInput(session, "covPrimBP2", choices = c("none", a), selected = input$variable)
    updateSelectInput(session, "covPrimiLL", choices = c("None", a), selected = input$variable)


  }

  # print results
  output$runscompar <- renderTable({

    results(dossier(stringToNumbers(input$number)))

  })

})


# predictions -------------------------------------------------------------
observeEvent(input$whichFunctionPred, {

  selected <- "indiv_plot_christelle"
  selected <- input$whichFunctionPred

  if(selected != "Default"){

  # read the file
    pathIndivPred <-  file.path(find.package("peccary"), "Add_function", "indiv_plots")

 lines <- readLines(file.path(pathIndivPred, paste0(selected,".R")))
  # extract Shiny-App requirement
 needed <- lines[(grep("Shiny-App-Input-Start", lines)+1):(grep("Shiny-App-Input-End", lines)-1)]
 toremo <- grep("= *$", needed)
 if(length(toremo) > 0) needed <- needed[- toremo]
 needed <- gsub(pattern = "(# *)|( *=.*)", replacement = "", needed)


 ## all possibilies
  }else{ # the default
    needed <- c("interactivePreds", "nIndPred2", "logPred2", "logxPred2", "PredPred2", "freeSalePred2", "filterID2", "lowerlimitPred", "upperlimitPred", "extrVal", "savePRED", "commPRED")


  }
  allposs <- c("interactivePreds", "nIndPred2", "logPred2", "logxPred2", "PredPred2", "freeSalePred2", "filterID2", "lowerlimitPred", "upperlimitPred", "extrVal", "stratTypePred", "xlabPred", "ylabPred", "savePRED", "commPRED")

 ## show the needed
 map(needed[needed != "run"], shinyjs::show)
 map(allposs[!(allposs %in% needed)], shinyjs::hide)

})


output$covPRED <- renderTable({

  data.frame(unclass( run_cov_summary(dossier(stringToNumbers(input$number)[[1]]))), check.names = FALSE, stringsAsFactors = FALSE)  %>%
    map_dfr(
      function(x){

        x[is.na(x) == T] <- ""
        x

      })


})


output$PredIndi <- renderPlot({

  test <- input$updatePred # for udpate only if clicking on it


  if(isolate(input$whichFunctionPred) == "Default"){
    output <- plot_pred(dossier(stringToNumbers(isolate(input$numberPred2))), valuelimit = isolate(input$extrVal),upperlimit = isolate(input$upperlimitPred),lowerlimit = isolate(input$lowerlimitPred), n = isolate(input$nIndPred2), ylog = isolate(input$logPred2), xlog = isolate(input$logxPred2), freeScale = isolate(input$freeSalePred2), filter = isolate(input$filterID2), pred = isolate(input$PredPred2))
  }else{

    selected <- "indiv_plot_christelle"
    selected <- input$whichFunctionPred
    pathIndivPred <-  file.path(find.package("peccary"), "Add_function", "indiv_plots")

    pathtemp <- file.path(pathIndivPred, paste0(selected,".R"))
    # Load and read
    source(pathtemp)
    lines <- readLines(pathtemp)

    # extract argument
    tibble(a = lines[(grep("Shiny-App-Input-Start", lines)+1):(grep("Shiny-App-Input-End", lines)-1)]) %>%
      mutate(b = gsub(pattern = "(# *)|( *=.+)", replacement = "", a)) %>%
      mutate(c = gsub(".+= *", "", a)) %>%
      select(-a) %>%
      filter(c != "") %>%
      mutate(d = case_when(b != "run" ~ paste0(c , " = isolate(input$",b,")"),
                           T ~ paste0(c , " = dossier(stringToNumbers(isolate(input$numberPred2)))"))) %>%
      pull(d) -> arguments

    output <- paste0(selected,"(", paste0(arguments, collapse = ", "),")") %>%
      parse_expr() %>%
      eval()


  }

  if(class(output)[[1]] == "list") output <- invoke(plot_grid, output, ncol = 1)

    # heighttest <<- 1

  return(output)
})


output$PredIndiInteractive <- renderPlotly({
  test <- input$updatePred
  plot_pred(dossier(stringToNumbers(input$numberPred2)), n = isolate(input$nIndPred2), ylog = isolate(input$logPred2), xlog = isolate(input$logxPred2), freeScale = isolate(input$freeSalePred2), filter = isolate(input$filterID2), pred = isolate(input$PredPred2), plotly = T)
})

observeEvent(input$savePRED, {

  plots_output <<- plots_output %>%
    add_row(Plot = "Pred",
            Runs = input$numberPred2,
            plots = list(
              plot_pred(dossier(stringToNumbers(input$numberPred2)), n = input$nIndPred2, ylog = input$logPred2, xlog = input$logxPred2, freeScale = input$freeSalePred2, filter = input$filterID2, pred = input$PredPred2)),
            Commentary =  input$commPRED#paste0(input$filterID2, ". ylog: ", input$logPred2, ". xlog: ", input$logxPred2, ". freeScale: ", input$freeSalePred2, ". pred: ", input$PredPred2, ". n: ", input$nIndPred2 )
    )

})


# GOF ---------------------------------------------------------------------


output$GOFstaticPlot <- renderPlot({


  title <- ggdraw() + draw_label(input$goftitle, fontface='bold')
  test <- input$updateGOF
  plot_grid(title,
            plot_GOF(dossier(stringToNumbers(input$nGOF)[1]),removeBLQ = isolate(input$remBLQGOF), plots_to_keep = as.double(stringToNumbers(input$SelectGOF)),  filter = isolate(input$filterGOF), xlog =  input$gofxlog, ylog =  input$gofylog, units_obs = input$gofunits_obs, units_time = input$gofunits_time, method_smooth = input$gofmethods),
            ncol=1, rel_heights=c(0.1, 1))
})


output$GOFinteractivePlot <- renderPlotly({

  title <- ggdraw() + draw_label(input$goftitle, fontface='bold')
  test <- input$updateGOF
  plot_grid(title,
            plot_GOF(dossier(stringToNumbers(input$nGOF)[1]),removeBLQ = isolate(input$remBLQGOF), plotly = T,  col_cov = input$colcov, cov_to_use =  input$covPrimGOF, plots_to_keep = as.double(stringToNumbers(input$SelectGOF)),  filter = isolate(input$filterGOF), xlog =  input$gofxlog, ylog =  input$gofylog, units_obs = input$gofunits_obs, units_time = input$gofunits_time, method_smooth = input$gofmethods ),
            ncol=1, rel_heights=c(0.1, 1))
})

output$multiGOFplot <- renderPlot({

  title <- ggdraw() + draw_label(input$goftitle, fontface='bold')
  test <- input$updateGOF

  gofs <- plot_GOF(dossier(stringToNumbers(input$nGOF), folder = T),removeBLQ = isolate(input$remBLQGOF),cov_to_use =  input$covPrimGOF,  plots_to_keep = as.double(stringToNumbers(input$SelectGOF)),  filter = isolate(input$filterGOF), xlog =  input$gofxlog, ylog =  input$gofylog, units_obs = input$gofunits_obs, units_time = input$gofunits_time, method_smooth = input$gofmethods) %>%
    arrange_(input$ArrangeGOF)

  if(input$SelectmultiGOF != ""){

    try( gofs <- gofs %>%
           filter(!!parse_expr(input$SelectmultiGOF)))
  }


  plot_grid(title,
            gofs %>%
              plot_invoke(),
            ncol=1, rel_heights=c(0.1, 1))

})

output$covGOF <- renderTable({

  data.frame(unclass( run_cov_summary(dossier(stringToNumbers(input$number)[[1]]))), check.names = FALSE, stringsAsFactors = FALSE)  %>%
    map_dfr(
      function(x){

        x[is.na(x) == T] <- ""
        x

      })


})

output$multiGOF <- renderTable({

  plot_GOF(dossier(stringToNumbers(input$nGOF), folder = T),cov_to_use =  input$covPrimGOF,  plots_to_keep = as.double(stringToNumbers(input$SelectGOF)),  filter = input$filterGOF) %>%
    select(-plot, -contains("data")) %>%
    arrange_(input$ArrangeGOF) -> temp

  if(input$SelectmultiGOF != ""){

    try( temp <- temp %>%
           filter(!!parse_expr(input$SelectmultiGOF)))
  }
return(temp)
})


observeEvent(input$saveGOF, {

  if(input$radioGOF %in% c("Single Static",  "Single Dynamic")){

    title <- ggdraw() + draw_label(input$goftitle, fontface='bold')


    plot_to_save <-
      plot_grid(title,
                plot_GOF(dossier(stringToNumbers(input$nGOF)[1]),plots_to_keep = as.double(stringToNumbers(input$SelectGOF)),  filter = input$filterGOF, xlog =  input$gofxlog, ylog =  input$gofylog, units_obs = input$gofunits_obs, units_time = input$gofunits_time, method_smooth = input$gofmethods),
                ncol=1, rel_heights=c(0.1, 1))
  }else{


    title <- ggdraw() + draw_label(input$goftitle, fontface='bold')
    plot_to_save <-

      plot_grid(title,
                plot_GOF(dossier(stringToNumbers(input$nGOF), folder = T),cov_to_use =  input$covPrimGOF,  plots_to_keep = as.double(stringToNumbers(input$SelectGOF)),  filter = input$filterGOF, xlog =  input$gofxlog, ylog =  input$gofylog, units_obs = input$gofunits_obs, units_time = input$gofunits_time, method_smooth = input$gofmethods) %>%
                  arrange_(input$ArrangeGOF) %>%
                  plot_invoke(),
                ncol=1, rel_heights=c(0.1, 1))
  }


  plots_output <<- plots_output %>%
    add_row(Plot = "GOF",
            Runs = input$nGOF,
            plots = list( plot_to_save),
            Commentary =  input$commGOF#paste0(input$filterID2, ". ylog: ", input$logPred2, ". xlog: ", input$logxPred2, ". freeScale: ", input$freeSalePred2, ". pred: ", input$PredPred2, ". n: ", input$nIndPred2 )
    )



})

# boxplot ------------------------------------------------------------------

output$covBP <- renderTable({

  data.frame(unclass( run_cov_summary(dossier(stringToNumbers(input$number)[[1]]))), check.names = FALSE, stringsAsFactors = FALSE)  %>%
    map_dfr(
      function(x){

        x[is.na(x) == T] <- ""
        x

      })


})

output$multiBox <- renderPlot({

  plot_random_effect_cov(dossier(as.double(stringToNumbers(input$nBoxPlots))),covPrim =  input$covPrimBP2, boxplotPoint = input$pointBP2, boxplotest = input$boxplottest) #, output = if_else(input$dissociateBP2 == T, 0,1)

})


observeEvent(input$saveBP, {

  plots_output <<- plots_output %>%
    add_row(Plot = "COV",
            Runs = input$nBoxPlots,
            plots = list(plot_random_effect_cov(dossier(as.double(stringToNumbers(input$nBoxPlots))),covPrim =  input$covPrimBP2, boxplotPoint = input$pointBP2)),
            Commentary =  input$commBP#paste0(input$filterID2, ". ylog: ", input$logPred2, ". xlog: ", input$logxPred2, ". freeScale: ", input$freeSalePred2, ". pred: ", input$PredPred2, ". n: ", input$nIndPred2 )
    )

})


# matrix ------------------------------------------------------------------



output$matrix <- renderPlot({
  plot_random_effect_matrix(dossier(stringToNumbers(input$nmatrix)[1]))
})



output$multiMatrix <- renderTable({

  plot_random_effect_matrix(dossier(stringToNumbers(input$nmatrix)),table = T, filterr = input$matrixfilter) %>%
    select(-plot, - run) %>%
    arrange_(input$ArrangeMatrix)

})

#
output$matrixMultiPlot <- renderPlot({
  plot_random_effect_matrix(dossier(stringToNumbers(input$nmatrix)),table = T, filterr = input$matrixfilter) %>%
    select(- run) %>%
    arrange_(input$ArrangeMatrix) %>%
    plot_invoke()
})

observeEvent(input$saveMATRIX, {

  if(input$radioMatrix == "Single"){


    plot_to_save <- plot_random_effect_matrix(dossier(stringToNumbers(input$nmatrix)[1]))
  }else{

    plot_to_save <-   plot_random_effect_matrix(dossier(stringToNumbers(input$nmatrix)),table = T, filterr = input$matrixfilter) %>%
      select(- run) %>%
      arrange_(input$ArrangeMatrix) %>%
      plot_invoke()

  }


  plots_output <<- plots_output %>%
    add_row(Plot = "Matrix",
            Runs = input$nmatrix,
            plots = list( plot_to_save),
            Commentary =  input$commMATRIX#paste0(input$filterID2, ". ylog: ", input$logPred2, ". xlog: ", input$logxPred2, ". freeScale: ", input$freeSalePred2, ". pred: ", input$PredPred2, ". n: ", input$nIndPred2 )
    )



})


# iLL ---------------------------------------------------------------------



output$iLL <- renderPlot({
  plot_individual_objective_function(dossier(stringToNumbers(input$niLL)), cov = input$covPrimiLL, histogram = input$HistoiLL)
})


observeEvent(input$saveILL, {

  plots_output <<- plots_output %>%
    add_row(Plot = "ILL",
            Runs = input$niLL,
            plots = list(     plot_individual_objective_function(dossier(stringToNumbers(input$niLL)), cov = input$covPrimiLL, histogram = input$HistoiLL)),
            Commentary =  input$commILL#paste0(input$filterID2, ". ylog: ", input$logPred2, ". xlog: ", input$logxPred2, ". freeScale: ", input$freeSalePred2, ". pred: ", input$PredPred2, ". n: ", input$nIndPred2 )
    )

})


# Manual Plot -------------------------------------------------------------

observeEvent(input$fileManualPLot, {


  try({
    pathManualPlot <-  input$fileManualPLot
    # pathManualPlot <- "file:///D:/these/Pecc_test/3_Models/1_Models/pecc_func.R"
    pathManualPlot <- gsub("file:///", "", pathManualPlot)

    source(pathManualPlot)
    lines <- readLines(pathManualPlot)
    # print("whaat")
    lines[grep("function", lines)] %>%
      gsub(pattern = " *<-.+", replacement = "", lines) -> functionss
    # print(functionss)
    # print(ls(name=.GlobalEnv))
    functionss <- functionss[functionss %in% ls(name=.GlobalEnv)]
    # print(functionss)
    if(length(functionss) > 0) {

      updateSelectInput(session, "selectManualFunction", choices = functionss, label = functionss[[1]])

    }


  })
})

observeEvent(input$selectManualFunction, {

  try({
    funcsel <- input$selectManualFunction
    # funcsel <- "testManual"
    arguments <- formals(funcsel)

    outputtemp <- tibble(names = names(arguments), value = map_chr(arguments, ~deparse(.x))) %>%
      spread(key = names, value = value)

    output$argManualFunction <- renderRHandsontable(rhandsontable(outputtemp, rowHeaders = F))
  })



})



observeEvent(input$ManualPlotGo, {


  try({

    funcsel <- isolate(input$selectManualFunction)
    # funcsel <- "xfoldCmax"
    # # print("funcsel")
    # print(funcsel)
    tableArg <- hot_to_r(isolate(input$argManualFunction)) %>% as.list()
    # tableArg <- outputtemp %>% as.list()
    # tableArg <- tibble(run = "3:5")

    if("run" %in% names(tableArg)){

      tableArg$run <- paste0("dossier(stringToNumbers(\"",tableArg$run,"\"))")

    }

    # print("tableArg")
    # print(funcsel)

    expr((!!parse_expr(funcsel))(!!!map(tableArg,~parse_expr(.x)))) -> exprfinal

    outputt <- try(eval(exprfinal))

    # print(class(outputt))
    if(class(outputt)[[1]] != "try-error"){

      if("ggplot" %in% class(outputt)){

        showNotification("Plot creation", type = "message", duration = 3)

        output$ManualPlotPlot <- renderPlot(outputt)

      }

    }else{

      showNotification("Function led to error", type = "error", duration = 5)


    }




  })




})



# sensititivy -------------------------------------------------------------

a <- try( results(dossier(stringToNumbers(input$number)) ))

if(class(a) != "try-error"){
  updateSelectInput(session, "parSens", choices = c("Select",a$Parameter))
}

output$sensitivity <- renderPlot({
  plot_sensitivity(object = dossier(stringToNumbers(input$number)), parameter = input$parSens)
})

observeEvent(input$saveSENS, {

  plots_output <<- plots_output %>%
    add_row(Plot = "SENS",
            Runs = input$number,
            plots = list(   plot_sensitivity(object = dossier(stringToNumbers(input$number)), parameter = input$parSens)),
            Commentary =  input$commSENS#paste0(input$filterID2, ". ylog: ", input$logPred2, ". xlog: ", input$logxPred2, ". freeScale: ", input$freeSalePred2, ". pred: ", input$PredPred2, ". n: ", input$nIndPred2 )
    )

})


# VPC ---------------------------------------------------------------------


observeEvent(input$loadVPC,{

  if(input$headerVPC == T){

    VPC <<- try(read.table(input$pathVPC,  header = T, na.strings = input$nastringVPC, sep = input$sepVPC, dec = input$decVPC))

  }else{

    colnamess <-  str_split(input$nameVPC, pattern = " ")[[1]] %>%
      gsub(pattern = " ",replacement = "")

    colnamess <- colnamess[colnamess != ""]


    VPC <<- try(read.table(input$pathVPC, col.names = colnamess, na.strings = input$nastringVPC, sep = input$sepVPC, dec = input$decVPC))

  }


  if(class(VPC) != "try-error"){
    updateSelectInput(session, "VPCX", choices = c("", names(VPC)), selected = names(VPC)[c(grep("TIME", toupper(names(VPC))),1)[[1]]])
    updateSelectInput(session, "VPCY", choices = c("", names(VPC)), selected = names(VPC)[c(grep("(IPRED)|(DV)", toupper(names(VPC))),1)[[1]]])
    updateSelectInput(session, "VPCsim", choices = c("", names(VPC)), selected = names(VPC)[c(grep("SIM", toupper(names(VPC))),1)[[1]]])
    updateSelectInput(session, "VPCobs", choices = c("", names(VPC)), selected = names(VPC)[c(grep("OBS", toupper(names(VPC))),1)[[1]]])
    updateSelectInput(session, "VPCCOV", choices = c("", names(VPC)), selected = "")

    # updateSelectInput(session, "VPCStand", choices = c("", names(VPC)), selected = "")
    # updateSelectInput(session, "covNCA", choices = c("", names(VPC)), selected = "")
    # updateSelectInput(session, "VPCID", choices = c("", names(VPC)), selected =  names(VPC)[grep("ID",names(VPC))[[1]]])
  }else{


    # updateSelectInput(session, "VPCX", choices = c(""), selected = "")
    # updateSelectInput(session, "VPCY", choices = c(""), selected = "")
    # updateSelectInput(session, "VPCCol", choices = c(""), selected = "")
    # updateSelectInput(session, "VPCWrap", choices = c(""), selected = "")
    # updateSelectInput(session, "VPCGrid", choices = c(""), selected = "")
    # updateSelectInput(session, "VPCStand", choices = c(""), selected = "")
    # updateSelectInput(session, "VPCID", choices = c(""), selected =  "")
    # updateSelectInput(session, "covNCA", choices = c(""), selected =  "")



  }

}
)


observeEvent(input$launchVPC,{



  output$VPCPlot <- renderPlot({



    if(is.null(input$VPCCOV) ){

      groupVPC <- ""
    }else{

      groupVPC <- input$VPCCOV

    }


    quantiles <- str_split(input$VPCquantile, " ")[[1]] %>% as.double

    # print(groupVPC)
    plot_vpc(dataset = VPC,xlabel = input$VPCxlabel, ylabel = input$VPCylabel, ylog = input$VPCYlog, group = groupVPC, type_scatter_CI = input$VPCtype, quantiles = quantiles,CIpct = input$VPCCIpct, x = input$VPCX, y = input$VPCY, sim = input$VPCsim, obs = input$VPCobs, ymin_displayed = input$VPCYMIN, loq_obs = input$VPCLOQ, filterr = input$filterrVPC)+
      theme_bw(base_size = input$VPCsizeText)

    #
  })

  output$PDPlot <- renderPlot({



    if(is.null(input$VPCCOV) ){

      groupVPC <- ""
    }else{

      groupVPC <- input$VPCCOV

    }

    plot_pred_dist(dataset = VPC,xlab = input$VPCxlabel, ylab =  input$VPCylabel, ylog = input$VPCYlog, group = groupVPC, x = input$VPCX, y = input$VPCY, sim = input$VPCsim, obs = input$VPCobs, ymin_displayed = input$VPCYMIN, loq_obs = input$VPCLOQ, filterr = input$filterrVPC)+
      theme_bw(base_size = input$VPCsizeText)

    #
  })



})

observeEvent(input$pathVPC,{
  #
  pathVPC <- input$pathVPC
  #   # pathVPC <- "file:///Z:/ETUDES/SPK/CLSPKPOOL/ANACIN/USERS/TDPE_CB_2/18_11_14_BiophaseMasterProject/ENCOURS_Post_Mentre_Meeting_January/PK_55/NM_VIEWER/1.01.V1_DOSE_vpc.TAB"
  #
  linescfl <- try(readLines(gsub(".TAB", ".cfl", pathVPC)))
  #


  if(class(linescfl) != "try-error" & length(linescfl) > 0){

    linescfl <- gsub(";.+", "", linescfl)

    dollartable <- grep("\\$TABLE", linescfl)
    linescfl[dollartable:length(linescfl)] %>%
      reduce(paste) %>%
      gsub(pattern = "NOHEADER.+", replacement = "") %>%
      gsub(pattern = "\\$TABLE ", replacement = "") -> colomns

    updateTextInput(session, "nameVPC", value =   colomns)
  }
  #
})


# forest ------------------------------------------------------------------

observeEvent(input$mb_load_forest_run, {

run <- as.double(isolate(input$nForest))
# run <- ""
# run <- 2
# print(run)
# print("la on fait le forest plot")
newtable <- try(plot_forest_run(dossier(run), onlytable = T))
# print("here")

if(class(newtable) != "try-error"){
output$table_forest <- renderRHandsontable({

  rhandsontable(newtable, rowHeaders = NULL)

})
}else{

  showNotification("Loading failed", type = "error", duration = 3)


}

})


observeEvent(input$mb_add_forest, {

  output$table_forest <- renderRHandsontable({

    levels <- c("itself", "cat_fold", "cat_log_fold","cont_linear",  "cont_ref", "IIV_lognormal")
    datainput <-hot_to_r(isolate(input$table_forest)) %>%
      bind_rows(

        tibble(label = "", parameter = "", use = T, delete = F)

      )

    rhandsontable(datainput, rowHeaders = NULL)

  })


})


observeEvent(input$mb_delete_forestt, {

  output$table_forest <- renderRHandsontable({


    datainput <-hot_to_r(isolate(input$table_forest))

    if(sum(datainput$delete == T) == nrow(datainput)){

      datainput$delete[[1]] <- F
    }


    rhandsontable(datainput %>% filter(delete == F), rowHeaders = NULL)

  })


})

observeEvent(input$forest_go, {


  plotforest <- plot_forest(datainput = hot_to_r(isolate(input$table_forest)) %>%
                              filter(use  & !is.na(method)) , n = 1000, returnExpr = T)

  output$forestPlot <- renderPlot({eval(plotforest) })

updateTextAreaInput(session, inputId = "codeforestpec", value = paste0("datainput <-",
                                                                       deparse(expr(tibble(!!!map(hot_to_r(isolate(input$table_forest)) %>% mutate(method = as.character(method)), ~.x))), width.cutoff = 500) %>%
                                                                         paste0(collapse = ""),
                                                                       "\n\n\nplot_forest(datainput = datainput, n = 1000, returnExpr = T)"))

updateTextAreaInput(session, inputId = "codeforest", value = deparse(plotforest, width.cutoff = 500) %>%
                      paste0(collapse = "") %>%
                      gsub(pattern = " %>% ", replacement = " %>% \n") %>%
                      gsub(pattern = "\n *", replacement = "\n") %>%
                      gsub(pattern = "\\{", replacement = "{ \n") %>%
                      gsub(pattern = "\\}", replacement = "\n } "))




deparse(plotforest, width.cutoff = 500) %>%
  paste0(collapse = "") %>%
  gsub(pattern = " %>% ", replacement = " %>% \n") %>%
  gsub(pattern = "\n *", replacement = "\n") %>%
  gsub(pattern = "\\{", replacement = "{ \n") %>%
  gsub(pattern = "\\}", replacement = "\n } ")%>% cat


})

# plot saved --------------------------------------------------------------


observeEvent(input$savedUpdate, {

  output$savedPlotsTable <- renderTable({

    plots_output%>%
      select(-plots) %>%
      rownames_to_column("n")
  })

})


observeEvent(input$pdfplots, {


  time <- gsub(" |-|:","_",Sys.time()) %>%
  {gsub("^20.._", "",.)} %>%
  {gsub("_..$", "", .)};time

  path_pdf <-  gsub("file:///", "", paste0(gsub("\"", "", input$path2),"/peccaryPlots_", time, ".pdf")) %>%
  {gsub("file:", "",.)} %>%
  {gsub("\\\\","/",.)}

  # print(path_pdf)
  pdf(file = path_pdf , width = 14, height = 10)
  map2(plots_output$plots, plots_output$Commentary, function(x,y) print(x +
                                                                          labs(caption = gsub("   ", "\n", y))+
                                                                          theme(plot.caption = element_text(hjust = 0.05, size = 14, face = "italic"))))
  dev.off()
  print("Tentative ouverture pdf")
  try(shell.exec(gsub("^//", "file://", path_pdf) ))

})


## Re displayed a saved plot
output$saved_displayed <- renderPlot({

  plots_output[[input$saved_n_to_display, "plots"]]+
    labs(caption = plots_output[[input$saved_n_to_display, "Commentary"]])+
    theme(plot.caption = element_text(hjust = 0))

})


# pdf creation ------------------------------------------------------------

observeEvent(input$allplotbutton, {

  AllPlots(dossier(stringToNumbers(input$number)), covPrim = input$variable, covSec = input$variable2 )
})




