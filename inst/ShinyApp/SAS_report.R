
# Template ----------------------------------------------------------------


observeEvent(input$reportTemplateLaunch, {

# print("heremotdherfuccccher")
  path_template <- isolate(input$reportTemplatePath)
  path_template <- "D:/Peccary/inst/Report/templateReport.txt"

  template <- readLines(path_template)

  # free introduction
  temp <- template[(grep("Pecc_balise_free_introduction", template)+1):(grep("Pecc_balise_bibliography", template)-1)]

  updateTextAreaInput(session, "Report_introduction", value =    paste0(temp, collapse = "\n") )

  # bibliography
  temp <- template[(grep("Pecc_balise_bibliography", template)+1):(grep("Pecc_balise_datasets", template)-1)]
  updateTextAreaInput(session, "Report_biblio", value =    paste0(temp, collapse = "\n") )

  # dataset
  temp <- template[(grep("Pecc_balise_datasets", template)+1):(grep("Pecc_premodeling", template)-1)]
  updateTextAreaInput(session, "Report_dataset", value =    paste0(temp, collapse = "\n") )

  # premodeling
  temp <- template[(grep("Pecc_premodeling", template)+1):(grep("Pecc_modeling", template)-1)]
  updateTextAreaInput(session, "Report_premodeling", value =    paste0(temp, collapse = "\n") )

  # modeling
  temp <- template[(grep("Pecc_modeling", template)+1):(grep("Pecc_run", template)-1)]
  updateTextAreaInput(session, "Report_modeling", value =    paste0(temp, collapse = "\n") )

  # run
  temp <- template[(grep("Pecc_run", template)+1):(grep("Pecc_simulation", template)-1)]
  updateTextAreaInput(session, "Report_run", value =    paste0(temp, collapse = "\n") )

  # simulation
  temp <- template[(grep("Pecc_simulation", template)+1):length(template)]
  updateTextAreaInput(session, "Report_simulation", value =    paste0(temp, collapse = "\n") )



})

# Selection dataset ----------------------------------------------------------------
observeEvent(input$reportDatasetInfo, {
try({
  # get value
  selectedDataset <- input$reportDatasetInfo

  # selectedDataset <- ""
  datasets_df %>%
    as_tibble %>%
    filter(n %in% gsub(":.+", "", selectedDataset)) %>%
    group_by(n) %>%
    nest() %>%
    # {.[[1, "data"]]} -> data
    mutate(line_chr = map(data, function(data){

     headermonolix <- try( data$monolix_header %>%
        as.character() %>%
        parse_expr() %>%
        eval)

     headerline <- ""
     if(class(headermonolix) != "try-error"){

       headermonolix[headermonolix != "drop"] -> headermonolix

       imap(headermonolix, function(x,y){
         paste0(y, " -> ", x)

       }) %>%
         paste0(collapse = " | ") -> headerline
     }

    temp <-  paste0(file.path("__\".", data$Path, data$File), "\":__\n\n+ Free commentary: ", data$commentary,".\n+ Separator: \"", data$sep,
            "\"\n+ Decimal: \"", data$dec, "\"\n+ NA: \"", data$na, "\"\n+ Main header: ", headerline)

    if(length(grep(":/", temp)) > 0 ) temp <- gsub("\\./", "", temp) # if complete path

    return(temp)
    })) %>%
    pull(line_chr) %>%
    paste(collapse = "\n\n") -> lineDataset

  # replace

  previous <- isolate(input$Report_dataset)
  #   previous <- "
  # Here is the used dataset:
  #
  # Pecc_insert_dataset_list_start (keep that line, will be removed in final output)
  # Pecc_insert_dataset_list_end (keep that line, will be removed in final output)
  # "
  previous <- str_split(previous, "\n")[[1]]
  balise1 <- grep("Pecc_insert_dataset_list_start", previous)
  balise2 <- grep("Pecc_insert_dataset_list_end", previous)

  new <- c(previous[0:(balise1)], lineDataset, previous[(balise2):length(previous)])

  updateTextAreaInput(session, "Report_dataset", value = paste0(new, collapse = "\n"))
})
})


# Selection Explo Plot ----------------------------------------------------


observeEvent(input$reportPlotExploSelect, {

  PlotSelected <<- input$reportPlotExploSelect


})


# save report -------------------------------------------------------------

observeEvent(input$reportSave, {

  path <- file.path(project_file, "0_pecc_project", "reports.rds")
  name <- isolate(input$reportnewVersion)


 testreportplottt <- as.character(isolate(input$reportPlotExploSelect))
 if(length(testreportplottt) == 0) testreportplottt <- NA else testreportplottt <- paste0(testreportplottt, collapse = " & ")


 reportDatasetInfo <- as.character(isolate(input$reportDatasetInfo))
 if(length(reportDatasetInfo) == 0) reportDatasetInfo <- NA else reportDatasetInfo <- paste0(reportDatasetInfo, collapse = " & ")

 reportDataExploSelect <- as.character(isolate(input$reportDataExploSelect))
 if(length(reportDataExploSelect) == 0) reportDataExploSelect <- NA else reportDataExploSelect <- paste0(reportDataExploSelect, collapse = " & ")

print(isolate(input$reportModelEq))

  new <- tibble(
          reportnewVersion = isolate(input$reportnewVersion),
          reportTemplatePath = isolate(input$reportTemplatePath),
         Report_introduction = isolate(input$Report_introduction),
         Report_biblio = isolate(input$Report_biblio),
         reportDatasetInfo = reportDatasetInfo,
         Report_dataset = isolate(input$Report_dataset),
         reportPlotExploSelect = testreportplottt,
         Report_premodeling = isolate(input$Report_premodeling),
         Report_modeling = isolate(input$Report_modeling),
         Report_run = isolate(input$Report_run),
         Report_simulation = isolate(input$Report_simulation),
         exportReportPath = isolate(input$exportReportPath),
         exportReportTitle = isolate(input$exportReportTitle),
         exportReportAuthor = isolate(input$exportReportAuthor),
         exportReportBibPath = isolate(input$exportReportBibPath),
         exportReportCSLPath = isolate(input$exportReportCSLPath),
         reportModelEq =  if_else(is.null(isolate(input$reportModelEq)),"", isolate(input$reportModelEq)),
         reportModelSimul =  if_else(is.null(isolate(input$reportModelSimul)),"", isolate(input$reportModelSimul)),
         reportRunfinalpat = isolate(input$reportRunfinalpat),
         reportDataExploSelect = reportDataExploSelect)


    sepp <- ";"
    previous <- try(readRDS(path))


    if(class(previous) != "try-error"){

      if(is.na(previous$reportDatasetInfo)) previous$reportDatasetInfo <- ""
      if(is.na(previous$reportPlotExploSelect)) previous$reportPlotExploSelect <- ""
      if(is.na(previous$reportDataExploSelect)) previous$reportDataExploSelect <- ""


      new <- bind_rows(new, previous %>%
                         filter(reportnewVersion != isolate(input$reportnewVersion))
      )}

    sepp <- ";"


    saveRDS(new, path)

    updateSelectInput(session, "reportVersion", choices = new$reportnewVersion, selected = name)

}
    )


# load report -------------------------------------------------------------


observeEvent(input$reportLoad, {


  selected <- isolate(input$reportVersion)
  # selected <- "test"
  path <- file.path(project_file, "0_pecc_project", "reports.rds")

  readRDS(path) %>%
    as_tibble %>%
    filter(reportnewVersion == selected) -> line


  updateTextInput(session, "reportnewVersion", value = line$reportnewVersion)
  updateTextInput(session, "reportTemplatePath", value = line$reportTemplatePath)
  updateTextAreaInput(session, "Report_introduction", value = line$Report_introduction)
  updateTextAreaInput(session, "Report_biblio", value = line$Report_biblio)

  updateTextAreaInput(session, "Report_premodeling", value = line$Report_premodeling)
  updateTextAreaInput(session, "Report_modeling", value = line$Report_modeling)
  updateTextAreaInput(session, "Report_run", value = line$Report_run)
  updateTextAreaInput(session, "Report_simulation", value = line$Report_simulation)
  updateTextInput(session, "exportReportPath", value = line$exportReportPath)
  updateTextInput(session, "exportReportTitle", value = line$exportReportTitle)
  updateTextInput(session, "exportReportAuthor", value = line$exportReportAuthor)
  updateTextInput(session, "exportReportBibPath", value = line$exportReportBibPath)
  updateTextInput(session, "exportReportCSLPath", value = line$exportReportCSLPath)
  updateTextInput(session, "reportRunfinalpat", value = line$reportRunfinalpat)



  try(updateSelectInput(session, "reportDatasetInfo", selected = line$reportDatasetInfo)) #, choices =  unique(paste0(datasets_df$n,":", datasets_df$File))
  try(updateSelectInput(session, "reportPlotExploSelect", selected = str_split(line$reportPlotExploSelect, " & ")[[1]]))
  try(updateSelectInput(session, "reportModelSimul", selected = str_split(line$reportModelSimul, " & ")[[1]]))
  try(updateSelectInput(session, "reportModelEq", selected = str_split(line$reportModelEq, " & ")[[1]]))
  try(updateSelectInput(session, "reportDataExploSelect", selected = str_split(line$reportDataExploSelect, " & ")[[1]]))

   updateTextAreaInput(session, "Report_dataset", value = line$Report_dataset)
})

# export ------------------------------------------------------------------

observeEvent(input$exportReportGo, {

  withProgress( min = 0, max = 1, message = "Step1 : creation .rmd file",expr = {
  print("debut exportation")

  ### header
title <- isolate(input$exportReportTitle)
author <-  isolate(input$exportReportAuthor)
bibliographypath <- isolate(input$exportReportBibPath)
stylecsl <- isolate(input$exportReportCSLPath)


header <- paste0( "---
title: \"", title ,"\"
author: \"", author, "\"
date: \"`r format(Sys.time())`\"
output:
  html_document:
    df_print: paged
    toc: yes
  pdf_document:
    df_print: paged
    toc: yes
  word_document:
    toc: yes
bibliography: ", bibliographypath, "
csl: ", stylecsl , "
---\n\n
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning = F, message = F)
library(peccary)
```\n\n"
)

  ###
  # intro <- "This document has been written by Thibaud Derippe, pharmacometricians in YYYY company. It is the report of a PK/PD modeling activities, regarding the following project: XXXX. \nHere are the main section of this document:\n1. Context of the study with bibliographic elements\n2. Presentation of the different datasets\n3. Pre-modeling activities (population description, graphical exploration, NCA)\n4. Description of the model equations\n5. Description of the final model\n6. Simulations with the final model\n"
  # biblio <- "\nHere, describe your project. Remember you can use [@rezrezrez] for citing any article.\n\n"
  # dataset <- "\nHere is the used dataset:\n\n--- Pecc_insert_dataset_list_start (keep that line, will be removed in final output)\n--- Pecc_insert_dataset_list_end (keep that line, will be removed in final output) \n"

  ### intro
  intro <- isolate(input$Report_introduction)
  intro <- paste0("# Introduction\n\n", intro)

  ### Biblio
  biblio <- isolate(input$Report_biblio)
  biblio <- paste0("# Project Presentation & Bibliographie\n\n", biblio)



# Pre-modeling ------------------------------------------------------------
print("premodeling")

  ### PlotExplo
  premodeling <- isolate(input$Report_premodeling)
#   premodeling <- "## Population description
#
# Here is a description of the population:
#
# --- Pecc_insert_pop_description
#
# ## Dataset exploration
#
# Here are some graphical exploration plots:
#
# --- Pecc_insert_PlotExplo
# "
  PlotSelected <- isolate(input$reportPlotExploSelect)
  # PlotSelected <- c("1: qPCR  copies/cell",  "4: FC + IFNG + lympho")
  # PlotSelected <- "1: aze"
  neededDataset <- character()
  # PlotSelected <<- PlotSelected
  read.table(stringsAsFactors = F, file.path(project_file, "0_pecc_project", "exploPlot.txt"), header = T) %>%
    as_tibble %>%
    mutate(output = paste0(gsub(":.+", "", preloadeddataset), ": ", Name)) %>%
    filter(output %in% PlotSelected) %>%
    group_by(output) %>%
    slice(1) %>%
    nest() %>%
    # {.[[2]][[1]]} ->> dataaa
    mutate(line_chr = map(data, function(data){
      # print("aaa")
     gridPeccary132 <-  if_else(is.na( data$exploGrid), "", as.character(data$exploGrid))

     data <- map_dfr(data, function(x){
# print(x[[1]])
     if(is.na(x)) x[is.na(x)] <- ""
     # if(x[is.null(x[[1]])])  x[is.null(x)] <- ""
      x
     })
# print(data$secondFilter)
     print("bbb")
     # print(data$bkgrd)
     if(is.null(data$bkgrd)) data$bkgrd <- F
     if(data$bkgrd == "") data$bkgrd <- F

      plottemp <- plot_spagh(bkgrd  = data$bkgrd , # NA
                                  filter2  = data$secondFilter, # NA
                                 removeNA = data$exploNA ,
                                 title = data$titleExplo,
                                 subtitle =  as.character(data$subtitleExplo), # NA
                                 xlabs = data$xlabsExplo,
                                 ylabs = data$ylabsExplo,
                                 caption =data$captionExplo, # NA
                                 sizetext = data$sizeTextExplo,
                                 df = explo,
                                 bkgrdalpha = if_else(is.na(data$all_bkgrdalpha), 0.3, as.double(data$all_bkgrdalpha)) , # NA
                                 filter = data$Filter,
                                 facetscale = data$scaleExplo,
                                 loq = as.double(data$exploLOQ),
                                 x = data$exploX,
                                 median = data$exploMedian,
                                 y = data$exploY,
                                 col =  data$Col,
                                 facetwrap =  data$Wrap,
                                 facetgrid = gridPeccary132,
                                 group = data$exploID,
                                 point = data$exploPoint,
                                 standardisation =  data$exploStand ,
                                 ylog = data$exploYlog,
                                 xlog = data$exploXlog,
                                 line_alpha = data$exploLine,
                                 output = "expr", workwithexpr = F ) %>%
        deparse(width.cutoff = 500) %>%
        paste(collapse = "")
print("ccc")
# print(plottemp2)
      paste0(plottemp, collapse = "\n") %>%
        gsub(pattern = "geom_", replacement = "\ngeom_") %>%
        gsub(pattern = "scale_", replacement = "\nscale_") %>%
        gsub(pattern = "labs\\(", replacement = "\nlabs(") %>%
        gsub(pattern = "facet_", replacement = "\nfacet_") %>%
        gsub(pattern = " %>% ", replacement = " %>%\n") -> plottemp2


      neededDataset <<- unique(c(neededDataset, data$preloadeddataset))

      plottemp2 <- gsub("explo *%>%", paste0(gsub("(^.+:)|(\\..+)", "",data$preloadeddataset), " %>%")
     , plottemp2)

      paste0("__",data$Name,"__\n", "\n```{r}\n", plottemp2, "\n```")



    })) %>%
    pull(line_chr) %>%
    paste0(collapse = "\n\n") -> plotsExplo

  #dataexplo
  try({
  exploselected <- isolate(input$reportDataExploSelect)

  metadataexplo <- readRDS(file.path(project_file, "0_pecc_project", "dataExplo.rds"))

  map(exploselected, function(x){

    numberda <- gsub(":.+", "", x)
    x <- gsub("^.+: ", "", x)
    nametemp <- x # for last row
    if(length(grep("_table1$", x)) == 1) type = "table1" else type = "count"
    x <- gsub("(_table1$)|(_count$)", "", x)

    metadataexplo %>%
      mutate(ndf = gsub(":.+", "", preloadeddataset)) %>%
      filter(ndf == numberda & dataexplonewVersion == x)-> line

    datasets_df %>%
      filter(n == numberda) %>%
      as_tibble %>%
      pull(File) %>%
      gsub(pattern = "\\..+", replacement = "") -> outputtemp

    dftemp <- outputtemp
    # apply filter
   if(line$filtertableexplo != ""){

     outputtemp <- paste0(outputtemp, " %>%\nfilter(", line$filtertableexplo, ")")
   }

    if(line$tableExploManipulation != ""){

      outputtemp <- paste0(outputtemp, "\n", line$tableExploManipulation)
    }

    if(length(line$groupbyCovExplo) > 0){
      grouptemp <- line$groupbyCovExplo[[1]]
      grouptemp <- paste0("\"", grouptemp,"\"")
      if(length(grouptemp) >1)  grouptemp <-  paste0("c(", paste0(grouptemp, collapse = ", "),")")

      outputtemp <- paste0(outputtemp, " %>%\npecc_search_cov(", grouptemp, ")")

    }


    if(type == "table1"){

     table1x <-  line$table1x[[1]]
     table1x <- paste0("\"", table1x,"\"")
     if(length(table1x) >1)  table1x <-  paste0("c(", paste0(table1x, collapse = ", "),")")

     outputtemp <-  paste0(outputtemp, " %>%\npecc_table1(rowl = ", table1x, ", coll = \"", line$table1y , "\") %>%\nkable")


    }else{

      countwhat <- if_else(line$countwhat != "" | is.na(line$countwhat), line$countwhat, "NULL")
      col_cov <- if_else(line$county != "" | is.na(line$county), line$county, "NULL")
      row_cov <- if_else(line$countx != "" | is.na(line$countx), line$countx, "NULL")

      outputtemp <- paste0(outputtemp, " %>%\npecc_count(metric = ", countwhat, ", col_cov = ", col_cov , ", row_cov = ", row_cov, ") %>%\nkable")


    }

    return(paste0("__",nametemp , ":__\n\n```{r}\n",outputtemp, "\n```"))

  }) %>%
    paste0(collapse = "\n\n") -> datadescription
  })

print("erezrez")
  premodel <- paste0("# Data exploration \n\n", isolate(input$Report_premodeling))
  premodel <- gsub("--- Pecc_insert_PlotExplo", plotsExplo, premodel)
 try({ premodel <- gsub("--- Pecc_insert_pop_description", datadescription, premodel)})






# datasets ----------------------------------------------------------------



  dataset <- isolate(input$Report_dataset)
  dataset <- str_split(dataset, "\n")[[1]]
  dataset <- dataset[- (grep("Pecc_insert_dataset_list", dataset))] %>%
    paste0(collapse = "\n")
  dataset <- paste0("# Dataset \n\n", dataset)
  # neededDataset <<- neededDataset
  for(a in neededDataset){

    datasets_df %>%
      as_tibble %>%
      filter(n == as.double(gsub(":.+", "", a))) %>%
      mutate(path =  file.path("__\".", Path, File)) -> temp



    replace <- paste0(temp$path, "\":__")

    if(length(grep(":/", replace)) > 0 ) replace <- gsub("\\./", "", replace) # if complete path


    if(length(grep(":/", temp$Path)) == 1){
      pathfile <- file.path(temp$Path, temp$File)
    }else{
      pathfile <- file.path(project_file, temp$Path, temp$File)
    }


    replacement <- paste0("```{r}\n", gsub("(^.+:)|(\\..+)", "",a),
           " <- read.table(\"", pathfile, "\", sep = \"",temp$sep, "\", dec = \"", temp$dec, "\", na.strings = \"" , temp$na, "\", header = T)",
"\n```")

    dataset <- gsub(replace, paste0(c(replace,replacement ), collapse = "\n\n"), dataset)


  }

print("begin model")
# Modeling ----------------------------------------------------------------

  runforeq <- isolate(input$reportModelEq)
  # runforeq <- "demoTheo"

  reportmodeling <- isolate(input$Report_modeling)
#   reportmodeling <- "
# ## Methodologie GÃ©nÃ©ral
#
#   Parameters of the model have been estimated with NONMEM/MONOLIX. M3 methods have been used in case of BLQ.
#
#   ## Equation
#
#   Here are the equation of the model :
#
#   --- Pecc_insert_equation
#
#   ![Scheme representation of the model](C:/Users/titi7/Documents/TheseExercice/image/methodes.PNG)
#   "

  models <-  try(read.table(file.path(project_file, "0_pecc_project","models.txt" ), stringsAsFactors = F))

if(is.null(runforeq)) runforeq <- ""
  if(class(models) != "try-error"  & gsub(" ", "", runforeq) != ""){

    models %>%
      as_tibble() %>%
      filter(name == runforeq) %>%
      pull(model) -> model

    model <- str_split(model, "\n")[[1]]
    model <- gsub("#.+", "", model)
    model <- model[model != ""]

    model <- paste0("         ", model)
    model <- paste0(model, collapse = "\n")

    reportmodeling <- gsub("--- Pecc_insert_equation", model, reportmodeling)
  }


  modeling <- paste0("# Modeling \n\n", reportmodeling)



  print("begin final run")
# Final run ---------------------------------------------------------------
 try({ runpath <- isolate(input$reportRunfinalpat)
  # runpath <- "D:/Peccary/Exemple_demo/nlmixr_test/monolixComparison"

  runReport <- paste0("# Final run \n\n", isolate(input$Report_run))

 runReportrep <- ""
  if(gsub(" ", "", runpath) != ""){
  runReportrep <- paste0("```{r}\nrunFinal <- createRun(\"", runpath, "\")
kable(results(runFinal))
plot_pred(runFinal, lowerlimit = 0.001)
plot_GOF(runFinal)
plot_random_effect_matrix(runFinal)
try(plot_random_effect_cov(runFinal))
```" )
  }
  runReport <- gsub("--- Pecc_insert_run",runReportrep,  runReport)
 })

# simulation --------------------------------------------------------------


  simulation <- paste0("# Final simulation \n\n", isolate(input$Report_simulation))
  # simulation <- "A partir du run final, des simulations ont pu être produites, afin de tester un schéma d'administration plus optimal.
  #
  # --- Pecc_insert_simulations"
  print("simulation")
try({

  runforeq <- isolate(input$reportModelSimul)
  models %>%
    as_tibble() %>%
    filter(name == runforeq) -> model

compartmet <- eval(parse_expr(model$compartmet))
parameter <- eval(parse_expr(model$parameter))
event <- eval(parse_expr(model$event))
todisplay <- eval(parse_expr(model$todisplay))%>%
                    mutate(Point = as.logical(Point),
                           Check = as.logical(Check))
plotstat <- eval(parse_expr(model$plotstat)) %>%
  mutate(ylog = as.logical(ylog),
         xlog = as.logical(xlog))
matrix_eta <- eval(parse_expr(model$matrix_eta)) %>%
  map(function(x){
    x[x == "NA"] <- ""
    x
  })

print("simulation2")
parameters <-  random_etas(n = 20,
                           parameter_df = parameter,
                           matrix_eta = matrix_eta,sd = T, returnExpr = T, matrixShiny = T)

print("parameters okay")

times <- seq(model$from, model$to, model$by)


event <- event %>%
  mutate(tlag = if_else(tlag == "FALSE", FALSE, T)) %>%
  mutate(F = if_else(F == "FALSE", FALSE, T)) %>%
  mutate(use = if_else(use == "FALSE", FALSE, T))

simulations <-   make_simulations(parameter = parameters,
                                  model = model$model,
                                  states =  compartmet,
                                  events =  event,times = times, timesF = T, Progress = F)
print("simulations okay")
# print(simulations)

xpointt <- "time"
ypointt  <- "observation"
ytype_headerr <- "observationtype"
plot_simulations <-  plot_simulations(simulations = simulations,ymindisplayed = 0.0001,
                                      plot_table = todisplay,
                                      plot_table_cov = plotstat,
                                      xpoint = xpointt,ypoint = ypointt,   ytype_header = ytype_headerr,
                                      n = 20)
print("plot okay")
# print(plot_simulations)
output_plot <-   plot_simulations %>%
  map(function(x){

    deparse(x, width.cutoff = 500) %>%
      paste(collapse = "\n") %>%
      gsub(pattern = "%>%", replacement = "%>%\n" ) %>%
      gsub(pattern = "  *", replacement = " " )


  }) %>%
  paste(collapse = "\n\n")
output_plot <- paste0(paste0("```{r}\n\ntimes <- seq(",model$from,",", model$to, ",",model$by,")\n\n"),output_plot,"\n\n```" )

simulation <- gsub("--- Pecc_insert_simulations", output_plot, simulation)

})

# final output and creation -----------------------------------------------


  paste0(c(header, intro, biblio, dataset, premodel,modeling,runReport,simulation, "# Bibliography"), collapse = "\n\n") -> rmdfile

  # create RMD
  pathoutput <- paste0(isolate(input$exportReportPath), ".rmd")
  fileConn<-file(pathoutput)
  writeLines(rmdfile, fileConn)
  close(fileConn)


  library(rmarkdown)

  if(isolate(input$exportReportFormat) == "word"){

    outputformat <- "word_document"
    ext <- ".docx"
  }else if(isolate(input$exportReportFormat) == "pdf"){

    outputformat <- "pdf_document"
    ext <- ".pdf"
  }else{

    outputformat <- "html_document"
    ext <- ".html"

  }
  incProgress(0.5, message = "Step2: knitting .rmd file (might take some time)" )
  try(render(pathoutput, output_format = outputformat))
  try(system2("open",paste0(isolate(input$exportReportPath), ext)))
  }) #end with progress
})
