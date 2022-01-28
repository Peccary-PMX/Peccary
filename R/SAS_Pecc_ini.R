
explo_headerevaled <<- tibble()

# Project Creation --------------------------------------------------------

observeEvent(input$Project_create,{


  # Take the path
  path <- isolate(input$Project_file_create) %>%
    # path <- "file:///D:/these/Pecc_test" %>%
    gsub(pattern = "file:///", replacement = "")


  dir.create(path)
  dir.create(file.path(path, "1_Data"))
  dir.create(file.path(path, "1_Data/1_as_received"))
  dir.create(file.path(path, "1_Data/2_manually_modified"))
  dir.create(file.path(path, "1_Data/3_final"))

  dir.create(file.path(path, "2_Analysis"))
  dir.create(file.path(path, "2_Analysis/1_Graphical_Exploratorion"))
  dir.create(file.path(path, "2_Analysis/2_NCA"))
  dir.create(file.path(path, "2_Analysis/3_Model_conception"))

  dir.create(file.path(path, "3_Models"))

  dir.create(file.path(path, "0_pecc_project"))


  write.table(x = tibble( n = numeric(), Path = character(),	File = character(),	  commentary =  character(),x = character(), id = character(), 	x_label = character(), y = character(), y_label = character(),
                          filter = character(), sep = character(), na = character(), dec = character()),
              file = file.path(path, "0_pecc_project/datasets.txt"), row.names = F, quote = F, sep = ";")


  updateTextInput(session, inputId = "path_models", value = file.path(path, "0_pecc_project/models.txt"))

  ## download plots
  output$datasets <- renderRHandsontable({


    datasets_df <- read.table(file.path(path, "/0_pecc_project/datasets.txt"), header = T, sep = ";", stringsAsFactors = F)

    rhandsontable(datasets_df, rowHeaders = NULL)

  })




  output$messages <- renderMenu({

    dropdownMenu(type = "tasks", .list =   list(taskItem(text = paste0("Project ",gsub(".+/", "", path) ," created & loaded"), value = 100)))
  })





})


# project load ------------------------------------------------------------


observeEvent(input$Project_load,{

  # print("projectload")
  project_file <<- isolate(input$Project_file) %>%
    gsub(pattern = "file:///", replacement = "")

  # print("test here")
  dataset_file <- read.table(file.path(project_file, "/0_pecc_project/datasets.txt"), header = T)

  ## setPat run

  updateTextInput(session, inputId = "path2", value = file.path(project_file, "3_Models/1_Models"))


  # print(dataset_file)
  if(nrow(dataset_file) > 0 ){
    if("default" %in% names(dataset_file)){


      default <- dataset_file %>%
        filter(default == T) %>%
        pull(n)

      dataset_file %>%
        select(-default)
    }else{
      default <- 1
    }


    # print("test here2")
    updateNumericInput(session, inputId = "dataset_default", value = default)
    # print("test here3")


    datasets_df <<- dataset_file %>%
      mutate(n = as.integer(n),
             x = as.character(x),
             x_label = as.character(x_label),
             y = as.character(y),
             y_label = as.character(y_label),
             commentary = as.character(commentary),
             filter = as.character(filter),
             sep = as.character(sep),
             na = as.character(na),
             dec = as.character(dec))
    # print("test here4")
    ## download dataset item
    output$datasets <- renderRHandsontable({



      rhandsontable(datasets_df, rowHeaders = NULL)

    })

    # print("test here4")
    ## update preloadeddataset

    selectedd <- unique(paste0(datasets_df$n[datasets_df$n == default],":", datasets_df$File[datasets_df$n == default]))

    updateSelectInput(session, inputId = "preloadeddataset", choices = c("Use external", unique(paste0(datasets_df$n,":", datasets_df$File))), selected = selectedd) # what if several with same name..
    updateTextInput(session, inputId = "path_models", value = file.path(project_file, "0_pecc_project/models.txt"))
  }
  # print("test there")

  ## say okay
  output$messages <- renderMenu({

    dropdownMenu(type = "tasks", .list =   list(taskItem(text = paste0("Project ",gsub(".+/", "", isolate(input$Project_file)) ," loaded"), value = 100)))
  })

  # print("here okay")

})



# Find datasets -----------------------------------------------------------

observeEvent(input$load_data_folders,{

  # print("over here ! ")
  previous <- try(hot_to_r(isolate(input$datasets)) )

  # print(previous)

  if(class(previous) != "try-error"){

    previous <- previous %>%
      mutate(n = as.integer(n),
             x = as.character(x),
             x_label = as.character(x_label),
             y = as.character(y),
             y_label = as.character(y_label),
             commentary = as.character(commentary),
             filter = as.character(filter),
             sep = as.character(sep),
             na = as.character(na),
             dec = as.character(dec))


  }else{

    previous <- tibble(File = "", n = 0L)
  }

  # print("there")
  all_files_scanned <-   tibble(Path = c("1_Data/1_as_received", "1_Data/2_manually_modified", "1_Data/3_final")) %>%
    mutate(File = map(Path, ~  list.files(file.path(project_file, .x)))) %>%
    unnest(File)



  if(nrow(all_files_scanned) > 0 ){
    all_files_scanned <- all_files_scanned %>%
      filter(!(File %in% unique(previous$File))) %>%
      rownames_to_column("n") %>%
      mutate(n = as.integer(n) + if_else(nrow(previous) == 0, 0L, max(previous$n))) %>%
      mutate(x = " ") %>%
      mutate(x_label= " ") %>%
      mutate(y = " ") %>%
      mutate(y_label= " ") %>%
      mutate(commentary = " ") %>%
      mutate(sep = ";") %>%
      mutate(dec = ".") %>%
      mutate(na = ".") %>%
      mutate(id = " ") %>%
      mutate(commentary = " ")

  }

  # print(all_files_scanned)

  if(previous$File[[1]] == ""){

    bindrowsss <- all_files_scanned

  }else{


    bindrowsss <- try( bind_rows(previous, all_files_scanned))

  }


  if(class(bindrowsss) != "try-error"){

    if(nrow(bindrowsss)>0)output$datasets <- renderRHandsontable( rhandsontable(bindrowsss, rowHeaders = NULL))
  }




})


# add temporar dataset ----------------------------------------------------

# observeEvent(input$addDataset,{
#
#   previous <- try(hot_to_r(isolate(input$datasets)) )
#
#   if(class(previous) != "try-error"){
#
#     previous <- previous %>%
#       mutate(n = as.integer(n),
#              x = as.character(x),
#              x_label = as.character(x_label),
#              y = as.character(y),
#              y_label = as.character(y_label),
#              commentary = as.character(commentary),
#              filter = as.character(filter),
#              sep = as.character(sep),
#              na = as.character(na),
#              dec = as.character(dec))
#
#
#   }else{
#
#     previous <- tibble(n = 1, File = "", n = 0L)
#   }
#
#   bindrowsss <- try( bind_rows(previous, all_files_scanned))
#
# })

# quicklook dataset -------------------------------------------------------

observeEvent(input$quicklookload,{



  datasetdf <- try( hot_to_r(isolate(input$datasets)) %>%
                      filter(n == isolate(input$quicklookn))
  )

  todo <- F
  ### is there already header file?
  # print("quoii")
  if("monolix_header" %in% names(datasetdf)){


    lineheader <- datasetdf %>% pull(monolix_header) %>% as.character()

    # lineheader <- expr(c(ID = "drop", TIME = "drop", TIME_AFTER_UCART_DOSE = "drop", DV = "drop", BLQ = "drop", AMT = "drop", EVID = "drop", MDV = "drop", CMT = "drop", SS = "drop", RATE = "drop", DOSE_ALEM = "drop", DOSE_ALEM_KG_JOUR = "drop", DOSE_ALEM_KG = "drop", DL_UCART = "drop", WT = "drop", HT = "drop", AGE = "drop", SEX = "drop", RACE = "drop", BSA = "drop", CREA = "drop", CRCL = "drop", Lymphocytes_avant_depletion = "drop", CD52 = "drop", Blastes2 = "drop")) %>%
    # deparse(width.cutoff = 500)
    ## if we have some information
    if(  !(gsub(" ", "",lineheader) == "" | is.na(lineheader)) ){




      gsub("^c","data.frame",lineheader) %>%  parse_expr() %>% eval -> temp     # output$defineheader <- renderRHandsontable(rhandsontable(temp))

      #making sure there is no new lines

      filepath <- file.path(project_file, datasetdf$Path, datasetdf$File)

      read.table(filepath, sep = isolate(input$quicklooksep), nrows = 1, header = T) %>%
        names -> checknewcol

      read.table("file:///D:/these/Pecc_test/1_Data/3_final/main_dataset_extended.data",sep = ";", nrows = 1, header = T) -> checknewcol

      for(a in names(checknewcol)){

        tryprevious <- temp[[a]]
        if(length(tryprevious) > 0){

          checknewcol[a] <- tryprevious

        }else{

          checknewcol[a] <- ""

        }


      }

      # checknewcol[!checknewcol%in% names(temp)] -> newcols
      #
      #  if(length(newcols) > 0){
      #
      #   for(a in newcols) temp[a] <- "drop"
      #
      #  }

      temp <- checknewcol %>% #temp
        map_df(function(x) x= factor(x, levels = c("drop", "ID", "time", "OBS", "AMT", "ADM","rate", "BLQ","EVID", "MDV", "YTYPE", "cov.cont", "cov.cat")))


      output$defineheader <- renderRHandsontable(rhandsontable(temp,rowHeaders = F,height = 500))
    }else{

      todo <- T

    }

  }
  ### else do differently....
  #
  # print("quoiié")
  if((!"monolix_header" %in% names(datasetdf)) | todo == T){
    # print("quoiié")
    testpath <- try(datasetdf %>%
                      mutate(filepath = map2(Path, File, ~ file.path(project_file, .x, .y)))
                    %>%
                      pull(filepath)
    )



    if(class(testpath) != "try-error") testpath <- try(

      testpath <- read.table(testpath[[1]], header = T, sep = isolate(input$quicklooksep), dec =  isolate(input$quicklookdec), na.strings = isolate(input$quicklookna))

    )



    if(class(testpath) != "try-error"){
      output$quicklookTable <- DT::renderDataTable(testpath)




      testpath %>%
        slice(1) %>%
        map_df(function(x) x= factor("drop", levels = c("drop", "ID", "time", "OBS", "AMT", "ADM","rate", "BLQ","EVID", "MDV", "YTYPE", "cov.cont", "cov.cat")))  -> temp
      # output$defineheader <- renderRHandsontable(rhandsontable(temp))

      output$defineheader <- renderRHandsontable(rhandsontable(temp,rowHeaders = F,height = 500))
    }







  }







})

### save

observeEvent(input$quicklooksave,{


  # print("here")
  temp <- hot_to_r(isolate(input$datasets)) %>%
    mutate(x = if_else(n == isolate(input$quicklookn), isolate(input$quicklookx), x)) %>%
    mutate(x_label= if_else(n == isolate(input$quicklookn), isolate(input$quicklookxlabel), x_label)) %>%
    mutate(y = if_else(n == isolate(input$quicklookn), isolate(input$quicklooky), y)) %>%
    mutate(y_label= if_else(n == isolate(input$quicklookn), isolate(input$quicklookylabel), y_label)) %>%
    mutate(commentary = if_else(n == isolate(input$quicklookn), isolate(input$quicklookcomment), commentary)) %>%
    mutate(sep = if_else(n == isolate(input$quicklookn), isolate(input$quicklooksep), sep)) %>%
    mutate(dec = if_else(n == isolate(input$quicklookn), isolate(input$quicklookdec), dec)) %>%
    mutate(na = if_else(n == isolate(input$quicklookn), isolate(input$quicklookna), na))

  express <- expr(c(!!!map(hot_to_r(isolate(input$defineheader)) %>% map_dfr(~ as.character(.x)), ~.x))) %>% deparse(width.cutoff = 500) %>%
    paste(collapse = " ")
  # print("here")
  # print(express)
  if("monolix_header" %in% names(temp) ){

    # print("here")
    temp <- temp %>%
      mutate(monolix_header =   if_else(n == isolate(input$quicklookn),  express, as.character(monolix_header)))

  }else{

    # print("plot")
    # print(expr(c(!!!map(hot_to_r(isolate(input$defineheader)) %>% map_dfr(~ as.character(.x)), ~.x))) %>% deparse)

    temp <- temp %>%
      mutate(monolix_header =   if_else(n == isolate(input$quicklookn),  express, " "))


  }
  # print("here")
  # mutate(id = if_else(n == isolate(input$quicklookn), isolate(input$quicklookna), id))


  explo_header <<- expr(c(!!!map(hot_to_r(isolate(input$defineheader)) %>% map_dfr(~ as.character(.x)), ~.x))) %>% deparse(width.cutoff = 500)

  output$datasets <- renderRHandsontable( rhandsontable( temp, rowHeaders = NULL))

})


observeEvent(input$quicklookn,{

  temp <- try(hot_to_r(isolate(input$datasets)) %>%
                filter(n == isolate(input$quicklookn)))


  #
  if(class(temp) != "try-error"){

    if(nrow(temp) >= 1){
      updateSelectInput(session, inputId =  "quicklooksep",  selected = temp$sep)
      updateSelectInput(session, inputId =  "quicklookdec",  selected = temp$dec)
      updateTextInput(session, inputId = "quicklookna", value = temp$na)
      updateTextInput(session, inputId = "quicklookx", value = temp$x)
      updateTextInput(session, inputId = "quicklookxlabel", value = temp$x_label)
      updateTextInput(session, inputId = "quicklooky", value = temp$y)
      updateTextInput(session, inputId = "quicklookylabel", value = temp$y_label)
      updateTextInput(session, inputId = "quicklookcomment", value = temp$commentary)
    }
  }else{

    updateSelectInput(session, inputId =  "quicklooksep",  selected = ";")
    updateSelectInput(session, inputId =  "quicklookdec",  selected = ".")
    updateTextInput(session, inputId = "quicklookna", value = ".")
    updateTextInput(session, inputId = "quicklookx", value = "")
    updateTextInput(session, inputId = "quicklookxlabel", value = "")
    updateTextInput(session, inputId = "quicklooky", value = "")
    updateTextInput(session, inputId = "quicklookylabel", value = "")
    updateTextInput(session, inputId = "quicklookcomment", value = "")

  }


  #
})




# save dataset ------------------------------------------------------------

observeEvent(input$save_datasets,{


  temp <- hot_to_r(isolate(input$datasets))

  if(isolate(input$dataset_default) %in% temp$n){


    temp <- temp %>%
      mutate(default = if_else(n == isolate(input$dataset_default), T , F))

  }



  write.table(temp, file = file.path(project_file, "/0_pecc_project/datasets.txt"), row.names = F)

  datasets_df <<- isolate(input$datasets)

})


# Pre-loaded dataset for exploration  ---------------------------------------------------------------

observeEvent(input$preloadeddataset, {
  # print("preloadeddataset")
  if(isolate(input$preloadeddataset) != "Use external"){




    line <-  try(
      datasets_df %>%
        filter(n == gsub(":.+", "", isolate(input$preloadeddataset)))
    )



    if(class(line)!= "try-error")  explo_path <<- try(file.path(project_file, line$Path, line$File))


    explo_na <<- line$na
    explo_sep <<- line$sep
    explo_dec <<- line$dec


    if("monolix_header" %in% names(line)){

      explo_header <<- as.character(line$monolix_header)
      explo_headerevaled <<- eval(parse_expr(explo_header))

    }else{


      explo_header <<- "F"
      explo_headerevaled <<- "F"
    }



    explo <<- try(read.table( explo_path, header = T, na.strings = explo_na, sep = explo_sep, dec = explo_dec))

    # print(explo)

    if(class(explo) != "try-error"){

      # updateTextInput(session, "pathExplo", value = path)
      updateSelectInput(session, "exploY", choices = c("", names(explo)), selected = line$y)
      updateSelectInput(session, "exploX", choices = c("", names(explo)), selected = line$x)
      updateTextAreaInput(session, "filterrExplo", value  = line$filter)
      # updateSelectInput(session, "sepExplo",  choices = c("Space" = "", ";", ".", ","), selected = line$sep)
      # updateSelectInput(session, "decExplo", choices = c(".", ","), selected = line$dec)
      # updateTextInput(session, "nastringExplo", value  = line$na)
      updateSelectInput(session, "exploCol", choices = c("", names(explo)), selected = "")
      updateSelectInput(session, "exploWrap", choices = c("", names(explo)), selected = "")
      updateSelectInput(session, "exploGrid", choices = c("", names(explo)), selected = "")
      updateSelectInput(session, "exploStand", choices = c("", names(explo)), selected = "")
      updateSelectInput(session, "covNCA", choices = c("", names(explo)), selected = "")
      updateSelectInput(session, "exploID", choices = c("", names(explo)), selected =  c(names(explo)[grep("ID",names(explo))], names(explo))[[1]])

    }


  }else{

    explo_path <<- isolate(input$path_dataset_manual)
    explo_na <<- isolate(input$nastringExplo)
    explo_sep <<- isolate(input$sepExplo)
    explo_dec <<- isolate(input$decExplo)

    explo <<- try(read.table( explo_path, header = T, na.strings = explo_na, sep = explo_sep, dec = explo_dec))


    # print("hereeeeeeee")

    if(class(explo) != "try-error"){

      # updateTextInput(session, "pathExplo", value = path)
      updateSelectInput(session, "exploY", choices = c("", names(explo)), selected = names(explo)[1])
      updateSelectInput(session, "exploX", choices = c("", names(explo)), selected = names(explo)[1])
      updateTextAeraInput(session, "filterrExplo", value  = line$filter)
      # updateSelectInput(session, "sepExplo",  choices = c("Space" = "", ";", ".", ","), selected = line$sep)
      # updateSelectInput(session, "decExplo", choices = c(".", ","), selected = line$dec)
      # updateTextInput(session, "nastringExplo", value  = line$na)
      updateSelectInput(session, "exploCol", choices = c("", names(explo)), selected = "")
      updateSelectInput(session, "exploWrap", choices = c("", names(explo)), selected = "")
      updateSelectInput(session, "exploGrid", choices = c("", names(explo)), selected = "")
      updateSelectInput(session, "exploStand", choices = c("", names(explo)), selected = "")
      updateSelectInput(session, "covNCA", choices = c("", names(explo)), selected = "")
      updateSelectInput(session, "exploID", choices = c("", names(explo)), selected =  c(names(explo)[grep("ID",names(explo))], names(explo))[[1]])

    }

  }


  # pre_loaded plots
  print("preloadPlot")

  path_temp <- try(file.path(project_file,"0_pecc_project/exploPlot.txt"))

  previous <- try(read.table(path_temp, header = T, na.strings = "NA"))

  if(class(previous) != "try-error"){

    previous %>%
      filter(preloadeddataset == isolate(input$preloadeddataset)) %>%
      select(Name, Filter, Col, Wrap) %>%
      mutate(Load = F) %>%
      mutate(pdf = F) %>%
      mutate(Name = as.character(Name)) %>%
      mutate(Filter = as.character(Filter)) %>%
      mutate(Col = as.character(Col)) %>%
      mutate(Wrap = as.character(Wrap)) %>%
      select(Load, pdf, everything())-> outpuutt



    output$PlotexplorationSaved <- renderRHandsontable(rhandsontable(outpuutt,rowHeaders = NULL))


  }

  # end function
  # print("preloadNCA")
  path_temp2 <- try(file.path(project_file,"0_pecc_project/NCA.txt"))

  previous2 <- try(read.table(path_temp2, header = T, na.strings = "NA"))

  if(class(previous2) != "try-error"){
    # print("tesst")
    previous2 %>%
      filter(preloadeddataset == isolate(input$preloadeddataset)) %>%
      select(Name, Filter) %>%
      mutate(Load = F) %>%
      mutate(pdf = F) %>%
      mutate(Name = as.character(Name)) %>%
      mutate(Filter = as.character(Filter)) %>%
      select(Load, pdf, everything())-> outpuutt2
    # print(outpuutt)
    output$NCASaved <- renderRHandsontable(rhandsontable(outpuutt2,rowHeaders = NULL))
  }
  # print("hereendokays")
})
#
