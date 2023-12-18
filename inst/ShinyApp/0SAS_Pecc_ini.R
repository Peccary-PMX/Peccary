# ### What data are available with <<-
# #  explo_headerevaled: give utility of each columns (id, DV, MDV...) might not be given
# #  explo
# #  explo_dec
# #  explo_NA
# #  explo_path
# #  explo_sep
# #  subPathPrevious
# #  dossier()
# #  manua function
# #  project_file path of the project
# #  datasets_df list of datasets
#
#
# # Opening actions ---------------------------------------------------------
#
#
# # project <- readRDS("C:/Users/thiba/OneDrive/Documents/testpecc3")

create_pecc_shiny_project <- function(pathtemp = ''){

  path <- pathtemp %>%
    # path <- "\"file:///D:/these/Pecc_test\"" %>%
    gsub(pattern = "(file:///)|(\")", replacement = "") %>%
    gsub(pattern = "\\\\",replacement =  "/")


  projectfile <- list()



  testroot <- str_split(path,pattern =   "/")[[1]]
  projectfile$root <- paste0(testroot[- length(testroot)], collapse = .Platform$file.sep)



  projectfile$path <- path


  # table containing all dataset metadata
  projectfile$datasets <- tibble( n = numeric(), Path = character(),	File = character(),	  commentary =  character(),x = character(), id = character(), 	x_label = character(), y = character(), y_label = character(),
                                  filter = character(), sep = character(), na = character(), dec = character())


  projectfile$exploPplot <-  tibble(Name = character(), preloadeddataset = character(),
                                    FilterexploX = character(), exploY= character(),
                                    Col= character(), Wrap = character(), exploGrid = character(),
                                    exploID = character(),exploPoint= character(),
                                    exploLine= character(), exploStand= character(),
                                    exploXlog= character(), exploYlog = character(),
                                    exploMedian = character(),exploNA = character(),
                                    exploLOQ = character(),scaleExplo = character(),
                                    titleExplo = character(),subtitleExplo = character(),
                                    xlabsExplo = character(),ylabsExplo = character(),
                                    captionExplo = character(),sizeTextExplo= character(),
                                    secondFilter= character(), allbackground= character(),
                                    addlineExplo= character(), all_bkgrdalpha= character())

  projectfile$models <-  list()

  projectfile
}

explo <<- data.frame()
explo_expr <<- character()
explo_headerevaled <<- tibble()
project_file <<- "none"
datasets_df <<- tibble()
project <<- create_pecc_shiny_project('')
#
output$mb_state <- renderRHandsontable({rhandsontable(tibble(Cmt = character(), t0 = character()),
                                                      width = 200, height = 200, rowHeaders = NULL)})

output$mb_paramater <-  renderRHandsontable( {rhandsontable(table_param(),
                                                            width = 200, height = 200, rowHeaders = NULL)})

output$mb_matrix <-  renderRHandsontable( {rhandsontable(tibble(),
                                                            width = 500, height = 200)})

output$mb_event <-  renderRHandsontable( {rhandsontable(table_input(var = "aze") %>% slice(0),
                                                         width = 500, height = 200, rowHeaders = NULL)})

output$mb_display2 <-  renderRHandsontable( {rhandsontable(table_display(possiblevalues = NA_character_) %>% slice(0),
                                                        width = 500, height = 200, rowHeaders = NULL)})

output$mb_output <-  renderRHandsontable( {rhandsontable(
  tibble(output = factor(),
         YTYPE = NA_integer_, err_add = "0.1", err_prop = "0.3", export = T, rm = F),
                                                           width = 500, height = 200, rowHeaders = NULL)})

# if(exists("dossier")){
#   print("iaijuzezaoipeuzapoeiz")
#   if(typeof(dossier) != "closure"){
#     print("iaijuzezaoipeuzapoeiz")
#     rm(dossier)
# }
# }
# Rhandsontable


output$table_forest <- renderRHandsontable({

levels <- c("itself", "cat_fold", "cat_log_fold","cont_linear",  "cont_ref", "IIV_lognormal")
  datainput <- tibble(label = "Uncertainty", parameter = "Cl", value = 3.65, RSE = 20, on = "Cl", method = factor("itself", levels = levels)) %>%
    bind_rows(

  tibble(label = "6_400", parameter = "DOSE", value = 2, RSE = 10, on = "Cl", method = factor("cat_fold", levels = levels)),
  tibble(label = "Age 50", parameter = "Age", value = 0.1, RSE = 10, on = "Cl", method = factor("cont_ref", levels = levels), cont_indiv = 60, cont_ref = 80),
      tibble(label = "IIV", parameter = "IIV", value = 0.1, RSE = 10, on = "Cl", method = factor("IIV_lognormal", levels = levels))

    )

  rhandsontable(datainput %>% mutate(use = T, delete = F), rowHeaders = NULL)

})




# When the app open, load library models from config files (for PeccaReverse section)
updateSelectInput(session, "modelLibInput" , choices = read.csv(file.path(find.package("peccary"), "Librairies_model", "library_peccary.csv" ), header = T, sep = ";")[[1]])

# When the app open, Peccary directrly propose the previous opened project to faciliate the loading
pathprev <- file.path(find.package("peccary"),  "previous_project_opened.txt")
prevpro <- read.table(pathprev,sep = ";", header = T) %>%
  filter(id == as.list(Sys.info())$user)

if(nrow(prevpro) > 0){
  prevpro %>%
    arrange(desc(date)) %>%
    slice(1) %>%
    pull(projectpath) -> lastprevpro
  updateSelectInput(session, inputId =  "recentprojectfile", choices = prevpro$projectpath, selected = lastprevpro)
}


# if(!exists("dossier")) dossier <<- "noYet"

# shinyjs::hide(id = "mb_model_cov")
# shinyjs::hide(id = "helperModel")


# Import function indiv Pred ----------------------------------------------

pathIndivPred <-  file.path(find.package("peccary"), "Add_function", "indiv_plots")

# initialise
add_function <- "Default"

# for loop to add every function
for(a in list.files(pathIndivPred)){
# read file
temp <- readLines(file.path(pathIndivPred, a)) %>%
    gsub(pattern = "#.+", replacement = "")
# take the first declared function
newfunction <- try(temp[grep(".+<- *function\\(", temp)] %>%
  gsub(pattern = " *<-.+", replacement = ""))

if(class(newfunction) != "try-error"){

  if(gsub(".R$","",a) == newfunction) add_function <- c(add_function, newfunction )
}

}

updateSelectInput(session = session, inputId = "whichFunctionPred", choices = add_function)

# previous pro modification
observeEvent(input$recentprojectfile,{

  updateTextInput(session,"Project_file", value = isolate(input$recentprojectfile))

})



observeEvent(input$getpathProject,{
 test <-  file.choose()
updateTextInput(session, 'Project_file_create', value = test)
})

observeEvent(input$getpathDataset,{
  test <-  file.choose()
  updateTextInput(session, 'path_dataset_manual', value = test)
})


observeEvent(input$getpathProjectexisting,{
  print('la')
  test <-  file.choose()
  updateTextInput(session, 'Project_file', value = test)
})




# Project Creation --------------------------------------------------------
# Goal: create a folder with some subfolder, inclunding one containing every stored metadata (dataset pathways, plots, model....)
observeEvent(input$Project_create,{

  cat('Start creation project\n')
  pathtemp<- 'C:/Users/thiba/OneDrive/Documents/peccary_test'
  pathtemp <- isolate(input$Project_file_create)

  # Take the path
  path <- pathtemp %>%
    # path <- "\"file:///D:/these/Pecc_test\"" %>%
    gsub(pattern = "(file:///)|(\")", replacement = "") %>%
    gsub(pattern = "\\\\",replacement =  "/")



  projectfile <- create_pecc_shiny_project(pathtemp)



  # testroot <- str_split(path,pattern =   "/")[[1]]
  # projectfile$root <- paste0(testroot[- length(testroot)], collapse = .Platform$file.sep)
  #
  #
  #
  # projectfile$path <- path
  #
  #
  # # table containing all dataset metadata
  # projectfile$datasets <- tibble( n = numeric(), Path = character(),	File = character(),	  commentary =  character(),x = character(), id = character(), 	x_label = character(), y = character(), y_label = character(),
  #                                 filter = character(), sep = character(), na = character(), dec = character())
  #
  #
  # projectfile$exploPplot <-  tibble(Name = character(), preloadeddataset = character(),
  #                                   FilterexploX = character(), exploY= character(),
  #                                   Col= character(), Wrap = character(), exploGrid = character(),
  #                                   exploID = character(),exploPoint= character(),
  #                                   exploLine= character(), exploStand= character(),
  #                                   exploXlog= character(), exploYlog = character(),
  #                                   exploMedian = character(),exploNA = character(),
  #                                   exploLOQ = character(),scaleExplo = character(),
  #                                   titleExplo = character(),subtitleExplo = character(),
  #                                   xlabsExplo = character(),ylabsExplo = character(),
  #                                   captionExplo = character(),sizeTextExplo= character(),
  #                                   secondFilter= character(), allbackground= character(),
  #                                   addlineExplo= character(), all_bkgrdalpha= character())
  #
  # projectfile$models <-  list()



  updateTextInput(session, inputId = "path_models", value = file.path(path, "0_pecc_project/models.txt"))

  # ## download plots
  # output$datasets <- renderRHandsontable({
  #
  #
  #   datasets_df <- read.table(file.path(path, "/0_pecc_project/datasets.txt"), header = T, sep = ";", stringsAsFactors = F)
  #
  #   rhandsontable(datasets_df, rowHeaders = NULL)
  #
  # })

  create_pecc_shiny_project <- function(pathtemp = ''){

path <- pathtemp %>%
  # path <- "\"file:///D:/these/Pecc_test\"" %>%
  gsub(pattern = "(file:///)|(\")", replacement = "") %>%
  gsub(pattern = "\\\\",replacement =  "/")


projectfile <- list()



testroot <- str_split(path,pattern =   "/")[[1]]
projectfile$root <- paste0(testroot[- length(testroot)], collapse = .Platform$file.sep)



projectfile$path <- path


# table containing all dataset metadata
projectfile$datasets <- tibble( n = numeric(), Path = character(),	File = character(),	  commentary =  character(),x = character(), id = character(), 	x_label = character(), y = character(), y_label = character(),
                                filter = character(), sep = character(), na = character(), dec = character())


projectfile$exploPplot <-  tibble(Name = character(), preloadeddataset = character(),
                                  FilterexploX = character(), exploY= character(),
                                  Col= character(), Wrap = character(), exploGrid = character(),
                                  exploID = character(),exploPoint= character(),
                                  exploLine= character(), exploStand= character(),
                                  exploXlog= character(), exploYlog = character(),
                                  exploMedian = character(),exploNA = character(),
                                  exploLOQ = character(),scaleExplo = character(),
                                  titleExplo = character(),subtitleExplo = character(),
                                  xlabsExplo = character(),ylabsExplo = character(),
                                  captionExplo = character(),sizeTextExplo= character(),
                                  secondFilter= character(), allbackground= character(),
                                  addlineExplo= character(), all_bkgrdalpha= character())

projectfile$models <-  list()

projectfile
}

  # Addition 02/12/2020 : store into
  pathprev <- file.path(find.package("peccary"),  "previous_project_opened.txt")
  prevpro <- read.table(pathprev,sep = ";", header = T)

  prevpro$id <- as.character(prevpro$id) # if empty
  prevpro$projectpath <- as.character(prevpro$projectpath) # if empty
  prevpro$date <- as.character(prevpro$date)


  prevpro <- prevpro %>%
    filter(!(id == as.list(Sys.info())$user & projectpath == path)) %>%
    add_row(id = as.list(Sys.info())$user, projectpath = path,  date = as.character(Sys.time()))

  write.table(prevpro, file = pathprev, quote = F, sep = ";", row.names = F)
  updateSelectInput(session, inputId =  "recentprojectfile", choices = prevpro$projectpath, selected = input$Project_file_create)



  saveRDS(projectfile, path)

  output$messages <- renderMenu({

    dropdownMenu(type = "tasks", .list =   list(taskItem(text = paste0("Project ",gsub(".+/", "", path) ," created & loaded"), value = 100)))
  })


  project <<- projectfile
  showNotification("Project created", type = "message", duration = 3, closeButton = T)

  cat('End creation project\n')
})
#
#
# # project load ------------------------------------------------------------
#
observeEvent(input$Project_load,{


  cat("Load Project")
  project_file <<- isolate(input$Project_file) %>%
    gsub(pattern = "file:///", replacement = "")

  ###

  project <<- readRDS(project_file)

print(project$models)
  datasets_df <- project$datasets

  # Reload list of datasets
  output$datasets <- renderRHandsontable({

    rhandsontable(project$datasets, rowHeaders = NULL)

  })




  if(nrow(datasets_df) > 0 ){
    if("default" %in% names(datasets_df)){


      default <- datasets_df %>%
        filter(default == T) %>%
        pull(n)
      #
      #       datasets_df %>%
      #         select(-default)
    }


    if(length(default) == 0)  default <- 1


    # And put all available in the list of available data
    selectedd <- unique(paste0(datasets_df$n[datasets_df$n == default],":", datasets_df$File[datasets_df$n == default]))

    # make sure all file exists (if some has been removed)
    temp_dataset <- datasets_df %>%
      mutate(test = map2_lgl(Path, File, function(Path, File){

        if(length(grep(":/", Path)) == 0){
          file_temp <- file.path(project_file, Path, File)
        }else{
          file_temp <- file.path(Path, File)
        }

        file.exists(file_temp)

      })) %>%
      filter(test == TRUE)

    updateSelectInput(session, inputId = "preloadeddataset", choices = c("Use external", unique(paste0(datasets_df$n,":", datasets_df$File))), selected = selectedd) # what if several with same name..
  }
  print('That is done !')
  # updateSelectInput(session, inputId = "reportDatasetInfo", choices = c(unique(paste0(datasets_df$n,":", datasets_df$File))))
  # updateTextInput(session, inputId = "path_models", value = file.path(project_file, "0_pecc_project/models.txt"))


  # Put the dataset in preload

 if(!is.null(names(project$models))) updateSelectInput(session, 'names_model', choices = names(project$models), selected = names(project$models)[[1]])

  ####

  if(dir.exists(file.path(project_file, "0_pecc_project"))) {

    # print('here')

    # Addition 02/12/2020 : store into
    pathprev <- file.path(find.package("peccary"),  "previous_project_opened.txt")
    prevpro <- read.table(pathprev,sep = ";", header = T)

    prevpro$id <- as.character(prevpro$id) # if empty
    prevpro$projectpath <- as.character(prevpro$projectpath) # if empty
    prevpro$date <-  as.character(prevpro$date)



    prevpro <- prevpro %>%
      filter(!(id == as.list(Sys.info())$user & projectpath == input$Project_file)) %>%
      add_row(id = as.list(Sys.info())$user, projectpath = input$Project_file, date =  as.character(Sys.time()))

    write.table(prevpro, file = pathprev, quote = F, sep = ";", row.names = F)



    updateSelectInput(session, inputId =  "recentprojectfile", choices = prevpro$projectpath, selected = input$Project_file)

    # print('here')


    dataset_file <- read.table(file.path(project_file, "/0_pecc_project/datasets.txt"), header = T, stringsAsFactors = F)

    # read.table("file:///D:/these/Pecc_test/0_pecc_project/datasets.txt", header = T) %>%
    #   names
    ## setPat run

    updateTextInput(session, inputId = "path2", value = file.path(project_file, "3_Models", "1_Models"))


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
      # download dataset item
      # if(nrow(datasets_df) > 0){
      # output$datasets <- renderRHandsontable({
      #
      #
      #
      #   rhandsontable(datasets_df, rowHeaders = NULL)
      #
      # })}

      # print("test here4")
      ## update preloadeddataset

      selectedd <- unique(paste0(datasets_df$n[datasets_df$n == default],":", datasets_df$File[datasets_df$n == default]))

      # make sure all file exists (if some has been removed)
      temp_dataset <- datasets_df %>%
        mutate(test = map2_lgl(Path, File, function(Path, File){

          if(length(grep(":/", Path)) == 0){
            file_temp <- file.path(project_file, Path, File)
          }else{
            file_temp <- file.path(Path, File)
          }

          file.exists(file_temp)

        })) %>%
        filter(test == TRUE)

      updateSelectInput(session, inputId = "preloadeddataset", choices = c("Use external", unique(paste0(datasets_df$n,":", datasets_df$File))), selected = selectedd) # what if several with same name..
      updateSelectInput(session, inputId = "reportDatasetInfo", choices = c(unique(paste0(datasets_df$n,":", datasets_df$File))))
      updateTextInput(session, inputId = "path_models", value = file.path(project_file, "0_pecc_project/models.txt"))
    }else{

      ### what happen if we have a project without dataset?
      ### for the moment there are no modification
      try( updateTextInput(session, inputId = "path_models", value = file.path(project_file, "0_pecc_project/models.txt"))
      )
    }
    # print("test there")

    ## say okay
    output$messages <- renderMenu({

      dropdownMenu(type = "tasks", .list =   list(taskItem(text = paste0("Project ",gsub(".+/", "", isolate(input$Project_file)) ," loaded"), value = 100)))
    })

    # load exploplot
    try({
      read.table(stringsAsFactors = F, file.path(project_file, "0_pecc_project", "exploPlot.txt"), header = T) %>%
        as_tibble %>%
        mutate(output = paste0(gsub(":.+", "", preloadeddataset), ": ", Name)) %>%
        pull(output) -> choicesPlot

      updateSelectInput(session, "reportPlotExploSelect", choices = choicesPlot)
    })
    # load report
    try({
      temppath <- file.path(project_file, "0_pecc_project", "reports.rds")

      rapports <- try(readRDS(temppath)[[1]])
      if(class(rapports) != "try-error"){

        selected <- NA
        if(length(rapports) > 0) selected <- rapports[[1]]
        updateSelectInput(session, "reportVersion", choices = rapports, selected = rapports[[1]])
      }
    })
    # List model
    try({
      models <-  try(read.table(file.path(project_file, "0_pecc_project","models.txt" ), stringsAsFactors = F) %>% pull(name))


      if(class(models) !="try-error"){
        updateSelectInput(session, inputId = "reportModelEq", choices = c(models[order(models)]))
        updateSelectInput(session, inputId = "reportModelSimul", choices = c(models[order(models)]))
      }
    })

    # List data explo
    try({
      temppath <- file.path(project_file, "0_pecc_project", "dataExplo.rds")

      rapports <- try(readRDS(temppath))
      if(class(rapports) != "try-error"){

        rapports %>%
          mutate(output = paste0(gsub(":.+", "", preloadeddataset), ": ", dataexplonewVersion)) %>%
          pull(output) -> rapports

        rapports <- map(rapports, ~ paste0(.x, c("_table1", "_count"))) %>% reduce(c)
        rapports <- rapports[order(rapports)]
        updateSelectInput(session, "reportDataExploSelect", choices = rapports, selected = NA)
      }
    })
  }else{

    # showNotification("Error: project does not exist", type = "error", closeButton = T, duration = 4 )

  } # end if dir exist
  print('Project load done')
})
#
#
#

# # Find datasets -----------------------------------------------------------
#
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


# guess sep  --------------------------------------------------------------

observeEvent(input$path_dataset_manual, {
  try({
  newpath <- input$path_dataset_manual
  print("Start path dataset manuel")
  print(newpath)
  # newpath <- "file:///D:/Peccary/Exemple_demo/DATA/Simeoni.txt"
  # newpath <- "file:///D:/Peccary/Exemple_demo/DATA/Theoph.txt"
newpath <- gsub("file:///", "", newpath) %>%
    gsub(pattern = "\\\\",  replacement = "/") %>%
    gsub(pattern = "\"",  replacement = "")


  tibble(totry = c(" ", ",", ";", ".")) %>%
    mutate(test = map_dbl(totry, function(x){

      test <- try(length(read.table(newpath, header = T, sep = x,  nrows = 1)), silent = T)
      if(class(test) == "try-error") test <- 0
      test
    })) %>%
    arrange(desc(test)) %>%
    slice(1) %>% pull(totry) -> sep

 updateSelectInput(session, inputId = "sepExplo", selected =  sep)
  }, silent = T)
})

# add temporar dataset ----------------------------------------------------

observeEvent(input$addDataset,{

print('Start add dataset')
  # print("################ Trying to add a dataset #################")
  previous <- try(hot_to_r(isolate(input$datasets)) )

  # print("Previous table:")
  # print(previous)

  newpath <- isolate(input$path_dataset_manual)

  # print("New dataset")
  # print(newpath)
  # print(file.exists(newpath))
  # newpath <- "\"file:///D:/Peccary/Exemple_demo/DATA/Simeoni.txt\""

  # newpath <- "\"D:\\Peccary\\Exemple_demo\\DATA\\Theoph.txt\""
  newpath <- gsub("file:///", "", newpath) %>%
    gsub(pattern = "\\\\",  replacement = "/") %>%
    gsub(pattern = "\"",  replacement = "")

  # print("New dataset after epuration")
  # print(newpath)
  # print(file.exists(newpath))

  # print("Split with /")
  newpathsplit <- str_split(newpath, "/")[[1]]


  sep <- isolate(input$sepExplo)
  # print("Sep")
  # print(sep)

  # print("Try read headerr")
  if(grepl("\\.xlsx?$", newpath)){

    require("readxl")
    headerr <- names(readxl::read_excel(newpath))

  }else{
# print("not a readx!")
    headerr <- names(read.table(newpath, sep = sep,header = T, nrows =  1))
    # print(headerr)
  }



  timehead <- c(headerr[grep("(time)|(^x$)", tolower(headerr))], "")[[1]]
  obshead <- c(headerr[grep("(^obs)|(^dv)|(^y$)|(^conc)", tolower(headerr))], "")[[1]]

  # print("Create the new line")
  new <- tibble(n = 1L,
                Path =  invoke(file.path, newpathsplit[-length(newpathsplit)]),
                File =  newpathsplit[length(newpathsplit)],
                commentary = "",
                x = timehead,
                y = obshead,
                x_label = "",
                y_label = "",
                filter = "",
                sep = isolate(input$sepExplo),
                na = isolate(input$nastringExplo),
                dec = isolate(input$decExplo),
                default = F,
                monolix_header = "",
                id = "")


  if(class(previous) != "try-error" & !is.null(previous)){
    # print("a2")
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

  if(nrow(previous) >0)  new$n <- max(previous$n) + 1L


    bindrowsss <- try( bind_rows(previous, new))

    output$datasets <- renderRHandsontable( rhandsontable( bindrowsss, rowHeaders = NULL))
    # print('checkl project dataset')
    # print(bindrowsss)
    project$datasets <<-   bindrowsss

    choicess <- unique(paste0(bindrowsss$n,":", bindrowsss$File))

    updateSelectInput(session, inputId = "preloadeddataset", choices = choicess) # what if several with same name..

  }else{

    output$datasets <- renderRHandsontable( rhandsontable( new, rowHeaders = NULL))
    project$datasets <<-   new
    choicess <- unique(paste0(new$n,":", new$File))

    selectedd <- choicess[[1]]

    updateSelectInput(session, inputId = "preloadeddataset", choices = c("Use external",choicess), selected = selectedd) # what if several with same name..


  }


  # print('checkl project dataset')
  # print(  project$datasets)
  # print(  hot_to_r(isolate(input$datasets)))

})


# quicklooksave -----------------------------------------------------------


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


  # monolix header

  test <-  tribble(~Name, ~Col,
                   "ID", isolate(input$IDheader),
                   "time", isolate(input$IDheader2),
                   "OBS", isolate(input$OBSheader),
                   "AMT", isolate(input$AMTheader),
                   "RATE", isolate(input$RATEheader),
                   "EVID", isolate(input$EVIDheader),
                   "MDV", isolate(input$MDVheader),
                   "YTYPE", isolate(input$YTYPEheader),
                   "cov.cat", isolate(input$COVCATheader),
                   "cov.cont", isolate(input$COVCONTheader),
                   "ADM", isolate(input$ADMheader)

  )


  express <- test %>%
    filter(Col != "") %>%
    mutate(end = paste0(Col, " = \"",Name, "\"")) %>%
    pull(end) %>%
    paste0(collapse = ", ") %>%
    {paste0("c(", ., ")")}


  if(express == "c()") express <- ""

  if("monolix_header" %in% names(temp) ){

    # print("here")
    temp <- temp %>%
      mutate(monolix_header =   if_else(n == isolate(input$quicklookn),  express, as.character(monolix_header)))

  }else{

    # print("plot")
    # print(expr(c(!!!map(hot_to_r(isolate(input$defineheader)) %>% map_dfr(~ as.character(.x)), ~.x))) %>% deparse)

    temp <- temp %>%
      mutate(monolix_header =   if_else(n == isolate(input$quicklookn),  express, ""))


  }
  # # print("here")
  # mutate(id = if_else(n == isolate(input$quicklookn), isolate(input$quicklookna), id))

  # explo_header
    # hot_to_r(isolate(input$datasets))


  # explo_header <<- expr(c(!!!map(hot_to_r(isolate(input$defineheader)) %>% map_dfr(~ as.character(.x)), ~.x))) %>% deparse(width.cutoff = 500)

  output$datasets <- renderRHandsontable( rhandsontable( temp, rowHeaders = NULL))
  datasets_df <- temp

  showNotification("Do not forget to save the whole table !", type = "message", duration = 3, closeButton = T)

})


# observEvent quicklook ---------------------------------------------------


observeEvent(input$quicklookn,{

  temp <- try(hot_to_r(isolate(input$datasets)) %>%
                filter(n == isolate(input$quicklookn)))



  ### first lines
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

##### monolix header

try({

  ## getting current header
  headerTable <- tibble(name = "", statut = "")
  # datasetdf <- datasets_df %>%
  #   filter(n == 13)
  datasetdf <- try( hot_to_r(isolate(input$datasets)) %>%
                      filter(n == isolate(input$quicklookn)))
  tryeval <- try(eval(parse_expr(as.character(datasetdf$monolix_header))))
  if(class(tryeval) != "try-error"){
  if(!is.na(tryeval)) headerTable <- try(tibble(name = names(tryeval), statut = tryeval))
  }

  ## getting name of columns
  if(length(grep(":/|\\\\", datasetdf$Path)) > 0 ){
    pathfile <- file.path(datasetdf$Path, datasetdf$File)
  }else{
    pathfile <- file.path(project_file,datasetdf$Path, datasetdf$File)

  }

  read.table(nrows = 0,  pathfile, sep = datasetdf$sep, header = T, dec = datasetdf$dec, na.strings = datasetdf$na, stringsAsFactors = F )  %>%
    names -> namescol




  ## let's update !

  updateSelectInput(session, "IDheader", choices = namescol, selected = c(headerTable$name[headerTable$statut == "ID"], NA)[[1]])
  updateSelectInput(session, "IDheader2", choices = namescol, selected = c(headerTable$name[headerTable$statut == "time"], NA)[[1]])
  updateSelectInput(session, "OBSheader", choices = namescol, selected = c(headerTable$name[headerTable$statut == "OBS"], NA)[[1]])
  updateSelectInput(session, "AMTheader", choices = namescol, selected = c(headerTable$name[headerTable$statut == "AMT"], NA)[[1]])
  updateSelectInput(session, "RATEheader", choices = namescol, selected = c(headerTable$name[headerTable$statut == "RATE"], NA)[[1]])
  updateSelectInput(session, "EVIDheader", choices = namescol, selected = c(headerTable$name[headerTable$statut == "EVID"], NA)[[1]])
  updateSelectInput(session, "MDVheader", choices = namescol, selected = c(headerTable$name[headerTable$statut == "MDV"], NA)[[1]])
  updateSelectInput(session, "YTYPEheader", choices = namescol, selected = c(headerTable$name[headerTable$statut == "YTYPE"], NA)[[1]])
  updateSelectInput(session, "COVCATheader", choices = namescol, selected = c(headerTable$name[headerTable$statut == "cov.cat"], NA)[[1]])
  updateSelectInput(session, "COVCONTheader", choices = namescol, selected = c(headerTable$name[headerTable$statut == "cov.cont"], NA)[[1]])
  updateSelectInput(session, "ADMheader", choices = namescol, selected = c(headerTable$name[headerTable$statut == "ADM"], NA)[[1]])

    # unique(headerTable$statut)
  # div(style="display:inline-block; width: 100px;height: 75px;", selectInput(inputId =  "IDheader2", label = "TIME", choices = c("lol","lal"), selected = "lol")),
  # div(style="display:inline-block; width: 100px;height: 75px;", selectInput(inputId = "OBSheader", label = "OBS", choices = c(""), selected = "")),
  # div(style="display:inline-block; width: 100px;height: 75px;", selectInput(inputId = "AMTheader", label = "AMT", choices = c(""), selected = "")),
  # div(style="display:inline-block; width: 100px;height: 75px;", selectInput(inputId = "AMTheader", label = "ADM", choices = c(""), selected = "")),
  # div(style="display:inline-block; width: 100px;height: 75px;", selectInput(inputId = "RATEheader", label = "RATE", choices = c(""), selected = "")),
  # div(style="display:inline-block; width: 100px;height: 75px;", selectInput(inputId = "EVIDheader", label = "EVID", choices = c(""), selected = "")),
  # div(style="display:inline-block; width: 100px;height: 75px;", selectInput(inputId = "MDVheader", label = "MDV", choices = c(""), selected = "")),
  # div(style="display:inline-block; width: 100px;height: 75px;", selectInput(inputId = "YTYPEheader", label = "YTYPE", choices = c(""), selected = "")),
  # div(style="display:inline-block; width: 100px;height: 75px;", selectInput(inputId = "COVCONTheader", label = "cov.cont", choices = c(""), selected = "")),
  # div(style="display:inline-block; width: 100px;height: 75px;", selectInput(inputId = "COVCATheader", label = "cov.cat", choices = c(""), selected = ""))

})
  #
})




# save dataset ------------------------------------------------------------

observeEvent(input$save_datasets,{

  print(hot_to_r(isolate(input$datasets)))
  temp <<- hot_to_r(isolate(input$datasets))


  project$datasets <-  hot_to_r(isolate(input$datasets))


  saveRDS(project, project$path)

  project <<- project
  # if(project_file != "none"){
  # temp <<- hot_to_r(isolate(input$datasets))
  #
  # if(isolate(input$dataset_default) %in% temp$n){
  #
  #
  #   temp <- temp %>%
  #     mutate(default = if_else(n == isolate(input$dataset_default), T , F))
  #
  # }
  #
  #
  #
  # write.table(temp, file = file.path(project_file, "/0_pecc_project/datasets.txt"), row.names = F)
  #
  # datasets_df <<- isolate(hot_to_r(input$datasets))
  #
  # }else{
  #
  #   showNotification("You need first to load / create a project !", type = "error", duration = 4, closeButton = T)
  # }

})

observeEvent(input$datasets,{

  datasets_df <<- hot_to_r(input$datasets)

})
#
# # Pre-loaded dataset for exploration  ---------------------------------------------------------------
#
observeEvent(input$preloadeddataset, {

  # if(exists(project))

  cat('Start Preload dataset')

  preloadeddataset <<- isolate(input$preloadeddataset) %>%
    gsub(pattern = "file:///",  replacement = "") %>%
    gsub(pattern = "\\\\",  replacement = "/") %>%
    gsub(pattern = "\"",  replacement = "")

#
updateTextInput(session, "tableExploManipulation", value = "")
#
# print(  hot_to_r(isolate(input$datasets)))
print(preloadeddataset)
  if(!preloadeddataset %in% c("Use external", ':')){

#
    # try useless because can return a 0xn df
    line <-  try(
      project$datasets  %>%
        filter(n == gsub(":.+", "", preloadeddataset))
    )




print("line")
print(line)

file <- gsub(" \\+ code$","", line$File)

      # if relative path from project
      if(file.exists(file.path(project_file, line$Path, file))){
        explo_path <<- try(file.path(project_file, line$Path, file))
      }else{
      # if absolute path
        explo_path <<- try(file.path(line$Path, file))

      }



#
print("explo_path")
print(explo_path)
print(line)


    explo_na <<- line$na
    explo_sep <<- line$sep
    explo_dec <<- line$dec

    # initialisation before if
print('line')
print(line)
    if("monolix_header" %in% names(line) ){
      if(!is.na(line$monolix_header)){
      if(gsub(" ", "",line$monolix_header) != ""){
      try(explo_header <<- as.character(line$monolix_header))
      try(explo_headerevaled <<- eval(parse_expr(explo_header)))
      }
      }
    }

    if(!exists("explo_header"))explo_header <<- "F"
    if(!exists("explo_headerevaled"))explo_headerevaled <<- "F"
# print("we try exploooo")

    if(grepl("\\.xlsx?$", explo_path)){

      require("readxl")
      explo <<- try(readxl::read_excel(explo_path))

    }else{

    explo <<- try(read.table( explo_path, header = T, na.strings = explo_na, sep = explo_sep, dec = explo_dec))

    }

    # Perfom modification if needed

    if(grepl(" \\+ code", preloadeddataset)){


    explo <<-
      try(eval(parse_expr(line$codeToEval)))

    }

# print(head(explo))
    # print(explo)

    if(class(explo) != "try-error"){

      if(!grepl("\\+ code$", line$File)){

        explo_expr <<- expr(explo <- read.table(file = !!explo_path, header = T, na.strings = !!explo_na, sep = !!explo_sep, dec = !!explo_dec))

      }else{

        explo_expr <<-   expr(explo <- {explo <- read.table(file = !!explo_path, header = T, na.strings = !!explo_na, sep = !!explo_sep, dec = !!explo_dec)
        !!!parse_exprs(line$codeToEval)})



      }



      # updateTextInput(session, "pathExplo", value = path)
      updateSelectInput(session, "exploY", choices = c("", names(explo)), selected = line$y)
      updateSelectInput(session, "exploX", choices = c("", names(explo)), selected = line$x)
      updateTextAreaInput(session, "filterrExplo", value  = line$filter)
      # updateSelectInput(session, "sepExplo",  choices = c("Space" = "", ";", ".", ","), selected = line$sep)
      # updateSelectInput(session, "decExplo", choices = c(".", ","), selected = line$dec)
      # updateTextInput(session, "nastringExplo", value  = line$na)
      updateSelectInput(session, "exploID", choices = c("", names(explo)), selected = names(explo)[c(grep("(id)|(subj)", tolower(names(explo))),1)[[1]]])

      updateSelectInput(session, "exploCol", choices = c("", names(explo)), selected = "")
      updateSelectInput(session, "exploWrap", choices = c("", names(explo)), selected = "")
      updateSelectInput(session, "exploGrid", choices = c("", names(explo)), selected = "")
      updateSelectInput(session, "exploStand", choices = c("", names(explo)), selected = "")
      updateSelectInput(session, "exploShape", choices = c("", names(explo)), selected = "")
      updateSelectInput(session, "exploLty", choices = c("", names(explo)), selected = "")
      updateSelectInput(session, "covNCA", choices = c("", names(explo)), selected = "")
      updateSelectInput(session, "groupNCA", choices = c("", names(explo)), selected = names(explo)[c(grep("(id)|(subj)", tolower(names(explo))),1)[[1]]])
         updateSelectInput(session, "blqNCA", choices = c("", names(explo)), selected =   names(explo)[c(grep("blq", tolower(names(explo))),NA)[[1]]])
      # updateSelectInput(session, "evidNCA", choices = c("", names(explo)), selected =   names(explo)[c(grep("evid", tolower(names(explo))),NA)[[1]]])

       updateSelectInput(session, "pknca_id", choices = c( names(explo)), selected = names(explo)[c(grep("(id)|(subj)", tolower(names(explo))),1)[[1]]])
      updateSelectInput(session, "pknca_cov", choices = c("", names(explo)), selected = "")
      updateSelectInput(session, "pknca_rateduration", choices = c("", names(explo)), selected = "")
      updateSelectInput(session, "pknca_dose", choices = c("", names(explo)), selected = names(explo)[c(grep("(amt)|(dose)", tolower(names(explo))),1)[[1]]])
      updateSelectInput(session, "pknca_ADM", choices = c("Single dose time 0", names(explo)), selected = names(explo)[c(grep("evid", tolower(names(explo))),1)[[1]]])

    }


  }
# #
#   # pre_loaded plots
  cat("preloadPlot\n")
#
#   path_temp <- try(file.path(project_file,"0_pecc_project/exploPlot.txt"))
#


 if(exists('project')){
  # if(class(previous) != "try-error"){
   previous <- try(project$exploPplot)

    if(nrow(previous) > 0){
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
 }
  cat("end preloadPlot\n")
  # }
#
#   # end function
#   # print("preloadNCA")
  # path_temp2 <- try(file.path(project_file,"0_pecc_project/NCA.txt"))

  # previous2 <- project$NCA
  #
  # if(!is.null(previous2)){
  #   # print("tesst")
  #   previous2 %>%
  #     filter(preloadeddataset == isolate(input$preloadeddataset)) %>%
  #     select(Name, Filter) %>%
  #     mutate(Load = F) %>%
  #     mutate(pdf = F) %>%
  #     mutate(Name = as.character(Name)) %>%
  #     mutate(Filter = as.character(Filter)) %>%
  #     select(Load, pdf, everything())-> outpuutt2
  #   # print(outpuutt)
  #   output$NCASaved <- renderRHandsontable(rhandsontable(outpuutt2,rowHeaders = NULL))
  # }
#   # print("hereendokays")
#
#
#   ### the dataset observation thing
#
  try(
    output$tableexplo <- DT::renderDataTable({

      # print("tableExplo")
      if(isolate(input$filterrExplo) == "" | is.na(isolate(input$filterrExplo))){

        temp <- explo
      }else{

        temp <- explo %>%
          filter_(isolate(input$filterrExplo))

      }

      if(!(isolate(input$filtertableexplo) == "" | is.na(isolate(input$filtertableexplo)))){

        temp <- temp %>%
          filter_(isolate(input$filtertableexplo))
      }

      if(!(isolate(input$tableExploManipulation) == "" | is.na(isolate(input$tableExploManipulation)))){

        code <- paste0("temp <- {temp\n", isolate(input$tableExploManipulation),"}") %>%
          gsub(pattern = "\n *%>%", replacement = "%>%")

        # print(code)
        eval(parse_expr(code))

        # eval(parse_expr(paste0("temp", test)))
        # # eval(parse_expr)
        #   test <- " %>% filter(mpg >3) %>% \n select(wt, qsec)"
      }

      if(length(isolate(input$groupbyCovExplo)) > 0){


        temp <- pecc_search_cov(temp, isolate(input$groupbyCovExplo))


      }

      updateSelectInput(session, "groupbyExplo", choices = names(temp),selected = NA)
      updateSelectInput(session, "groupbyCovExplo", choices = names(temp),selected = NA)
      updateSelectInput(session, "table1reduceBy", choices = names(temp),selected = NA)
      updateSelectInput(session, "table1x", choices = c("All", names(temp)),selected = "All")
      updateSelectInput(session, "table1y", choices = names(temp),selected = NA)
    return(temp)

    }, options = list(pageLength = 10, scrollX = TRUE))

  )


 updateSelectInput(session, 'names_model', choices = names(project$models))
#
#   # load data exploration
#
  # temppath <- project$datasets # file.path(project_file, "0_pecc_project", "dataExplo.rds")
  #
  # rapports <- try(readRDS(temppath))
  # if(class(rapports) != "try-error"){
  #
  #   rapports  %>%
  #     filter(preloadeddataset == isolate(input$preloadeddataset) ) %>%
  #     pull(dataexplonewVersion) -> rapports
  #
  #   if(length(rapports) > 0){
  #   selected <- NA
  #   if(length(rapports) > 0) selected <- rapports[[1]]
  #   updateSelectInput(session, "dataexploVersion", choices = rapports, selected = rapports[[1]])
  #   }else{
  #
  #     updateSelectInput(session, "dataexploVersion", choices = "", selected = "")
  #
  #   }
  #
  #
  # }
#
print('end preloaddataset')
})
# # #
