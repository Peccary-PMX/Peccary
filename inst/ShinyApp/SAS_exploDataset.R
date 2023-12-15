# Function for applying modification

pecc_new_table <- function(fortable1 = F){

  ## Step1: compute the new table with all the modification
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
  searchcovby <- isolate(input$groupbyCovExplo)


  if(fortable1 == T){

    table1reduceBy <-  isolate(input$table1reduceBy)
    table1reduceBy <- table1reduceBy[table1reduceBy != ""]
    table1reduceBy <- table1reduceBy[!is.na(table1reduceBy)]
  if(length(table1reduceBy) > 0 )  searchcovby <- c(searchcovby,table1reduceBy ) %>% unique

  }


  if(length(searchcovby) > 0){


    temp <- pecc_search_cov(temp, !!!parse_exprs(searchcovby))


  }

  return(temp)
}




modifExpr <- function(fortable1 = F){

  original <- explo_path

  modif <- expr(explo)

  if(!(isolate(input$filterrExplo) == "" | is.na(isolate(input$filterrExplo)))){


    modif <- expr(!!modif %>%
                    filter_(!!isolate(input$filterrExplo)))

  }


  # print("aaaa")

  if(!(isolate(input$filtertableexplo) == "" | is.na(isolate(input$filtertableexplo)))){


    modif <- expr(!!modif %>%
                    filter_(!!isolate(input$filtertableexplo)))

  }
  # print("bbb")
  if(!(isolate(input$tableExploManipulation) == "" | is.na(isolate(input$tableExploManipulation)))){

    modif <- expr(!!modif -> temp)


    modif <- expr({!!modif
      !!!parse_exprs(isolate(input$tableExploManipulation))
    })


  }


  # print("cccc")

  searchcovby <- isolate(input$groupbyCovExplo)


# print("here")
# print(searchcovby)
  if(fortable1 == T){
    table1reduceBy <-  isolate(input$table1reduceBy)
    table1reduceBy <- table1reduceBy[table1reduceBy != ""]
    table1reduceBy <- table1reduceBy[!is.na(table1reduceBy)]
    if(length(table1reduceBy) > 0 )  searchcovby <- c(searchcovby,table1reduceBy ) %>% unique
  }

  if(length(searchcovby) > 0){
    # print("here")

    modif <-  expr(pecc_search_cov(!!modif , !!!parse_exprs(searchcovby), returnExp = T))

    modif <- eval(modif)

  }

  # print("ddd")
  return(modif)

}


# save report -------------------------------------------------------------


observeEvent(input$dataexploSave, {

  if(project_file != "none"){

  path <- file.path(project_file, "0_pecc_project", "dataExplo.rds")
  name <- isolate(input$dataexplonewVersion)

  new <- tibble(
    dataexplonewVersion = isolate(input$dataexplonewVersion),
    filtertableexplo = isolate(input$filtertableexplo),
    tableExploManipulation = isolate(input$tableExploManipulation),
    groupbyCovExplo = list(isolate(input$groupbyCovExplo)),
    table1x = list(isolate(input$table1x)),
    table1y = isolate(input$table1y),
    countwhat = isolate(input$countwhat),
    countx = isolate(input$countx),
    county = isolate(input$county),
    preloadeddataset = isolate(input$preloadeddataset),
    filterrExplo = isolate(input$filterrExplo)

)


  sepp <- ";"
  previous <- try(readRDS(path))


  if(class(previous) != "try-error"){
    new <- bind_rows(new, previous %>%
                       filter(dataexplonewVersion != name)
    )}

  sepp <- ";"


  saveRDS(new, path)

  updateSelectInput(session, "dataexploVersion", choices = new$dataexplonewVersion, selected = name)
  }else{

    showNotification("You need first to load / create a project !", type = "error", duration = 4, closeButton = T)

  }


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
}
)


# load report -------------------------------------------------------------


observeEvent(input$dataexploLoad, {

  selected <- isolate(input$dataexploVersion)
  # selected <- "test"
  path <- file.path(project_file, "0_pecc_project", "dataExplo.rds")
# print(selected)
  readRDS(path) %>%
    as_tibble %>%
    filter( dataexplonewVersion == selected) -> line


  # les avant go

  updateSelectInput(session, "preloadeddataset", selected = line$preloadeddataset)
  updateTextInput(session, "filterrExplo", value = line$filterrExplo)
  updateTextInput(session, "filtertableexplo", value = line$filtertableexplo)
  updateTextAreaInput(session, "tableExploManipulation", value = line$tableExploManipulation)
  updateSelectInput(session, "groupbyCovExplo", selected = line$groupbyCovExplo)



  # table1x = list(isolate(line$table1x)),
  # table1y = isolate(line$table1y),
  # countwhat = isolate(line$countwhat),
  # countx = isolate(input$countx),
  # county = isolate(input$county),

test <- try({
  pecc_new_table()

  })

if(class(test) != "try-error"){
  output$tableexplo <- DT::renderDataTable({

        test
      }, options = list(pageLength = 10, scrollX = TRUE))

}


temp <- test
temp2 <- test
# print("here")
testt <- which(names(test) == isolate(line$groupbyCovExplo))
if(length(testt) > 0 ) temp2 <- temp2[-testt]
imap_chr(temp2, function(x,y){

  if(is.factor(x)) x <- as.character(x)

  sum(is.na(x)) -> nna1
  sum(is.na(as.double(x))) -> nna2

  if(nna1 == nna2 ) return(y)
  return(NA)

}) -> namescont

# print("here2")
c( paste0(  names(temp2)[!is.na(namescont)], c("_cont")),
   paste0(  names(temp2)[!is.na(namescont)], c("_cat")),
   paste0(names(temp2)[is.na(namescont)], c("_cat")))  -> choicesx

choicesx <- choicesx[order(choicesx)]

# print("here3")
updateSelectInput(session, "table1x", choices = choicesx, selected = line$table1x[[1]])
updateSelectInput(session, "table1y", choices = names(temp2)[order(names(temp2))], selected = line$table1y)
#

# print("here4")
imap_chr(temp, function(x,y){
  # print(y)
  unique(x) -> uniquex
  # uniquex <- c(0,1,NA,2)
  length(uniquex[!uniquex %in% c(0,1)&!is.na(uniquex)]) -> testwrongvalue

  if(testwrongvalue>0) return(NA)

  if(sum(uniquex %in% c(0,1)) >0) return(y)
  return(NA)

}) -> boollist

boollist <- boollist[!is.na(boollist)] %>% unname

# print("here5")
updateSelectInput(session, "countx", choices =  names(temp2)[order(names(temp2))], selected = line$countx)
updateSelectInput(session, "county", choices = names(temp2)[order(names(temp2))], selected = line$county)
updateSelectInput(session, "countwhat", choices = boollist[order(boollist)], selected = line$countwhat)
#
#
#
# updateSelectInput(session, "table1x", selected = line$table1x[[1]])




  })


# Apply modification-----------------------------------------------------------
observeEvent(input$abtableexplo,{

  # print("here")
try(
  output$tableexplo <- DT::renderDataTable({

  # print("tableExplo")
 temp <- pecc_new_table()

  updateSelectInput(session, "groupbyExplo", choices = names(temp))

  # previous <-  isolate(input$groupbyExplo)
  # updateSelectInput(session, "groupbyCovExplo", choices = names(temp),selected = previous)

temp2 <- temp
testt <- which(names(temp) == isolate(input$groupbyCovExplo))
if(length(testt) > 0 ) temp2 <- temp2[-testt]
  imap_chr(temp2, function(x,y){

    if(is.factor(x)) x <- as.character(x)

    sum(is.na(x)) -> nna1
    sum(is.na(as.double(x))) -> nna2

    if(nna1 == nna2 ) return(y)
    return(NA)

    }) -> namescont


 c( paste0(  names(temp2)[!is.na(namescont)], c("_cont")),
  paste0(  names(temp2)[!is.na(namescont)], c("_cat")),
  paste0(names(temp2)[is.na(namescont)], c("_cat")))  -> choicesx

 choicesx <- c("All", choicesx[order(choicesx)])

  updateSelectInput(session, "table1x", choices = choicesx, selected = "All")
  updateSelectInput(session, "table1y", choices = names(temp2)[order(names(temp2))], selected = NA)

  imap_chr(temp, function(x,y){
    # print(y)
    unique(x) -> uniquex
    # uniquex <- c(0,1,NA,2)
    length(uniquex[!uniquex %in% c(0,1)&!is.na(uniquex)]) -> testwrongvalue

    if(testwrongvalue>0) return(NA)

    if(sum(uniquex %in% c(0,1)) >0) return(y)
    return(NA)

  }) -> boollist

  boollist <- boollist[!is.na(boollist)] %>% unname

  updateSelectInput(session, "countx", choices =  names(temp2)[order(names(temp2))], selected = NA)
  updateSelectInput(session, "county", choices = names(temp2)[order(names(temp2))], selected = NA)
  updateSelectInput(session, "plotcov_x", choices = choicesx, selected = NA)
  updateSelectInput(session, "plotcov_y", choices = choicesx, selected = NA)
  updateSelectInput(session, "countwhat", choices = boollist[order(boollist)], selected = NA)



  # print(temp)
  return(temp)

}, options = list(pageLength = 10, scrollX = TRUE))

) # end of the try

})



# storemodif --------------------------------------------------------------

observeEvent(input$storemodif,{

# part 1: keep track of modification

modif <- modifExpr()

# Part 2: store it

  curentmodel <- isolate(input$preloadeddataset) %>%
    gsub(pattern = ":.+", replacement = "") %>%
    as.double


  datasets_df %>%
    filter(n == curentmodel) %>%
    mutate(n = max(datasets_df$n) + 1 ) %>%
    mutate(File = paste0(File," + code")) -> newline

  # print(deparase(modif))
 newline$codeToEval <- deparse(modif) %>% paste0(collapse = "\n")


  datasets_df <<- bind_rows(datasets_df, newline)


  updateSelectInput(session, inputId = "preloadeddataset", choices = unique(paste0(datasets_df$n,":", datasets_df$File)), selected = isolate(input$preloadeddataset)) # , selected = unique(paste0(datasets_df$n,":", datasets_df$File))[nrow(datasets_df)]

  output$datasets <- renderRHandsontable( rhandsontable( datasets_df, rowHeaders = NULL))

  showNotification("Save the dataset table if you want to re-use it when reoping the project !", type = "message", duration = 10, closeButton = T)


  # searchcovby <- isolate(input$groupbyCovExplo)
  #
  # if(length(searchcovby) > 0){
  #
  #
  #   temp <- pecc_search_cov(temp, !!!parse_exprs(searchcovby))
  #
  #
  # }

})

# Create new dataset with modification ------------------------------------
observeEvent(input$copymodif,{



  modif <- modifExpr()


  ## Step2: save the new dataset

  #  explo_dec
  #  explo_NA
  #  explo_path
  #  explo_sep
 original <- explo_path

 analyse <-  str_split(original, pattern = "/|(\\\\)")[[1]]
 last <- analyse[length(analyse)]
 last <- gsub("_peccModif.+\\.", ".", last)
 extension <- gsub(".+\\.", ".", last)

 listfiles <- list.files(paste0(analyse[-length(analyse)], collapse = .Platform$file.sep))
 listfiles <- listfiles[grepl("_peccModif", listfiles)]
 listfiles <- listfiles[order(listfiles)]
 if(length(listfiles) > 0)  last <- listfiles[length(listfiles)]


 if(grepl("_peccModif", last)){

  temp <- gsub(".+_peccModif", "", last) %>%
    gsub(pattern = "\\..+", replacement = "")

  if(temp == ""){

    lastnew <- gsub("_peccModif", "_peccModif2", last)

  }else{

    lastnew <- gsub(paste0("_peccModif", temp), paste0("_peccModif", as.double(temp) +1), last)

  }

 }else{

  lastnew <- gsub(extension, paste0("_peccModif", extension), last)

 }

 analyse[length(analyse)] <- lastnew
 finalpath <- paste0(analyse, collapse = .Platform$file.sep)

 write.table(pecc_new_table(), finalpath, sep = explo_sep, dec = explo_dec, na= explo_na,quote = F, row.names = F)


 # step3: complete the dataset table
 curentmodel <- isolate(input$preloadeddataset) %>%
   gsub(pattern = ":.+", replacement = "") %>%
   as.double

 datasets_df %>%
   filter(n == curentmodel) %>%
   mutate(File = lastnew) %>%
   mutate(n = max(datasets_df$n) + 1 ) -> newline

 newline$codeToEval <- paste0("From ", isolate(input$preloadeddataset),":\n", deparse(modif) %>% paste0(collapse = "\n"))

 datasets_df <<- bind_rows(datasets_df, newline)
 updateSelectInput(session, inputId = "preloadeddataset", choices = unique(paste0(datasets_df$n,":", datasets_df$File)), selected = isolate(input$preloadeddataset)) # , selected = unique(paste0(datasets_df$n,":", datasets_df$File))[nrow(datasets_df)]

 output$datasets <- renderRHandsontable( rhandsontable( datasets_df, rowHeaders = NULL))

 showNotification("Save the dataset table if you want to re-use it when reoping the project.\nOtherwise you might want to remove the dataset after use", type = "message", duration = 10, closeButton = T)

 # print(previous)

 # newpath <- isolate(input$path_dataset_manual)
 # # newpath <- "file:///D:/these\\Pecc_test/1_Data/3_final/ALEMTUZUMAB_20_04_07.csv"
 # # newpath <- "\"file:///D:/Peccary/Exemple_demo/DATA/Simeoni.txt\""
 # newpath <- gsub("file:///", "", newpath) %>%
 #   gsub(pattern = "\\\\",  replacement = "/") %>%
 #   gsub(pattern = "\"",  replacement = "")

})


# table1 ------------------------------------------------------------------

observeEvent(input$table1go,{
  # print("ici deja")
  ## copy past from above
  temp <- pecc_new_table(fortable1 = T)

  ###
   ### teeest
  # print("ererezrezrezrez")
  # print(dim(isolate(input$tableexplo)))
  # if(sum( dim(isolate(input$tableexplo)) == dim(temp)) != 2){
  #   print("aie")
  #   output$tableexplo <- DT::renderDataTable(temp)
  # }

  ## table1
print("ici deja")


# list of argument to pass to pecc_table1_original function
listarg <- list()

listarg$df <- expr(!!modifExpr(fortable1 = T))
reduceBy <- isolate(input$table1reduceBy)


if(!is.null(reduceBy)){
  reduceBy <- reduceBy[-which(reduceBy %in% isolate(input$groupbyCovExplo))]
 if(length(reduceBy)>0) listarg$reduceBy  <- parse_expr(reduceBy %>% paste0(collapse = " + "))

}
coltemp <- isolate(input$table1y)
if(!is.null(coltemp)){

listarg$col1 <- parse_expr(coltemp[[1]])

if(length(coltemp) > 1) listarg$col2 <-  parse_expr(coltemp[[2]])
}

rowstemp <- isolate(input$table1x); if(is.null(rowstemp)) rowstemp <- "All"

if(rowstemp[[1]] != "All"){

  table1code <-  expr(pecc_table1_original(!!! parse_exprs(rowstemp), !!!listarg, outputExpr = T))%>%
    eval
}else{

  table1code <-  expr(pecc_table1_original(!!!listarg, outputExpr = T))%>%
    eval

}


print(table1code)
   text <-   paste0("# ", deparse(explo_expr, width.cutoff = 500),"\n\n", deparse(table1code) %>% paste0(collapse = "\n"))

updateTextAreaInput(session, inputId = "table1_code", value =text)

print('preevalcode1')
   table1 <- try( eval(table1code))
   print(table1)
print('postevalcode1')
# print(table1)
   # print("ici deja")
# print("on va slicer2")
# print(table1 %>% as_tibble)

if(class(table1)[[1]] != "try-error"){

  print("here")
  output$table1output <- renderTable(table1)

  }
# print('voila ')
#   colnamestable1 <- table1 %>%
#     slice(1) %>%
#     as.data.frame()
#   colnamestable1[1, ] <- names(table1)
#
#   table1 <- bind_rows(colnamestable1, table1)
#   # print("on va slicer2")
#
# if(table1[[1]][[1]] == "Table1") table1[[1]][[1]] <- ""
#
#   output$table1output <- renderRHandsontable(
#
#     rhandsontable(table1, colHeaders = F, rowHeaders = F))
#
# }
#
#    #
#    text <-    deparse(expr(pecc_table1(dataset  = !!modifExpr(), rowl = c(!!!isolate(input$table1x)), coll = !!isolate(input$table1y) )), width.cutoff = 500) %>%
#      paste0(collapse = "\n")
#    text <-   paste0("# ", deparse(explo_expr, width.cutoff = 500), "\n\n",text, "\n\n# Note: peccary independant code not available for now, but output here is easily verifiable")
#
# updateTextAreaInput(session, inputId = "table1_code", value =text)


})


# plot cov -------------------------------------------------------------------

observeEvent(input$plotcov_go,{

  try({
x <- isolate(input$plotcov_x)
y <- isolate(input$plotcov_y)

xexpr <- parse_expr(gsub("(_cat$)|(_cont$)", "", x))

if(y != "") yexpr <- parse_expr(gsub("(_cat$)|(_cont$)", "", y))
# print("hereaaaa")
if(grepl("_cat$", x) & y == ""){


 expoutput <-  expr( !!modifExpr() %>%
         ggplot()+
         geom_bar(aes(!!xexpr))+
         theme_bw())


}else if(grepl("_cat$", x) & grepl("_cat$", y) ){

  expoutput <-   expr(!!modifExpr() %>%
         ggplot()+
         geom_bar(aes(factor(!!xexpr), fill = factor(!!yexpr)))+
         labs(x = !!deparse(xexpr), fill  = !!deparse(yexpr))+
         theme_bw())


}else if(grepl("_cat$", x) &  grepl("_cont$", y)){


  expoutput <-   expr( plot_boxplot(df = !!modifExpr() , x = !!xexpr,  !!yexpr ))

}else if(grepl("_cont$", x) &  grepl("_cat$", y)){


  expoutput <-   expr( plot_boxplot(df = !!modifExpr() , x = !!yexpr,  !!xexpr ))


}else if(grepl("_cont$", x) &  grepl("_cont$", y)){


  expoutput <-  expr(plot_correlation(df = !!modifExpr(), x = !!xexpr, !!yexpr))


}else if(grepl("_cont$", x) & y == ""){

  expoutput <-  expr( !!modifExpr() %>%
                        ggplot()+
                        geom_histogram(aes(!!xexpr))+
                        theme_bw())

}

text <-   paste0("# ", deparse(explo_expr, width.cutoff = 500), "\n\n",deparse(expoutput, width.cutoff = 500) %>% paste0(collapse = "\n"), "\n\n# Note: peccary independant code not available for now, but output here is easily verifiable")

updateTextAreaInput(session, "plotcov_code", value = text)

output$plotcovplot <- renderPlot(eval(expoutput))
})
})

# count -------------------------------------------------------------------


observeEvent(input$countgo,{

  temp <- pecc_new_table()



  ## count begin
  if(isolate(input$county) == "" | is.na(isolate(input$county))){
    colcov <- NULL
  }else{

    colcov <- isolate(input$county)
  }

  if(isolate(input$countx) == "" | is.na(isolate(input$countx))){
    rowcov <- NULL
  }else{

    rowcov <- isolate(input$countx)
  }

# print(pecc_count)
  # print(pecc_count(df = temp, metric = isolate(input$countwhat),
  #            col_cov = colcov,
  #            row_cov = rowcov, str_input = T))

  couunt <- pecc_count(df = temp, metric = isolate(input$countwhat),
                        col_cov = colcov,
                        row_cov = rowcov, str_input = T)

  colname1 <- colnames(couunt)[1]
  colnames(couunt)[1] <- gsub("\\(.+", "", colname1)
  colnamess <-colnames(couunt)
  colnamessdf <- couunt %>% slice(1) %>% as.data.frame()
  colnamessdf[1, ] <-  colnamess

  naline <- couunt[[1]][[nrow(couunt)]]
  couunt <- couunt[-nrow(couunt), ]
  output$countoutput <- renderRHandsontable(

    rhandsontable(rbind(colnamessdf, couunt), colHeaders = F, rowHeaders = F))

  output$countbytext <- renderText(paste0("Number of ", gsub("(.+\\()|\\)", "", colname1), "\n", naline))




text <-    deparse(expr(pecc_count(df =   !!modifExpr(), metric = !!isolate(input$countwhat),
                                   col_cov = !!colcov,
                                   row_cov = !!rowcov, str_input = T)), width.cutoff = 500) %>%
  paste0(collapse = "\n")

text <-   paste0("# ", deparse(explo_expr, width.cutoff = 500), "\n\n",text, "\n\n# Note: peccary independant code not available for now, but output here is easily verifiable")

  updateTextAreaInput(session, inputId = "count_code", value =text)

})

#
# # explo dataset 2 ---------------------------------------------------------
#


observeEvent(eventExpr = input$filtertableexplo,{
try({
  print("okay")

  # same as tablexplo1 = take data
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

  # group_by handling

  if(length(isolate(input$groupbyExplo))>0){

    temp <- temp %>%
      ungroup() %>%
      group_by(!!!parse_exprs(isolate(input$groupbyExplo)))

  }

  # if tally

  if(isolate(input$tableexploselected) %in% c("tally")){

    # print(temp)

    explodataset2 <<- eval(expr(temp %>%
                                  !!parse_expr(isolate(input$tableexploselected))))

  }else if(isolate(input$tableexploselected) %in% c("distinct")){

    explodataset2 <<- temp %>%
      distinct(!!!parse_exprs(isolate(input$groupbyExplo)))

  }

  updateSelectInput(session, "groupbyExplo3", choices = isolate(input$groupbyExplo))

  output$tableexplo2 <- DT::renderDataTable(datatable(explodataset2, rownames = F),  options = list(scrollX = TRUE))
})
})
#
#
# # explo dataset 3 ---------------------------------------------------------
#
observeEvent(input$launchtableexplo3,{


  try({

  outputt <- explodataset2 %>%
    ungroup() %>%
    group_by(!!parse_expr(isolate(input$groupbyExplo3))) %>%
    tally()

  outputt <- outputt %>%
    mutate(pct =paste0( round(100 * n / sum(outputt$n), 1), "%")) %>%
    mutate(n = paste0(n,"/",sum(outputt$n)))

  output$tableexplo3 <- DT::renderDataTable(datatable(outputt, rownames = F),  options = list(scrollX = TRUE))
})
})



