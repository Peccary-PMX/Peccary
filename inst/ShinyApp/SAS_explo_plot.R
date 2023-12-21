

# save exploplot ----------------------------------------------------------


observeEvent(input$plotexplosaveaction,{

project
  if(exists('project')){

  # path_temp <- file.path(project_file,"0_pecc_project/exploPlot.txt")

  previous <- project$exploPplot #try(read.table(path_temp, header = T))
# print('try-here')
  tibble(
    Name = isolate(input$plotexploname),
    preloadeddataset = isolate(input$preloadeddataset),
    Filter = isolate(input$filterrExplo),
    exploX = isolate(input$exploX),
    exploY = isolate(input$exploY),
    Col = isolate(input$exploCol),
    exploShape = isolate(input$exploShape),
    exploLty= isolate(input$exploLty),
    Wrap = isolate(input$exploWrap),
    exploGrid = isolate(input$exploGrid),
    exploID = isolate(input$exploID),
    exploPoint = isolate(input$exploPoint),
    exploLine = isolate(input$exploLine),
    exploStand = isolate(input$exploStand),
    exploXlog = isolate(input$exploXlog),
    exploYlog = isolate(input$exploYlog),
    exploMedian = isolate(input$exploMedian),
    exploNA = isolate(input$exploNA),
    exploLOQ = isolate(input$exploLOQ),
    scaleExplo = isolate(input$scaleExplo),
    titleExplo = isolate(input$titleExplo),
    subtitleExplo = isolate(input$subtitleExplo),
    xlabsExplo = isolate(input$xlabsExplo),
    ylabsExplo = isolate(input$ylabsExplo),
    captionExplo = isolate(input$captionExplo),
    sizeTextExplo = isolate(input$sizeTextExplo),
    secondFilter = isolate(input$secondFilter),
    bkgrd = isolate(input$allbackground),
    # addlineExplo = deparse(expr(tibble(!!!map(isolate(hot_to_r(input$addlinesexploration)) %>% mutate_if(is.factor, as.character), ~.x))), width.cutoff = 500),
    all_bkgrdalpha = isolate(input$all_bkgrdalpha),
    colLabExplo = isolate(input$colLabExplo),
    colValuesExplo = isolate(input$colValuesExplo),
    shapeLabExplo = isolate(input$shapeLabExplo),
    shapeValuesExplo = isolate(input$shapeValuesExplo),
    ltyLabExplo = isolate(input$ltyLabExplo),
    ltyValuesExplo = isolate(input$ltyValuesExplo)
  ) -> new_line




 if(nrow(previous) == 0){


   outputt <- new_line
 }else{

   previous %>%
     filter(!(Name == isolate(input$plotexploname))) %>%
     bind_rows(new_line) -> outputt
 }

 project$exploPplot <-  outputt

  saveRDS(project, project$path)


  output$PlotexplorationSaved <- renderRHandsontable(rhandsontable(outputt %>%
                                                                     filter(preloadeddataset == isolate(input$preloadeddataset)) %>%
                                                                     select(Name, Filter, Col, Wrap) %>%
                                                                     mutate(Load = F) %>%
                                                                     mutate(pdf = F) %>%
                                                                     select(Load, pdf, everything()),rowHeaders = NULL))

  project <<- project
  ## update report possibilities
  try({read.table(stringsAsFactors = F, file.path(project_file, "0_pecc_project", "exploPlot.txt"), header = T) %>%
    as_tibble %>%
    mutate(output = paste0(gsub(":.+", "", preloadeddataset), ": ", Name)) %>%
    pull(output) -> choicesPlot

  updateSelectInput(session, "reportPlotExploSelect", choices = choicesPlot)
})


  }else{

    showNotification("You need first to load / create a project !", type = "error", duration = 4, closeButton = T)

  }

})


# load saved plot ---------------------------------------------------------


observeEvent(input$PlotexplorationSaved,{

  hot_to_r(isolate(input$PlotexplorationSaved)) %>%
    filter(Load == T) -> temp

  if(nrow(temp) > 0){

    # path_temp <- file.path(project_file,"0_pecc_project/exploPlot.txt")

    temp <- project$exploPplot %>% #read.table(path_temp, header = T, stringsAsFactors = F) %>%
      filter(Name == temp$Name, Filter == temp$Filter | is.na(Filter)) %>% slice(1)

    # print(temp)
    updateTextInput(session, inputId = "plotexploname",value =  temp$Name)
    updateTextInput(session, inputId = "filterrExplo", value = temp$Filter)
    updateSelectInput(session, inputId = "exploX", selected = temp$exploX)
    updateSelectInput(session, inputId = "exploY", selected = temp$exploY)
    updateSelectInput(session, inputId = "exploCol", selected = temp$Col)
    updateSelectInput(session, inputId = "exploWrap", selected = temp$Wrap)
    updateSelectInput(session, inputId = "exploGrid", selected = temp$exploGrid)
    updateSelectInput(session, inputId = "exploID", selected = temp$exploID)
    updateNumericInput(session, inputId = "exploPoint", value = temp$exploPoint)
    updateNumericInput(session, inputId = "exploLine", value = temp$exploLine)
    updateSelectInput(session, inputId = "exploStand", selected = temp$exploStand)
    updateCheckboxInput(session, inputId = "exploXlog", value = temp$exploXlog )
    updateCheckboxInput(session, inputId = "exploYlog", value = temp$exploYlog )
    updateCheckboxInput(session, inputId = "exploNA", value = temp$exploNA)
    updateCheckboxInput(session, inputId = "exploMedian", value = temp$exploMedian )
    updateNumericInput(session, inputId = "exploLOQ", value = temp$exploLOQ)
    updateSelectInput(session, inputId = "scaleExplo", selected  = temp$scaleExplo)
    updateTextInput(session, inputId = "titleExplo",value =  temp$titleExplo)
    updateTextInput(session, inputId = "subtitleExplo",value =  temp$subtitleExplo)
    updateTextInput(session, inputId = "xlabsExplo",value =  temp$xlabsExplo)
    updateTextInput(session, inputId = "ylabsExplo",value =  temp$ylabsExplo)
    updateTextInput(session, inputId = "captionExplo",value =  temp$captionExplo)
    updateNumericInput(session, inputId = "sizeTextExplo", value = temp$sizeTextExplo)
    updateTextInput(session, inputId = "secondFilter",value =  temp$secondFilter)
    updateCheckboxInput(session, inputId = "allbackground", value = temp$bkgrd)
    updateNumericInput(session, inputId = "all_bkgrdalpha", value = temp$all_bkgrdalpha)
    updateTextInput(session, inputId = "exploShape",value =  temp$exploShape)
    updateTextInput(session, inputId = "exploLty",value =  temp$exploLty)
    updateTextInput(session, inputId = "colLabExplo",value =  temp$colLabExplo)
    updateTextInput(session, inputId = "colValuesExplo",value =  temp$colValuesExplo)
    updateTextInput(session, inputId = "shapeLabExplo",value =  temp$shapeLabExplo)
    updateTextInput(session, inputId = "shapeValuesExplo",value =  temp$shapeValuesExplo)
    updateTextInput(session, inputId = "ltyLabExplo",value =  temp$ltyLabExplo)
    updateTextInput(session, inputId = "ltyValuesExplo",value =  temp$ltyValuesExplo)


    # addliness <-

    testaddline <- try( temp$addlineExplo%>%
                          parse_expr %>%
                          eval %>%
                          mutate(type = factor(type, levels = c("hline", "vline"))) %>%
                          mutate(lty = factor(lty, levels = c( "solid", "dashed", "dotted", "dotdash", "longdash", "twodash")))
    )

    if(class(testaddline) != "try-error"){

      output$addlinesexploration <- renderRHandsontable({

        rhandsontable(testaddline, width = 300, height = 500)

      })
    }


    output$PlotexplorationSaved <- renderRHandsontable(rhandsontable(hot_to_r(isolate(input$PlotexplorationSaved)) %>%
                                                                       mutate(Load = F) %>%
                                                                       mutate(pdf = F) %>%
                                                                       select(Load, pdf, everything()),rowHeaders = NULL))

  }



})

# output$PlotexplorationSaved <- renderRHandsontable(rhandsontable(mtcars %>% mutate(test = input$exploX)))

output$addlinesexploration <- renderRHandsontable({

  rhandsontable(tibble(type = factor(c("hline", "vline"), levels = c("hline", "vline")),
                       intercept = c("0","0"), lty = factor(c("dashed", "dotted"), levels = c( "solid", "dashed", "dotted", "dotdash", "longdash", "twodash")),display = c(F, F)), width = 300, height = 500)

})

# exploPLot ---------------------------------------------------------------


  output$exploPlot <- renderPlot({
    # print("explotplot")
# print(input$filterrExplo)
    inputplot <- list(  df = expr(explo),
                        x = input$exploX,
                        y = input$exploY)

    if(input$exploID != "") inputplot$group <-    input$exploID
    if(input$exploCol != "") inputplot$col <-     input$exploCol
    if(input$exploWrap != "")  inputplot$facetwrap <-  input$exploWrap
    if(input$exploGrid != "")  inputplot$facetgrid <-   input$exploGrid
    if(input$scaleExplo != "free") inputplot$facetscale <- input$scaleExplo
    if(input$exploMedian != F)  inputplot$median <- input$exploMedian
    if(input$exploPoint != 1) inputplot$point_alpha <- input$exploPoint
    if(input$exploLine != 1)   inputplot$line_alpha <- input$exploLine
    if(input$exploStand != "")  inputplot$standardisation <- input$exploStand
    if(input$exploXlog!= F) inputplot$xlog <- input$exploXlog
    if(input$exploYlog!= T) inputplot$ylog <- input$exploYlog
    if(input$exploLOQ != -1) inputplot$loq <- as.double(input$exploLOQ)
    if(input$filterrExplo != "") inputplot$filter <-  input$filterrExplo
    if(input$titleExplo != "") inputplot$title <- input$titleExplo
    if(input$subtitleExplo != "") inputplot$subtitle <- input$subtitleExplo
    if(input$captionExplo != "") inputplot$caption <- input$captionExplo
    if(input$xlabsExplo != "") inputplot$xlabs <- input$xlabsExplo
    if(input$ylabsExplo != "") inputplot$ylabs <- input$ylabsExplo
    if(input$sizeTextExplo != 15) inputplot$sizetext <- input$sizeTextExplo
    if(input$exploNA != T) inputplot$removeNA <- input$exploNA
    if(input$allbackground != T) inputplot$bkgrd <- input$allbackground
    if(input$secondFilter != "") inputplot$filter2 <-  input$secondFilter
    if(input$exploShape != "") inputplot$shape <-  input$exploShape
    if(input$exploLty != "") inputplot$lty <-  input$exploLty
    if(input$all_bkgrdalpha != 0.3) inputplot$bkgrdalpha <- input$all_bkgrdalpha
    if(input$colValuesExplo != "") inputplot$colmanual <- parse_expr(input$colValuesExplo)
    if(input$shapeValuesExplo != "") inputplot$shapemanual <- parse_expr(input$shapeValuesExplo)
    if(input$ltyValuesExplo != "") inputplot$ltymanual <- parse_expr(input$ltyValuesExplo)
    if(input$colLabExplo != "") inputplot$collabs <- input$colLabExplo
    if(input$shapeLabExplo != "") inputplot$shapelabs <- input$shapeLabExplo
    if(input$ltyLabExplo != "") inputplot$ltylabs <- input$ltyLabExplo
    print(input$exploPlotly)
    if(input$exploPlotly) inputplot$plotly <- TRUE
# print("filterwtf")
    # print(inputplot$filter)
    # i can't parse expr the filter because when it is too long it create erros (in two rows???)
    # parse_expr( "TIME_NEG_A < 100 & NMID2 > 200 & YTYPE %in% c(52,110) & MTX != 20")
# print("testici")
# print(inputplot$ltylabs)
addlines <-  try(hot_to_r(input$addlinesexploration) %>% filter(display == T))

if(class(addlines) == "try-error"){

  addlines <- tibble(type = "none")

}else{

  if(nrow(addlines) >0) inputplot$addlines <- expr(tibble(type = !!as.character(addlines$type), intercept = !!addlines$intercept, lty = !!as.character(addlines$lty)))


  }


    output_temp2 <-  expr(plot_spagh(!!!inputplot,  output =  "expr", workwithexpr = F ))
    # print(output_temp2)
    # print(eval(output_temp2))

    # return peccindependant code
    updateTextAreaInput(session, "codeexplo", value =  paste0("# ", paste0(deparse(explo_expr, width.cutoff = 500), collapse = "\n#"),
                                          "\n# breaks_log <- lapply(-7:7, function(x) 1:9*10^x) %>% reduce(c)",
                                          "\n# labels_log <- as.character(breaks_log); labels_log[-seq(1,length(labels_log),9)] <- \"\"\n\n",
                                          deparse(eval(output_temp2)) %>%
                                            paste(collapse = " ") %>%
                                            gsub(pattern = "%>% *", replacement = "%>%\n") %>%
                                            gsub(pattern = "\\+ *", replacement = "+\n") %>%
                                            gsub(pattern = "  *", replacement = " ") )


    )
                        #
    # return pecc dependant code
    updateTextAreaInput(session, "codeexplopec", value =  paste0("# ", paste0(deparse(explo_expr, width.cutoff = 500), collapse = "\n#"),"\n\n",
                                                              deparse(output_temp2, width.cutoff = 500) %>%
                                                                gsub(pattern = ", output = \"expr\"", replacement = "") %>%
                                                                paste(collapse = " ") %>%
                                                                gsub(pattern = "%>% *", replacement = "%>%\n") %>%
                                                                gsub(pattern = "\\+ *", replacement = "+\n") %>%
                                                                gsub(pattern = "  *", replacement = " ") )


    )

    if(input$exploPlotly){

      output$exploPlotly <- renderPlotly({


      return(ggplotly(eval(eval(output_temp2))))

    })

    }


    return(eval(eval(output_temp2)))

  })









# pdf explo ---------------------------------------------------------------

observeEvent(input$pdfPLotExplo, {
  print("pdfPlotExplo")
  temp <-  hot_to_r(isolate(input$PlotexplorationSaved)) %>%
    filter(pdf == T)

  # For testing
  # temp <- tibble(Load = F, pdf = T, Name = c("dose standardisation", "PK"), Filter = "", Col = "doseCat", Wrap = "")

  if(nrow(temp) > 0){

    path_temp <- project$exploPplot

    temp <- project$exploPplot %>%
      filter(Name %in% temp$Name)

  path <-  paste0('peccary_plots_',  as.character(Sys.time()) %>% gsub(pattern = '\\..*', replacement= '') %>% gsub(pattern = ' |:|-', replacement= '_'),'.pdf')

    try(pdf(path, width = 12))

    temp %>%
      # slice(1) -> x
      as_tibble %>%
      rownames_to_column() %>%
      group_split(rowname) %>%
      map(function(x){


        if(x$exploNA ==T){


          explo_temp <- explo %>%
            filter_(paste0("!is.na(",x$exploY, ")"))

        }else{
          explo_temp <- explo

        }

        col <- ifelse(is.na(x$Col), "", x$Col)
        Wrap <-  ifelse(is.na(x$Wrap), "", x$Wrap)
        Grid <- ifelse(is.na(x$exploGrid) | x$exploGrid == "NA", "", x$exploGrid)
        standar <- ifelse(is.na(x$exploStand) | x$exploStand == "NA", "", x$exploStand)
        Filter <- ifelse(is.na(x$Filter) | x$Filter == "NA", "", x$Filter)
        # print("subtitle")
        # print(x$subtitleExplo)
        # print(x$captionExplo)
        output_temp <- plot_spagh(workwithexpr = F, df = explo_temp,filter = Filter, facetscale = x$scaleExplo, loq = as.double(x$exploLOQ), x = x$exploX,median = x$exploMedian,  y = x$exploY, col = col, facetwrap = Wrap, facetgrid = Grid, group = x$exploID, point = x$exploPoint, standardisation = standar, ylog = x$exploYlog, xlog = x$exploXlog) #, ind_alpha = x$exploLine
        if(!(x$titleExplo %in%c("", "NA")|is.na(x$titleExplo))) output_temp <- output_temp + labs(subtitle = x$titleExplo)
        if(!(x$subtitleExplo %in%c("", "NA"))) output_temp <- output_temp + labs(subtitle = x$subtitleExplo)
        if(x$subtitleExplo  %in%c("", "NA")|is.na(x$subtitleExplo)) output_temp <- output_temp + labs(subtitle = NULL)
        if(!(x$xlabsExplo %in%c("", "NA"))) output_temp <- output_temp + labs(x = x$xlabsExplo)
        if(!(x$ylabsExplo %in%c("", "NA"))) output_temp <- output_temp + labs(y = x$ylabsExplo)
        if(!(x$captionExplo %in%c("", "NA") |is.na(x$captionExplo))) output_temp <- output_temp + labs(caption = x$captionExplo)
        if(as.double(x$exploLOQ) < 0) output_temp <- output_temp + guides(shape = F, lty = F)

        # print("la")
        # print(output_temp)
        output_temp+
          theme_bw(base_size = x$sizeTextExplo)+
          theme(plot.title = element_text(hjust = 0.5),
                plot.subtitle = element_text(hjust = 0.5),
                plot.caption = element_text(hjust = 0, face = "italic", colour = "grey18"))


      })

    dev.off()
    showNotification(paste0('File created:\n', getwd(), '/\n', path,'\n'), type = "message", duration = 10, closeButton = T)
    # Sys.sleep(3)
    # try(shell.exec(path))

  }

})
