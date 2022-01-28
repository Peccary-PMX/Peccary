
observeEvent(input$vpc_pckg_simdata,{
library(vpc)
  path_temp <- input$vpc_pckg_simdata

  try({
header <- readLines(path_temp, n = 1)
npointvirgule <- length(str_split(header, ";")[[1]])
nvirgule <- length(str_split(header, ";")[[1]])

sep = " "
if(npointvirgule > 1) sep = ";"
if(nvirgule > 1) sep = ","

vpc_pckg_sim <- list()
vpc_pckg_sim$df <- read.table(path_temp, sep = sep, header = T) %>%
  as_tibble()

vpc_pckg_sim$expr <- expr(read.table(!!path_temp, sep = !!sep, header = T))

vpc_pckg_sim <<- vpc_pckg_sim
c(names(vpc_pckg_sim$df)[grep("ID", names(vpc_pckg_sim$df))], NA)[[1]]
updateSelectInput(session, inputId = "vpc_pckg_simdata_ID", choices = names(vpc_pckg_sim$df),selected = c(names(vpc_pckg_sim$df)[grep("ID", names(vpc_pckg_sim$df))], NA)[[1]])
updateSelectInput(session, inputId = "vpc_pckg_simdata_REP", choices = names(vpc_pckg_sim$df),selected = c(names(vpc_pckg_sim$df)[grep("REP", names(vpc_pckg_sim$df))], NA)[[1]])
updateSelectInput(session, inputId = "vpc_pckg_simdata_TIME", choices = names(vpc_pckg_sim$df),selected = c(names(vpc_pckg_sim$df)[grep("TIME", names(vpc_pckg_sim$df))], NA)[[1]])
updateSelectInput(session, inputId = "vpc_pckg_simdata_PRED", choices = names(vpc_pckg_sim$df),selected = c(names(vpc_pckg_sim$df)[grep("PRED", names(vpc_pckg_sim$df))], NA)[[1]])
  })

})


observeEvent(input$vpc_pckg_obsdata,{

  path_temp <- input$vpc_pckg_obsdata

  try({
    header <- readLines(path_temp, n = 1)
    npointvirgule <- length(str_split(header, ";")[[1]])
    nvirgule <- length(str_split(header, ";")[[1]])

    sep = " "
    if(npointvirgule > 1) sep = ";"
    if(nvirgule > 1) sep = ","

    vpc_pckg_obs <- list()
    vpc_pckg_obs$df <- read.table(path_temp, sep = sep, header = T) %>%
      as_tibble()

    vpc_pckg_obs$expr <- expr(read.table(!!path_temp, sep = !!sep, header = T))

    vpc_pckg_obs <<- vpc_pckg_obs
    c(names(vpc_pckg_obs$df)[grep("ID", names(vpc_pckg_sim$df))], NA)[[1]]
    updateSelectInput(session, inputId = "vpc_pckg_obsdata_ID", choices = names(vpc_pckg_obs$df),selected = c(names(vpc_pckg_obs$df)[grep("ID", names(vpc_pckg_obs$df))], NA)[[1]])
    updateSelectInput(session, inputId = "vpc_pckg_obsdata_DV", choices = names(vpc_pckg_obs$df),selected = c(names(vpc_pckg_obs$df)[grep("DV", names(vpc_pckg_obs$df))], NA)[[1]])
    updateSelectInput(session, inputId = "vpc_pckg_obsdata_TIME", choices = names(vpc_pckg_obs$df),selected = c(names(vpc_pckg_obs$df)[grep("TIME", names(vpc_pckg_obs$df))], NA)[[1]])
    updateSelectInput(session, inputId = "vpc_pckg_obsdata_MDV", choices = names(vpc_pckg_obs$df),selected = c(names(vpc_pckg_obs$df)[grep("MDV", names(vpc_pckg_obs$df))], NA)[[1]])
  })

})


observeEvent(input$vpc_pckg_go,{
print("HEY !")




  showw <- isolate(input$vpc_pckg_show)

  showwlist <- list()
  showbase <-  tribble(~a, ~bool,
          "obs_dv", F,
          "obs_ci", T,
          "pi", F,
          "pi_as_area", F,
          "pi_ci", T,
          "obs_median", T,
          "sim_median", F,
          "sim_median_ci", T)

  for(a in showbase$a){


   if(a %in% showw & showbase$bool[showbase$a == a] == F){
     showwlist[[a]] <- T
   }else if(!a %in% showw & showbase$bool[showbase$a == a] == T){

     showwlist[[a]] <- F
   }

  }





  argum <- list()
  argum$sim <- expr(vpc_pckg_sim$df)

  if(isolate(input$vpc_pckg_simdata_ID) != "ID") argum$sim <- expr(!! argum$sim %>% mutate(ID = !!isolate(input$vpc_pckg_simdata_ID)))
  if(isolate(input$vpc_pckg_simdata_REP) != "REP") argum$sim <- expr(!! argum$sim %>% mutate(REP = !!isolate(input$vpc_pckg_simdata_REP)))
  if(isolate(input$vpc_pckg_simdata_TIME) != "TIME") argum$sim <- expr(!! argum$sim %>% mutate(TIME = !!isolate(input$vpc_pckg_simdata_TIME)))
  if(isolate(input$vpc_pckg_simdata_PRED) != "PRED") argum$sim <- expr(!! argum$sim %>% mutate(PRED = !!isolate(input$vpc_pckg_simdata_PRED)))


  argum$obs <- expr(vpc_pckg_obs$df)

  if(isolate(input$vpc_pckg_obsdata_ID) != "ID") argum$obs <- expr(!! argum$obs %>% mutate(ID = !!isolate(input$vpc_pckg_obsdata_ID)))
  if(isolate(input$vpc_pckg_obsdata_DV) != "DV") argum$obs <- expr(!! argum$obs %>% mutate(DV = !!isolate(input$vpc_pckg_obsdata_DV)))
  if(isolate(input$vpc_pckg_obsdata_TIME) != "TIME") argum$obs <- expr(!! argum$obs %>% mutate(TIME = !!isolate(input$vpc_pckg_obsdata_TIME)))
  if(isolate(input$vpc_pckg_obsdata_MDV) != "MDV") argum$obs<- expr(!! argum$obs %>% mutate(MDV= !!isolate(input$vpc_pckg_obsdata_MDV)))

  if( isolate(input$vpc_pckg_bins)!= "jenks")  argum$bins <- expr(!!isolate(input$vpc_pckg_bins))

  if( isolate(input$vpc_pckg_bin_mid )!= "mean") argum$bin_mid <- expr(!!isolate(input$vpc_pckg_bin_mid))

  if(isolate(input$vpc_pckg_software) != "auto")   argum$software <- expr(!!isolate(input$vpc_pckg_software))
  if(length(showwlist) >0)   argum$show <- expr(list(!!!showwlist))
  if(isolate(input$vpc_pckg_pred_corr) == T) argum$pred_corr <- expr(!!isolate(input$vpc_pckg_pred_corr))
  if(isolate(input$vpc_pckg_pred_corr_lower_bnd) > 0) argum$pred_corr_lower_bnd <- expr(!!isolate(input$pred_corr_lower_bnd))
 # print(isolate(input$vpc_pckg_n_bins))
   if(isolate(input$vpc_pckg_n_bins) > 0) argum$n_bins <- expr(!!isolate(input$vpc_pckg_n_bins))
  if(isolate(input$vpc_pckg_pi_low) != 0.05 |isolate(input$vpc_pckg_pi_high) != 0.95 )   argum$pi <- expr(c(!!isolate(input$vpc_pckg_pi_low),!!isolate(input$vpc_pckg_pi_high) ))
  if(isolate(input$vpc_pckg_ci_low) != 0.05 |isolate(input$vpc_pckg_ci_high) != 0.95 )    argum$ci <- expr(c(!!isolate(input$vpc_pckg_ci_low),!!isolate(input$vpc_pckg_ci_high)))
  if(!is.na(isolate(input$vpc_pckg_uloq))) argum$uloq <- isolate(input$vpc_pckg_uloq)
  if(!is.na(isolate(input$vpc_pckg_lloq))) argum$uloq <- isolate(input$vpc_pckg_lloq)
  if(isolate(input$vpc_pckg_log_y) == T)  argum$log_y <- isolate(input$vpc_pckg_log_y)
  if(isolate(input$vpc_pckg_log_y_min) != 0.001) argum$log_y_min <- isolate(input$vpc_pckg_log_y_min)
  if(isolate(input$vpc_pckg_xlab) != "" ) argum$xlab <- isolate(input$vpc_pckg_xlab)
  if(isolate(input$vpc_pckg_ylab) != "" ) argum$ylab <- isolate(input$vpc_pckg_ylab)
  if(isolate(input$vpc_pckg_title) != "") argum$title <- isolate(input$vpc_pckg_title)
  if(isolate(input$vpc_pckg_smooth) == F) argum$smooth <- isolate(input$vpc_pckg_smooth)
  if(isolate(input$vpc_pckg_facet) != "wrap") argum$facet <- isolate(input$vpc_pckg_facet)

  argum <<- argum
  expre <<- expr(vpc_vpc(!!!argum))

  text <- deparse(expre) %>% paste0(collapse = " ") %>% gsub(pattern = "  *", replacement = " ")

  text <- paste0( "df_sim <-",  deparse(vpc_pckg_sim$expr) %>% paste(collapse = ""), "\n\n",
      "df_obs <-",  deparse(vpc_pckg_obs$expr) %>% paste(collapse = ""), "\n\n",
    gsub("vpc_pckg_sim\\$df", "df_sim", text) %>%
      gsub(pattern = "vpc_pckg_obs\\$df", replacement = "df_obs")) %>%
    gsub(pattern = "  *", replacement = " ")


   updateTextAreaInput(session = session, inputId = "vpc_pckg_code", value = text)

# print(expre)
 output$vpc_pckg_plot <- renderPlot(
          eval(expre)
         )



  })

 # pi = c(isolate(input$vpc_pcgk_pi_low),isolate(input$vpc_pcgk_pi_high) ), ci = c(isolate(input$vpc_pcgk_ci_low),isolate(input$vpc_pcgk_ci_high) )
