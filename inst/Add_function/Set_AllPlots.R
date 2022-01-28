 require(tidyverse)
 require(cowplot)
 
 #### PLOTS ####
 
 GoF_plot <- function(run_object){
   
   dat <- run_object@IPRED %>%
     left_join(run_object@OBS)
   
   if(is.null(try(dat$YTYPE,silent = T))) dat$YTYPE = 1
   
   dat %>%
     group_by(YTYPE) %>%
     nest() %>%
     #{.[[1, "data"]]} -> x ; y = 1
     mutate(plot = map2(data, YTYPE, function(x,y){
       
       gg_ipred <- ggplot(x) + theme_bw() +
         geom_point(aes(x = OBS, y = IPRED)) +
         geom_abline(col = "red") + 
         labs(x = "Observations", y = "Individual predictions") +
         ggtitle(paste0("YTYPE = ",y))
       
       gg_pred <- ggplot(x) + theme_bw() +
         geom_point(aes(x = OBS, y = PRED)) +
         geom_abline(col = "red") + 
         labs(x = "Observations", y = "Population predictions") +
         ggtitle(" ")
       
       return(gg_tot <- plot_grid(gg_ipred,gg_pred))
       
       })) %>%
     pull(plot)
  
 }
 
 GoF_plot_log <- function(run_object){
   
   dat <- run_object@IPRED %>%
     left_join(run_object@OBS)
   
   if(is.null(try(dat$YTYPE,silent = T))) dat$YTYPE = 1
   
   dat %>%
     group_by(YTYPE) %>%
     nest() %>%
     #{.[[1, "data"]]} -> x ; y = 1
     mutate(plot = map2(data, YTYPE, function(x,y){
       
       gg_ipred <- ggplot(x) + theme_bw() +
         geom_point(aes(x = OBS, y = IPRED)) +
         geom_abline(col = "red") + 
         labs(x = "Observations", y = "Individual predictions") +
         ggtitle(paste0("YTYPE = ",y)) +
         scale_y_log10() + scale_x_log10()
       
       gg_pred <- ggplot(x) + theme_bw() +
         geom_point(aes(x = OBS, y = PRED)) +
         geom_abline(col = "red") + 
         labs(x = "Observations", y = "Population predictions") +
         ggtitle(" ") +
         scale_y_log10() + scale_x_log10()
       
       return(gg_tot <- plot_grid(gg_ipred,gg_pred))
       
     })) %>%
     pull(plot)
   
 }
 
 NPDE_plot <- function(run_object){
   
   dat <- run_object@residus %>%
     left_join(run_object@IPRED)
   
   if(is.null(try(dat$YTYPE,silent = T))) dat$YTYPE = 1
   
   dat %>%
     group_by(YTYPE) %>%
     nest() %>%
     #{.[[1, "data"]]} -> x ; y = 1
     mutate(plot = map2(data, YTYPE, function(x,y){
       
       gg_npde_time <- ggplot(x) + theme_bw() +
         geom_point(aes(x = TIME, y = NPDE)) +
         geom_hline(yintercept = 0, col = "red") +
         ggtitle(paste0("YTYPE = ",y)) + 
         labs(x = "Time", y = "NPDE") 
       
       gg_npde_pred <- ggplot(x) + theme_bw() +
         geom_point(aes(x = PRED, y = NPDE)) +
         geom_hline(yintercept = 0, col = "red") +
         ggtitle(" ") + 
         labs(x = "Population predictions", y = "NPDE") 
   
       return(gg_npde <- plot_grid(gg_npde_time,gg_npde_pred))
       
     })) %>%
     pull(plot)
   
 }

 IWRES_plot <- function(run_object){
   
   dat <- run_object@residus %>%
     left_join(run_object@IPRED)
   
   if(is.null(try(dat$YTYPE,silent = T))) dat$YTYPE = 1
   
   dat %>%
     mutate(YTYPE = ifelse(contains(YTYPE),YTYPE,1))
   
   dat %>%
     group_by(YTYPE) %>%
     nest() %>%
     #{.[[1, "data"]]} -> x ; y = 1
     mutate(plot = map2(data, YTYPE, function(x,y){
       
       gg_IWRES_time <- ggplot(x) + theme_bw() +
         geom_point(aes(x = TIME, y = IWRES)) +
         geom_hline(yintercept = 0, col = "red") +
         ggtitle(paste0("YTYPE = ",y)) + 
         labs(x = "Time", y = "IWRES") 
       
       gg_IWRES_pred <- ggplot(x) + theme_bw() +
         geom_point(aes(x = PRED, y = IWRES)) +
         geom_hline(yintercept = 0, col = "red") +
         ggtitle(" ") + 
         labs(x = "Population predictions", y = "IWRES") 
       
       return(gg_IWRES <- plot_grid(gg_IWRES_time,gg_IWRES_pred))
       
     })) %>%
     pull(plot)
   
 }
 
 indiv_plot_auto_pred <- function(N = 9, run_object){
   
     dat <- run_object@IPRED %>%
       left_join(run_object@OBS)
       
    
   dat %>%
     group_by(ID) %>%
     #filter(get(mdv) == 0) %>%
     nest()%>%
     #{.[[1, "data"]]} -> x ; y = 1
     mutate(plot = map2(data, ID,function(x,y){
       
       gg <- ggplot(x) +  
         xlab("Time") + ylab("Concentration") +
         theme_bw() +
         geom_line(aes(x=TIME, y=IPRED), col = "green")  +
         ggtitle(paste0("ID = ",y)) +
         geom_point(aes(x=TIME, y=OBS), shape = 3, color = "blue") + 
         geom_line(aes(x=TIME, y=PRED), col = "red", linetype = "dashed")
       
       if(!is.null(try(dat$YTYPE,silent = T)))  gg <- gg + facet_wrap(~YTYPE, scales = "free")
       
       return(gg)
       
     })) %>%
     rowid_to_column() %>% 
     mutate(forplotgrid = ceiling(rowid/N)) %>% 
     group_by(forplotgrid) %>% 
     nest() %>% 
     mutate(plot = map(data,~ invoke(plot_grid, .x$plot))) %>% 
     pull(plot)
   
 }
 
 indiv_plot_auto_pred_log <- function(N = 9, run_object){
   
   dat <- run_object@IPRED %>%
     left_join(run_object@OBS)
   
   
   dat %>%
     group_by(ID) %>%
     #filter(get(mdv) == 0) %>%
     nest()%>%
     #{.[[1, "data"]]} -> x ; y = 1
     mutate(plot = map2(data, ID,function(x,y){
       
       gg <- ggplot(x) +  
         xlab("Time") + ylab("Concentration") +
         theme_bw() +
         geom_line(aes(x=TIME, y=IPRED), col = "green")  +
         ggtitle(paste0("ID = ",y)) +
         geom_point(aes(x=TIME, y=OBS), shape = 3, color = "blue") + 
         geom_line(aes(x=TIME, y=PRED), col = "red", linetype = "dashed") +
         scale_y_log10()
       
       if(!is.null(try(dat$YTYPE,silent = T)))  gg <- gg + facet_wrap(~YTYPE, scales = "free")
       
       return(gg)
       
     })) %>%
     rowid_to_column() %>% 
     mutate(forplotgrid = ceiling(rowid/N)) %>% 
     group_by(forplotgrid) %>% 
     nest() %>% 
     mutate(plot = map(data,~ invoke(plot_grid, .x$plot))) %>% 
     pull(plot)
   
 }
 
 indiv_plot_auto <- function(N = 9, run_object){
   
   dat <- run_object@IPRED %>%
     left_join(run_object@OBS)
   
   
   dat %>%
     group_by(ID) %>%
     #filter(get(mdv) == 0) %>%
     nest()%>%
     #{.[[1, "data"]]} -> x ; y = 1
     mutate(plot = map2(data, ID,function(x,y){
       
       gg <- ggplot(x) +  
         xlab("Time") + ylab("Concentration") +
         theme_bw() +
         geom_line(aes(x=TIME, y=IPRED), col = "green")  +
         ggtitle(paste0("ID = ",y)) +
         geom_point(aes(x=TIME, y=OBS), shape = 3, color = "blue")
       
       if(!is.null(try(dat$YTYPE,silent = T)))  gg <- gg + facet_wrap(~YTYPE, scales = "free")
       
       return(gg)
       
     })) %>%
     rowid_to_column() %>% 
     mutate(forplotgrid = ceiling(rowid/N)) %>% 
     group_by(forplotgrid) %>% 
     nest() %>% 
     mutate(plot = map(data,~ invoke(plot_grid, .x$plot))) %>% 
     pull(plot)
   
 }
 
 indiv_plot_auto_log <- function(N = 9, run_object){
   
   dat <- run_object@IPRED %>%
     left_join(run_object@OBS)
   
   
   dat %>%
     group_by(ID) %>%
     #filter(get(mdv) == 0) %>%
     nest()%>%
     #{.[[1, "data"]]} -> x ; y = 1
     mutate(plot = map2(data, ID,function(x,y){
       
       gg <- ggplot(x) +  
         xlab("Time") + ylab("Concentration") +
         theme_bw() +
         geom_line(aes(x=TIME, y=IPRED), col = "green")  +
         ggtitle(paste0("ID = ",y)) +
         geom_point(aes(x=TIME, y=OBS), shape = 3, color = "blue") + 
         scale_y_log10()
       
       if(!is.null(try(dat$YTYPE,silent = T)))  gg <- gg + facet_wrap(~YTYPE, scales = "free")
       
       return(gg)
       
     })) %>%
     rowid_to_column() %>% 
     mutate(forplotgrid = ceiling(rowid/N)) %>% 
     group_by(forplotgrid) %>% 
     nest() %>% 
     mutate(plot = map(data,~ invoke(plot_grid, .x$plot))) %>% 
     pull(plot)
   
 }
 
 #### TABLES ####
 
 Estimates_table <- function(run_object){
   
   dat <- run_object@estimation
   
 }
 
 #### FINAL FUNCTION ####
 
 AllPlots <- function(run_path, work_path){
   
   run_object <- createRun(run_path)
  
   try(GoF_plot(run_object))
   try(GoF_plot_log(run_object))
   try(NPDE_plot(run_object))
   try(IWRES_plot(run_object))
   try(indiv_plot_auto_pred_log(run_object = run_object))
   try(indiv_plot_auto_pred(run_object = run_object))
   try(indiv_plot_auto_log(run_object = run_object))
   try(indiv_plot_auto(run_object = run_object))
 } 
 