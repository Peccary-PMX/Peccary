# NMBehaviour:
#   - "png": juste look for a png and display if it existe
#   - "ifnopng": if the png file does not existe, create it
#   - "create": create even if it exist

# names <- c("SIMUL", "IPRED", "ID", "TIME", "OBS", "EVID", "CMT",
#            "LLOQ", "BLQ", "X_DOSE", "X_DRUG", "X_CELL", "YTYPE", "Baseline", "IDPK")
#
# dataset <- read.table(col.names = names, file = "file:///Z:/ETUDES/SPK/CLSPKPOOL/ANACIN/USERS/TDPE_CB_2/18_11_14_BiophaseMasterProject/ENCOURS_Post_Mentre_Meeting_January/PK_65/NM_VIEWER/1.00.SAEM_PK_Classic_vpc.TAB", header = T) %>%
#   as_tibble() %>%
#   filter(YTYPE == 1) %>%
#   mutate(tag = paste0(X_DOSE,X_DRUG,X_CELL))
#
#
#
#
# read.table("Z:/ETUDES/S20098/CS_20098/ANACIN/PK/CS_PED_SEMI_PHYSIO/05_COVARIABLES/NM_VIEWER/04_75054_sansadda_FU1_vpc500_10p_theo/04_75054_sansadda_FU1_vpc500_10p_theo.TAB", header = T) %>%
#   as_tibble() -> dataset
# names(dataset) <- c("SIM", "ID",  "TIME",  "TAD", "AMT", "EVID",  "IPRED", "PRED",  "THEO",  "CLINT", "VC",  "KA",  "PER", "FLAG",  "BLQ", "STDY",  "DOSE",  "IIV_VC",  "IIV_CLINT", "IOV_KA",  "OBS", "DV",  "OBS10", "IPRED10", "covPUB",  "covCLASS")
#
# read.table("file:///Z:/ETUDES/SPK/CLSPKPOOL/ANACIN/USERS/TDPE_CB_2/18_11_14_BiophaseMasterProject/ENCOURS_Post_Mentre_Meeting_January/PK_55/NM_VIEWER/1.01.V1_DOSE_vpc.TAB", header = T) %>%
#   as_tibble -> dataset
#
# names(dataset) <- c("SIMUL", "IPRED", "ID", "TIME", "OBS", "EVID", "CMT", "LLOQ", "BLQ",  "X_DOSE", "X_DRUG", "X_CELL", "YTYPE", "Baseline", "IDPK")
#
#
# pecc_vpc_manual(dataset = dataset, stat_obs = F, xlabel = "Time (hours)", ylabel = "Concentration (ng/mL)", group = "X_DOSE")

#' Visual Prediction Check
#'
#' @description Compute visual predicted check plot
#' @param dataset Dataframe containing simulations
#' @param id Name of the column with ID
#' @param x Name of x columns (Time columns most of time)
#' @param y Name of Y columns (IPRED, DV...)
#' @param sim Name of simulation columns (SIM, SIMUL...)
#' @param obs Name of observation columns (to add dots)
#' @param group Name of one or several group to split (e.g. "AGE + SEX")
#' @param ylog Booléen, if the plot must be turned in logarithmic scale
#' @param stat_obs Booléen, if stats of observation (q5, q50, q95) must be displayed
#' @param loq_obs Value of loq for observation
#' @param ymin_displayed Value of loq for simulations (on test, use with precaution)
#' @param xlabel Name of the label for x
#' @param ylabel Name of the label for y
#' @author Thibaud Derippe
#'
#' @return A ggplot object (modifiable)
#' @export
#'
#' @examples
plot_vpc <- function(dataset, x = "TIME", y = "IPRED", sim = "SIMUL" ,  obs = "OBS", type_scatter_CI = "CI", quantiles = c(5,50,95), CIpct = 5,  statobs = T,  group = "", ylog = T, stat_obs = F, loq_obs = 0, xlabel = "", ylabel = "",  ymin_displayed = 0, filterr = ""){

#print("plotvpc")
  if(filterr != ""){

    dataset <- dataset %>%
      filter_(filterr)

  }


  dataset %>%
    rename_(x = x) %>%
    rename_(y = y) %>%
    rename_(sim = sim) %>%
    rename_(obs = obs)-> dataset2


  if(loq_obs > 0){

    dataset2 <-  dataset2 %>%
      mutate(obs = if_else(obs <= loq_obs, as.double(loq_obs), obs))

  }


  # To allow for instance DOSE + STDY group
if(group != ""){


if(length(group) == 1){
  group_analyse <-
    strsplit(group, "\\+")[[1]] %>%
    gsub(pattern = " ", replacement = "")
}else{

  group_analyse <- group
}

  if(length(group_analyse)>1){

    dataset2 %>%
      ungroup() %>%
      mutate(group =  apply(   dataset2[ , group_analyse ] %>% imap_dfr( function(x,y) { paste0(y, ": ",x)}) , 1 , paste , collapse = " - " )) -> dataset2

  }else{

    dataset2 <- dataset2 %>%
      ungroup() %>%
      rename_(group = group) %>%
      mutate(group = paste0(group_analyse,": ", group))

  }
}

#### scatter vpc
  if(type_scatter_CI == "scatter"){


    if(group == ""){


      dataset2 %>%
        group_by(x) %>%
        summarise(q5 = quantile(y, quantiles[1]/100), q50 = quantile(y, quantiles[2]/100), q95 = quantile(y, quantiles[3]/100))  %>%
        ungroup()->  temp


      temp <- temp[, c(1, which(quantiles != 0) + 1)] ## remove a quantile = 0




    }else{


      dataset2 %>%
        group_by(x, group) %>%
        summarise(q5 = quantile(y, quantiles[1]/100), q50 = quantile(y, quantiles[2]/100), q95 = quantile(y, quantiles[3]/100)) %>%
        ungroup()->  temp

      temp <- temp[, c(1:2, which(quantiles != 0) + 2)] ## remove a quantile = 0

    }





    temp %>%
      # gather(starts_with("q5"), starts_with("q50"), starts_with("q95") ,value = "value", key = "key") %>%
      gather(starts_with("q5"), starts_with("q50"), starts_with("q95") ,value = "value", key = "key") %>%
      mutate(key = case_when(key == "q50"~ paste0("q", quantiles[2]),
                             key == "q5"~ paste0("q", quantiles[1]),
                             key == "q95"~ paste0("q", quantiles[3]))) %>%
      ggplot()+
      geom_line(aes(x, value, col = key))+
      scale_color_manual(values = c("dodgerblue1", "brown2", "dodgerblue1")[which(quantiles != 0)])+
      theme_bw() +
      labs(col = "Prediction\nQuantiles") -> plot_temp




  }

####Confidence interval vpc
  if(type_scatter_CI == "CI"){


    if(group ==""){


      dataset2 %>%
        group_by(sim, x) %>%
        summarise(q5 = quantile(y, quantiles[1]/100), q50 = quantile(y, quantiles[2]/100), q95 = quantile(y, quantiles[3]/100)) %>%
        gather(q5, q50, q95 ,value = "value", key = "key") %>%
        group_by(x, key) %>%
        summarise(q5 = quantile(value, CIpct/100),q50 = quantile(value, 0.5), q95 = quantile(value, (100-CIpct)/100)) %>%
        ungroup()-> temp


      obs <-  dataset2 %>%
        filter(sim == 1) %>%
        group_by(sim, x) %>%
        summarise(q5 = quantile(obs, quantiles[1]/100), q50 = quantile(obs, quantiles[2]/100), q95 = quantile(obs, quantiles[3]/100)) %>%
        gather(q5, q50, q95 ,value = "value", key = "key")

    }else{


      dataset2 %>%
        group_by(sim, x, group) %>%
        summarise(q5 = quantile(y,  quantiles[1]/100), q50 = quantile(y,  quantiles[2]/100), q95 = quantile(y,  quantiles[3]/100)) %>%
        gather(q5, q50, q95 ,value = "value", key = "key") %>%
        group_by(x, key, group) %>%
        summarise(q5 = quantile(value, CIpct/100),q50 = quantile(value, 0.5), q95 = quantile(value, (100-CIpct)/100)) %>%
        ungroup()-> temp


      obs <-  dataset2 %>%
        filter(sim == 1) %>%
        group_by(sim, x, group) %>%
        summarise(q5 = quantile(obs, quantiles[1]/100), q50 = quantile(obs, quantiles[2]/100), q95 = quantile(obs, quantiles[3]/100)) %>%
        gather(q5, q50, q95 ,value = "value", key = "key")


    }


    if(quantiles[1] == 0 ) temp <- temp %>% filter(key != "q5")
    if(quantiles[2] == 0 ) temp <- temp %>% filter(key != "q50")
    if(quantiles[3] == 0 ) temp <- temp %>% filter(key != "q95")
    ## Initial plot


    legendd <- paste0("Quantiles\n",CIpct, "-50-", 100 - CIpct, "%\nConf. Inter.")

    temp %>%
      mutate(key = case_when(key == "q50"~ paste0("q", quantiles[2]),
                             key == "q5"~ paste0("q", quantiles[1]),
                             key == "q95"~ paste0("q", quantiles[3]))) %>%
      # gather(starts_with("q5"), starts_with("q50"), starts_with("q95") ,value = "value", key = "key") %>%
      ggplot()+
      geom_line(aes(x, q50, col = key))+
      geom_ribbon(aes(x, ymin = q5, ymax = q95, fill = key), alpha = 0.3)+
      scale_color_manual(values = c("dodgerblue1", "brown2", "dodgerblue1")[which(quantiles != 0)])+
      scale_fill_manual(values = c("dodgerblue1", "brown2", "dodgerblue1")[which(quantiles != 0)])+
      geom_line(data = obs, aes(x, value, lty = key))+
      scale_linetype_manual(values = c(2,1,2))+
      theme_bw() +
      labs(col = legendd, fill = legendd, lty = "Observed") -> plot_temp


  }





  #print("bbbb")
    ### Log scale

    if(ylog == T)
      plot_temp <- plot_temp +
      scale_y_log10(breaks = breaks_log, labels = labels_log)

  #print("ccc")
    ### Faceting

    if(group != ""){

      plot_temp <-  plot_temp +
          facet_wrap(~group)

    }

    ### Add observation
  #print("aaa")
    # dot

    data_dot <- dataset2 %>%
      filter(sim == 1 ) %>%
      mutate(BLQ = if_else(obs <= loq_obs, "Yes", "No")) %>%
      mutate(obs = if_else(obs <= loq_obs, as.double(loq_obs), obs))

    plot_temp <- plot_temp +
      geom_point(data =  data_dot  , aes(x,obs, shape = BLQ))+
      scale_shape_manual(values = c(19,8))


    #print(loq_obs)
    #print(typeof(loq_obs))
    if(loq_obs != 0){

      plot_temp <- plot_temp +
        geom_hline(yintercept = loq_obs, lty = 3)

    }

    # stat
    if( stat_obs == T ){

      if(group == ""){


      obstat <- dataset2 %>%
        filter(sim == 1 ) %>%
          group_by(x) %>%
          summarise(q5 = quantile(obs, 0.05), q50 = quantile(obs, 0.50), q95 = quantile(obs, 0.95)) %>%
          gather(q5, q50, q95, value = "value", key = "key")

      }else{

        obstat <- dataset2 %>%
          filter(sim == 1 ) %>%
          group_by(x, group) %>%
          summarise(obs_q5 = quantile(obs, 0.05), obs_q50 = quantile(obs, 0.50), obs_q95 = quantile(obs, 0.95)) %>%
          gather(obs_q5, obs_q50, obs_q95, value = "value", key = "key")

      }

      plot_temp <- plot_temp +
        geom_line(data = obstat, aes(x, value, lty = key))+
        scale_linetype_manual(values = c(3,2,3))+
        labs(lty = "Obs\nQuantiles")



    }

    if(ymin_displayed > 0 ){


      plot_temp <- plot_temp +
        coord_cartesian(ylim = c(ymin_displayed, max(c(temp$q95, dataset$OBS))))


    }

    plot_temp <- plot_temp +
      labs(x  = if_else(xlabel == "", x, xlabel) , y = if_else(ylabel =="", y, ylabel))

return(plot_temp)

}

#
#
# p<- c("SIMUL", "IPRED", "ID", "TIME", "OBS", "EVID", "CMT", "LLOQ", "BLQ",  "X_DOSE", "X_DRUG", "X_CELL", "YTYPE", "Baseline", "IDPK")
# dataset <- read.table( col.names = p ,file = "file:///Z:/ETUDES/SPK/CLSPKPOOL/ANACIN/USERS/TDPE_CB_2/18_11_14_BiophaseMasterProject/ENCOURS_Post_Mentre_Meeting_January/PK_55/NM_VIEWER/1.01.V1_DOSE_vpc.TAB")
#
#
# plot_vpc(dataset = dataset %>% filter(YTYPE == 1),group = "X_DOSE + X_DRUG", type_scatter_CI = "CI",  x = "TIME", y = "IPRED", sim = "SIMUL", obs = "OBS",quantiles = c(5,50,95), loq_obs = 0.1 , ymin_displayed = 0.01)
# plot_vpc(dataset = dataset, filterr = "YTYPE == 1 & TIME < 10", group =c("X_DOSE"), type_scatter_CI = "CI",  x = "TIME", y = "IPRED", sim = "SIMUL", obs = "OBS",quantiles = c(5,50,95), loq_obs = 0.1 , ymin_displayed = 0.01)

# names(dataset)[1] <- "SIM"
#
#
#   # dtest <- d
#   # d <- dtest
#   ## Binage 1
#
#   d <- d %>%
#     mutate(TIME_BIN =  bining(d$TIME, times1_group_2 = bin_None0_Time1_Group2, step_ngroup = nbin))
#
#   ##################################################
#
#
#   if(length(d$OBS)==0){
#     names(d)[grep("OBS", names(d))[[1]]] <- "OBS"
#   }
#
#   # d <- d[d$OBS != 0, ]
#   # d$OBS[d$OBS < 1] <- 1
#   # d$IPRED[d$IPRED < 1] <- 1
#
#   d <- d %>%
#     filter(EVID == 0)
#
#   if("LLOQ" %in% names(d)) d <- d %>% filter(OBS > LLOQ)
#
#   dd <- d %>%
#     filter(SIMUL == 1)
#
#
#   d$lastpoint <- 0
#
#   for(ids in unique(d$ID)){
#
#
#     cmax = max(d$OBS[d$ID == ids])
#     d$lastpoint[d$ID == ids & d$OBS == cmax] <- 1
#
#
#   }
#
#
#
#   ############ Prediction Interval
#   # Gather all sampling, for each time (+- cov) calculate quantile
#   if(cov != ""){
#     IP <- d %>%
#       group_by_(cov, "TIME_BIN") %>%
#       summarise("q5" = quantile(IPRED, 0.05), "q50" = quantile(IPRED, 0.5), "q95" = quantile(IPRED,0.95))
#   }else{
#     IP <- d %>%
#       group_by_("TIME_BIN") %>%
#       summarise("q5" = quantile(IPRED, 0.05), "q50" = quantile(IPRED, 0.5), "q95" = quantile(IPRED,0.95))
#   }
#
#
#   IP <- IP %>%
#     rename(q05.Sim = q5) %>%
#     rename(q95.Sim = q95) %>%
#     rename(q50.Sim = q50 ) %>%
#     gather(q05.Sim, q50.Sim, q95.Sim, key = "Quantiles", value = "values" )
#
#
#   ############ Confidence Interval
#   # Do the same thing but also per simulation
#
#   if(cov != ""){
#     IC <- d %>%
#       group_by_("SIMUL", cov, "TIME_BIN") %>%
#       summarise("q5" = quantile(IPRED, 0.05), "q50" = quantile(IPRED, 0.5), "q95" = quantile(IPRED,0.95))
#   }else{
#     IC <- d %>%
#       group_by_("SIMUL", "TIME_BIN") %>%
#       summarise("q5" = quantile(IPRED, 0.05), "q50" = quantile(IPRED, 0.5), "q95" = quantile(IPRED,0.95))
#   }
#
#   ## And no we take quantiles
#
#   if(cov != ""){
#     ICf <-  IC %>%
#       gather(q5, q50, q95, key = "IC", value = "value") %>%
#       group_by_(cov, "TIME_BIN", "IC") %>%
#       summarise("q5" = quantile(value, 0.05), "q95" = quantile(value, 0.95))
#   }else{
#     ICf <-  IC %>%
#       gather(q5, q50, q95, key = "IC", value = "value") %>%
#       group_by_("TIME_BIN", "IC") %>%
#       summarise("q5" = quantile(value, 0.05), "q95" = quantile(value, 0.95))
#   }
#
#   #### And now observation
#
#   if(cov != ""){
#     Data <- d %>%
#       filter(SIMUL == 1 ) %>%
#       select_(cov, "TIME_BIN", "OBS", "ID", "lastpoint")
#   }else{
#     Data <- d %>%
#       filter(SIMUL == 1 ) %>%
#       select_("TIME_BIN", "OBS", "ID", "lastpoint")
#   }
#
#   if(cov != ""){
#     Datastat <- Data %>%
#       group_by_(cov, "TIME_BIN") %>%
#       summarise("q5" = quantile(OBS, 0.05), "q50" = quantile(OBS, 0.5), "q95" = quantile(OBS,0.95))
#   }else{
#     Datastat <- Data %>%
#       group_by_( "TIME_BIN") %>%
#       summarise("q5" = quantile(OBS, 0.05), "q50" = quantile(OBS, 0.5), "q95" = quantile(OBS,0.95))
#   }
#
#   Datastat <- Datastat %>%
#     rename(q05.Obs = q5) %>%
#     rename(q95.Obs = q95) %>%
#     rename(q50.Obs = q50 ) %>%
#     gather(q05.Obs, q50.Obs, q95.Obs, key = "Observation", value = "values" )
#
#   #### Filtering
#
#   if(qSim[[1]] == 0) IP <- IP %>% filter(Quantiles != "q05.Sim")
#   if(qSim[[2]] == 0) IP <- IP %>% filter(Quantiles != "q50.Sim")
#   if(qSim[[3]] == 0) IP <- IP %>% filter(Quantiles != "q95.Sim")
#
#   if(qCI[[1]] == 0) ICf <- ICf %>% filter(IC != "q5")
#   if(qCI[[2]] == 0) ICf <- ICf %>% filter(IC != "q50")
#   if(qCI[[3]] == 0) ICf <- ICf %>% filter(IC != "q95")
#
#
#   if(qObs[[1]] == 0) ICf <- ICf %>% filter(IC != "q05.Obs  ")
#   if(qObs[[2]] == 0) ICf <- ICf %>% filter(IC != "q50.Obs  ")
#   if(qObs[[3]] == 0) ICf <- ICf %>% filter(IC != "q95.Obs  ")
#
#
#
#
#   ggplot(data = ICf) +
#     labs(title=paste0(object@name, "\nVisual Predicted Check (", max(IC$SIMUL) ,") logarithmic"), x="Time", y="Variable", colour="Simulation", fill = "Confidence\nInterval") +
#     geom_ribbon(aes(x=TIME_BIN, ymin=q5,ymax=q95, alpha=0.2, fill= IC))+
#     # geom_ribbon(aes(x=TIME, ymin=q5.5,ymax=q5.95, alpha=0.2), fill="blue")+
#     # geom_ribbon(aes(x=TIME, ymin=q95.5,ymax=q95.95, alpha=0.2), fill="green")+
#     geom_line(data = IP, aes(TIME_BIN, values, col = Quantiles))+
#     geom_line(data = Datastat, aes(TIME_BIN, values, lty = Observation))+
#     geom_point(data = d %>% filter(SIMUL == 1), aes(TIME, OBS))+
#     # geom_hline(yintercept=25, linetype="dashed", size=1.2)+
#     theme_bw()+
#     guides(alpha=FALSE)+
#     theme(plot.title = element_text(hjust = 0.5))+
#     scale_y_log10(labels = labels_log, breaks = breaks_log)+
#     scale_color_manual(values = c("steelblue1", "brown1","steelblue1")[as.logical(qSim)])+
#     scale_fill_manual(values = c("steelblue1", "brown1","steelblue1")[as.logical(qCI)])+
#     scale_linetype_manual(values = c(2,1,2))-> log ; log
#
#   if(cov != ""){
#     log <- log +   facet_wrap(as.formula(paste0("~",cov)),scales="free"); log
#   }
#
#   arith <- log +
#     labs(title=paste0(object@name, "\nVisual Predicted Check (", max(IC$SIMUL) ,") arithmetic"), x="Time", y="Variable", colour="obs")+
#     scale_y_continuous()
#
#   output <- tibble(Yaxis = c("Log", "Arit"), plot = list(log, arith), run = object@name)
#
#   if(tableOutput == T){
#
#     return(output)
#   }
#
#   if(arit1_log2_both3 == 1) output <- output[2, ]
#   if(arit1_log2_both3 == 2) output <- output[1, ]
#
#
#   # save the file
#   name_file <-str_split(gsub("\\\\","/",object@path), "/")[[1]]  [length( str_split(gsub("\\\\","/",object@path), "/")[[1]] )]
#
#   setwd(paste0(gsub(paste0(name_file,"$"),"", object@path)))
#   name <- paste0(gsub(".+/", "", object@path),"VPC.png")
#
#
#   try(ggsave(filename = name,plot = invoke(plot_grid, output$plot, ncol = 1),  width = 30, height = 20, units = "cm"), silent = T)
#   # shell.exec(name)
#
#   output <-  invoke(plot_grid, output$plot, ncol = 1)
#
#
#
#
# }
#
#
#
#
#
# setMethod(f = "plot_vpc",
#           signature = "run",
#           definition =  function(object, bin_None0_Time1_Group2 = 0, nbin = 5, qObs = c(1,1,1), qSim = c(1,1,1), qCI = c(1,1,1), output_monolix = "pd",  NMBehaviour = "create", cov = "", arit1_log2_both3 = 2, tableOutput = F) {
#             if(cov == "all") cov  <- ""
#             ##################### M O N O L I X #############################
#             if(object@software == "Monolix"){
#
#               #print("Monolix Run")
#               pathvpc <- paste0(object@path,"/VPC.png")
#               vpc <- try(rasterGrob(readPNG(pathvpc), interpolate=TRUE))
#               vpc_plot <- try(ggplot()+
#                 annotation_custom(vpc, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf))
#
#               pathpd <- paste0(object@path,"/prediction_distribution.png")
#               pd <- try(rasterGrob(readPNG(pathpd), interpolate=TRUE))
#               # strangely for me class(pd_plot) is not type-error even i think it should be...
#               if(class(pd) != "try-error"){
#               pd_plot <- try(ggplot()+
#                 annotation_custom(pd, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf))
#               }else{
#                 pd_plot <- pd
#               }
#
#                 if(output_monolix == "both" & class(vpc_plot)[1] != "try-error" & class(pd_plot)[1] != "try-error" ){
#                  output <- plot_grid(vpc_plot, pd_plot, ncol = 1)
#                 }else if (output_monolix =="vpc" | (output_monolix == "both" & class(vpc_plot)[1] != "try-error" & class(pd_plot)[1] == "try-error" )){
#                   output <- vpc_plot
#                 }else{
#                   output <- pd_plot
#                 }
#
#               ##################### N O N M E M #############################
#             }else if(object@software == "NONMEM"){
#               #print("NONMEM Run")
#               test <- try(rasterGrob(readPNG(paste0(object@path,"VPC.png")), interpolate=TRUE) )
#
#
#               if(NMBehaviour == "png" |(NMBehaviour == "ifnopng"  & class(test) != "try-error" ) ){
#                 # try(shell.exec(paste0(object@path, "VPC.png")), silent = T)
#
#                 #print("Loading Pre Existing png file")
#                 pd <- test
#                 output <- ggplot()+
#                   annotation_custom(pd, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)
#
#
#               }else{
#
#
#
#            d <- try(as.tibble(read.table(paste0(object@path,"_VPC.TAB"), header = T)),silent = T)
#          try(
#             if(class(d) =="try-error"){
#              # if one header have been put
#              line <- readLines(paste0(object@path,"_VPC.TAB"))
#              lineToSkip <- c(grep("TABLE NO", line), grep("IPRED", line))
#              line <- line[- lineToSkip]
#
#              lines <- str_split(line, " ")
#              lines <- lapply(lines, function(x){
#                x <- x[x != ""]
#              })
#
#              d <- as.data.frame(do.call(rbind, lines))
#
#              d[] <- lapply(d, function(x){
#                x <- as.double(as.character(x))
#                x
#              })
#
#            }
#          )
#            testpng <- file.exists(paste0(object@path, "VPC.png"))
#            if (NMBehaviour == "ifnopng" & testpng == T){
#              stop <- 1
#            }else{
#              stop <- 0
#            }
#
#            if(class(d) != "try-error" & stop == 0){
#              #print("debut creation run")
#              # find names columnes
#              resFiles <- readLines(paste0(object@path,"_VPC.res"), skipNul = T)
#              resFiles <- gsub('[\r\n\t]', ' ', resFiles )
#              resFiles <- gsub(';.+', '', resFiles )
#              balise <- grep("\\$TABLE", resFiles)
#              balise2 <- grep("NOPRINT", resFiles)
#              colnamesLines <- resFiles[balise:balise2];colnamesLines
#              colnamesLines <- do.call(paste, as.list(colnamesLines))
#              colnamesLines <- gsub("\\$TABLE", "", colnamesLines)
#              colnamesLines <- gsub("NOHEADER.+", "", colnamesLines)
#              colnamesLines <- gsub("NOAPPEND.+", "", colnamesLines)
#              colnamesLines <- gsub("NOPRINT.+", "", colnamesLines)
#              colnames <-  str_split(colnamesLines, " ")[[1]][str_split(colnamesLines, " ")[[1]] !=""]
#              names(d) <- colnames; d
#
#              ############# B I N A G E ###########################
#
#              # dtest <- d
#              # d <- dtest
#              ## Binage 1
#
#              d <- d %>%
#                mutate(TIME_BIN =  bining(d$TIME, times1_group_2 = bin_None0_Time1_Group2, step_ngroup = nbin))
#
#              ##################################################
#
#
#              if(length(d$OBS)==0){
#                names(d)[grep("OBS", names(d))[[1]]] <- "OBS"
#              }
#
#              # d <- d[d$OBS != 0, ]
#              # d$OBS[d$OBS < 1] <- 1
#              # d$IPRED[d$IPRED < 1] <- 1
#
#              d <- d %>%
#                filter(EVID == 0)
#
#              if("LLOQ" %in% names(d)) d <- d %>% filter(OBS > LLOQ)
#
#              dd <- d %>%
#                filter(SIMUL == 1)
#
#
#              d$lastpoint <- 0
#
#              for(ids in unique(d$ID)){
#
#
#                cmax = max(d$OBS[d$ID == ids])
#                d$lastpoint[d$ID == ids & d$OBS == cmax] <- 1
#
#
#              }
#
#
#
#               ############ Prediction Interval
#              # Gather all sampling, for each time (+- cov) calculate quantile
#              if(cov != ""){
#              IP <- d %>%
#                group_by_(cov, "TIME_BIN") %>%
#                summarise("q5" = quantile(IPRED, 0.05), "q50" = quantile(IPRED, 0.5), "q95" = quantile(IPRED,0.95))
#              }else{
#                IP <- d %>%
#                  group_by_("TIME_BIN") %>%
#                  summarise("q5" = quantile(IPRED, 0.05), "q50" = quantile(IPRED, 0.5), "q95" = quantile(IPRED,0.95))
#              }
#
#
#              IP <- IP %>%
#                rename(q05.Sim = q5) %>%
#                rename(q95.Sim = q95) %>%
#                rename(q50.Sim = q50 ) %>%
#                gather(q05.Sim, q50.Sim, q95.Sim, key = "Quantiles", value = "values" )
#
#
#              ############ Confidence Interval
#              # Do the same thing but also per simulation
#
#              if(cov != ""){
#              IC <- d %>%
#                group_by_("SIMUL", cov, "TIME_BIN") %>%
#                summarise("q5" = quantile(IPRED, 0.05), "q50" = quantile(IPRED, 0.5), "q95" = quantile(IPRED,0.95))
#              }else{
#                IC <- d %>%
#                  group_by_("SIMUL", "TIME_BIN") %>%
#                  summarise("q5" = quantile(IPRED, 0.05), "q50" = quantile(IPRED, 0.5), "q95" = quantile(IPRED,0.95))
#              }
#
#              ## And no we take quantiles
#
#              if(cov != ""){
#                ICf <-  IC %>%
#                  gather(q5, q50, q95, key = "IC", value = "value") %>%
#                  group_by_(cov, "TIME_BIN", "IC") %>%
#                  summarise("q5" = quantile(value, 0.05), "q95" = quantile(value, 0.95))
#              }else{
#                ICf <-  IC %>%
#                  gather(q5, q50, q95, key = "IC", value = "value") %>%
#                  group_by_("TIME_BIN", "IC") %>%
#                  summarise("q5" = quantile(value, 0.05), "q95" = quantile(value, 0.95))
#              }
#
#             #### And now observation
#
#              if(cov != ""){
#              Data <- d %>%
#                filter(SIMUL == 1 ) %>%
#              select_(cov, "TIME_BIN", "OBS", "ID", "lastpoint")
#              }else{
#                Data <- d %>%
#                  filter(SIMUL == 1 ) %>%
#                  select_("TIME_BIN", "OBS", "ID", "lastpoint")
#              }
#
#              if(cov != ""){
#              Datastat <- Data %>%
#                group_by_(cov, "TIME_BIN") %>%
#                summarise("q5" = quantile(OBS, 0.05), "q50" = quantile(OBS, 0.5), "q95" = quantile(OBS,0.95))
#              }else{
#                Datastat <- Data %>%
#                  group_by_( "TIME_BIN") %>%
#                  summarise("q5" = quantile(OBS, 0.05), "q50" = quantile(OBS, 0.5), "q95" = quantile(OBS,0.95))
#              }
#
#              Datastat <- Datastat %>%
#                rename(q05.Obs = q5) %>%
#                rename(q95.Obs = q95) %>%
#                rename(q50.Obs = q50 ) %>%
#                gather(q05.Obs, q50.Obs, q95.Obs, key = "Observation", value = "values" )
#
#              #### Filtering
#
#              if(qSim[[1]] == 0) IP <- IP %>% filter(Quantiles != "q05.Sim")
#              if(qSim[[2]] == 0) IP <- IP %>% filter(Quantiles != "q50.Sim")
#              if(qSim[[3]] == 0) IP <- IP %>% filter(Quantiles != "q95.Sim")
#
#              if(qCI[[1]] == 0) ICf <- ICf %>% filter(IC != "q5")
#              if(qCI[[2]] == 0) ICf <- ICf %>% filter(IC != "q50")
#              if(qCI[[3]] == 0) ICf <- ICf %>% filter(IC != "q95")
#
#
#              if(qObs[[1]] == 0) ICf <- ICf %>% filter(IC != "q05.Obs  ")
#              if(qObs[[2]] == 0) ICf <- ICf %>% filter(IC != "q50.Obs  ")
#              if(qObs[[3]] == 0) ICf <- ICf %>% filter(IC != "q95.Obs  ")
#
#
#
#
#              ggplot(data = ICf) +
#                labs(title=paste0(object@name, "\nVisual Predicted Check (", max(IC$SIMUL) ,") logarithmic"), x="Time", y="Variable", colour="Simulation", fill = "Confidence\nInterval") +
#               geom_ribbon(aes(x=TIME_BIN, ymin=q5,ymax=q95, alpha=0.2, fill= IC))+
#                # geom_ribbon(aes(x=TIME, ymin=q5.5,ymax=q5.95, alpha=0.2), fill="blue")+
#                # geom_ribbon(aes(x=TIME, ymin=q95.5,ymax=q95.95, alpha=0.2), fill="green")+
#                geom_line(data = IP, aes(TIME_BIN, values, col = Quantiles))+
#                geom_line(data = Datastat, aes(TIME_BIN, values, lty = Observation))+
#                geom_point(data = d %>% filter(SIMUL == 1), aes(TIME, OBS))+
#                # geom_hline(yintercept=25, linetype="dashed", size=1.2)+
#                theme_bw()+
#                guides(alpha=FALSE)+
#                theme(plot.title = element_text(hjust = 0.5))+
#                scale_y_log10(labels = labels_log, breaks = breaks_log)+
#                scale_color_manual(values = c("steelblue1", "brown1","steelblue1")[as.logical(qSim)])+
#                scale_fill_manual(values = c("steelblue1", "brown1","steelblue1")[as.logical(qCI)])+
#                scale_linetype_manual(values = c(2,1,2))-> log ; log
#
#              if(cov != ""){
#               log <- log +   facet_wrap(as.formula(paste0("~",cov)),scales="free"); log
#              }
#
#              arith <- log +
#                labs(title=paste0(object@name, "\nVisual Predicted Check (", max(IC$SIMUL) ,") arithmetic"), x="Time", y="Variable", colour="obs")+
#                scale_y_continuous()
#
#              output <- tibble(Yaxis = c("Log", "Arit"), plot = list(log, arith), run = object@name)
#
#              if(tableOutput == T){
#
#                return(output)
#              }
#
#              if(arit1_log2_both3 == 1) output <- output[2, ]
#              if(arit1_log2_both3 == 2) output <- output[1, ]
#
#
#              # save the file
#              name_file <-str_split(gsub("\\\\","/",object@path), "/")[[1]]  [length( str_split(gsub("\\\\","/",object@path), "/")[[1]] )]
#
#              setwd(paste0(gsub(paste0(name_file,"$"),"", object@path)))
#              name <- paste0(gsub(".+/", "", object@path),"VPC.png")
#
#
#             try(ggsave(filename = name,plot = invoke(plot_grid, output$plot, ncol = 1),  width = 30, height = 20, units = "cm"), silent = T)
#               # shell.exec(name)
#
#           output <-  invoke(plot_grid, output$plot, ncol = 1)
#
#
#            }}}
#
#             test <- try(output)
#             if(class(test)[1] != "try-error"){
#             return(output)
#             }else{
#               return(output)
#             }
#
#             }
# )
#
# # plot_vpc(vpc(1), arit1_log2_both3 = 2, bin_None0_Time1_Group2 = 1, nbin = 0.2, qCI = c(1,1,1))
#
# setMethod(f = "plot_vpc",
#           signature = "dossier",
#           definition =  function(object, output_monolix = "pd", NMBehaviour = "create", cov = "", arit1_log2_both3 = 3, tableOutput = F){
#
#             tibble(n_run = object@lastSelected) %>%
#               mutate(run = map(n_run, function(x) select_run(object, x))) %>%
#               mutate(ind = map(run, function(x) plot_vpc(x, tableOutput = T, arit1_log2_both3 = arit1_log2_both3, cov = cov))) %>%
#               unnest(ind)
#
#             })
#
