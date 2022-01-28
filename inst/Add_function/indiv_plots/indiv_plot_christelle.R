## Authors: Christelle Rodrigues (unless mentioned otherwise)
## License: GPL-3

### Indiv plot ###

### Shiny-App-Input-Start ###
# run = object
# nIndPred2 = N
# logPred2 = log_y
# logxPred2 =
# PredPred2 = pred
# xlabPred = x_lab
# ylabPred = y_lab
# stratTypePred  = strat_type
# freeSalePred2 =
# filterID2 = filterr
# lowerlimitPred =
# upperlimitPred =
# extrVal =
### Shiny-App-Input-End ###

indiv_plot_christelle <- function(N = 10, path = NULL, object=NULL, pred = F, x_lab = "", y_lab = "", log_y = F, strat_type = F, filterr = ""){

  if(!(is.null(path))) {
    tmp <- createRun(path)
  dat <- tmp@IPRED %>%
    left_join(tmp@OBS)
  }

  if(!(is.null(object))) {
    dat <- object@IPRED %>%
      left_join(object@OBS)
  }

  ### added  by Thibaud Derippe
  if(filterr != "") dat <- dat %>%
      filter(!!parse_expr(filterr))
  ### back to main author

  dat %>%
    group_by(ID) %>%
    #filter(get(mdv) == 0) %>%
    nest()%>%
    #{.[[1, "data"]]} -> x ; y = 1
    mutate(plot = map2(data, ID,function(x,y){

      gg <- ggplot(x) +
        xlab(x_lab) + ylab(y_lab) +
        theme_bw() +
        geom_line(aes_string("TIME","IPRED"), col = "green")  +
        ggtitle(paste0("ID = ",y)) +
        geom_point(aes_string("TIME","OBS"), shape = 3, color = "blue")

      if(pred == T)  gg <- gg + geom_line(aes_string(x="TIME",y="PRED"), col = "red", linetype = "dashed")

      if(log_y == T) gg <- gg + scale_y_log10()

      if(strat_type == T)  gg <- gg + facet_wrap(~YTYPE, scales = "free")

      return(gg)

    })) %>%
    rowid_to_column() %>%
    mutate(forplotgrid = ceiling(rowid/N)) %>%
    group_by(forplotgrid) %>%
    nest() %>%
    mutate(plot = map(data,~ invoke(plot_grid, .x$plot))) %>%
    pull(plot)


}
