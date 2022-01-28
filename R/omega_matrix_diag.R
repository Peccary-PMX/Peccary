
#' @export
diag_to_pecc <- function(diag){
  # diag <- tibble(ka = "0.3F", cl = "0.2", Vd = "0")
  cmatrix <- diag

  tempmatrix <-  matrix(nrow = length(cmatrix), ncol = length(cmatrix))

  for(a in 1:length(cmatrix)){

    tempmatrix[a, ] <- c(rep("0", a - 1), cmatrix[[a]], rep(NA,length(cmatrix) - a  ))
  }

  tempmatrix <- tempmatrix %>% as.data.frame()
  tempmatrix <- map_dfr(tempmatrix,~ as.character(.x))
  colnames(tempmatrix) <- names(cmatrix)
  rownames(tempmatrix) <- names(cmatrix)

  return(tempmatrix)

}
