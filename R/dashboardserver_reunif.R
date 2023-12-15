#' @export
pecc_server_reunif <- function(input, output, session) {


  source(file.path(find.package("peccary"), "ShinyApp", "0SAS_Pecc_ini.R" ), local = T)
  source(file.path(find.package("peccary"), "ShinyApp", "SAS_exploDataset.R" ), local = T)
  source(file.path(find.package("peccary"), "ShinyApp", "SAS_explo_plot.R" ), local = T)
  source(file.path(find.package("peccary"), "ShinyApp", "SAS_NCA.R" ), local = T)
  source(file.path(find.package("peccary"), "ShinyApp", "SAS_ModelBuilding.R" ), local = T)
  source(file.path(find.package("peccary"), "ShinyApp", "SAS_folder.R" ), local = T)
  source(file.path(find.package("peccary"), "ShinyApp", "vpc_pckg.R" ), local = T)
  source(file.path(find.package("peccary"), "ShinyApp", "SAS_report.R" ), local = T)

  # # source("D:/Peccary/inst/ShinyApp/SAS_Pecc_ini.R", local = T)
  # source("D:/Peccary/inst/ShinyApp/SAS_exploDataset.R", local = T)
  # source("D:/Peccary/inst/ShinyApp/SAS_explo_plot.R", local = T)
}


#' @export
pecc_server_reunif_test <- function(input, output, session) {

  source("inst/ShinyApp/0SAS_Pecc_ini.R", local = T)
  source("inst/ShinyApp/SAS_exploDataset.R", local = T)
  source("inst/ShinyApp/SAS_explo_plot.R", local = T)
  source("inst/ShinyApp/SAS_NCA.R", local = T)
  source("inst/ShinyApp/SAS_folder.R", local = T)
  source("inst/ShinyApp/SAS_ModelBuilding.R", local = T)
  source("inst/ShinyApp/SAS_report.R", local = T)
  # module <- list.files("D:/Peccary/inst/Module")
  #
  # if(length(grep("serv\\.R", module)) > 0 ){
  #   for(a in module[grep("serv\\.R", module)]){
  #     print(a)
  #     source(file.path("D:/Peccary/inst/Module", a))
  #   }
  #
  # }

  source("inst/ShinyApp/vpc_pckg.R", local = T)
}

#' @export
go <- function() shinyApp(pecc_ui_reunif, pecc_server_reunif_test)


