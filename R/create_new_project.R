#' create_pecc_shiny_project
#' Create a shiny project object
#' @author Thibaud Derippe
#'
#' @export
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
