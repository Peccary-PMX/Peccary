#' SetClass S4 run Object
#' Author: Thibaud Derippe
#'
#' @slot name name of the run (usefull for plots title for instance)
#' @slot path root of the run, such as with name + path we can have the complete pathway of the run
#' @slot status  usefull only for NONMEM: is the run successfully converged or did it failed (and a key work of the nature of the error)
#' @slot FO : objective function value
#' @slot Estimation : parameter estimation values with RSE
#' @slot OBS : every observation
#' @slot IPRED : every estimation
#' @slot cov : ID every covariable concerning each ID
#' @slot randomEffect : random effect values
#' @slot residus : residual values
#' @slot administration : sampling schedule
#' @slot loq : loq values
#' @slot software : NONMEM or Monolix
#' @slot date : date of creation


setClass(Class = "project",
         representation = representation("name" = "character",
                                         "root" = "character",
                                         "data" = "list",
                                         "model" = "list"

         )
)


#' dossier_fill_COMMUN
#' Author: Thibaud Derippe
setGeneric("project_fill<-",
           function(object, value){standardGeneric("project_fill<-")}
)






setReplaceMethod(
  f="project_fill",
  signature = "project",
  definition = function(object, value){



    value <- gsub("^file:///", "", value)

    # For link like that "file://emsw0136/_cinetiq/CINETIQ/2_Scientifique/Software/R/Package_Thibaud/Exemple_demo"
    value <- gsub("file:", "", value)

    # First we store the pathway to the racine attribute
    object@root <- value

    return(object)

  })


object <- new("project")
path <- "emsw0136/_cinetiq/CINETIQ/2_Scientifique/Software/R/Package_Thibaud/Exemple_demo"
project_fill(object) <- gsub("\"", "", path)



object


