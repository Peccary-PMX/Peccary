#' compteur
#' Function for auto-incrementation.  used for saving ggplots in list, before a do.call
#' @author Thibaud Derippe
#'
#' @return la fonction renvoie la moyenne d'un vecteur
#' @import magrittr
#' @export
compteur <- function(){


  i <- 0
  function(){
    i <<- i + 1
    i
  }

}

# data test
#' @export
pecc_df_to_code <- function(df){

  df <- map_if(df, is.factor,~ as.character(.x)) %>%
    as.data.frame()

  expr(tibble(!!!(map(df, ~ expr(!!.x)))))

}

#' @export
copy_tibbled <- function(copy, header = F){


  first_line <-  copy %>%
    gsub(pattern = "\n.*", replacement = "") %>%
    str_split(pattern = "\t")

  ncol <-  length(first_line[[1]])

  if(header ==F)
    colname <- paste0("V", 1:ncol)
  else
    colname <- first_line[[1]]

  eval(parse_expr(

    paste0("tribble(", paste0("~", colname) %>% paste(collapse = ","),",\"",
           copy %>%
             gsub(pattern = "\t|\n", replacement = ",") %>%
             gsub(pattern = ",",replacement = "\",\""), "\")")

  ))

}




####################################### breaks and logs

#' @export
breaks_log <- lapply(-30:30, function(x) 1:9*10^x) %>% reduce(c)

#' @export
labels_log <- as.character(breaks_log); labels_log[-seq(1,length(labels_log),9)] <- ""

#' Ggplot + theme_bw + plot.(sub)title(element.text = hjust(0.5))
#' @export
scale_y_log10_pecc <- function(){

  scale_y_log10(breaks = breaks_log, labels = labels_log )

}

#' Ggplot + theme_bw + title/subtitle centered + caption left and italic
#' @export
ggplot_pecc <- function(data = NULL, mapping = aes()){

  ggplot(data, mapping)+
    theme_bw()+
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.1, face = "italic"))

}




##################################### string to number

#' string to number
#' To transform a string of numbers into numbers (only used in Shiny app)
#' @author Thibaud Derippe
#' @param numbers a string
#' @return la fonction renvoie la moyenne d'un vecteur
#' @export
stringToNumbers <- function( numbers){



  plus <- str_split(numbers, "-")[[1]][1]
  moins <- str_split(numbers, "-")[[1]][2]


  plussuite <- str_split(plus, " ")[[1]][str_split(plus, " ")[[1]] != ""]
  plussuite <-  gsub("i", "", plussuite)
  # if(length(grep("i",plussuite))>0) plussuite <- plussuite[-grep("i",plussuite)]

  moinssuite <- str_split(moins, " ")[[1]][str_split(moins, " ")[[1]] != ""]

  # if(length(grep("i",moinssuite))>0) moinssuite <- moinssuite[-grep("i",moinssuite)]



  numbersplus <- integer()

  for(a in plussuite){

    split <- str_split(a, ":")[[1]]

    if(length(split) == 1){
      numbersplus <- c(numbersplus, as.integer(a))
    } else{

      numbersplus <- c(numbersplus, seq(as.integer(split[1]), as.integer(split[2])))
    }

  }


  numbersmoins <- integer()
  for(a in moinssuite){

    split <- str_split(a, ":")[[1]]

    if(length(split) == 1){
      numbersmoins <- c(numbersmoins, as.integer(a))
    } else{

      numbersmoins <- c(numbersmoins, seq(as.integer(split[1]), as.integer(split[2])))
    }

  }

  if(length(numbersmoins) == 1 & is.na(numbersmoins[[1]]) == T){
    return(unique(numbersplus))
  }else{
    return(unique(numbersplus[- which( numbersplus   %in% numbersmoins )]))
  }
}


################ bining (experimental)


#' bining
#' To transform a string of numbers into numbers (only used in Shiny app)
#' @author Thibaud Derippe
#' @return la fonction renvoie la moyenne d'un vecteur
#' @export
bining <- function(vect, times1_group_2 = 1, step_ngroup ){

  if(times1_group_2 == 0){

    return(vect)

  }
  #### First method

  if(times1_group_2 == 1){

    seqq <- seq(min(vect), max(vect) + step_ngroup, step_ngroup )

    sapply(vect, function(x){

      firstT <- seqq[ x < seqq ][[1]]
      # value <- mean(c(firstT, firstT - step_ngroup))
      return(firstT - step_ngroup)
    })

    #### Second method n group

  } else if(times1_group_2 == 2){

    tibble(vect) %>%
      mutate(groups = cut(vect, breaks = step_ngroup )) -> temp

    return( temp %>%
              left_join(
                tibble(groups = unique(temp$groups)) %>%
                  mutate(newtime = map_dbl(groups, function(x) {

                    strsplit(gsub("\\(|\\[|\\]", "", x), ",")[[1]] %>%
                      as.double() %>%
                      min

                  }))) %>%
                  {.$newtime})

  }


}



####

#' readTablePerso
#' @description Because of monolix while using inter occasion... If read table doesn't work (beause of "#") I do a readLines then I transform it in table (which take times... probably better options exist)
#' @author Thibaud Derippe
#' @param numbers a string
#' @export
readTablePerso <- function(path, header = T, skip = 0){

  d <- try(read.table(path, header = header, skip = skip, comment.char = ""))

  if(class(d) == "try-error"){
    line <- readLines(path, skipNul = T); line
    line <- gsub('[\r\n\t]', ' ', line ); line

    title <-  str_split(line[1], " ")[[1]][str_split(line[1], " ")[[1]] != ""]; title

    line <- line[- 1]
    lines <- str_split(line, " ")
    lines <- lapply(lines, function(x){
      x <- x[x != ""]
    })

    d <- as.data.frame(do.call(rbind, lines))

    d[] <- lapply(d, function(x){

      if(is.na(as.double(as.character(x))[[1]]) == F){
        x <- as.double(as.character(x))
      }
      x
    })


    names(d) <- title
    print("Table Create through ReadLines function")
  }



  return(as.tibble(d))
}


