observeEvent(input$vpc_pckg2_go,{ print("lol") })
# observeEvent(input$vpc_pckg2_simdata,{
# print("ici")
# # #   try({
# # #   path_temp <- input$vpc_pckg2_simdata
# # #
# # # #
# # # #     header <- readLines(path_temp, n = 1)
# # # #     npointvirgule <- length(str_split(header, ";")[[1]])
# # # #     nvirgule <- length(str_split(header, ";")[[1]])
# # #
# # #     sep = " "
# # #     # if(npointvirgule > 1) sep = ";"
# # #     # if(nvirgule > 1) sep = ","
# # #
# # #     vpc_pckg2_sim <- list()
# # #     vpc_pckg2_sim$df <- read.table(path_temp, sep = sep, header = T) %>%
# # #       as_tibble()
# # #
# # #     vpc_pckg2_sim$expr <- expr(read.table(!!path_temp, sep = !!sep, header = T))
# # #
# # #     vpc_pckg2_sim <<- vpc_pckg2_sim
# # #     c(names(vpc_pckg2_sim$df)[grep("ID", names(vpc_pckg2_sim$df))], NA)[[1]]
# # #     updateSelectInput(session, inputId = "vpc_pckg2_simdata_ID", choices = names(vpc_pckg2_sim$df),selected = c(names(vpc_pckg2_sim$df)[grep("ID", names(vpc_pckg2_sim$df))], NA)[[1]])
# # #     updateSelectInput(session, inputId = "vpc_pckg2_simdata_REP", choices = names(vpc_pckg2_sim$df),selected = c(names(vpc_pckg2_sim$df)[grep("REP", names(vpc_pckg2_sim$df))], NA)[[1]])
# # #     updateSelectInput(session, inputId = "vpc_pckg2_simdata_TIME", choices = names(vpc_pckg2_sim$df),selected = c(names(vpc_pckg2_sim$df)[grep("TIME", names(vpc_pckg2_sim$df))], NA)[[1]])
# # #     updateSelectInput(session, inputId = "vpc_pckg2_simdata_PRED", choices = names(vpc_pckg2_sim$df),selected = c(names(vpc_pckg2_sim$df)[grep("PRED", names(vpc_pckg2_sim$df))], NA)[[1]])
# # #   })
# #
# })
# #
