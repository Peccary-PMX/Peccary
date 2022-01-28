tabItem(tabName = "test",


                        plotOutput("vpc_pckg2_plot", width = "700px", height = "700px"),

                        box(collapsible = T, collapsed = F,width = 12,
                            div(style="display:inline-block; width: 400px;height: 75px;", textInput(inputId = "vpc_pckg2_simdata", label = "Path sim")),
                            div(style="display:inline-block; width: 100px;height: 75px;", selectInput(inputId = "vpc_pckg2_simdata_ID", label = "ID", choices = c(""), selected = "")),
                            div(style="display:inline-block; width: 100px;height: 75px;", selectInput(inputId = "vpc_pckg2_simdata_REP", label = "REP", choices = c(""), selected = "")),
                            div(style="display:inline-block; width: 100px;height: 75px;", selectInput(inputId = "vpc_pckg2_simdata_TIME", label = "TIME", choices = c(""), selected = "")),
                            div(style="display:inline-block; width: 100px;height: 75px;", selectInput(inputId = "vpc_pckg2_simdata_PRED", label = "PRED", choices = c(""), selected = "")),
                            div(style="display:inline-block; width: 400px;height: 75px;", textInput(inputId = "vpc_pckg2_obsdata", label = "Path obs")),
                            div(style="display:inline-block; width: 100px;height: 75px;", selectInput(inputId = "vpc_pckg2_obsdata_ID", label = "ID", choices = c(""), selected = "")),
                            div(style="display:inline-block; width: 100px;height: 75px;", selectInput(inputId = "vpc_pckg2_obsdata_TIME", label = "TIME", choices = c(""), selected = "")),
                            div(style="display:inline-block; width: 100px;height: 75px;", selectInput(inputId = "vpc_pckg2_obsdata_DV", label = "DV", choices = c(""), selected = "")),
                            div(style="display:inline-block; width: 100px;height: 75px;", selectInput(inputId = "vpc_pckg2_obsdata_MDV", label = "MDV", choices = c(""), selected = ""))
                        ),

                        div(style="display:inline-block; width: 100px;height: 75px;", actionButton(inputId = "vpc_pckg2_go", label = "create VPC")),
                        div(style="display:inline-block; width: 100px;height: 75px;", selectInput(inputId = "vpc_pckg2_bins", label = "Bins", choices = c("jenks", "density", "time", "data", "none", "pretty"), selected = "jenks")),
                        div(style="display:inline-block; width: 100px;height: 75px;", numericInput(inputId = "vpc_pckg2_n_bins", label = "n_bins", value = 0)),
                        div(style="display:inline-block; width: 100px;height: 75px;", selectInput(inputId = "vpc_pckg2_bin_mid", label = "bin_mid", choices = c("mean", "middle"), selected = "mean")),
                        div(style="display:inline-block; width: 100px;height: 75px;", selectInput(inputId = "vpc_pckg2_software", label = "software", choices = c("auto", "nonmem", "phoenix"), selected = "auto")),
                        div(style="display:inline-block; width: 400px;height: 75px;", selectInput(inputId = "vpc_pckg2_show", label = "show", choices = c("obs_dv", "obs_ci", "pi", "pi_as_area","pi_ci", "obs_median","sim_median","sim_median_ci" ), selected =  c("obs_dv", "obs_ci", "pi_ci", "obs_median","sim_median_ci"), multiple = T)),
                        div(style="display:inline-block; width: 100px;height: 75px;", checkboxInput(inputId = "vpc_pckg2_pred_corr", label = "pred_corr", value = F)),
                        div(style="display:inline-block; width: 150px;height: 75px;", numericInput(inputId = "vpc_pckg2_pred_corr_lower_bnd", label = "pred_corr_lower_bnd",value = 0)),
                        div(style="display:inline-block; width: 100px;height: 75px;", numericInput(inputId = "vpc_pckg2_pi_low", label = "pi_low",value = 0.05)),
                        div(style="display:inline-block; width: 100px;height: 75px;", numericInput(inputId = "vpc_pckg2_pi_high", label = "pi_high",value = 0.95)),
                        div(style="display:inline-block; width: 100px;height: 75px;", numericInput(inputId = "vpc_pckg2_ci_low", label = "ci_low",value = 0.05)),
                        div(style="display:inline-block; width: 100px;height: 75px;", numericInput(inputId = "vpc_pckg2_ci_high", label = "ci_high",value = 0.95)),
                        div(style="display:inline-block; width: 100px;height: 75px;", numericInput(inputId = "vpc_pckg2_uloq", label = "uloq",value = NA)),
                        div(style="display:inline-block; width: 100px;height: 75px;", numericInput(inputId = "vpc_pckg2_lloq", label = "lloq",value = NA)),
                        div(style="display:inline-block; width: 100px;height: 75px;", checkboxInput(inputId = "vpc_pckg2_log_y", label = "log_y", value = F)),
                        div(style="display:inline-block; width: 100px;height: 75px;", numericInput(inputId = "vpc_pckg2_log_y_min", label = "log_y_min",value = 0.001)),
                        div(style="display:inline-block; width: 100px;height: 75px;", textInput(inputId = "vpc_pckg2_xlab", label = "xlab")),
                        div(style="display:inline-block; width: 100px;height: 75px;", textInput(inputId = "vpc_pckg2_ylab", label = "ylab")),
                        div(style="display:inline-block; width: 100px;height: 75px;", textInput(inputId = "vpc_pckg2_title", label = "title")),
                        div(style="display:inline-block; width: 100px;height: 75px;", checkboxInput(inputId = "vpc_pckg2_smooth", label = "smooth", value = T)),
                        div(style="display:inline-block; width: 100px;height: 75px;", selectInput(inputId = "vpc_pckg2_facet", label = "facet", choices = c("wrap", "columns", "rows"), selected = "wrap")),

                        textAreaInput(inputId = "vpc_pckg2_code", label = "Code")
)
