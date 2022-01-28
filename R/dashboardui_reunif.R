# library(shiny)
# library(shinydashboard)
#

#' @export
pecc_ui_reunif <- function(){dashboardPage(


  # Header ------------------------------------------------------------------


  dashboardHeader(title = "Peccary",



                  dropdownMenuOutput(outputId = "messages")
  ),


  # Sidebar -----------------------------------------------------------------


  dashboardSidebar(

    sidebarMenu(
      style = "overflow: visible",
      menuItem("Project", tabName = "Welcome", icon = icon("Project")
               # menuSubItem("Project", tabName = "", icon = icon("dashboard")),

      ),
      menuItem("Pre-modelisation", icon = icon("bacon"), tabName = "Pre-modelisation",
               menuSubItem("Datasets", tabName = "Datasets", icon = icon("dashboard")),
               menuSubItem("Dataset explo", tabName = "datasetExplo", icon = icon("dashboard")),
               menuSubItem("Graphical exploration", tabName = "graph", icon = icon("dashboard")),
               menuSubItem("NCA", tabName = "NCA", icon = icon("dashboard")),
               menuSubItem("PKNCA", tabName = "PKNCA", icon = icon("dashboard")),
               selectInput(inputId = "preloadeddataset", label = "Preloaded dataset", choices = c("Use external")),
               # textInput("pathExplo", "Path of a dataset", value = ""),
               textAreaInput("filterrExplo", "Filter", value = ""),
               selectInput(inputId = "exploX", label = "X", choices = ""),
               selectInput(inputId = "exploY", label = "Y", choices = "")
               # actionButton("loadExplo", "Load"),






      ),
      menuItem("Model building ", icon = icon("deSolve"), tabName = "deSolve"),
      # menuItem("Run manager ", icon = icon("deSolve"), tabName = "runmanager"),
      menuItem("Models analysis",icon = icon("deSolve"), tabName = "modelAnalysis",

               textInput("path2", "Path to folder", value = "D:/Peccary/Exemple_demo"),
               actionButton("folderupdate", "Go"),
               selectInput(inputId = "subPath",multiple = F, label = "subPath", choices = "Root", selected = "subF"),


               textInput("number", "Runs" ),

               menuSubItem("Folder", tabName = "folder", icon = icon("dashboard")),
               menuSubItem("Preds", tabName = "prediction", icon = icon("dashboard")),
               menuSubItem("GOF", tabName = "GOF", icon = icon("dashboard")),
               menuSubItem("Covariate", tabName = "covariate", icon = icon("dashboard")),
               menuSubItem("Matrix", tabName = "matrix", icon = icon("dashboard")),
               menuSubItem("iLL", tabName = "iLL", icon = icon("dashboard")),
               menuSubItem("Sensitivity", tabName = "sensitivity", icon = icon("dashboard")),
               menuSubItem("Forest", tabName = "forestPlot", icon = icon("dashboard")),
               menuSubItem("VPC", tabName = "simulation", icon = icon("dashboard")),
               menuSubItem("Plot saved", tabName = "rapports" , icon = icon("dashboard")),
               actionButton("allplotbutton", "AllPlots"),
               textInput(inputId = "gofunits_obs", label = "units obs"),
               textInput(inputId = "gofunits_time", label = "units time"),

               radioButtons("variable", "CovPrim:",
                            choices = "none"
                            # c( "none" = "none",
                            #   "PRED" = "pred",
                            #   "GOF" = "GOF",
                            #   "RE_Boxplot" = "box",
                            #   "RE_matrix" = "matrix")
               ),

               radioButtons("variable2", "CovSec:",
                            choices = "none"
                            # c( "none" = "none",
                            #   "PRED" = "pred",
                            #   "GOF" = "GOF",
                            #   "RE_Boxplot" = "box",
                            #   "RE_matrix" = "matrix")
               )
      ),

      menuItem("Manual", tabName = "ManualFunc", icon = icon("deSolve")),
      menuItem("vpc_pckg", tabName = "vpc_pckg", icon = icon("deSolve")),
      menuItem("Report", tabName = "Report", icon = icon("deSolve")),


      map(unique(gsub("(_serv)|(_ui)|(\\.R)", "", list.files("D:/Peccary/inst/Module"))), function(x){

        menuItem(x, tabName = x, icon = icon("deSolve"))
      }),
      menuItem("Authors", tabName = "Authors", icon = icon("deSolve"))



    )



  ),
  dashboardBody(

    useShinyjs(),
    do.call(tabItems, append(list(

      tabItem(tabName = "Welcome",
              h2("Welcome to Peccary !", align = "center"),
              br("With Peccary, pharmacometrics becomes simpler and faster"),
              br("You can find a html tutorial to use this shiny App here:"),
              file.path(find.package("peccary"), "Peccary_Documentation.html"),
              br(""),
              tags$a(href="https://github.com/Peccary-PMX/Peccary/releases/download/R4.0/Peccary_Documentation.html", "also downlodable here", target="_blank" ),
              br(""),
              br("Along with a youtube video:"),
              tags$a(href = "https://www.youtube.com/watch?v=d77c4v2_jcw","https://www.youtube.com/watch?v=d77c4v2_jcw"),
              h3("Load Project"),
              br("First, do you have a Peccary project you want to load?"),
              br(""),
              selectInput(inputId = "recentprojectfile", label = "Previous Project", width ="800", choices = c("No Previous Project Available")),
              br(""),
              textInput(inputId = "Project_file", label = "Path to Project File", width = "800", value = ""),
              actionButton(inputId = "Project_load", label = "Load project"),
              br("(if you want to change the loaded project, it is safier to restart Peccary Shiny App first)"),
              br(""),
              h3("Create Project"),
              br("Otherwise, do you want to create one?"),
              br(""),
              textInput(inputId = "Project_file_create", label = "Set where to put the project", width = "800", value = "file:///D:/these/demo"),
              # br("Do you want to create a complete project folder with subfolders? It will help you to store your files"),
              # checkboxInput(inputId = "creation_folder_cb", label = "Yes, create folders and subfolders", value = T),
              br("Click on the button below and let's start !"),
              br(""),
              actionButton(inputId = "Project_create", label = "Create project")


      ),


      # Datasets handling -------------------------------------------------------


      tabItem(tabName = "Datasets",
              h2("Datasets Loading", align = "center"),
              h3("Dataset loading and input settings"),
              br("Here is the place where you can store your data files, make graphical exploration, NCA analysis and pre-modelling work !"),
              br("First, let's look at wich data are available: "),
              rHandsontableOutput(outputId = "datasets"),
              br("If there is no data files, you can first  try to upload the one in the 1_DATA folder "),
              br(""),
              actionButton(inputId = "load_data_folders", label = "Try automatic loading"),
              br(""),
              br("Otherwise, you can add manually a file or just temporary use:"),
              br(""),
              textInput(inputId = "path_dataset_manual", label = "Add path to a dataset", width = "800"),

              fluidRow(
                div(style="display:inline-block; width: 100px;height: 75px;",  selectInput("sepExplo", "Sep", choices = c("Space" = "", ";", ".", ","), selected = "Space")),
                div(style="display:inline-block; width: 100px;height: 75px;", selectInput("decExplo", "Dec", choices = c(".", ","), selected = ".")),
                div(style="display:inline-block; width: 100px;height: 75px;",  textInput("nastringExplo", "NA", value = ".")),
                div(style="display:inline-block; width: 100px;height: 75px;",  actionButton("addDataset", label = "add"))
              ),
              br(""),
              numericInput(inputId = "dataset_default", label = "Choose which dataset you want to open by default", value = 1),
              br(""),
              br("After additions, please make sure to save them"),
              actionButton(inputId = "save_datasets", label = "Save modifications"),
              h3("Dataset informations (column header)"),
              br("here, you can simply fill the informations on the table above"),
              fluidRow(
                div(style="display:inline-block; width: 100px;height: 75px;", numericInput(inputId = "quicklookn", label = "Number of the dataset", value = 0, min = 0)),
                div(style="display:inline-block; width: 100px;height: 75px;", selectInput(inputId = "quicklooksep", label = "sep", choices = c("", " ", ";", ",", "."), selected = ";")),
                div(style="display:inline-block; width: 100px;height: 75px;",selectInput(inputId = "quicklookdec", label = "dec", choices = c(""," ", ";", ",","."), selected = ".")),
                div(style="display:inline-block; width: 100px;height: 75px;", textInput(inputId = "quicklookna", label = "na_string", value = ".")),
                div(style="display:inline-block; width: 100px;height: 75px;", textInput(inputId = "quicklookx", label = "x", value = "")),
                div(style="display:inline-block; width: 100px;height: 75px;",textInput(inputId = "quicklookxlabel", label = "x label", value = "")),
                div(style="display:inline-block; width: 100px;height: 75px;",textInput(inputId = "quicklooky", label = "y", value = "")),
                div(style="display:inline-block; width: 100px;height: 75px;",textInput(inputId = "quicklookylabel", label = "y label", value = "")),
                div(style="display:inline-block; width: 400px;height: 75px;",textInput(inputId = "quicklookcomment", label = "comment", value = ".")),
                # div(style="display:inline-block; width: 100px;height: 75px;",actionButton(inputId = "quicklookload", label = "load")),
                div(style="display:inline-block; width: 100px;height: 75px;",actionButton(inputId = "quicklooksave", label = "Import"))
                # div(style="display:inline-block; width: 150px;height: 75px;", selectInput(inputId = "colourPred2", label = "COV", choices = "None"))
              ),
              fluidRow(
                div(style="display:inline-block; width: 100px;height: 75px;", selectInput(inputId =  "IDheader", label = "ID", choices = c(""), selected = "")),
                div(style="display:inline-block; width: 100px;height: 75px;", selectInput(inputId =  "IDheader2", label = "TIME", choices = c(""), selected = "")),
                div(style="display:inline-block; width: 100px;height: 75px;", selectInput(inputId = "OBSheader", label = "OBS", choices = c(""), selected = "")),
                div(style="display:inline-block; width: 100px;height: 75px;", selectInput(inputId = "AMTheader", label = "AMT", choices = c(""), selected = "")),
                div(style="display:inline-block; width: 100px;height: 75px;", selectInput(inputId = "ADMheader", label = "ADM", choices = c(""), selected = "")),
                div(style="display:inline-block; width: 100px;height: 75px;", selectInput(inputId = "RATEheader", label = "RATE", choices = c(""), selected = "")),
                div(style="display:inline-block; width: 100px;height: 75px;", selectInput(inputId = "EVIDheader", label = "EVID", choices = c(""), selected = "")),
                div(style="display:inline-block; width: 100px;height: 75px;", selectInput(inputId = "MDVheader", label = "MDV", choices = c(""), selected = "")),
                div(style="display:inline-block; width: 100px;height: 75px;", selectInput(inputId = "YTYPEheader", label = "YTYPE", choices = c(""), selected = "")),
                div(style="display:inline-block; width: 100px;height: 75px;", selectInput(inputId = "COVCONTheader", label = "cov.cont", choices = c(""), selected = "")),
                div(style="display:inline-block; width: 100px;height: 75px;", selectInput(inputId = "COVCATheader", label = "cov.cat", choices = c(""), selected = ""))
              )


              # selectInput(inputID = "headerEVID", label = "EVID"),
              # selectInput(inputID = "headerMDV", label = "MDV"),
              # selectInput(inputID = "headerYTYPE", label = "YTYPE"),
              # selectInput(inputID = "headerCOVCONT", label = "cov.cont"),
              # selectInput(inputID = "headerCOVCAT", label = "cov.cat"),
              # rHandsontableOutput(outputId = "defineheader", height = 200),
              # DT::dataTableOutput(outputId = "quicklookTable")




      ),

      tabItem(tabName = "widgets",
              h2("Widgets tab content"),


              fluidRow(
                box(plotOutput("plot1", height = 250)),

                box(
                  title = "Controls",
                  sliderInput("slider", "Number of observations:", 1, 100, 50)
                )
              )
      ),


      # Dataset exploration ---------------------------------------------------


      tabItem(tabName = "datasetExplo",
              div(style="display:inline-block; width: 300px;height: 75px;",  selectInput(inputId = "dataexploVersion", label = "Version", choices = c(""), selected = "")),
              div(style="display:inline-block; width: 75px;height: 75px;",    actionButton(inputId = "dataexploLoad", label = "Load")),
              div(style="display:inline-block; width: 300px;height: 75px;",    textInput(inputId = "dataexplonewVersion", label = "Name")),
              div(style="display:inline-block; width: 75px;height: 75px;",    actionButton(inputId = "dataexploSave", label = "Save")),

              textInput(inputId = "filtertableexplo", label = "Exploratory filter", value = ""),

              fluidRow(
                column(4,
                       textAreaInput(inputId = "tableExploManipulation", label = "Free Manipulation (temp being loaded dataset)", resize = "both", height = 180),
                ),
                column(1,
                       br(""),
                       actionButton(inputId = "storemodif", label = "Store modif"),
                       br(""),
                       actionButton(inputId = "copymodif", label = "Create file with modif"),
                       # br(""),
                      # actionButton(inputId = "overwrite", label = "Overwrite original ds")
                )
              ),


              div(style="display:inline-block; width: 150px;height: 75px;", selectInput(inputId = "groupbyCovExplo", label = "Extract cov by", multiple = T, choices = "")),
              # div(style="display:inline-block; width: 150px;height: 75px;", checkboxInput(inputId = "covreduceExplo", label = "Cov reduce", value = T)),
              actionButton(inputId = "abtableexplo", label = "Go"),

              dataTableOutput("tableexplo"),

              box(title = "table1", collapsible = T, collapsed = T,width = 1000,
                  div(style="display:inline-block; width: 150px;height: 75px;", selectInput(inputId = "table1x", label = "In rows", multiple = T, choices = "")),
                  div(style="display:inline-block; width: 150px;height: 75px;", selectInput(inputId = "table1y", label = "In col", multiple = F, choices = "")),
                  div(style="display:inline-block; width: 150px;height: 75px;", actionButton(inputId = "table1go", label = "Go")),
                  rHandsontableOutput(outputId = "table1output"),

                  box(title = "Code", collapsible = T, collapsed = T, width = 400,


                      textAreaInput(inputId = "table1_code",label = "Code",  width = "100%")


                  )
              ),





              box(title = "Count", collapsible = T, collapsed = T,width = 1000,
                  div(style="display:inline-block; width: 150px;height: 75px;", selectInput(inputId = "countwhat", label = "Count what", multiple = F, choices = "")),
                  div(style="display:inline-block; width: 150px;height: 75px;", selectInput(inputId = "countx", label = "Row", multiple = F, choices = "")),
                  div(style="display:inline-block; width: 150px;height: 75px;", selectInput(inputId = "county", label = "Col", multiple = F, choices = "")),
                  div(style="display:inline-block; width: 150px;height: 75px;", actionButton(inputId = "countgo", label = "Go")),
                  textOutput(outputId = "countbytext"),
                  rHandsontableOutput(outputId = "countoutput"),

                  box(title = "Code", collapsible = T, collapsed = T, width = 400,


                      textAreaInput(inputId = "count_code",label = "Code",  width = "100%")


                  )
              ),


              box(title = "plot cov", collapsible = T, collapsed = T,width = 1000,
                  div(style="display:inline-block; width: 150px;height: 75px;", selectInput(inputId = "plotcov_x", label = "covariate 1", multiple = F, choices = "")),
                  div(style="display:inline-block; width: 150px;height: 75px;", selectInput(inputId = "plotcov_y", label = "covariate 2", multiple = F, choices = "")),
                  div(style="display:inline-block; width: 150px;height: 75px;", actionButton(inputId = "plotcov_go", label = "Go")),
                  shinyjqui::jqui_resizable(plotOutput("plotcovplot", width = "800px", height = "600px")),

                  box(title = "Code", collapsible = T, collapsed = T, width = 400,


                      textAreaInput(inputId = "plotcov_code",label = "Code",  width = "100%")


                  )
              )

              # actionButton("loadexplo", label = "Start"),
              # fluidRow(
              #
              #   div(style="display:inline-block; width: 150px;height: 75px;", selectInput(inputId = "groupbyExplo", label = "Group by", multiple = T, choices = "")),
              #   div(style="display:inline-block; width: 150px;height: 75px;",   selectInput(inputId = "tableexploselected",label = "Table2", choices = c("tally", "distinct"))),
              #   div(style="display:inline-block; width: 150px;height: 75px;",   actionButton(inputId = "launchtableexplo2", label = "Load second table"))
              # ),
              # dataTableOutput("tableexplo2"),
              # fluidRow(
              #
              #   div(style="display:inline-block; width: 150px;height: 75px;", selectInput(inputId = "groupbyExplo3", label = "Variable one", choices = "")),
              #   div(style="display:inline-block; width: 150px;height: 75px;",   selectInput(inputId = "tableexploselected3",label = "Table3", choices = c("percentage","table1","detail" ))),
              #   div(style="display:inline-block; width: 150px;height: 75px;",   actionButton(inputId = "launchtableexplo3", label = "Load third table"))
              # ),
              # dataTableOutput("tableexplo3")
      ),

      # graph dataset -----------------------------------------------------------


      tabItem(tabName = "graph",


              conditionalPanel(
                condition = 'input.exploPlotly == false',
                shinyjqui::jqui_resizable(plotOutput("exploPlot", width = "800px", height = "700px"))
              ),

              conditionalPanel(
                condition = 'input.exploPlotly == true',
                shinyjqui::jqui_resizable( plotlyOutput("exploPlotly", width = "800px", height = "700px"))
              ),

              box(
                rHandsontableOutput(outputId = "PlotexplorationSaved"),
                actionButton(inputId =  "pdfPLotExplo", label = "Pdf export"),
                title = "Plots saved", collapsible = T, collapsed = F),


              fluidRow(

                div(style="display:inline-block; width: 100px;height: 75px;", checkboxInput(inputId = "exploPlotly", label = "Plotly", value = FALSE)),
                div(style="display:inline-block; width: 250px;height: 75px;", textInput("secondFilter", "second Filter", value = "")),
                div(style="display:inline-block; width: 100px;height: 75px;", checkboxInput(inputId = "allbackground", label = "grey_all", value = FALSE)),
                div(style="display:inline-block; width: 150px;height: 75px;", selectInput(inputId = "exploCol", label = "Col", choices = "")),

                div(style="display:inline-block; width: 150px;height: 75px;", selectInput(inputId = "exploID", label = "Group", choices = "")),
                div(style="display:inline-block; width: 150px;height: 75px;", selectInput(inputId = "exploShape", label = "Shape", choices = "")),
                div(style="display:inline-block; width: 150px;height: 75px;", selectInput(inputId = "exploLty", label = "Lty", choices = "")),
                div(style="display:inline-block; width: 150px;height: 75px;", selectInput(inputId = "exploWrap", label = "Wrap", choices = "")),
                div(style="display:inline-block; width: 150px;height: 75px;", selectInput(inputId = "exploGrid", label = "Grid", choices = "")),
                div(style="display:inline-block; width: 150px;height: 75px;", selectInput(inputId = "exploStand", label = "Standardization", choices = "")),
                div(style="display:inline-block; width: 100px;height: 75px;", checkboxInput(inputId = "exploXlog", label = "Xlog", value = FALSE)),
                div(style="display:inline-block; width: 100px;height: 75px;", checkboxInput(inputId = "exploYlog", label = "Ylog", value = TRUE)),
                div(style="display:inline-block; width: 100px;height: 75px;", checkboxInput(inputId = "exploMedian", label = "Median", value = FALSE)),
                div(style="display:inline-block; width: 100px;height: 75px;", checkboxInput(inputId = "exploNA", label = "Remove NA", value = TRUE)),
                div(style="display:inline-block; width: 150px;height: 75px;", numericInput(inputId = "exploLOQ", label = "LOQ", value = -1, step = 1)),
                # div(style="display:inline-block; width: 100px;height: 75px;", checkboxInput(inputId = "all_bkgrd", label = "bkgd", value = FALSE)),
                div(style="display:inline-block; width: 150px;height: 75px;", selectInput("scaleExplo", "Wrap scale",selected = "free", choices = c("fixed", "free", "free_x", "free_y"))),
                div(style="display:inline-block; width: 150px;height: 75px;", textInput(inputId = "plotexploname", label = "Name")),
                div(style="display:inline-block; width: 150px;height: 75px;", actionButton(inputId = "plotexplosaveaction",label = "Save"))


              ),

              box(width = 1000,
                  div(style="display:inline-block; width: 150px;height: 75px;", numericInput(inputId = "exploPoint", label = "Point-Alpha", value = 1, min = 0, max = 1, step = 0.1)),
                  div(style="display:inline-block; width: 150px;height: 75px;", numericInput(inputId = "exploLine", label = "Line-Alpha", value = 1, min = 0, max = 1, step = 0.1)),
                  div(style="display:inline-block; width: 150px;height: 75px;", numericInput(inputId = "all_bkgrdalpha", label = "bckgd_alpha", min = 0, max = 1 , value = 0.3, step = 0.1)),
                  div(style="display:inline-block; width: 150px;height: 75px;", textInput("titleExplo", "Title", value = "")),
                  div(style="display:inline-block; width: 150px;height: 75px;", textInput("subtitleExplo", "subtitle", value = "")),
                  div(style="display:inline-block; width: 150px;height: 75px;", textInput("xlabsExplo", "xlabs", value = "")),
                  div(style="display:inline-block; width: 150px;height: 75px;", textInput("ylabsExplo", "ylabs", value = "")),
                  div(style="display:inline-block; width: 150px;height: 75px;", textInput("captionExplo", "caption", value = "")),
                  div(style="display:inline-block; width: 150px;height: 75px;", textInput("colLabExplo", "colLab", value = "")),
                  div(style="display:inline-block; width: 150px;height: 75px;", textInput("colValuesExplo", "colValues", value = "")),
                  div(style="display:inline-block; width: 150px;height: 75px;", textInput("shapeLabExplo", "shapeLab", value = "")),
                  div(style="display:inline-block; width: 150px;height: 75px;", textInput("shapeValuesExplo", "shapeValues", value = "")),
                  div(style="display:inline-block; width: 150px;height: 75px;", textInput("ltyLabExplo", "ltyLab", value = "")),
                  div(style="display:inline-block; width: 150px;height: 75px;", textInput("ltyValuesExplo", "ltyValues", value = "")),
                  div(style="display:inline-block; width: 150px;height: 75px;", numericInput(inputId = "sizeTextExplo", label = "Size Text", value = 15, step = 1)),
                  div(style="display:inline-block; width: 150px;height: 75px;", actionButton(inputId = "addlineexplo",label = "addline")),
                  div(style="display:inline-block; width: 150px;height: 75px;", actionButton(inputId = "removelineexplo",label = "removelin")),
                  rHandsontableOutput(outputId = "addlinesexploration"),

                  title = "More control",
                  collapsible = T, collapsed = T),



              box(width = 1000,
                textAreaInput(inputId = "codeexplopec", label = "Short Peccary Code ", height = 100,width = 1000),
                textAreaInput(inputId = "codeexplo", label = "Peccary Independant Code", height = 300,width = 1000),
                title = "R code", collapsible = T, collapsed = T)




      ),

      # NCA ---------------------------------------------------------------------


      tabItem(tabName = "NCA",


              shinyjqui::jqui_resizable(plotOutput("plotNCAcov", width = "800px", height = "600px")),

              box(
                rHandsontableOutput(outputId = "NCASaved"),
                actionButton(inputId =  "pdfNCA", label = "NCA export"),
                title = "Plots saved", collapsible = T, collapsed = F),

              div(style="display:inline-block; width: 150px;height: 75px;", selectInput(inputId = "groupNCA", label = "Group", choices = "", multiple = T)),
              div(style="display:inline-block; width: 150px;height: 75px;", selectInput(inputId = "covNCA", label = "Cov", choices = "")),
              div(style="display:inline-block; width: 150px;height: 75px;", selectInput(inputId = "blqNCA", label = "BLQ", choices = "")),
              # div(style="display:inline-block; width: 150px;height: 75px;", selectInput(inputId = "evidNCA", label = "EVID", choices = "")),
              div(style="display:inline-block; width: 150px;height: 75px;", numericInput(inputId = "Nsignif", label = "Nsignif", value = 3, min = 0, step = 1)),
              div(style="display:inline-block; width: 150px;height: 75px;", selectInput(inputId = "nca_admin", label = "Admin option", choices = c("None", "IVbolus -> backextrapolation", "Other -> add observation = 0 at time = 0"))),
              div(style="display:inline-block; width: 150px;height: 75px;", numericInput(inputId = "auc_0_x", label = "AUC_0_x", value = 0, min = 0, step = 1)),

              div(style="display:inline-block; width: 150px;height: 75px;", checkboxInput(inputId = "nca_forcecat", label = "ForceBoxplot", value = F)),
              div(style="display:inline-block; width: 150px;height: 75px;", checkboxInput(inputId = "nca_teststat", label = "TestStat", value = T)),
              div(style="display:inline-block; width: 150px;height: 75px;", checkboxInput(inputId = "nca_bxpltpoint", label = "ShowBPpoints", value = T)),
              div(style="display:inline-block; width: 150px;height: 75px;", checkboxInput(inputId = "nca_corCI", label = "ShowCI", value = F)),
              div(style="display:inline-block; width: 150px;height: 75px;", checkboxInput(inputId = "nca_corregline", label = "ShowRegLine", value = T)),
              div(style="display:inline-block; width: 150px;height: 75px;", selectInput(inputId = "nca_method_box", label = "Stat boxplot", choices = c("wilcox.test","t.test", "anova", "kruskal.test") )),
              div(style="display:inline-block; width: 150px;height: 75px;", selectInput(inputId = "nca_method_corr", label = "Stat correlation", choices = c("spearman","pearson", "kendall") )),
              div(style="display:inline-block; width: 150px;height: 75px;", checkboxInput(inputId = "nca_xlog", label = "xlog", value = F)),
              div(style="display:inline-block; width: 150px;height: 75px;", checkboxInput(inputId = "nca_ylog", label = "ylog", value = T)),
              div(style="display:inline-block; width: 150px;height: 75px;", textInput(inputId = "nca_unitx", label = "Unit x ", value = "")),
              div(style="display:inline-block; width: 150px;height: 75px;", textInput(inputId = "nca_unity", label = "Unit y ", value = "")),
              div(style="display:inline-block; width: 150px;height: 75px;", textInput(inputId = "nca_labelx", label = "Label x ", value = "")),
              div(style="display:inline-block; width: 150px;height: 75px;", textInput(inputId = "nca_labely", label = "Label y ", value = "")),
              div(style="display:inline-block; width: 150px;height: 75px;", textInput(inputId = "nca_tabletitle", label = "Title", value = "")),
              div(style="display:inline-block; width: 150px;height: 75px;", textInput(inputId = "nca_name", label = "Name ", value = "")),
              div(style="display:inline-block; width: 150px;height: 75px;", actionButton(inputId = "nca_save", label = "Save")),
            # selectInput(inputId = "nca_table_display", label = "Table type", choices = c("Individuals", "Per cov")),
            br(""),
            h3("Individual Values"),
              DT::dataTableOutput("tableNCA"),

            h3("Stat per cov"),
            DT::dataTableOutput("tableNCA2"),
     box(title = "Export & Code", collapsible = T, collapsed = T, width = 400,

         fluidRow(
           div(style="display:inline-block; width: 300px;height: 75px;", textInput(inputId = "nca_path_export", "Path to export", value = "")),
           div(style="display:inline-block; width: 200px;height: 75px;",  selectInput("nca_to_export", "What to export", choices = c("Individuals", "Per cov", "Both"))),
           div(style="display:inline-block; width: 100px;height: 75px;",  actionButton("nca_export_go", "Launch")),
           textAreaInput(inputId = "nca_code_peccdep",label = "Short Code Peccary",  width = "100%",height = 100),
           textAreaInput(inputId = "nca_code_peccindep",label = "Peccary Independent Code",  width = "100%",height = 500)
         )

     )

      ),


# PKNCA -------------------------------------------------------------------


      tabItem("PKNCA",

              # h2("PKNCA package", align = "center"),
              # h3("made by Bill Denney et. al", align = "center"),
              # br(""),
              div(style="display:inline-block; width: 150px;height: 75px;", actionButton(inputId = "pknca_go", label = "Go")),
              # div(style="display:inline-block; width: 150px;height: 75px;", selectInput(inputId = "pknca_y", label = "NCA Y", choices = names(Theoph), selected = "conc")),
              # div(style="display:inline-block; width: 150px;height: 75px;", selectInput(inputId = "pknca_x", label = "NCA X", choices = names(Theoph), selected = "Time")),
              div(style="display:inline-block; width: 150px;height: 75px;", selectInput(inputId = "pknca_id", label = "NCA ID", choices = names(Theoph), selected = "Subject")),
              div(style="display:inline-block; width: 150px;height: 75px;", selectInput(inputId = "pknca_dose", label = "NCA Dose", choices = names(Theoph), selected = "Dose")),
              div(style="display:inline-block; width: 150px;height: 75px;", selectInput(inputId = "pknca_ADM", label = "NCA EVID", choices = names(Theoph), selected = "Dose")),
              div(style="display:inline-block; width: 150px;height: 75px;", selectInput(inputId = "pknca_route", label = "NCA route", choices = c("extravascular", "IV bolus", "IV perf (rate)", "IV perf (time perf)"), selected = "intravascular")),
              div(style="display:inline-block; width: 150px;height: 75px;", selectInput(inputId = "pknca_rateduration", label = "RateDuration", choices =names(Theoph), selected = "Dose")),
              div(style="display:inline-block; width: 150px;height: 75px;", selectInput(inputId = "pknca_cov", label = "Cov group", choices = names(Theoph), selected = "Dose")),
              div(style="display:inline-block; width: 300px;height: 75px;", checkboxGroupInput(inputId = "pknca_output_sel", label = "output", choices = c("Indiv.", "Summary", "plot"), selected = c("Indiv.", "Summary", "plot"),inline = T)),
              box(title = "Option of PKNCA package",width = 10, collapsible = T, collapsed = T,
                  div(style="display:inline-block; width: 150px;height: 75px;", selectInput(inputId = "pknca_auc.method", label = "auc.method", choices = c("lin up/log down", "linear"), selected = "lin up/log down")),
                  div(style="display:inline-block; width: 150px;height: 75px;", numericInput(inputId = "pknca_adj.r.squared.factor", label = "adj.r.squared.factor",value = 0.0001)),
                  div(style="display:inline-block; width: 150px;height: 75px;", selectInput(inputId = "pknca_max.missing", label = "max.missing",selected = "drop", choices = "drop")),
                  div(style="display:inline-block; width: 150px;height: 75px;", selectInput(inputId = "pknca_conc.na", label = "conc.na",choices = c("drop", "keep"), selected = "drop" )),
                  div(style="display:inline-block; width: 150px;height: 75px;", checkboxInput(inputId = "pknca_first.tmax", label = "first.tmax",value = T)),
                  div(style="display:inline-block; width: 150px;height: 75px;", checkboxInput(inputId = "pknca_allow.tmax.in.half.life", label = "allow.tmax.in.half.life",value = F)),
                  div(style="display:inline-block; width: 150px;height: 75px;", numericInput(inputId = "pknca_min.hl.points", label = "min.hl.points",value = 3)),
                  div(style="display:inline-block; width: 150px;height: 75px;", numericInput(inputId = "pknca_min.span.ratio", label = "min.span.ratio",value = 2)),
                  div(style="display:inline-block; width: 150px;height: 75px;", numericInput(inputId = "pknca_max.aucinf.pext", label = "max.aucinf.pext",value = 20)),
                  div(style="display:inline-block; width: 150px;height: 75px;", numericInput(inputId = "pknca_min.hl.r.squared", label = "min.hl.r.squared",value = 0.9)),
                  br(""),
                  # br("See https://cran.r-project.org/web/packages/PKNCA/vignettes/Options-for-Controlling-PKNCA.html for an explenation of these options"),s
                  tags$a(href="https://cran.r-project.org/web/packages/PKNCA/vignettes/Options-for-Controlling-PKNCA.html", "See https://cran.r-project.org/web/packages/PKNCA/vignettes/Options-for-Controlling-PKNCA.html for an explenation of these options")

                  ),
              rHandsontableOutput(outputId = "pknca_output"),
               rHandsontableOutput(outputId = "pknca_output2"),
              box(title = "Code",width = 10, collapsible = T, collapsed = T,
                  textAreaInput(inputId = "pknca_code",label = "Code",  width = "100%")
              ),
              shinyjqui::jqui_resizable(plotOutput(outputId = "pknca_plot"))

              ),


    # run manager -------------------------------------------------------------
      # tabItem(tabName = "runmanager",
      #         div(style="display:inline-block; width: 200px;height: 75px;", rHandsontableOutput(outputId  = "runman_table"))
      #
      # ),


      # Model building -------------------------------------------------------------

      tabItem(tabName = "deSolve",


              fluidRow(
                div(style="display:inline-block; width: 200px;height: 75px;", textInput(inputId = "path_models", "File of models", value = "D:/these/Pecc_test")),
                div(style="display:inline-block; width: 400px;height: 75px;", selectInput(inputId = "names_model", "Model", choices = "")),
                div(style="display:inline-block; width: 100px;height: 75px;", actionButton(inputId = "load_model", "Load")),
                div(style="display:inline-block; width: 200px;height: 75px;", textInput(inputId = "name_model", "Name to Save")),
                div(style="display:inline-block; width: 100px;height: 75px;", actionButton(inputId = "save_model", "Save"))
                # div(style="display:inline-block; width: 100px;height: 75px;", checkboxInput(inputId = "helpmodel", "display model helper"))
              ),


              # box(title = "Covariate equations", collapsible = T, collapsed = T, width = 400,
# conditionalPanel('input.helpmodel == true',
             # box(id = "helperModel", title = "helperModel",
              # textInput(inputId = "nameLibOutput", label = "Desired Name", value = "auto",width = "50%"),
              # textInput(inputId = "targetLibOutput", label = "Namelinks", value = "auto",width = "50%"),

              # )),

              # rHandsontableOutput(outputId= "modelLibInput"),
              fluidRow(
                column(6,
                      textAreaInput("mb_model", "Write your model (_plot & _output)", value = "",width = "500", height = "500")
                  )    ,
                column(6,
                       selectInput(multiple = T, inputId = "modelLibInput", "Library-input", choices  = c("a", "b")),

               textAreaInput("mb_model_cov", "Opti. covariate equations", value = "",width = "200", height = "200")
                )

              ),


              # box(title = "Model Librairies", collapsible = T, collapsed = T, width = 400,
              #
              #     fluidRow(
              #
              #     ),
              #
              #     textAreaInput("mb_model_helper", "Proposition", value = "")
              #
              # ),


              fluidRow(
                div(style="display:inline-block; width: 100px;height: 75px;", actionButton("mb_load_model", "Load Model")),
                div(style="display:inline-block; width: 200px;height: 75px;", checkboxInput("mb_load_cov", label = "Use covariate", value = F)),
                div(style="display:inline-block; width: 100px;height: 75px;", actionButton("mb_add_event", "Add event")),
                div(style="display:inline-block; width: 100px;height: 75px;", actionButton("mb_delete_event", "Delete event"))
              ),



              fluidRow(
                div(style="display:inline-block; width: 200px;height: 200px;", rHandsontableOutput("mb_state", width = 200, height = 200)),
                div(style="display:inline-block; width: 200px;height: 200px;", rHandsontableOutput("mb_paramater", width = 200, height = 200)),
                div(style="display:inline-block; width: 200px;height: 200px;",  rHandsontableOutput("mb_event", width = 200, height = 200))
              ),



              fluidRow(
                div(style="display:inline-block; width: 520px;height: 200px;",rHandsontableOutput("mb_matrix", width = 500, height = 200)),
                div(style="display:inline-block; width: 200px;height: 200px;",  rHandsontableOutput("mb_output", width = 200, height = 200))
              ),


              div(style="display:inline-block; width: 150px;height: 75px;", actionButton("update_matrix", label = "update matrix")),
              div(style="display:inline-block; width: 100px;height: 75px;", checkboxInput("matrix_diag", label = "full matrix", value = T)),
              div(style="display:inline-block; width: 200px;height: 75px;", selectInput("matrix_sd_var", label = "coded in", choices = c("sd", "var"), selected = "sd")),
              div(style="display:inline-block; width: 100px;height: 75px;", actionButton("mb_output_add", label = "add output")),
              div(style="display:inline-block; width: 150px;height: 75px;", actionButton("mb_output_remv", label = "remove output")),
              div(style="display:inline-block; width: 200px;height: 75px;",  selectInput("IDImpMod", "Id", choices = "Initial")),
              div(style="display:inline-block; width: 300px;height: 75px;", checkboxInput("useImpData", "use imported data", value = F)),







              # Import model -------------------------------------------------------------

#
#               box(title = "Import model", collapsible = T, collapsed = T, width = 400,
#
#                   fluidRow(
#                     # div(style="display:inline-block; width: 300px;height: 75px;", textInput(inputId = "pathImpMod", "Path to the model", value = "file:///D:/these/Pecc_test/3_Models/1_Models/000_20_04_03/model_base.mlxtran")),
#
#                     # div(style="display:inline-block; width: 100px;height: 75px;",  actionButton("launchImpMod", "Launch")),
#                      # div(style="display:inline-block; width: 100px;height: 75px;",  actionButton("pdfImpData", "Launch pdf")),
#                     # div(style="display:inline-block; width: 100px;height: 75px;",  actionButton("pdfImpData2", "Launch pdf"))
#                     #    actionButton("mb_add_plot", "Add Plot"),
#                     # actionButton("mb_load_plot", "Load Plot"),
#                     # actionButton("mb_delete_plot", "Delete Plot"),
#                     # checkboxInput("mb_matrix_display", label = "Simulations", value = F)
#                   )
#
#               ),



              # plot & simulation -------------------------------------------------------



              box(title = "Plot & Simulations", collapsible = T, collapsed = F, width = 400,
                  fluidRow(
                    # div(style="display:inline-block; width: 300px;height: 75px;", checkboxGroupInput("mb_display", "Plot-to-display", inline = T, choices = c(""))),
                    div(style="display:inline-block; width: 500px;height: 200px;", rHandsontableOutput("mb_display2", width = 300, height = 100)),
                    div(style="display:inline-block; width: 400px;height: 200px;", rHandsontableOutput("mb_plot_stat", width = 400, height = 100))
                    # div(style="display:inline-block; width: 50px;height: 75px;", checkboxInput(inputId = "mb_ylog", label = "y log", value = F)),
                    # div(style="display:inline-block; width: 50px;height: 75px;", checkboxInput(inputId = "mb_wrap", label = "Wrap", value = F))
                    # div(style="display:inline-block; width: 200px;height: 75px;", checkboxInput(inputId = "mb_add_point", label = "Add points explo", value = F))

                  ),
                  fluidRow(
                    div(style="display:inline-block; width: 80px;",   numericInput("mb_time_from", value = 0,label = "From")),
                    div(style="display:inline-block; width: 80px;",  numericInput("mb_time_to", value = 100,label = "To")),
                    div(style="display:inline-block; width: 80px;", numericInput("mb_time_by", value = 1,label = "By")),
                    div(style="display:inline-block; width: 80px;", numericInput(inputId = "mb_nsimul", label = "N simul", value = 0)),
                    div(style="display:inline-block; width: 80px;", numericInput(inputId = "mb_minvaluesimul", label = "min value", value = 0)),
                    div(style="display:inline-block; width: 80px;", numericInput(inputId = "mb_maxvaluesimul", label = "max value", value = NA)),
                    div(style="display:inline-block; width: 80px;",  selectInput("mb_ode_solver", "ODE solver", choices = c("deSolve", "RxODE"), selected = "RxODE", width = "80px")),
                    actionButton("mb_add_plot", "Add Plot"),
                    actionButton("mb_load_plot", "Load Plot"),
                    actionButton("mb_delete_plot", "Delete Plot"),
                    actionButton("mb_estimatePopParam", "Try estimation"),
                    checkboxInput("mb_add_error_plot", label = "Include error", value = F)
                  ),
                  shinyjqui::jqui_resizable(plotOutput("mb_plot", width = "900px", height = "700px")),

                  box(textAreaInput(inputId = "Code_plot_sim", label = "", width = 500,height = 800, resize = "none"),title = "Code", collapsible = T, collapsed = T, width = 400)
              ),


              # optimal design ----------------------------------------------------------


              box(title = "Optimal Design", collapsible = T, collapsed = T, width = 1200,
                  fluidRow(
                    # div(style="display:inline-block; width: 300px;height: 200px;",  textInput("samplingTime", label = "Sampling")),
                    div(style="display:inline-block; width: 1200px;height: 200px;",rHandsontableOutput("OD_sampling", width = 1200, height = 200)),
                    div(style="display:inline-block; width: 150px;height: 200px;",  actionButton("OptimDesign_add", "Add line")),
                    div(style="display:inline-block; width: 150px;height: 200px;",  actionButton("OptimDesign_delete", "Delete line")),
                    div(style="display:inline-block; width: 150px;height: 200px;",  actionButton("OptimDesign", "Eval design"))
                  ),

                  plotOutput("OptimPlot", width = "400px", height = "300px"),
                  textAreaInput(inputId = "Result_Optimal_Design", label = "", width = 500,height = 300, resize = "none"),
                  box(textAreaInput(inputId = "Code_Optimal_Design", label = "", width = 500,height = 300, resize = "none"),title = "Code", collapsible = T, collapsed = T, width = 400)

              ),



# Scheme model ------------------------------------------------------------


box(title = "Scheme of the model", collapsible = T, collapsed = T, width = 1200,
    fluidRow(
      # div(style="display:inline-block; width: 300px;height: 200px;",  textInput("samplingTime", label = "Sampling")),
      # div(style="display:inline-block; width: 1200px;height: 200px;",rHandsontableOutput("OD_sampling", width = 1200, height = 200)),
      # div(style="display:inline-block; width: 150px;height: 200px;",  actionButton("OptimDesign_add", "Add line")),
      div(style="display:inline-block; width: 150px;height: 200px;",  numericInput(inputId = "scheme_size", label = "Size", value = 0.9, min = 0, max = 1, step = 0.1)),
      div(style="display:inline-block; width: 150px;height: 200px;",  actionButton("scheme_launch", "Go"))
    ),

    shinyjqui::jqui_resizable(plotOutput("scheme_plot", width = "1000px", height = "800px"))
    # plotOutput("OptimPlot", width = "400px", height = "300px"),
    # textAreaInput(inputId = "Result_Optimal_Design", label = "", width = 500,height = 300, resize = "none"),
    # box(textAreaInput(inputId = "Code_Optimal_Design", label = "", width = 500,height = 300, resize = "none"),title = "Code", collapsible = T, collapsed = T, width = 400)

),



# nlmixr ------------------------------------------------------------------

box(title = "nlmixr", collapsible = T, collapsed = T, width = 400,

    div(style="display:inline-block; width: 50px;height: 75px;", actionButton("nlmixr_translate", "Generate code")),
    textAreaInput("nlmixr_result",label = ""),
    # div(style="display:inline-block; width: 400px;height: 75px;", textInput("nlmixr_path", label = "Path", value = "D:/Peccary/Exemple_demo/saemix_test")),
    div(style="display:inline-block; width: 100px;height: 75px;", actionButton("nlmixr_go", "Estimate"))


),



              # monolix launcher --------------------------------------------------------


              box(title = "Monolix", collapsible = T, collapsed = T, width = 400,

                  div(style="display:inline-block; width: 200px;height: 75px;", actionButton("ode_modelsave_monolix", "Generate Mlxtran")),
                  div(style="display:inline-block; width: 200px;height: 75px;", textInput("ode_projectpath_monolix", label = "Monolix project path", value = "")),
                  div(style="display:inline-block; width: 200px;height: 75px;", actionButton("launch_monolix", label = "Launch Monolix")),
                  div(style="display:inline-block; width: 400px;height: 75px;", textInput("monolix_filter", label = "Additional Filter", value = "")),
                  div(style="display:inline-block; width: 400px;height: 75px;", textInput("monolix_description", label = "Additional Filter", value = "")),

                  tabBox(
                    title = "Monolix launcher",
                    # The id lets us use input$tabset1 on the server to find the current tab
                    id = "tabset1", height = "750px",
                    tabPanel("Model file",
                             actionButton("ode_trigger_monolix", "Launch ode translation"),
                             textAreaInput("ode_output", "Model", value = "", width = "400", height = "500"  ) #  ,  div(style="display:inline-block; width: 1000px;height: 700px;",
                    ),
                    tabPanel("NMLXTRAN",
                             actionButton(inputId = "nmlxtran_launch", label = "launch"),
                             textAreaInput("nmlxtran_output", "Model", value = "", width = "400", height = "500"  ))
                  )
              ),


              # nonmem code -------------------------------------------------------------
             box(title = "NONMEM", collapsible = T, collapsed = T, width = 400,
                  # div(style="display:inline-block; width: 250px;height: 75px;", selectInput("ode_input", "Exporte software",choices = c("NONMEM", "Monolix"), selected = "NONMEM")),
                  div(style="display:inline-block; width: 150px;height: 75px;", selectInput("ode_baselines2", "Baseline parameters",choices = "", selected = "", multiple = T)),
                  div(style="display:inline-block; width: 50px;height: 75px;", checkboxInput(inputId = "ode_mu", label = "mu-ref", value = T)),
                  div(style="display:inline-block; width: 50px;height: 75px;", checkboxInput(inputId = "ode_blq", label = "BLQ", value = F)),
                  div(style="display:inline-block; width: 50px;height: 75px;", actionButton("ode_trigger_nonmem", "Launch ode translation")),

                  # tabBox(
                  # title = "Nonmem launcher",
                  # The id lets us use input$tabset1 on the server to find the current tab
                  # id = "tabset1", height = "750px",
                  # tabPanel("Nonmem", "First tab content",
                  textAreaInput("ode_output_nonmem", "Model", value = "", width = "400", height = "700"  ) #  ,  div(style="display:inline-block; width: 1000px;height: 700px;",
                  # ),
                  # tabPanel("CTRLFILE", "Tab content 2")
                  # )
              ),

      box(title = "ADAPT", collapsible = T, collapsed = T, width = 400,

          div(style="display:inline-block; width: 50px;height: 75px;", actionButton("ode_trigger_adapt", "Launch ADAPT translation")),

       textAreaInput("ode_output_adapt", "Model", value = "", width = "400", height = "700"  ) #  ,  div(style="display:inline-block; width: 1000px;height: 700px;",
     )

              # box(title = "saemix", collapsible = T, collapsed = T, width = 400,
              #
              #     div(style="display:inline-block; width: 400px;height: 75px;", textInput("saemix_path", label = "Path", value = "D:/Peccary/Exemple_demo/saemix_test")),
              #     div(style="display:inline-block; width: 50px;height: 75px;", actionButton("saemix_go", "Go")),
              #     div(style="display:inline-block; width: 400px;height: 400px;", textAreaInput("nlmixr_result",label = ""))
              #
              # )

              # conditionalPanel(condition = 'input.model_sections == "Monolix_Launcher"',
              #
              #                  shinyTree("tree", stripes = TRUE, multiple = TRUE, animation = TRUE, contextmenu = TRUE)
              # )

      ),
      # folder ------------------------------------------------------------------

      tabItem(tabName =  "folder",
              textOutput(outputId = "namecurrendfolder"),
              # conditionalPanel(
              # condition = 'input.path2 != ""',
              # tableOutput("table"),
              # ),
              div(style = 'overflow-x: scroll', tableOutput('table')),
              # conditionalPanel(
              # condition = 'input.path2 != ""',
              tableOutput("runscompar")
              # )

      ),


      # prediction --------------------------------------------------------------

      tabItem(tabName = "prediction",



              checkboxInput(
                inputId = 'interactivePreds', label = 'Interactive figure', value = FALSE
              ),

              conditionalPanel(
                condition = 'input.interactivePreds == true',
                plotlyOutput('PredIndiInteractive',width = "1000px", height = "800px" )
              ),

              conditionalPanel(
                condition = 'input.interactivePreds == false',

                conditionalPanel(
                  condition = 'input.path2 != ""',
                  shinyjqui::jqui_resizable( plotOutput('PredIndi', width = "1000px", height = "800px"))
                )  ),

              conditionalPanel(
                condition = 'input.path2 != ""',
                fluidRow(

                  div(style="display:inline-block; width: 100px;height: 75px;",   selectInput(inputId = "whichFunctionPred",label = "Function",  choices = "Default", selected = "Default")),
                  div(style="display:inline-block; width: 100px;height: 75px;", textInput("numberPred2", "Runs" )),
                  # div(style="display:inline-block; width: 100px;height: 75px;", checkboxInput(inputId = "singmult", label = "S/M", value = TRUE)),
                  div(style="display:inline-block; width: 100px;height: 75px;",  numericInput(inputId = "nIndPred2", label = "n",value = 20, min = 1)),
                  div(style="display:inline-block; width: 100px;height: 75px;", checkboxInput(inputId = "logPred2", label = "ylog", value = TRUE)),
                  div(style="display:inline-block; width: 100px;height: 75px;", checkboxInput(inputId = "logxPred2", label = "xlog", value = FALSE)),
                  div(style="display:inline-block; width: 100px;height: 75px;", checkboxInput(inputId = "PredPred2", label = "Pred", value = TRUE)),
                  div(style="display:inline-block; width: 150px;height: 75px;", checkboxInput(inputId = "freeSalePred2", label = "Free Scales", value = TRUE)),
                  div(style="display:inline-block; width: 300px;height: 75px;", textInput(inputId = "filterID2", label = "Filter")),
                  div(style="display:inline-block; width: 300px;height: 75px;", actionButton(inputId = "updatePred", label = "Update PLot")),
                  div(style="display:inline-block; width: 100px;height: 75px;",  numericInput(inputId = "lowerlimitPred", label = "low limit",value = NA)),
                  div(style="display:inline-block; width: 100px;height: 75px;",  numericInput(inputId = "upperlimitPred", label = "high limit",value = NA)),
                  div(style="display:inline-block; width: 100px;height: 75px;", checkboxInput(inputId = "extrVal", label = "Extr. Value", value = F)),
                  div(style="display:inline-block; width: 100px;height: 75px;", checkboxInput(inputId = "stratTypePred", label = "strat", value = F)),
                  div(style="display:inline-block; width: 100px;height: 75px;", textInput(inputId = "xlabPred", label = "xlab", value = "")),
                  div(style="display:inline-block; width: 100px;height: 75px;", textInput(inputId = "ylabPred", label = "ylab", value = ""))
                  # div(style="display:inline-block; width: 150px;height: 75px;", selectInput(inputId = "colourPred2", label = "COV", choices = "None"))
                )
              ),

              conditionalPanel(
                condition = 'input.path2 != ""',
                fluidRow(
                  div(style="display:inline-block; width: 150px;height: 75px;",  actionButton("savePRED", "Save Plot")),
                  div(style="display:inline-block; width: 800px;height: 75px;", textInput("commPRED", "Commentary (new line with three spaces)"  ))
                )
              ),



              conditionalPanel(
                condition = 'input.path2 != ""',
                tableOutput("covPRED")
              ),

              conditionalPanel(
                condition = 'input.path2 != ""',
                tableOutput("table2")
              )

      ),


      # gof ---------------------------------------------------------------------


      tabItem( tabName = 'GOF',
               radioButtons(inputId = 'radioGOF', label = "", choices = c("Single Static", "Single Dynamic", "Multiple Static"), inline =  T),
               # fluidRow(
               #   div(style="display:inline-block; width: 100px;height: 75px;", checkboxInput(inputId = 'interactive', label = 'Interactive figure', value = FALSE )),
               #   div(style="display:inline-block; width: 100px;height: 75px;", checkboxInput(inputId = 'multiGOFcheck', label = '', value = FALSE ))
               #        ),
               #


               conditionalPanel(
                 condition = 'input.radioGOF == "Single Dynamic"',
                 plotlyOutput('GOFinteractivePlot',width = "1000px", height = "700px" )
               ),

               conditionalPanel(
                 condition = 'input.radioGOF == "Single Static"',

                 conditionalPanel(
                   condition = 'input.path2 != ""',

                   shinyjqui::jqui_resizable( plotOutput('GOFstaticPlot', width = "1000px", height = "700px"))
                 )),


               conditionalPanel(
                 condition = 'input.radioGOF == "Multiple Static"',
                 shinyjqui::jqui_resizable(plotOutput('multiGOFplot', width = "1000px", height = "700px"))
               ),


               fluidRow(
                 div(style="display:inline-block; width: 100px;height: 75px;", textInput("nGOF", "Runs" )),
                 div(style="display:inline-block; width: 150px;height: 75px;", selectInput(inputId = "covPrimGOF", label = "Prim.Cov", choices = "")),
                 div(style="display:inline-block; width: 150px;height: 75px;", checkboxInput(inputId = "colcov", label = "Col Cov", value = FALSE)),
                 div(style="display:inline-block; width: 300px;height: 75px;", textInput(inputId = "filterGOF", label = "Filter")),
                 div(style="display:inline-block; width: 150px;height: 75px;", checkboxInput(inputId = "remBLQGOF", label = "Remove BLQ", value = TRUE)),
                 div(style="display:inline-block; width: 300px;height: 75px;", actionButton(inputId = "updateGOF", label = "Update")),
                 div(style="display:inline-block; width: 150px;height: 75px;", textInput(inputId = "SelectGOF", label = "SelectGOF")),
                 div(style="display:inline-block; width: 150px;height: 75px;", checkboxInput(inputId = "gofxlog", label = "xlog", value = TRUE)),
                 div(style="display:inline-block; width: 150px;height: 75px;", checkboxInput(inputId = "gofylog", label = "ylog", value = TRUE)),
                 div(style="display:inline-block; width: 150px;height: 75px;", selectInput(inputId = "gofmethods", label = "method smooth",choices = c("auto", "none", "lm", "glm", "gam", "loess"), selected = "auto")),
                 div(style="display:inline-block; width: 150px;height: 75px;", textInput(inputId = "goftitle", label = "Title"))
               ),

               fluidRow(
                 div(style="display:inline-block; width: 150px;height: 75px;",  actionButton("saveGOF", "Save plot")),
                 div(style="display:inline-block; width: 800px;height: 75px;", textInput("commGOF", "Commentary (new line with three spaces)"  ))
               ),

               conditionalPanel(
                 condition = 'input.path2 != ""',
                 tableOutput("covGOF")
               ),

               div(style="display:inline-block; width: 150px;height: 75px;", textInput(inputId = "SelectmultiGOF", label = "Filter table below")),
               radioButtons(inputId = "ArrangeGOF", label = "", choices = c( "name", "run", "COV"), inline =  T),

               conditionalPanel(
                 condition = 'input.path2 != ""',
                 tableOutput("multiGOF")
               )

      ),


      # covariate ---------------------------------------------------------------


      tabItem( tabName = "covariate",


               conditionalPanel(
                 condition = 'input.path2 != ""',
                 shinyjqui::jqui_resizable( plotOutput("multiBox", width = "1000px", height = "600px"))
               ),
               fluidRow(
                 div(style="display:inline-block; width: 100px;height: 75px;", textInput("nBoxPlots", "Runs" )),
                 div(style="display:inline-block; width: 150px;height: 75px;", selectInput(inputId = "covPrimBP2", label = "Prim.Cov", choices = "None")),
                 div(style="display:inline-block; width: 150px;height: 75px;", checkboxInput(inputId = "pointBP2", label = "Points", value = FALSE)),
                 div(style="display:inline-block; width: 150px;height: 75px;", selectInput(inputId = "boxplottest", label = "Pvalue test",choices = c("t.test", "wilcox.test",  "anova", "kruskal.test" ), selected = "anova"))
                 # div(style="display:inline-block; width: 150px;height: 75px;", checkboxInput(inputId = "dissociateBP2", label = "Dissociated", value = FALSE))
               ),

               fluidRow(
                 div(style="display:inline-block; width: 150px;height: 75px;",  actionButton("saveBP", "Save plot")),
                 div(style="display:inline-block; width: 800px;height: 75px;", textInput("commBP", "Commentary (new line with three spaces)"  ))
               ),

               conditionalPanel(
                 condition = 'input.path2 != ""',
                 tableOutput("covBP"))
               ,

               conditionalPanel(
                 condition = 'input.path2 != ""',
                 tableOutput("table4"))
      ),


      # matrix ------------------------------------------------------------------



      tabItem( tabName = "matrix",
               radioButtons(inputId = 'radioMatrix', label = "", choices = c("Single", "Multiple Run"), inline =  T),

               conditionalPanel(
                 condition = 'input.radioMatrix == "Multiple Run"',
                 shinyjqui::jqui_resizable(plotOutput("matrixMultiPlot", width = "1000px", height = "600px"))
               ),

               conditionalPanel(
                 condition = 'input.radioMatrix == "Single"',

                 conditionalPanel(
                   condition = 'input.path2 != ""',
                   shinyjqui::jqui_resizable(plotOutput("matrix", width = "1000px", height = "600px"))
                   )
               ),



               fluidRow(
                 div(style="display:inline-block; width: 100px;height: 75px;", textInput("nmatrix", "Runs" )),
                 div(style="display:inline-block; width: 100px;height: 75px;", textInput("matrixfilter", "Filter" )),
                 div(style="display:inline-block; width: 150px;height: 75px;",    radioButtons(inputId = "ArrangeMatrix", label = "", choices = c( "name", "run"), inline =  T))
                 # div(style="display:inline-block; width: 150px;height: 75px;", checkboxInput(inputId = "pointBP2", label = "Points", value = FALSE))
                 # div(style="display:inline-block; width: 150px;height: 75px;", checkboxInput(inputId = "dissociateBP2", label = "Dissociated", value = FALSE))
               ),

               fluidRow(
                 div(style="display:inline-block; width: 150px;height: 75px;",  actionButton("saveMATRIX", "Save plot")),
                 div(style="display:inline-block; width: 800px;height: 75px;", textInput("commMATRIX", "Commentary (new line with three spaces)"  ))
               ),


               # radioButtons(inputId = "ArrangeMatrix", label = "", choices = c("run", "COV", "name"), inline =  T),
               conditionalPanel(
                 condition = 'input.path2 != ""',
                 tableOutput("multiMatrix"))

               # tableOutput("table4")
      ),


      #   ############################## ill #######################

      tabItem( tabName = "iLL",

               conditionalPanel(
                 condition = 'input.path2 != ""',
                 shinyjqui::jqui_resizable(plotOutput("iLL", width = "1000px", height = "600px"))
               ),

               fluidRow(
                 div(style="display:inline-block; width: 100px;height: 75px;", textInput("niLL", "Runs" )),
                 div(style="display:inline-block; width: 150px;height: 75px;", selectInput(inputId = "covPrimiLL", label = "Prim.Cov", choices = "None")),
                 # div(style="display:inline-block; width: 150px;height: 75px;", selectInput(inputId = "covPrimBP2", label = "Prim.Cov", choices = "None")),
                 div(style="display:inline-block; width: 150px;height: 75px;", checkboxInput(inputId = "HistoiLL", label = "Histogram", value = FALSE))
                 # div(style="display:inline-block; width: 150px;height: 75px;", checkboxInput(inputId = "dissociateBP2", label = "Dissociated", value = FALSE))
               ),


               fluidRow(
                 div(style="display:inline-block; width: 150px;height: 75px;",  actionButton("saveILL", "Save plot")),
                 div(style="display:inline-block; width: 800px;height: 75px;", textInput("commILL", "Commentary (new line with three spaces)"  ))
               )

               # tableOutput("table4")
      ),


      # Manual plpot ------------------------------------------------------------

      tabItem( tabName = "ManualFunc",

               fluidRow(
                 # div(style="display:inline-block; width: 150px;height: 75px;",  actionButton("saveSENS", "Save plot")),
                 div(style="display:inline-block; width: 800px;height: 75px;", textInput("fileManualPLot",label = "file",value =  "D:/these/Pecc_test/3_Models/1_Models/pecc_func.R"  )),
                 div(style="display:inline-block; width: 800px;height: 75px;", selectInput("selectManualFunction", label = "Function", choices = "wait")),
                 rHandsontableOutput("argManualFunction"),
                 div(style="display:inline-block; width: 800px;height: 75px;", actionButton("ManualPlotGo","Go !"))
               ),
               shinyjqui::jqui_resizable( plotOutput("ManualPlotPlot", width = "900px", height = "900px"))
      ),

      #   ############################## sensititivity #######################

      tabItem( tabName = "sensitivity",


               conditionalPanel(
                 condition = 'input.path2 != ""',
                 tableOutput("table5")),
               fluidRow(column(4,  wellPanel(selectInput("parSens", "Parameter", choices = "")))),
               fluidRow(
                 div(style="display:inline-block; width: 150px;height: 75px;",  actionButton("saveSENS", "Save plot")),
                 div(style="display:inline-block; width: 800px;height: 75px;", textInput("commSENS", "Commentary (new line with three spaces)"  ))
               ),
               conditionalPanel(
                 condition = 'input.path2 != ""',
                 plotOutput("sensitivity", width = "1500px", height = "900px"))
      ),



      # saved plot --------------------------------------------------------------


      tabItem(tabName = "rapports",

              tableOutput("savedPlotsTable"),
              fluidRow(
                div(style="display:inline-block; width: 150px;height: 75px;",  actionButton("savedUpdate", "Update saved list")),
                div(style="display:inline-block; width: 100px;height: 75px;",  numericInput(inputId = "saved_n_to_display", label = "ID to display", value = 0)),
                actionButton("pdfplots", "Export Saved Plots")
              ),
              plotOutput("saved_displayed", width = "1000px", height = "600px")

      ),


      # VPC perso---------------------------------------------------------------------

      tabItem(tabName = "simulation",

              fluidRow(
                div(style="display:inline-block; width: 300px;height: 75px;", textInput("pathVPC", "Path of a dataset", value = "")),

                div(style="display:inline-block; width: 50px;height: 75px;", selectInput("sepVPC", "Sep", choices = c("", ";", ".", ","), selected = "")),
                div(style="display:inline-block; width: 50px;height: 75px;", selectInput("decVPC", "Dec", choices = c(".", ","), selected = ".")),
                div(style="display:inline-block; width: 50px;height: 75px;", textInput("nastringVPC", "NA", value = "")),
                div(style="display:inline-block; width: 50px;height: 75px;", checkboxInput(inputId = "headerVPC", label = "header", value = F))

              ),

              fluidRow(

                div(style="display:inline-block; width: 300px;height: 75px;", textInput("nameVPC", "Names of column", value = "")),
                div(style="display:inline-block; width: 50px;height: 75px;", actionButton(inputId = "loadVPC", label = "load file"))

              ),


              conditionalPanel(
                condition = 'input.radioVPC == "Visual Predicted Check"',
                plotOutput("VPCPlot", width = "800px", height = "700px")),

              conditionalPanel(
                condition = 'input.radioVPC == "Prediction Distributions"',
                plotOutput("PDPlot", width = "800px", height = "700px")),


              div(style="display:inline-block; width: 500px;height: 75px;",  radioButtons(inline = T , inputId = 'radioVPC', label = "", choices = c("Visual Predicted Check", "Prediction Distributions"))),


              fluidRow(

                div(style="display:inline-block; width: 300px;height: 75px;", textInput("filterrVPC", "Filter", value = "")),
                div(style="display:inline-block; width: 150px;height: 75px;", selectInput(inputId = "VPCX", label = "X", choices = "")),
                div(style="display:inline-block; width: 150px;height: 75px;", selectInput(inputId = "VPCY", label = "Y", choices = "")),
                div(style="display:inline-block; width: 150px;height: 75px;", selectInput(inputId = "VPCsim", label = "sim", choices = "")),
                div(style="display:inline-block; width: 150px;height: 75px;", selectInput(inputId = "VPCobs", label = "obs", choices = "")),
                div(style="display:inline-block; width: 150px;height: 75px;", selectInput(inputId = "VPCtype", label = "type", choices = c("Conf Int VPC" = "CI", "scatter VPC" = "scatter"))),
                div(style="display:inline-block; width: 150px;height: 75px;", textInput("VPCquantile", "Quantiles", value = "5 50 95")),
                div(style="display:inline-block; width: 150px;height: 75px;", numericInput(inputId = "VPCCIpct", label = "Confidence Intervale", value = 5, min = 0, max = 49, step = 1)),
                # div(style="display:inline-block; width: 150px;height: 75px;", numericInput(inputId = "VPCLine", label = "Line-Alpha", value = 1, min = 0, max = 1, step = 0.1)),
                div(style="display:inline-block; width: 150px;height: 75px;", selectInput(inputId = "VPCCOV", label = "Covariate", choices = "", multiple = T)),
                # div(style="display:inline-block; width: 100px;height: 75px;", checkboxInput(inputId = "VPCXlog", label = "Xlog", value = FALSE)),
                div(style="display:inline-block; width: 100px;height: 75px;", checkboxInput(inputId = "VPCYlog", label = "Ylog", value = TRUE)),
                # div(style="display:inline-block; width: 100px;height: 75px;", checkboxInput(inputId = "VPCMedian", label = "Median", value = FALSE)),
                # div(style="display:inline-block; width: 100px;height: 75px;", checkboxInput(inputId = "VPCNA", label = "Remove NA", value = FALSE)),
                div(style="display:inline-block; width: 150px;height: 75px;", numericInput(inputId = "VPCLOQ", label = "LOQ obs", value = 0, step = 1)),
                div(style="display:inline-block; width: 150px;height: 75px;", numericInput(inputId = "VPCYMIN", label = "ymin", value = 0, step = 1)),
                div(style="display:inline-block; width: 150px;height: 75px;", textInput("VPCxlabel", "xlabel", value = "Time")),
                div(style="display:inline-block; width: 150px;height: 75px;", textInput("VPCylabel", "ylabel", value = "Concentration")),
                div(style="display:inline-block; width: 150px;height: 75px;", numericInput(inputId = "VPCsizeText", label = "Text size", value = 20, step = 1)),
                div(style="display:inline-block; width: 50px;height: 75px;", actionButton(inputId = "launchVPC", label = "create VPC"))


              )


      ),


# forest ------------------------------------------------------------------


tabItem(tabName = "forestPlot",

        fluidRow(
          div(style="display:inline-block; width: 100px;height: 75px;", textInput("nForest", "Runs" )),
          div(style="display:inline-block; width: 100px;height: 75px;", actionButton("mb_load_forest_run", "Prefill w run")),
          div(style="display:inline-block; width: 100px;height: 75px;", actionButton("mb_add_forest", "Add line")),
          div(style="display:inline-block; width: 100px;height: 75px;", actionButton("mb_delete_forestt", "Delete line")),
          div(style="display:inline-block; width: 150px;height: 75px;", actionButton(inputId = "forest_go", label = "Go"))

             ),
        rHandsontableOutput("table_forest"),
        shinyjqui::jqui_resizable(plotOutput("forestPlot", width = "1000px", height = "700px")),

        box(width = 1000,
            textAreaInput(inputId = "codeforestpec", label = "Short Peccary Code ", height = 100,width = 1000),
            textAreaInput(inputId = "codeforest", label = "Peccary Independant Code", height = 300,width = 1000),
            title = "R code", collapsible = T, collapsed = T)
),



      # VPC package---------------------------------------------------------------------
      tabItem(tabName = "vpc_pckg",


              plotOutput("vpc_pckg_plot", width = "700px", height = "700px"),

              box(collapsible = T, collapsed = F,width = 12,
                  div(style="display:inline-block; width: 400px;height: 75px;", textInput(inputId = "vpc_pckg_simdata", label = "Path sim")),
                  div(style="display:inline-block; width: 100px;height: 75px;", selectInput(inputId = "vpc_pckg_simdata_ID", label = "ID", choices = c(""), selected = "")),
                  div(style="display:inline-block; width: 100px;height: 75px;", selectInput(inputId = "vpc_pckg_simdata_REP", label = "REP", choices = c(""), selected = "")),
                  div(style="display:inline-block; width: 100px;height: 75px;", selectInput(inputId = "vpc_pckg_simdata_TIME", label = "TIME", choices = c(""), selected = "")),
                  div(style="display:inline-block; width: 100px;height: 75px;", selectInput(inputId = "vpc_pckg_simdata_PRED", label = "PRED", choices = c(""), selected = "")),
                  div(style="display:inline-block; width: 400px;height: 75px;", textInput(inputId = "vpc_pckg_obsdata", label = "Path obs")),
                  div(style="display:inline-block; width: 100px;height: 75px;", selectInput(inputId = "vpc_pckg_obsdata_ID", label = "ID", choices = c(""), selected = "")),
                  div(style="display:inline-block; width: 100px;height: 75px;", selectInput(inputId = "vpc_pckg_obsdata_TIME", label = "TIME", choices = c(""), selected = "")),
                  div(style="display:inline-block; width: 100px;height: 75px;", selectInput(inputId = "vpc_pckg_obsdata_DV", label = "DV", choices = c(""), selected = "")),
                  div(style="display:inline-block; width: 100px;height: 75px;", selectInput(inputId = "vpc_pckg_obsdata_MDV", label = "MDV", choices = c(""), selected = ""))
              ),

              div(style="display:inline-block; width: 100px;height: 75px;", actionButton(inputId = "vpc_pckg_go", label = "create VPC")),
              div(style="display:inline-block; width: 100px;height: 75px;", selectInput(inputId = "vpc_pckg_bins", label = "Bins", choices = c("jenks", "density", "time", "data", "none", "pretty"), selected = "jenks")),
              div(style="display:inline-block; width: 100px;height: 75px;", numericInput(inputId = "vpc_pckg_n_bins", label = "n_bins", value = 0)),
              div(style="display:inline-block; width: 100px;height: 75px;", selectInput(inputId = "vpc_pckg_bin_mid", label = "bin_mid", choices = c("mean", "middle"), selected = "mean")),
              div(style="display:inline-block; width: 100px;height: 75px;", selectInput(inputId = "vpc_pckg_software", label = "software", choices = c("auto", "nonmem", "phoenix"), selected = "auto")),
              div(style="display:inline-block; width: 400px;height: 75px;", selectInput(inputId = "vpc_pckg_show", label = "show", choices = c("obs_dv", "obs_ci", "pi", "pi_as_area","pi_ci", "obs_median","sim_median","sim_median_ci" ), selected =  c("obs_dv", "obs_ci", "pi_ci", "obs_median","sim_median_ci"), multiple = T)),
              div(style="display:inline-block; width: 100px;height: 75px;", checkboxInput(inputId = "vpc_pckg_pred_corr", label = "pred_corr", value = F)),
              div(style="display:inline-block; width: 150px;height: 75px;", numericInput(inputId = "vpc_pckg_pred_corr_lower_bnd", label = "pred_corr_lower_bnd",value = 0)),
              div(style="display:inline-block; width: 100px;height: 75px;", numericInput(inputId = "vpc_pckg_pi_low", label = "pi_low",value = 0.05)),
              div(style="display:inline-block; width: 100px;height: 75px;", numericInput(inputId = "vpc_pckg_pi_high", label = "pi_high",value = 0.95)),
              div(style="display:inline-block; width: 100px;height: 75px;", numericInput(inputId = "vpc_pckg_ci_low", label = "ci_low",value = 0.05)),
              div(style="display:inline-block; width: 100px;height: 75px;", numericInput(inputId = "vpc_pckg_ci_high", label = "ci_high",value = 0.95)),
              div(style="display:inline-block; width: 100px;height: 75px;", numericInput(inputId = "vpc_pckg_uloq", label = "uloq",value = NA)),
              div(style="display:inline-block; width: 100px;height: 75px;", numericInput(inputId = "vpc_pckg_lloq", label = "lloq",value = NA)),
              div(style="display:inline-block; width: 100px;height: 75px;", checkboxInput(inputId = "vpc_pckg_log_y", label = "log_y", value = F)),
              div(style="display:inline-block; width: 100px;height: 75px;", numericInput(inputId = "vpc_pckg_log_y_min", label = "log_y_min",value = 0.001)),
              div(style="display:inline-block; width: 100px;height: 75px;", textInput(inputId = "vpc_pckg_xlab", label = "xlab")),
              div(style="display:inline-block; width: 100px;height: 75px;", textInput(inputId = "vpc_pckg_ylab", label = "ylab")),
              div(style="display:inline-block; width: 100px;height: 75px;", textInput(inputId = "vpc_pckg_title", label = "title")),
              div(style="display:inline-block; width: 100px;height: 75px;", checkboxInput(inputId = "vpc_pckg_smooth", label = "smooth", value = T)),
              div(style="display:inline-block; width: 100px;height: 75px;", selectInput(inputId = "vpc_pckg_facet", label = "facet", choices = c("wrap", "columns", "rows"), selected = "wrap")),

              textAreaInput(inputId = "vpc_pckg_code", label = "Code")
      ),


# Report ------------------------------------------------------------------


tabItem(tabName = "Report",

        div(style="display:inline-block; width: 300px;height: 75px;",  selectInput(inputId = "reportVersion", label = "Report Version", choices = c(""), selected = "")),
        div(style="display:inline-block; width: 75px;height: 75px;",    actionButton(inputId = "reportLoad", label = "Load")),
        div(style="display:inline-block; width: 300px;height: 75px;",    textInput(inputId = "reportnewVersion", label = "Name")),
        div(style="display:inline-block; width: 75px;height: 75px;",    actionButton(inputId = "reportSave", label = "Save")),

        box(title = "Prototype", collapsible = T, collapsed = T,width = 12,
        div(style="display:inline-block; width: 400px;height: 75px;",  textInput(inputId = "reportTemplatePath", label = "Prototype File", value = "D:/Peccary/inst/Report/templateReport.txt")),
        div(style="display:inline-block; width: 200px;height: 75px;",    actionButton(inputId = "reportTemplateLaunch", label = "Load Template"))
        ),

        box(title = "Free introduction", collapsible = T, collapsed = T,width = 12,
            textAreaInput(inputId = "Report_introduction", label = "Introduction")
          ),

        box(title = "Project Presentation / Bibliographie", collapsible = T, collapsed = T,width = 12,
            # div(style="display:inline-block; width: 600px;height: 100px;", selectInput(inputId = "reportDatasetInfo", label = "Info on", choices = c(""), selected = "", multiple = T)),
            textAreaInput(inputId = "Report_biblio", label = "Project Presentation")
        ),
        box(title = "Dataset information", collapsible = T, collapsed = T,width = 12,
            div(style="display:inline-block; width: 600px;height: 100px;", selectInput(inputId = "reportDatasetInfo", label = "Info on", choices = c(""), selected = "", multiple = T)),
            textAreaInput(inputId = "Report_dataset", label = "Dataset information")
        ),

        box(title = "Pre-modeling", collapsible = T, collapsed = T,width = 12,
            div(style="display:inline-block; width: 600px;height: 100px;", selectInput(inputId = "reportDataExploSelect", label = "DataExplo", choices = c(""), selected = "", multiple = T)),
            div(style="display:inline-block; width: 600px;height: 100px;", selectInput(inputId = "reportPlotExploSelect", label = "PlotExplo", choices = c(""), selected = "", multiple = T)),
            textAreaInput(inputId = "Report_premodeling", label = "Output")
        ),

        box(title = "Modeling", collapsible = T, collapsed = T,width = 12,
            div(style="display:inline-block; width: 400px;height: 100px;", selectInput(inputId = "reportModelEq", label = "model", choices = c(""), selected = "", multiple = T)),
            textAreaInput(inputId = "Report_modeling", label = "Output")
        ),

        box(title = "Final run", collapsible = T, collapsed = T,width = 12,
            div(style="display:inline-block; width: 600px;height: 100px;", textInput(inputId = "reportRunfinalpat", label = "Run final", value = "")),
            textAreaInput(inputId = "Report_run", label = "Output")
        ),

        box(title = "Simulation", collapsible = T, collapsed = T,width = 12,
            div(style="display:inline-block; width: 400px;height: 100px;", selectInput(inputId = "reportModelSimul", label = "model", choices = c(""), selected = "", multiple = T)),
            textAreaInput(inputId = "Report_simulation", label = "Output")
        ),

        box(title = "Export report", background="blue", collapsible = T, collapsed = T,width = 12,

            div(style="display:inline-block; width: 300px;height: 100px;", textInput(inputId = "exportReportPath", label = "Output path", value = "D:/Peccary/Exemple_demo/report/demo")),
            div(style="display:inline-block; width: 75px;height: 100px;", selectInput(inputId = "exportReportFormat", label = "Format", choices = c("word", "html", "pdf"), selected = "word", multiple = F)),
            div(style="display:inline-block; width: 200px;height: 75px;",    actionButton(inputId = "exportReportGo", label = "Load Template")),

            box(title = "Options", collapsible = T, background="blue", collapsed = T,
                div(style="display:inline-block; width: 150px;height: 75px;",    textInput(inputId = "exportReportTitle", label = "Title", value = "UCART19")),
                div(style="display:inline-block; width: 150px;height: 75px;",    textInput(inputId = "exportReportAuthor", label = "Author", value = "Thibaud Derippe")),
            div(style="display:inline-block; width: 150px;height: 75px;",    textInput(inputId = "exportReportBibPath", label = "Bib path (optional)", value = "D:/Peccary/Exemple_demo/report/These_Peccary.bib")),
            div(style="display:inline-block; width: 150px;height: 75px;",    textInput(inputId = "exportReportCSLPath", label = "CSL path (optional)", value = "D:/Peccary/Exemple_demo/report/elsevier-vancouver.csl"))
            )
        )


),
# Authors ------------------------------------------------------------------


tabItem(tabName = "Authors",

        h2("Authors", align = "center"),
        br("Peccary aims to be a collaborative project where anyone can add or modify functionnalities."),
        h3("List of all Peccary contributors (alphabetical order)"),
        br(""),
        br(HTML("<b><u>Aziz Ouerdani</b></u>: improved 3SetReplace_NONMEM_Run.R file")),
        br(HTML("<b><u>Blaise Pasquiers</b></u>: provide lots of feedback and improvement thoughts")),
        br(HTML("<b><u>Christelle Rodrigues</b></u>: wrote indiv_plot_christelle.R (additional function)")),
        br(HTML("<b><u>Thibaud Derippe</b></u>: designed and wrote the first Peccary version (more than 50 files, including the Shiny App)")),
        br(""),
        br("")
   )


), map(unique(gsub("(_serv)|(_ui)|(\\.R)", "", list.files("D:/Peccary/inst/Module"))), function(x){

        eval(parse_expr(paste(readLines(file.path("D:/Peccary/inst/Module", paste0(x,"_ui.R"))), collapse = "")))
        #
        # print(class(eval(parse_expr(paste0(x,"_ui")))))
        # return(eval(parse_expr(paste0(x,"_ui"))))
      }))
    )


    # tabItems(

      # Project load/creation ---------------------------------------------------


      # for(a in unique(gsub("(_serv)|(_ui)|(\\.R)", "", list.files("D:/Peccary/inst/Module")))){
      #
      #   eval(parse_expr(paste(readLines(file.path("D:/Peccary/inst/Module", paste0(a,"_ui.R"))), collapse = "")))
      #
      # }
      # eval(map(unique(gsub("(_serv)|(_ui)|(\\.R)", "", list.files("D:/Peccary/inst/Module"))), function(x){
      #
      #  parse_expr(paste(readLines(file.path("D:/Peccary/inst/Module", paste0(x,"_ui.R"))), collapse = ""))
      # #
      #   # print(class(eval(parse_expr(paste0(x,"_ui")))))
      #   # return(eval(parse_expr(paste0(x,"_ui"))))
      # })[[1]])
      ### don't touch
    # )## end tab items


  )
)
}
go <- function() shinyApp(pecc_ui_reunif, pecc_server_reunif_test)
#' @export
peccary_app3 <- function(){

  rm(InfoModelPecc, envir = global_env())
  shinyApp(pecc_ui_reunif(), pecc_server_reunif)
  }

