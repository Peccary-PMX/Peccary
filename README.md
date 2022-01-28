# Peccary

Peccary is a R package made to improve pharmacometrics daily efficiency. 

## Installation 

To install Peccary, please follow the following steps

**Step1**

First download the *.rar* folder of your choice (built in R3.6 or R4.0) on GitHub *releases* section: 

https://github.com/Peccary-PMX/Peccary/releases


**Step2**

Unzip the folder and deposit its four sub-folders  (named *peccary*, *PeccAnalysis*, *PeccaReverse* and *PeccaResults*) directly in your library folder (do *".libPaths()"* on your R console to know  its path). 

**Step3**

Peccary uses several R packages that need to be pre-installed. To check and download missing packages, please execute the following code:

> packagesneeded <- c("ggplot2","stringr","dplyr","tidyr","purrr","tibble","cowplot","gridExtra","magrittr","knitr","plotly", "shiny","forcats","data.tree","collapsibleTree", "ggpubr","rlang","deSolve", "rhandsontable","shinyjqui","shinydashboard","shinyTree","DT","PKNCA","shinyjs","vpc", "RxODE")

>install.packages(setdiff(packagesneeded, rownames(installed.packages())))

## How to use Peccary

Once Peccary has been installed and loaded (*"library(peccary)*"), the easiest way to use it is through its Shiny Application. To launch it please execute

> peccary_app3() 

For now, two main and complementary sources of documentation / tutorial exists regarding the Shiny Application:

1. a manual ([click here to download](https://github.com/Peccary-PMX/Peccary/releases/download/R4.0/Peccary_Documentation.html)), also foundable  in the releases folder, the Peccary sub-folder or in the GitHub repository.
2. youtube videos, especially https://www.youtube.com/watch?v=d77c4v2_jcw being a good starting point for using and learning Peccary

Please asks if you want more documentation.

## Contact

If you have any questions, bugs reporting or suggestions, you can either contact me  through the GitHub *Issues* section or by email to peccary.pmx@gmail.com (or thibaud.derippe@gmail.com).

Thanks  ! 

Thibaud
