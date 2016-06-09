# Required R libraries (need to be installed - it can take a few minutes the first time you run the project)

# installs all necessary libraries from CRAN
get_libraries <- function(filenames_list) { 
  lapply(filenames_list,function(thelibrary){    
    if (do.call(require,list(thelibrary)) == FALSE) 
      do.call(install.packages,list(thelibrary)) 
    do.call(library,list(thelibrary))
  })
}

# installs all necessary libraries from CRAN
get_libraries <- function(filenames_list) { 
  lapply(filenames_list,function(thelibrary){    
    if (do.call(require,list(thelibrary)) == FALSE) 
      do.call(install.packages,list(thelibrary)) 
    do.call(library,list(thelibrary))
  })
}

libraries_used=c("devtools","knitr","graphics","grDevices","xtable","pryr",
                 "Hmisc","vegan","fpc","GPArotation","FactoMineR","cluster",
                 "psych","stringr","googleVis", "png","ggplot2","googleVis", "gridExtra", "shiny")
get_libraries(libraries_used)

if (require(slidifyLibraries) == FALSE) 
  install_github("slidifyLibraries", "ramnathv")
if (require(slidify) == FALSE) 
  install_github("slidify", "ramnathv") 
