packages <- c("tidyverse",    # ggplot, dplyr, etc. 
              "TSA",          # time series stuff
              "lubridate",    # date time 
              "roxygen2"      # function documentation
              )    

# check if packages are installed, if not, install
install.packages(setdiff(packages, rownames(installed.packages())))  

# load packages
library(tidyverse)
library(TSA)
library(lubridate)
library(roxygen2)


