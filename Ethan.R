packages <- c("tidyverse",    # ggplot, dplyr, etc. 
              "TSA",          # time series stuff
              "lubridate",    # date time 
              "roxygen2",     # function documentation
              "data.table"    # read web hosted file
              )    

# check if packages are installed, if not, install
install.packages(setdiff(packages, rownames(installed.packages())))  

# load packages
library(tidyverse)
library(TSA)
library(lubridate)
library(roxygen2)
library(data.table)

df0 <- fread('https://house-stock-watcher-data.s3-us-west-2.amazonaws.com/data/all_transactions.csv')

df <- df0 %>%
  filter(!is.na(ticker))

