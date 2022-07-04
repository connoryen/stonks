# ======================================================================
# Preliminaries 
# ======================================================================

packages <- c("tidyverse",    # ggplot, dplyr, etc. 
              "TSA",          # time series stuff
              "lubridate",    # date time 
              "roxygen2",     # function documentation
              "data.table",   # read web hosted file
              "tidyquant",    # pull stock data
              "rvest"         # web scrapping
)    

# check if packages are installed, if not, install
install.packages(setdiff(packages, rownames(installed.packages())))

# load packages
lapply(packages, require, character.only = TRUE)

# yfinance
remotes::install_github("ljupch0/yfinance")
library(yfinance)

# ======================================================================
# Load processed data
# ======================================================================

# find data files
data_files <- file.info(list.files("Processed Data/", full.names = T))
# assign most recently modified file to df
df <- read.csv(rownames(data_files)[which.max(data_files$mtime)])
