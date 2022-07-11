# ======================================================================
# Preliminaries 
# ======================================================================

packages <- c("tidyverse",    # ggplot, dplyr, etc. 
              "lubridate",    # date time 
              "roxygen2",     # function documentation
              "data.table",   # read web hosted file
              "tidyquant"    # pull stock data
)    

# check if packages are installed, if not, install
install.packages(setdiff(packages, rownames(installed.packages())))

# load packages
lapply(packages, require, character.only = TRUE)

# yfinance
remotes::install_github("ljupch0/yfinance")
library(yfinance)

# ======================================================================
# Load data
# ======================================================================
data_files <- file.info(list.files("Processed Data/", full.names = T))
df <- read.csv(rownames(data_files)[which.max(data_files$mtime)])
# ======================================================================
# By Industry *
# ======================================================================

# summary for industries
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
df %>%
  filter(industry != "",
         year(transaction_date) >= 2022) %>%
  group_by(industry) %>%
  summarise(n_purchases = sum(type == "purchase"),
            n_sales = sum(type == "sale_full"),
            net_transactions = n_purchases - n_sales,
            unique_buyers = length(unique(representative[type == 
                                                           "purchase"]))) %>%
  arrange(-net_transactions) %>%
  view()

# Oil & Gas Midstream consists of points in the oil production process th=at falls between the exploration/
# production of gas and the distribution or refining/purifying of gas. Includes storage/processing/transportation
# Includes: companies specializing in operating tanker ships/pipelines/storage facilities

# Oil & Gas E&P = upstream segment of the oil and gas industry.
# Includes: companies involved in drilling/extracting oil and search and exploration for oil.

Gasdf <- df[grep("Gas", df$industry),]
Gasdf %>%
  filter(year(transaction_date) >= 2022) %>%
  group_by(industry) %>%
  summarise(n_purchases = sum(type == "purchase"),
            n_sales = sum(type == "sale_full"),
            net_transactions = n_purchases - n_sales,
            unique_buyers = length(unique(representative[type == 
                                                           "purchase"]))) %>%
  arrange(-net_transactions) %>%
  view()

Gasdf %>%
  filter(representative == "Hon. Virginia Foxx") %>%
  arrange(type, lower_amount) %>%
  view()
