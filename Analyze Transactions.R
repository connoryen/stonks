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
# Load data
# ======================================================================

# find data files
data_files <- file.info(list.files("Processed Data/", full.names = T))
# assign most recently modified file to df
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

# Look at securities in an industry
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# Telecom
df %>%
  filter(industry == "Telecom",
         year(transaction_date) >= 2022) %>%
  group_by(ticker) %>%
  summarise(min_total_purchases = sum(lower_amount[type == "purchase"]),
            max_total_purchases = sum(upper_amount[type == "purchase"]),
            min_total_sales = sum(lower_amount[type == "sale_full"]),
            max_total_sales = sum(upper_amount[type == "sale_full"]),
            n_transactions = n(), 
            unique_buyers = length(unique(representative[type == 
                                                           "purchase"]))) %>%
  view()

#Drug Manufacturers—General
df %>%
  filter(industry == "Drug Manufacturers—General",
         year(transaction_date) >= 2022) %>%
  group_by(ticker) %>%
  summarise(min_total_purchases = sum(lower_amount[type == "purchase"]),
            max_total_purchases = sum(upper_amount[type == "purchase"]),
            min_total_sales = sum(lower_amount[type == "sale_full"]),
            max_total_sales = sum(upper_amount[type == "sale_full"]),
            n_transactions = n(), 
            unique_buyers = length(unique(representative[type == 
                                                           "purchase"]))) %>%
  view()

# Look at particular securities
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
df %>%
  filter(year(transaction_date) == 2022,
         ticker %in% c("T")) %>%
  view()

df %>%
  filter(year(transaction_date) == 2022,
         ticker %in% c("ABBV")) %>%
  view()

# ======================================================================
# By ticker **
# ======================================================================

df %>%
  filter(year(transaction_date) >= 2022) %>%
  group_by(ticker) %>%
  summarise(n_purchases = sum(type == "purchase"),
            n_sales = sum(type == "sale_full"),
            net_transactions = n_purchases - n_sales) %>%
  arrange(-net_transactions) %>%
  view()

# Look at particular security
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
df %>%
  filter(year(transaction_date) == 2022,
         ticker %in% c("T")) %>%
  view()


# ======================================================================
# By representative
# ======================================================================
  
# summary for representatives
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
df %>%
  filter(industry != "") %>%
  group_by(representative) %>%
  summarise(industry_diversity = length(unique(industry)),
            portfolio_diversity = length(unique(ticker)),
            reported_income_min = sum(lower_amount[type == "sale_full"]) - 
              sum(upper_amount[type == "purchase"]),
            reported_income_max = sum(upper_amount[type == "sale_full"]) - 
              sum(lower_amount[type == "purchase"]),
            reported_income_upper = sum(upper_amount[type == "sale_full"]) - 
              sum(upper_amount[type == "purchase"]),
            reported_income_lower = sum(lower_amount[type == "sale_full"]) - 
              sum(lower_amount[type == "purchase"]),
            n_transactions = n()) %>%
  arrange(-reported_income_min) %>%
  view()

# Look at particular representative
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
df %>%
  filter(representative == "Hon. Donald Sternoff Beyer") %>%
  arrange(type, lower_amount) %>%
  view()


