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

# ======================================================================
# Load data
# ======================================================================

# find data files
data_files <- file.info(list.files("Processed Data/", full.names = T))
# assign most recently modified file to df
df <- read.csv(rownames(data_files)[which.max(data_files$mtime)])


df %>%
  filter(representative == "Hon. Virginia Foxx")

# ======================================================================
# By Industry *
# ======================================================================

# summary for industries
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
df %>%
  filter(industry != "") %>%
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
  filter(industry == "Telecom Services") %>%
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
  filter(industry == "Drug Manufacturers—General") %>%
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
df %>% filter(ticker %in% c("T")) %>% view()
df %>% filter(ticker %in% c("ABBV")) %>% view()

# ======================================================================
# By ticker **
# ======================================================================

df %>%
  group_by(ticker) %>%
  summarise(n_purchases = sum(type == "purchase"),
            n_sales = sum(type == "sale_full"),
            net_transactions = n_purchases - n_sales,
            unique_buyers = length(unique(representative[type == 
                                                           "purchase"]))) %>%
  arrange(-net_transactions) %>%
  view()




