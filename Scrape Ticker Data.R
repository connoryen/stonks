# ======================================================================
# Preliminaries 
# ======================================================================

packages <- c("tidyverse",    # ggplot, dplyr, etc. 
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
# Scrape industry data from Yahoo Finance
# ======================================================================

# Scrape sector and industry from yahoo finance
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#' Scrape the sector(s) and industry of a company
#' 
#' This returns the sector(s) and industry of a ticker using data from Yahoo
#' finance.
#' 
#' @param ticker ticker abbreviation of the company in question.
#' 
#' @return vector of sector and industry of the company
#' 
#' @examples
#' scrape_company_type("DoesNotExist")
#' scrape_company_type("GILD")
#' scrape_company_type("AAPL")
scrape_company_type <- function(ticker){
  # Check input
  if(!is.character("ticker")) 
    stop("Error: ticker input is not a string")
  
  rv <- try(
    paste0("https://finance.yahoo.com/quote/", ticker, "/profile") %>%
      read_html() %>%
      # select the node with class "asset-profile-container." This comes from 
      #   inspecting the yahoo finance pages.
      html_nodes(".asset-profile-container") %>%
      # get the text contained within <span> </span?
      html_elements("span") %>%
      html_text2() %>%
      as_tibble() %>%
      slice(c(2,4)) %>%
      # convert to vector
      pull(),
    silent = TRUE
  )
  if (class(rv)=="try-error") NA else {
    c("sector" = rv[1], "industry" = rv[2])
  }
}

# Test ..........
scrape_company_type("DoesNotExist")
scrape_company_type("GILD")
scrape_company_type("AAPL")[["sector"]]

# Get Industry and Sector for companies listed in AMEX, NASDAQ, and NYSE
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# list of tickers
tickers <- stockSymbols()
validTicker <- filter(tickers, Exchange %in% c("AMEX", "NASDAQ", "NYSE"))
validTickers <- tickers[["Symbol"]]


# Start clock
ptm <- proc.time()

# takes ~67s to run 20 rows. (~11 hours to run all 12210 tickers)
data.frame(ticker = validTickers[1:10]) %>%
  rowwise() %>%
  mutate(sector = scrape_company_type(ticker)[["sector"]],
         industry = scrape_company_type(ticker)[["industry"]]) %>%
  write.csv(., "Industry_and_Sector.csv", row.names = FALSE)

# Stop clock
proc.time() - ptm
