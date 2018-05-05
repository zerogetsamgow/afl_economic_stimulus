## load libraries
library(dplyr)
library(openxlsx)
library(readxl)

## Get national accounts data from ABS
url <- file.path("http://www.abs.gov.au/ausstats/",
                 "meisubs.NSF/log?openagent&5206001_key_aggregates.zip&5206.0&Time%20Series%20",
                 "Spreadsheet&24FF946FB10A10CDCA258192001DAC4B&0&Jun%202017&06.09.2017&Latest")

download.file(url = url, destfile = "./afl_economic_stimulus/abs_local.zip", mode="wb")
abs_xls <- unzip("./afl_economic_stimulus/abs_local.zip", exdir="./afl_economic_stimulus/")

## Create data frame from abs_xls
## Column 1 is the date (quarterly)
## Column 54 is Gross domestic product: Chain volume measures $ Millions Seasonally Adjusted
## Column 18 is Gross domestic product: Chain volume measures $ Millions Trend
abs_gdp_df <- read_xls(abs_xls, sheet = "Data1", skip=10, col_names = FALSE) %>%
	            .[,c(1,18,54)] %>%
	            dplyr::rename(., abs_date=X__1,gdp_cvm_trend=X__18, gdp_cvm_sa=X__54) %>%
	            mutate(abs_month=as.integer(month(abs_date)),
	            			 season=as.integer(year(abs_date))) %>%
	            filter(abs_month==9) %>%
	            ## Calculate the growth in GDP from the end of the September quarter in the season to the end of the September Quarter the following year
	            mutate(gdp_cvm_sa_growth=c(diff(gdp_cvm_sa),NA)/gdp_cvm_sa*100)
