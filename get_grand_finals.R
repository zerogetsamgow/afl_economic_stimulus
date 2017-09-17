## load libraries
library(xml2)
library(plyr)
library(dplyr)
library(tidyr)
library(rvest)
library(lubridate)
library(textclean)

## define get_grand_final function
get_grand_final <- function(season) {
	
	## create variable for URL of season on afltables
	afltables <- read_html(paste0("http://afltables.com/afl/seas/",as.character(season),".html"))
	
	## Count number of nodes of type center table. Grand final data is stored in last table
	node_type <- "center table"
	grand_final_table_no <-  afltables %>% html_nodes(node_type)  %>% length()
	
	grand_final_table <- afltables %>%
								       html_nodes(node_type)  %>%
								       .[[grand_final_table_no]] %>%
								       html_table(fill = TRUE, head = FALSE) %>%
								       data.frame()
	
	## Convert table to single row	
	grand_final_as_row <- cbind(season, grand_final_table[1,], grand_final_table[2,]) 

	
	## Rename variables
	names(grand_final_as_row) <- c("season",
													      "team_home", "quarters_home", "score_home",
													      "date_venue",
													      "team_away", "quarters_away", "score_away",
													       "result")
	
	## Deprecated current
	alternate_names <- c("Footscray","Kangaroos")
	current_names <- c("Western Bulldogs","North Melbourne")
	
	## Clean data into separate variables
	
	## Create a list of AFL 2017 preliminary finalists
	preliminary_finalists_2017 <- c("Adelaide", "Richmond", "Geelong", "Greater Western Sydney")
	
	
	
	grand_final_as_row <- grand_final_as_row %>%
		                    mutate(team_home = factor(mgsub(team_home, alternate_names,current_names), levels=all_teams),
				 										   team_away = factor(mgsub(team_away, alternate_names,current_names), levels=all_teams),
				 										   result = mgsub(result, alternate_names,current_names),
															 local_time = as.POSIXct(strptime(sub("([0-9]){1}:","0\\1:", ## Add leading zero to hour
				 							 																 sub("[A-Za-z]{3}\\s","",   ## Remove Day string
				 							 															   gsub("\\s+(\\(|Att).*$", "", date_venue))), ## Remove text after date
				 					 																		 "%d-%b-%Y %I:%M %p")), ## Format of character string to be converted to date.
				                       venue = sub(".*Venue: ", "", date_venue),
			                      	 attendance = as.integer(sub(",", "",gsub("^.*Att: ([0-9,]+).*$", "\\1", date_venue))),
				                       premier = factor(ifelse(!grepl("Match drawn",result),sub(" won.*", "", result),""), levels=all_teams),
				 											 runner_up = ifelse(premier==team_home, team_away, ifelse(premier=="","",team_home)),
															 win_margin = ifelse(grepl("Match drawn",result),0,as.integer(gsub(".*by\\s([0-9]+)\\spt.*","\\1",result)))) %>%
															 separate(quarters_home, into = paste0("q", 1:4, "_home"), sep = " ") %>%
															 separate(quarters_away, into = paste0("q", 1:4, "_away"), sep = " ") %>%
		                           mutate(preliminary_finalist=(premier %in% preliminary_finalists_2017))
	
	return(grand_final_as_row)
						 
}

## Define list of seasons we want to collect from AFL Tables
seasons <- c(1946:2016)


## Create data frame of grand finals for seasons in list
grand_finals_df <-  seasons %>% ldply(., get_grand_final) 

## Save data as .csv, in case I cant access AFL tables from work
## write.csv(grand_finals_df,"./afl_economic_stimulus/grand_finals_local.csv", row.names = FALSE)
