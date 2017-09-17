library(magrittr)
library(dplyr)
library(ggplot2)

## Get grand final data and gdp growth data
## source('~/afl_economic_stimulus/get_grand_finals.R') 
## If you can not access AFL tables use local .csv with code below
 ## library(readr)
 ## List of all teams needed if can not use get_grand_finals.R if no access to afltables.com
 ## all_teams <- c("Adelaide", "Brisbane Lions", "Carlton", "Collingwood","Essendon","Fremantle","Geelong", 
 ##							 "Gold Coast Suns","Greater Western Sydney","Hawthorn","Melbourne","North Melbourne",
 ##							 "Port Adelaide","Richmond","Sydney","St Kilda","Western Bulldogs","West Coast")
 ## grand_finals_df <-"./afl_economic_stimulus/grand_finals_local.csv" %>% read_csv() %>% dplyr::mutate(premier=factor(premier, levels=all_teams))

## source('~/afl_economic_stimulus/get_gdp_data.R')

## Set treasury colours
source('~/afl_economic_stimulus/set_treasury_colours.R')

## Join selected variables from AFL data frame with GDP data frame and create summary stats
premier_gdp_df <- grand_finals_df %>% 
	                select(premier, season, preliminary_finalist) %>%        
	                inner_join(., abs_gdp_df, by=c("season"="season")) %>%   ##use inner join as we add in teams without premierships since 1960 below
	                group_by(premier) %>%
	                ## mutate rather than summarise so we can use original variable in plots
	                dplyr::mutate(mean_gdp_cvm_sa_growth=mean(gdp_cvm_sa_growth),
	                    					max_gdp_cvm_sa_growth=max(gdp_cvm_sa_growth),
	                    					min_gdp_cvm_sa_growth=min(gdp_cvm_sa_growth),
	                							premierships=n(),
	                							premiership_years=list(season)) %>%
	                ungroup()
	                    
## Coerce means for teams without premierships to minus five to put at bottom of order and out of chart limits
non_premiers <- data.frame(premier=c("Greater Western Sydney", "Fremantle", "Gold Coast Suns"), 
													 season=c(rep(2017,3)),
													 gdp_cvm_sa_growth=c(rep(-5,3)),
													 mean_gdp_cvm_sa_growth=c(rep(-5,3)),
													 preliminary_finalist=c(TRUE,FALSE,FALSE)) %>%
	              mutate(premier=factor(premier,levels=all_teams))
             
## Add non-premiers and Reorder data frame by mean gdp then preliminary_finalist
premier_gdp_df <- premier_gdp_df %>%
	                    full_join(., non_premiers, by=c("premier"="premier","season"="season","gdp_cvm_sa_growth"="gdp_cvm_sa_growth","mean_gdp_cvm_sa_growth"="mean_gdp_cvm_sa_growth","preliminary_finalist"="preliminary_finalist")) %>%
                      dplyr::mutate(premier=reorder(reorder(premier, mean_gdp_cvm_sa_growth), preliminary_finalist))

## Define a theme for the chart
theme_premier_gdp <- theme(text = element_text(size=12),
											 plot.title = element_text(size=12),
											 plot.subtitle = element_text(size=10),
	                     axis.title.y = element_blank(),
											 axis.title.x = element_text(size=10, vjust = -1),
											 axis.line.y = element_blank(),
											 axis.ticks.y = element_blank(),
											 legend.position = "bottom",
											 panel.background = element_blank(),
											 panel.grid.major.x = element_line())

## Plot a chart of teams versus gdp growth
gdp_by_team_point <- premier_gdp_df %>%
	                   ggplot(aes(y=premier, x=gdp_cvm_sa_growth)) +
	                   geom_point(aes(colour="A", fill="A"), size=4)  +
	                   geom_point(aes(x=mean_gdp_cvm_sa_growth, colour="B", fill="B"), size=4) +
										 ## include next row for crude labels.
										 ## geom_text(size=2.5, position="dodge",check_overlap=TRUE, angle=90, colour="black", hjust=1.8, aes(label=substr(season,3,4)))+
	                   scale_colour_manual(values=c("A"=corp_blue_lt, "B"=corp_green_lt), labels=c("Individual premiership years","Mean growth")) +
	                   scale_fill_manual(values=c("A"=corp_blue_lt, "B"=corp_green_lt), guide=FALSE) + 
	                   scale_y_discrete(drop=FALSE)+
	                   scale_x_continuous(expand=c(0,0), limits=c(-3,10), breaks=c(-2.5,0,2.5,5,7.5)) +
	                   labs(title="AFL Premiers and economic growth, 1960 to 2016",
	                 		  subtitle="The Australian economy, on average, has grown faster after Adelaide premierships\nthan following premierships of any other 2017 preliminary finalist.",
	                 		  x="Growth in GDP (chain volume measure), per cent",
	                 		  colour="") +
	                  theme_premier_gdp

## Save the chart          
ggsave("gdp_by_team_point.png",plot=gdp_by_team_point, path="~/afl_economic_stimulus/", units = "mm", width = 180, height = 170)	                 

## Create point range chart with ggplot
gdp_by_team_pointrange <- premier_gdp_df %>%
													ggplot(aes(x=premier, group=premier, y=gdp_cvm_sa_growth, ymin=min_gdp_cvm_sa_growth, ymax=max_gdp_cvm_sa_growth, colour=preliminary_finalist)) +
	geom_pointrange(size=4, fatten=.7) +
	coord_flip() +
	scale_colour_manual(values=tsy_corp_pal_lt, labels=c("Individual premiership years","Mean growth"), guide=FALSE) +
	scale_fill_manual(values=tsy_corp_pal_lt, guide=FALSE)			+
	geom_point(aes(y=mean_gdp_cvm_sa_growth) , colour=corp_orange_lt, size=3) +
  scale_y_continuous(expand=c(0,0), limits=c(-3,10), breaks=c(-2.5,0,2.5,5,7.5)) +
	labs(title="AFL Premiers and economic growth, 1960 to 2016",
			 subtitle="The Australian economy, on average, has grown faster after Adelaide premierships\nthan following premierships of any other 2017 preliminary finalist.",
			 y="Growth in GDP (chain volume measure), per cent",
			 colour="") +
	theme_premier_gdp


## Save the chart          
ggsave("gdp_by_team_pointrange.png",plot=gdp_by_team_pointrange, path="~/afl_economic_stimulus/", units = "mm", width = 180, height = 170)	                 

