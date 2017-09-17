margin_gdp_df <- grand_finals_df %>% 
	               select(premier, season, win_margin, preliminary_finalist) %>%        
               	inner_join(., abs_gdp_df, by=c("season"="season"))

## Create decade variable to group margin-gdp pairs
margin_gdp_df$decade <- factor(((margin_gdp_df$season) %/% 10) * 10)
margin_gdp_df$era <- factor(((margin_gdp_df$season-5) %/% 10) * 10)

## Define theme for plot
theme_margin_gdp <- theme(text = element_text(size=12),
													 plot.title = element_text(size=12),
													 plot.subtitle = element_text(size=10),
													 axis.title.x = element_blank(),
													 axis.title.y = element_text(size=10, vjust = -1),
													 axis.line.x = element_blank(),
													 axis.ticks.x = element_blank(),
													 legend.position = "bottom",
													 panel.background = element_blank(),
													 panel.grid.major.x = element_line(),
													 legend.box.background = element_blank())


## Create chart plotting margins versus gdp growth grouped by decade
gdp_v_margin_point <- margin_gdp_df %>%
											filter(!decade %in% c(1950,1960)) %>%
											ggplot(aes(y=gdp_cvm_sa_growth, x=win_margin)) +  ## Add to aes to colour/group by decade  colour=decade, group=decade'
											geom_point(aes()) +
											geom_smooth(method = "lm", formula = y ~ x + I(x*x))  +
											scale_colour_manual(values=tsy_corp_pal_combined) +
											scale_fill_manual(values=tsy_corp_pal_combined) +
											scale_y_continuous(expand=c(0,0), limits=c(-4,10), breaks=c(-2.5,0,2.5,5,7.5)) +
											labs(title="Growth in GDP and Australian Football League GF margins, 1960 to 2016",
							  			 		 subtitle="Australian GDP is positively correlated with the Grand Final margin.",
													 y="Growth in GDP (chain volume measure), per cent",
													 x="Grand Final winning margin, points",
													 colour="") +
										 theme_scatter

ggsave("gdp_v_margin_point.png",plot=gdp_v_margin_point, path="~/afl_economic_stimulus/", units = "mm", width = 170, height = 170)	                 

