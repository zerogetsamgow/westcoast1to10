library(xml2)
library(dplyr)
library(tidyr)
library(rvest)
library(lubridate)
library(tidyverse)
options(timeout= 4000000)

## Read in all west coast eagles games from AFL tables

wce_season_url <- "http://afltables.com/afl/teams/westcoast/allgames.html"
download.file(wce_season_url , destfile = "wce_season_url.html", quiet=TRUE, mode="wb")
wce_season_html <- read_html("wce_season_url.html")

## Count the number of games html_nodes("td a")
count_game_urls <- wce_season_html %>% html_nodes("td a") %>% length()

## Function to obtain url for game nodes

get_wce_game_urls <- function(games) {
	
	## create variable for URL of game on afltables
	game_urls <- list()
	
	for (game in c(1:games)) {
	 		game_url <- wce_season_html %>% 
	 			          html_nodes("td a") %>% 
	 			          .[[game]] %>% html_attr("href") %>% 
	 			          gsub(., pattern="^../..", replacement="")
	 		game_urls <- append(game_urls,game_url)
	}
	
	return(game_urls)
	
}

## Call function
game_url_df <- get_wce_game_urls(count_game_urls) %>% enframe %>% unnest()

names(game_url_df) <-c("game_id", "game_url")

## Build data frame of team sheets
team_sheets_df <- data.frame()
for (url_stub in game_url_df$game_url)  {
	## Read HTML for game by appending rest of url
	game_url <- paste0("https://afltables.com/afl",url_stub)
	download.file(game_url , destfile = "game_url.html", quiet=TRUE, mode="wb")
	game_html <- read_html("game_url.html")
	
	## Check thead for each tabel to find which one  has West Coast Match Stats
	wce_player_table <- game_html %>% 
		                  html_nodes("table") %>%  
		                  html_text() %>% 
		                  grepl("West Coast Match Statistics", .) %>% 
		                  which(.==TRUE)
	
	#wce_game_info <- game_html %>% html_nodes("table") %>%  .[[1]] %>% html_text() 
	
 	## Get the team sheet base on home or away team
	team_sheet <- game_html %>% html_nodes("table") %>% 
		
		            .[[wce_player_table]] %>% 
		            html_table(fill = TRUE, head = FALSE) %>%
		            # remove header row
								.[-1,]

	## rename columns  
	names(team_sheet) <-  c("player_number",team_sheet[2,2:ncol(team_sheet)]) 
	team_sheet <-team_sheet %>% .[-1,] %>%
	             data.frame() 
	
	                        %>% 
		           .[,1:2] %>%
		        
							 mutate(game_url=url_stub)
	team_sheets_df <- rbind(team_sheets_df,team_sheet)
	
}

top_ten_df <- data.frame('player_number'=c(1:10))

all_ten_played <- team_sheets_df %>% 
	                mutate(player_number=as.integer(gsub("[^0-9]", "", player_number))) %>%
 								  inner_join(.,top_ten_df) %>%
 								  group_by(game_url) %>%
 								  mutate(how_many=n()) %>%
 								  filter(how_many==10)
