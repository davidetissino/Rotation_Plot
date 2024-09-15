library(janitor)
library(hablar)
library(jsonlite)
library(httr)
library(ggtext)
library(ggplot2)
library(ggprism)
library(tidyverse)
library(dplyr)
library(RColorBrewer)
library(nbastatR)
library(ggimage)
library(tictoc)
library(ggpubr)


#### DESCRIPTION 
## 


# csv with team names, slugs, colors 
tms <- read_csv('/Users/davidetissino/Desktop/R/data/teams.csv')

# increase buffer size for scraping
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)


# Teams BOXSCORES Infos ====

## 2023-24 RS Team Logs ##
# headers <- c(
#   `Connection` = 'keep-alive',
#   `Accept` = 'application/json, text/plain, */*',
#   `x-nba-stats-token` = 'true',
#   `X-NewRelic-ID` = 'VQECWF5UChAHUlNTBwgBVw==',
#   `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.87 Safari/537.36',
#   `x-nba-stats-origin` = 'stats',
#   `Sec-Fetch-Site` = 'same-origin',
#   `Sec-Fetch-Mode` = 'cors',
#   `Referer` = 'https://stats.nba.com/players/shooting/',
#   `Accept-Encoding` = 'gzip, deflate, br',
#   `Accept-Language` = 'en-US,en;q=0.9'
# )
# 
# url <- 'https://stats.nba.com/stats/leaguegamelog?Counter=1000&DateFrom=&DateTo=&Direction=DESC&ISTRound=&LeagueID=00&PlayerOrTeam=T&Season=2023-24&SeasonType=PlayIn&Sorter=DATE'
# 
# res <- GET(url = url, add_headers(.headers = headers))
# 
# json_resp <- fromJSON(content(res, "text"))
# 
# pi_logs_24 <- data.frame(json_resp$resultSets$rowSet)
# 
# colnames(pi_logs_24) <- json_resp[["resultSets"]][["headers"]][[1]]


### Load CSV TMS Boxscores 2023-24  ====
##### RS ----
TMS_boxscores_24_RS <- read.csv('/Users/davidetissino/Desktop/R/RotationPlot/Data/TeamBoxscore/2023-24/Boxscores_2023-24_RS.csv')

TMS_boxscores_24_RS$type <- 'RS'

##### Play-In ----
TMS_boxscores_24_pi <- read.csv('/Users/davidetissino/Desktop/R/RotationPlot/Data/TeamBoxscore/2023-24/Boxscores_2023-24_pi.csv')

TMS_boxscores_24_pi$type <- 'pi'

##### Playoffs ----
TMS_boxscores_24_PO <- read.csv('/Users/davidetissino/Desktop/R/RotationPlot/Data/TeamBoxscore/2023-24/Boxscores_2023-24_PO.csv')

TMS_boxscores_24_PO$type <- 'PO'


# Merge into one 
### Teams Boxscores 2023-24 ====
TMS_boxscores_24 <- rbind(TMS_boxscores_24_RS, TMS_boxscores_24_pi, TMS_boxscores_24_PO)


# UNIQUE game IDs ====
unique_IDs_24 <- unique(TMS_boxscores_24$GAME_ID)





# ROTATION Infos ====
# Scraping function 
# rotation_info <- function(game_id) {
#   
#   url <- paste0('https://stats.nba.com/stats/gamerotation?GameID=', game_id)
#   
#   res <- GET(url = url, add_headers(.headers = headers))
#   
#   json_resp <- fromJSON(content(res, 'text'))
#   
#   # dataframes for home & away 
#   # then will merge 
#   home <- data.frame(json_resp[["resultSets"]][["rowSet"]][[2]])
#   away <- data.frame(json_resp[["resultSets"]][["rowSet"]][[1]])
#   
#   colnames(home) <- json_resp[["resultSets"]][["headers"]][[1]]
#   colnames(away) <- json_resp[["resultSets"]][["headers"]][[1]]
#   
#   home <- home %>%
#     clean_names() %>%
#     retype()
#   
#   away <- away %>%
#     clean_names() %>%
#     retype()
#   
#   # one player column 
#   home$player <- paste0(home$player_first, ' ', home$player_last)
#   away$player <- paste0(away$player_first, ' ', away$player_last)
#   
#   # add location column 
#   home$location <- 'Home'
#   away$location <- 'Away'
#   
#   # remove & clean some columns
#   home <- home[, c(14, 13, 8:12, 1:7)]
#   away <- away[, c(14, 13, 8:12, 1:7)]
#   
#   ### merge into one #
#   df <- rbind(home, away)
#   
#   # function to convert time data
#   convert_to_time_string <- function(seconds) {
#     time_strings <- sprintf("%02d:%02d", as.integer(seconds) %/% 60, as.integer(seconds) %% 60)
#     return(time_strings)
#   }
#   
#   # Correct format to in & out time home & away
#   df$in_time_real <- lapply(df$in_time_real / 10, convert_to_time_string)
#   df$out_time_real <- lapply(df$out_time_real / 10, convert_to_time_string)
#   
#   # epoch to convert to UNIX time
#   date <- as.Date("2000-01-01")
#   
#   # Convert "mm:ss" to Unix time (seconds since Unix Epoch)
#   df$in_unix_time <- as.integer(as.POSIXct(paste(date, df$in_time_real), format = "%Y-%m-%d %M:%S"))
#   df$out_unix_time <- as.integer(as.POSIXct(paste(date, df$out_time_real), format = "%Y-%m-%d %M:%S"))
#   
#   # remove some columns
#   #df <- df[, -c(7, 9:14)]
#   
#   return(df)
#   
# }
# 
# 
# rot <- rotation_info('0022300001')
# 
# # map all single dfs into one 
# tic()
# fin <- map_df(unique_IDs_24, rotation_info)
# toc()


### Load CSV Rotations 2023-24 ====
##### RS ---- 
rotations_24_RS <- read.csv('/Users/davidetissino/Desktop/R/RotationPlot/Data/Rotations/2023-24/Rotations_2023-24_RS.csv')

rotations_24_RS$type <- 'RS'

##### Play-in ----
rotations_24_pi <- read.csv('/Users/davidetissino/Desktop/R/RotationPlot/Data/Rotations/2023-24/Rotations_2023-24_pi.csv')

rotations_24_pi$type <- 'pi'

##### Playoffs ----
rotations_24_PO <- read.csv('/Users/davidetissino/Desktop/R/RotationPlot/Data/Rotations/2023-24/Rotations_2023-24_PO.csv')

rotations_24_PO$type <- 'PO'


# merge into one
### Rotations 2023-24 ====
rotations_24 <- rbind(rotations_24_RS, rotations_24_pi, rotations_24_PO)







# Players BOXSCORES Infos ####
# headers <- c(
#     `Connection` = 'keep-alive',
#     `Accept` = 'application/json, text/plain, */*',
#     `x-nba-stats-token` = 'true',
#     `X-NewRelic-ID` = 'VQECWF5UChAHUlNTBwgBVw==',
#     `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.87 Safari/537.36',
#     `x-nba-stats-origin` = 'stats',
#     `Sec-Fetch-Site` = 'same-origin',
#     `Sec-Fetch-Mode` = 'cors',
#     `Referer` = 'https://stats.nba.com/players/shooting/',
#     `Accept-Encoding` = 'gzip, deflate, br',
#     `Accept-Language` = 'en-US,en;q=0.9'
#   )
# 
# url <- 'https://stats.nba.com/stats/leaguegamelog?Counter=1000&DateFrom=&DateTo=&Direction=DESC&ISTRound=&LeagueID=00&PlayerOrTeam=P&Season=2023-24&SeasonType=Playoffs&Sorter=DATE'
# 
# res <- GET(url = url, add_headers(.headers = headers))
# 
# json_resp <- fromJSON(content(res, "text"))
# 
# rs_logs <- data.frame(json_resp[["resultSets"]][["rowSet"]][[1]])
# 
# colnames(rs_logs) <- json_resp[["resultSets"]][["headers"]][[1]]


### Load CSV PL Boxscores 2023-24 ====
##### RS ----
PL_boxscores_24_RS <- read.csv('/Users/davidetissino/Desktop/R/RotationPlot/Data/PlayerBoxscore/2023-24/Player_boxscores_2023-24_RS.csv')

PL_boxscores_24_RS$type <- 'RS'

##### Play-In ----
PL_boxscores_24_pi <- read.csv('/Users/davidetissino/Desktop/R/RotationPlot/Data/PlayerBoxscore/2023-24/Player_boxscores_2023-24_pi.csv')

PL_boxscores_24_pi$type <- 'pi'

##### Playoffs ----
PL_boxscores_24_PO <- read.csv('/Users/davidetissino/Desktop/R/RotationPlot/Data/PlayerBoxscore/2023-24/Player_boxscores_2023-24_PO.csv')

PL_boxscores_24_PO$type <- 'PO'


# merge into one
### Players Boxscores 2023-24 ====
PL_boxscores_24 <- rbind(PL_boxscores_24_RS, PL_boxscores_24_pi, PL_boxscores_24_PO)










# filter for specific GAME IDs
rs_logs <- rs_logs %>% 
  filter(
    GAME_ID == trial[1] | GAME_ID == trial[2]
    )

# change column name to merge 
colnames(rs_logs)[3] <- 'player'

# numeric PTS column for after
rs_logs$PTS <- as.numeric(rs_logs$PTS)



# FINAL DF ====
final <- merge(fin, rs_logs, by = 'player')








# remove unnecessary columns
final <- final[, -c(10:12, 14,15, 17, 20:36, 39, 40)]

# rearrange columns order
final <- final[, c(7, 11, 10, 2, 12, 1, 13:15, 3, 4, 6, 8, 9, 5)]

final$MIN <- as.numeric(final$MIN)
final$PTS <- as.numeric(final$PTS)
final$PLUS_MINUS <- as.numeric(final$PLUS_MINUS)

final <- final %>% 
  arrange(location, desc(-MIN)) %>%
  arrange(factor(location, levels = c("Home", "Away")))



# Abbreviate names
# distinguishies between 2-wrd and 3-wrd names 
final$short_player <- sapply(final$player, function(name) {
  # Split the name by spaces
  parts <- unlist(strsplit(name, " "))
  
  # If there are three words, return first initial, second word, and third word
  if (length(parts) == 3) {
    return(paste0(substr(parts[1], 1, 1), ". ", parts[2], " ", parts[3]))  # First initial, second, and third word
  }
  
  # If there are exactly two words, return first initial and second word (full last name)
  if (length(parts) == 2) {
    return(paste0(substr(parts[1], 1, 1), ". ", parts[2]))  # First initial and full second word (last name)
  }
  
  # If the name does not match the expected pattern, return the original name
  return(name)
})



# add a + sign if plus/minus > 0
final <- final %>%
  mutate(
    PLUS_MINUS = ifelse(PLUS_MINUS >= 0, paste("+ ", PLUS_MINUS, sep = ""), paste('- ', abs(PLUS_MINUS), sep = '')))


# merge shortened name with clean plus/minus
final$pm_info <- paste0(final$short_player, ' (', final$MIN , ' min, ' , final$PLUS_MINUS, ')')
final$pts_info <- paste0(final$short_player, ' (', final$MIN , ' min, ' , final$PTS, ' pts)')



# Add a unique identifier for each player
players <- unique(final$player)
player_ids <- seq_along(players)
player_mapping <- data.frame(player = players, player_id = player_ids)


df_end <- merge(final, player_mapping, by = "player")


# Define the coordinates of rectangles
df_end$ymin <- df_end$player_id - 0.35
df_end$ymax <- df_end$player_id + 0.35




# TEAMS INFOS ====
playing_teams <- data.frame(
  slugTeam = unique(df_end$TEAM_ABBREVIATION),
  loc = unique(df_end$location)
)

teams <- merge(playing_teams, tms, by = 'slugTeam')

# Game Date
game_date <- unique(df_end$GAME_DATE) %>% 
  as.Date() %>% 
  format(., "%B %d, %Y")


## Away & Home Team Name 
away_team <- teams$team[teams$loc == 'Away']
home_team <- teams$team[teams$loc == 'Home']


## Away & Home slugs
away_slug <- teams$slugTeam[teams$loc == 'Away']
home_slug <- teams$slugTeam[teams$loc == 'Home']

## Away & Home Team Points
away_points <- sum(po_logs$PTS[po_logs$TEAM_NAME == away_team])
home_points <- sum(po_logs$PTS[po_logs$TEAM_NAME == home_team])




# ROTATION PLOT ####

df_end %>%
  ggplot(aes(ymin = ymin, ymax = ymax, xmin = in_unix_time, xmax = out_unix_time, fill = pt_diff)) +
  geom_rect(color = 'black') +
  geom_label(
    size = 5,
    color = 'black', 
    fill = 'white',
    fontface = 'bold',
    aes(
      y = (ymin + ymax) / 2, 
      x = (out_unix_time + in_unix_time) / 2,
      label = pt_diff)
  ) +
  scale_x_continuous(breaks = c(946681200, 946681920, 946682640, 946683360, 946684080),
                     labels = c("0", "12", "24", "36", "48"), 
                     guide = 'prism_minor', 
                     limits = c(946681200, 946684080),
                     minor_breaks = seq(946681200, 946684080, 60)
  ) +
  scale_y_continuous(breaks = df_end$player_id, labels = df_end$pm_info) +
  scale_fill_gradient2(low = 'firebrick3', mid = 'floralwhite', high = 'forestgreen') +
  facet_grid(
    TEAM_ABBREVIATION ~ ., 
    scales = "free_y", 
    space = "free_y"
  ) +
  labs(
    x = "", 
    y = "", 
    
    title = paste(
      away_team, '-', away_points, '@', home_points, '-', home_team
    )
    
  ) + 
  theme_minimal() +
  coord_cartesian(xlim = c(946681328, 946683952)) +
  theme(
    
    text = element_text(family='Menlo', color = 'black'), 
    axis.title.x = element_text(face='bold', size=23, margin=margin(t=7)),
    axis.text.y = element_text(margin = margin(r = 5, unit = "pt"), hjust = 0.95, color = 'black', size = 15), 
    axis.text.x = element_text(margin = margin(t = 5, unit = "pt"), color = 'black', size = 15), 
    axis.ticks.x = element_line(color = 'black'),
    
    plot.background = element_rect(color = 'white'),
    plot.margin = margin(.5, .5, 0, 2.5, "cm"), 
    plot.caption = element_text(color = 'gray40', size = 10),
    plot.subtitle=element_text(size=17, hjust = 0.35, margin = margin(t = 20, b = 15)), 
    
    ##### Plot TITLE ====
    plot.title = element_text(face='bold', size=30, hjust = 0.9, margin = margin(t = 10, b = 10)), 
    
    panel.grid = element_line(color = "#afa9a9"),
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect('white'), 
    panel.border = element_rect(colour = 'black', fill = NA, size = 1), 
    
    panel.spacing = unit(1, 'lines'),
    
    legend.position = "right",  # Position the legend at the top of the plot
    legend.title = element_text(size = 15, face = 'bold', angle = -90),  # Set the legend title
    legend.box = 'vertical',
    legend.background = element_rect(fill = 'floralwhite', color = 'black'),
    legend.margin = margin(18,5,18,5),
    
    strip.text = element_text(face = 'bold', size = 15), 
    strip.background = element_rect(color = 'black', fill = 'floralwhite', size = 1.3)
    
  ) +
  guides(
    fill = guide_colorbar(
      barwidth = 1.5, 
      barheight= 15, 
      title = "Point diff. in stint", ## LEGEND TITLE
      title.position = "right", 
      title.hjust = 0.5,
      label.theme = element_text(size = 14), 
      hjust = 0.3
    )
  )


ggsave(paste0('/Users/davidetissino/Desktop/rotation', game_id, '.png'), dpi = 'retina', height = 6, width = 10)



