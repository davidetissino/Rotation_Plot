#### PBP DATA - MARGIN DIFF####

library(nbastatR)
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
library(ggpubr)


# csv with team names, slugs, colors 
tms <- read_csv('/Users/davidetissino/Desktop/R/data/teams.csv')

# increase buffer size for scraping
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)





# Load libraries
library(dplyr)
library(readr)  # or you can use read.csv which comes with base R

# Step 1: Set the working directory to the folder where your CSVs are stored
setwd("/Users/davidetissino/Desktop/samos/game_rotations/2023-24_po")  # Replace with your actual folder path

# Step 2: Get a list of all CSV files in the folder
file_list <- list.files(pattern = "*.csv")  # Adjust if files have different extensions

# Step 3: Read all CSVs into a list of data frames
df_list <- lapply(file_list, read_csv)  # or use read.csv for base R

# Step 4: Combine all data frames into a single dataframe
# If all CSVs have the same structure:
combined_df <- bind_rows(df_list)

combined_df <- combined_df[, -1]






## 2023-24 RS Team Logs ####
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
# url <- 'https://stats.nba.com/stats/leaguegamelog?Counter=1000&DateFrom=&DateTo=&Direction=DESC&ISTRound=&LeagueID=00&PlayerOrTeam=T&Season=2023-24&SeasonType=Regular%20Season&Sorter=DATE'
# 
# res <- GET(url = url, add_headers(.headers = headers))
# 
# json_resp <- fromJSON(content(res, "text"))
# 
# rs_logs_24 <- data.frame(json_resp$resultSets$rowSet)
# 
# colnames(rs_logs_24) <- json_resp[["resultSets"]][["headers"]][[1]]  


##### Load CSV ====
rs_logs_24 <- read.csv('/Users/davidetissino/Desktop/R/RotationPlot/Data/TeamsLogs24.csv')

# UNIQUE game IDs ====
unique_IDs_24 <- unique(rs_logs_24$GAME_ID)



## PBP Margin Data nbastatR ====

pbp <- play_by_play(game_ids = '0022300001')


pbp <- pbp[, - c(1, 4:12, 14:16)]

# new column with point differential (margin)
pbp$margin <- pbp$scoreAway - pbp$scoreHome


game_start <- pbp[-c(2:nrow(pbp)), ]


# remove all columns containing NAs
pbp <- pbp %>%
  drop_na()


pbp <- rbind(game_start, pbp)

pbp$margin[pbp$minuteGame == 0.0000000] <- 0



# create column with seconds
pbp$seconds <- pbp$minuteGame * 60



##### Time Formatting 2 ====
convert_seconds_to_time_string <- function(seconds) {
  
  # Extract minutes and seconds
  minutes <- floor(seconds / 60)
  seconds <- seconds %% 60
  
  # Format the time string
  time_string <- sprintf("%02d:%02d", as.integer(minutes), as.integer(seconds))
  
  return(time_string)
}

# MM:SS time format to new column
pbp$MIN <- sapply(pbp$seconds, convert_seconds_to_time_string)

# unix time format from epoch date
pbp$unix_time <- as.integer(as.POSIXct(paste(date, pbp$MIN), format = "%Y-%m-%d %M:%S"))


##### Evaluations to Plot ====
# variable for margin = 0
t <- 0  

# take only relevant columns
fin <- pbp %>% 
  select(unix_time, margin) 

# identify points completely > or < 0 
fin <- fin %>% 
  arrange(unix_time) %>%
  mutate(
    above_t = margin >= t
  ) %>% 
  mutate(
    changed = is.na(lag(above_t)) | lag(above_t) != above_t
  ) %>% 
  mutate(
    section_id = cumsum(changed)
  ) %>% 
  select(- above_t, - changed)

# calculate the x-coordinate of the intersection point with 0 
# (the y-coordinate would be t), & add this to the data frame
fin <- rbind(
  fin, 
  fin %>% 
    group_by(section_id) %>% 
    filter(unix_time %in% c(min(unix_time), max(unix_time))) %>% 
    ungroup() %>% 
    mutate(
      mid_unix = ifelse(
        section_id == 1 | section_id == lag(section_id), 
        NA, 
        unix_time - (unix_time - lag(unix_time)) / 
          (margin - lag(margin)) * (margin - t))) %>% 
    select(
      mid_unix, margin, section_id
    ) %>% 
    rename(
      unix_time = mid_unix
    ) %>%
    mutate(margin = t) %>% 
    na.omit())


end_margin <- max(abs(fin$margin))



# GAME MARGIN PLOT ####
margin <- fin %>%
  ggplot(aes(x = unix_time, y = margin)) +
  # home & away slugs in differential 
  geom_text(
    x = 946681270, 
    y = -end_margin + 3, 
    aes(
      label = home_slug, 
      size = 8, 
      fontface = 'bold'
    )
  ) + 
  geom_text(
    x = 946681270, 
    y = end_margin - 3, 
    aes(
      label = away_slug, 
      size = 8, 
      fontface = 'bold'
    )
  ) + 
  # geom_image(x = 946681300, y = -18, aes(image = teams$logo[teams$loc == 'Home']), size = 0.25) +
  # geom_image(x = 946681300, y = 18, aes(image = teams$logo[teams$loc == 'Away']), size = 0.25) +
  # ribbons to fill areas between 0 and margin
  geom_ribbon(
    data = . %>% filter(margin >= 0),
    aes(ymin = 0, ymax = margin),
    fill = teams$secondary[teams$loc == 'Away'],
    color = teams$primary[teams$loc == 'Away'], 
    size = 1.5
  ) +
  geom_ribbon(
    data = . %>% filter(margin <= 0),
    aes(ymin = margin, ymax = 0),
    fill = teams$secondary[teams$loc == 'Home'], 
    color = teams$primary[teams$loc == 'Home'], 
    size = 1.5
  ) +
  #horizontal line on 0
  geom_hline(
    yintercept = 0,
    color = 'grey50',
    size = 1.5
  ) +
  geom_vline(
    xintercept = c(946681920, 946682640, 946683360), 
    color = 'grey80',
    size = .5
  ) +
  ##### Y differentials ====
scale_y_continuous(
  breaks = c(-10, 0, 10),
  labels = c("-10","0", "+10"), 
) +
  scale_x_continuous(
    breaks = c(946681200, 946681920, 946682640, 946683360, 946684080),
    labels = c("0", "12", "24", "36", "48"),
    guide = 'prism_minor',
    limits = c(946681200, 946684080),
    minor_breaks = seq(946681200, 946684080, 60)
  ) +
  coord_cartesian(
    xlim = c(946681328, 946683952),
    ylim = c(-end_margin, end_margin)
  ) +
  theme(
    panel.border = element_rect(colour = 'black', fill = NA, size = 1), 
    panel.background = element_rect('white')
  ) + 
  labs(
    x = 'game minute',
    y = paste0('point differential \n', '(', away_slug, ' - ', home_slug, ')'), 
    title = '', 
    caption = '@dvdtssn | stats.nba.com', 
  ) + 
  theme(
    text = element_text(family='PT Mono', color = 'black'), 
    axis.title.x = element_text(face='bold', size=15, margin=margin(t=10)),
    axis.title.y = element_text(margin = margin(r = 5), angle = 360, vjust = 0.5, size = 18, face = 'bold'),
    axis.text.y = element_text(margin = margin(r = 5, unit = "pt"), face = 'bold', hjust = 0.95, color = 'black', size = 13), 
    axis.text.x = element_text(face='bold', color = 'black', size = 15),
    axis.ticks.x = element_line(color = 'black'),
    
    
    ##### Plot MARGINS ====
    ## for each series 
    plot.margin = margin(0, 130, 30, 110, "pt"), # OKC - DAL
    # plot.margin = margin(0, 130, 30, 66, "pt"), # NYK - IND
    # plot.margin = margin(0, 130, 30, 42, "pt"), # CLE - BOS 
    # plot.margin = margin(0, 130, 30, 102, "pt"), # DEN - MIN 
    
    
    plot.background = element_rect('white'),
    plot.caption = element_text(color = 'black', size = 15),
    plot.title = element_text(face='bold', size=17, hjust = 0.5),
    plot.subtitle=element_text(size=13, hjust = 0.5), 
    
    panel.grid = element_line(color = "#afa9a9"),
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect('white'), 
    panel.border = element_rect(colour = 'black', fill = NA, size = 1), 
    
    legend.position = 'none'
  )

margin

ggsave(paste0('/Users/davidetissino/Desktop/margin', game_id, '.png'), dpi = 'retina', height = 6, width = 10)



## COMBINED PLOT ####

ggarrange(
  rotation, 
  margin, 
  ncol = 1, 
  nrow = 2,
  align = 'hv', 
  heights = c(3, 1)
)


#
ggsave(paste0('/Users/davidetissino/Desktop/combination', away_slug, '@', home_slug, game_id, '.png'), dpi = 'retina', height = 11, width = 17)






