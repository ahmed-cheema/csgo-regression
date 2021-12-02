# The objective of this script is to use the function
# scrapeHltv, which retrieves the information for a single game,
# to obtain the information for every eligible game on hltv.org
# through a loop
library(svMisc)

# Load libraries and function for scraping an HLTV page
# Assumes scrapeHltv.R is inside current working directory
source('scrapeHltv.R')

# Add condition to pass NA if scrapeHltv returns an error
scrapeHltv <- possibly(scrapeHltv,
                       otherwise=NA)

matchesUrl <- 'https://www.hltv.org/results?content=stats&stars=1&gameType=CSGO&offset='
prefix <- 'https://www.hltv.org'

# Each page on hltv.org features 100 matches - thus, we'll use the offset in
# matches_url to loop through all of the pages and retrieve all of the matches
# and add them to a growing dataframe
df_list <- vector(mode='list')
offsets <- seq(0,11101,100)

for (x in 1:length(offsets)) {
  # Progress bar to track for loop as it may take some time
  progress(x,118)
  
  offset <- offsets[x]
  
  # Read the html webpage for the match results
  html <- read_html(paste0(matchesUrl,offset))
  
  # Parse the link to each individual match shown on the page
  links <- html %>%
    html_elements('[class=a-reset]') %>%
    html_attr('href')
  links <- paste0(prefix,grep('matches',links,value=T))
  
  # Now that we have the link to each individual match on this page,
  # we can feed this into the scrapeHltv function one-by-one
  page_list <- vector(mode='list')
  
  for (i in 1:length(links)) {
    page_list[[i]] <- scrapeHltv(links[i])
  }
  
  # Binds the dataframe for each game on the page together (ignoring nulls)
  df_list[[x]] <- bind_rows(Filter(function(a) any(!is.na(a)), page_list))
  
  # End of progress bar
  if (x == 118) message("Done!")
}

df <- bind_rows(df_list)

# Replace NA with 0
df[is.na(df)] <- 0

# Create dummy variables for mapName
df <- df %>%
  mutate(Inferno=ifelse(mapName == "Inferno",1,0),
         Ancient=ifelse(mapName == "Ancient",1,0),
         Dust2=ifelse(mapName == "Dust2",1,0),
         Mirage=ifelse(mapName == "Mirage",1,0),
         Nuke=ifelse(mapName == "Nuke",1,0),
         Overpass=ifelse(mapName == "Overpass",1,0),
         Vertigo=ifelse(mapName == "Vertigo",1,0),
         Cache=ifelse(mapName == "Cache",1,0),
         Train=ifelse(mapName == "Train",1,0),
         Tuscan=ifelse(mapName == "Tuscan",1,0),
         Cobblestone=ifelse(mapName == "Cobblestone",1,0))

# Remove duplicate columns and erroneous values
df <- df %>%
  distinct() %>%
  filter(mapId != '0')

########

# Read html for page with player ratings 
url <- 'https://www.hltv.org/stats/players?startDate=2016-02-19&rankingFilter=Top20&endDate=2021-12-01&minMapCount=0'
html <- read_html(url)

# Extract table from html object and do basic cleaning
rtg <- html %>% 
  html_table() %>%
  first() %>%
  select(Player,`Rating2.0`) %>%
  rename(player=Player,
         rating=`Rating2.0`)
