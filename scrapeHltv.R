library(tidyverse)
library(lubridate)
library(stringi)
library(janitor)
library(rvest)

# Function for scraping all necessary information from a single game page on HLTV

scrapeHltv <- function(url) {
  
  # Reads the html webpage for a single match between two teams
  # html is a html object 
  html <- read_html(url)
  
  # Parses the URL for the unique ID representing the match
  matchId <- strsplit(strsplit(url,'matches/')[[1]][2],'/')[[1]][1]
  
  # Parses the date (%Y-%m-%d) for the match 
  date <- html %>% 
    html_element('.date') %>%
    html_attr('data-unix') %>%
    as.numeric() %>%
    (function(x) x/1000) %>%
    as_datetime() %>%
    as.Date(format='%m/%d/%Y') %>%
    as.character()
  
  # Parse the team names
  teamNames <- html %>%
    html_elements('div[class=team]') %>%
    html_text2()
  teamNames <- substr(teamNames,1,nchar(teamNames)-2) %>%
    str_trim()
  
  # Parse the ID representing each map played in the match
  mapIds <- html %>%
    html_elements('.dynamic-map-name-short') %>%
    html_attr('id') %>%
    list() %>%
    lapply(function(x) x[x!='all']) %>%
    lapply(function(x) x[!is.na(x)]) %>%
    unlist()
  
  # Retrieve the html code containing the info for each map
  mapInfo <- html %>%
    html_elements("div[class*=played]")
  
  # Parse the names for each map and then discard that code
  mapNames <- html_text2(mapInfo)[c(T,F)]
  mapNames <- mapNames[mapNames != '']
  mapInfo <- mapInfo[c(F,T)]
  
  # Discards unnecessary information if the first map was defaulted for whatever reason
  if (mapNames[1] == 'Default') {
    mapNames <- mapNames[-1]
    mapInfo <- mapInfo[2:length(mapInfo)]
  }
  
  # Retrieve the html code containing the stat tables for each map
  mapTables <- html %>%
    html_elements('div[id*=-content]')
  
  # Ignore first element, which has the overall data for all maps in the match
  mapTables <- mapTables[-1]
  
  # For loop to obtain information for each map such as teams and scores
  # Stores information in a dataframe
  gameInfo <- NULL
  
  for (i in 1:length(mapNames))  {
    mn <- mapNames[i]
    mpid <- mapIds[i]
    info <- mapInfo[i]
    
    # Get the name of the teams playing
    # Get the total score for the map
    totalScores <- info %>%
      html_elements('div[class=results-team-score]') %>%
      html_text() %>%
      as.integer()
    
    # Determine whether the first side played is CT or T
    firstSide <- (info %>%
                    html_elements('div[class=results-center-half-score]') %>%
                    as.character() %>%
                    strsplit('span class=\"'))[[1]][2] %>%
      substr(1,2)
    
    # Get vector of CT scores
    ctScores <- info %>%
      html_elements('span[class=ct]') %>%    
      html_text() %>%
      as.numeric()
    
    # Get vector of T scores
    tScores <- info %>%
      html_elements('span[class=t]') %>%    
      html_text() %>%
      as.numeric()
    
    # Get scores for each team based on the first side
    # First score being the CT score, second score being the T score
    if (firstSide == 'ct') {
      teamOneScores <- c(ctScores[1],tScores[2])
      teamTwoScores <- c(ctScores[2],tScores[1])
    } else {
      teamOneScores <- c(ctScores[2],tScores[1])
      teamTwoScores <- c(ctScores[1],tScores[2])
    }
    
    # Converts information into dataframes to be stored
    gameInfo <- rbind(gameInfo,data.frame(mapName=mn,matchId=matchId,mapId=mpid,date=date,
                                          tTeam=teamNames[1],ctTeam=teamNames[2],
                                          tScore=teamOneScores[2],ctScore=teamTwoScores[1]))  
    gameInfo <- rbind(gameInfo,data.frame(mapName=mn,matchId=matchId,mapId=mpid,date=date,
                                          tTeam=teamNames[2],ctTeam=teamNames[1],
                                          tScore=teamTwoScores[2],ctScore=teamOneScores[1]))
  }
  
  # Now that we have a dataframe for map information, we need player info
  # In this loop, we'll build a dataframe with player info for each map/side
  mapDfs <- NULL
  for (i in 1:length(mapTables)) {
    
    # Get stat tables for a map
    dfs <- mapTables[i] %>%
      html_elements('table[class$=hidden]') %>%
      html_table(header=T)
    
    # Add necessary and remove unnecessary info from each map's stat table
    df_list <- list()
    ix <- 0
    for (d in dfs) {
      ix <- ix+1
      
      team <- names(d)[1]
      d$player <- word(pull(d[team]),-1)
      d$mapId <- mapIds[i]
      d$mapName <- mapNames[i]
      d$matchId <- matchId
      d$team <- team
      
      d <- d[c('player','matchId','mapName','team','mapId','Rating2.0')]
      names(d) <- c('player','matchId','mapName','team','mapId','rating')
      
      df_list[[ix]] <- data.frame(d)
    }
    
    # Create ctf (data frame with CT information)
    # Create tef (data frame with T information)
    ctf = rbind(df_list[[1]],df_list[[3]])
    tef = rbind(df_list[[2]],df_list[[4]])
    
    # Replace rating with reciprocal of rating
    tef$rating <- ifelse(tef$rating == 0, 0, 1/tef$rating)
    ctf$rating <- ctf$rating*-1
    
    # Join data frames into a single one with one observation for each player
    # Includes each player's rating on both T and CT side
    tf <- inner_join(ctf,tef,by='player')
    tf <- tf[c('player','matchId.x','mapId.x','mapName.x','team.x','rating.y','rating.x')]
    names(tf) <- c('player','matchId','mapId','mapName',
                   'team','tRating','ctRating')
    tf <- tf %>% arrange(team,player)
    
    # Gets rows from gameInfo corresponding to the map being looped
    db <- gameInfo %>% filter(mapId == mapIds[i])
    
    # Fills dataframe of map & player information combined
    mf <- NULL
    for (n in 1:2) {
      # Determines the T and CT teams
      tTeam <- db$tTeam[n]
      ctTeam <- db$ctTeam[n]
      
      # Uses the above teams to make a T and CT dataframe
      tdf <- tf %>% filter(team == tTeam)
      ctdf <- tf %>% filter(team == ctTeam)
      
      # Creates dataframe with players and ratings on this side
      # CT ratings multiplied by -1
      tb <- data.frame(player=c(tdf$player,ctdf$player),
                       rtg=c(tdf$tRating,ctdf$ctRating))
      
      # Remove accents from player names
      tb$player <-  stri_trans_general(tb$player,"Latin-ASCII")
      
      # Transposes dataframe
      tb <- data.frame(t(as.matrix(tb))) %>%
        row_to_names(1) %>%
        mutate_all(as.numeric)
      rownames(tb) <- NULL
      
      # Adds columns for match/map information
      for (x in c('matchId','mapId','mapName','date',
                  'tTeam','ctTeam','tScore','ctScore')) {
        tb[x] <- db[,x][n]
      }
      
      # Changes order of columns
      tb <- tb %>%
        dplyr::select(matchId,mapId,mapName,date,tTeam,
                      ctTeam,tScore,ctScore,everything())
      
      mf <- rbind(mf,tb)
    }
    mapDfs <- bind_rows(mapDfs,mf)
  }
  mapDfs
}
