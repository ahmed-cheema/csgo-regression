library(tidyverse)

# Read in data
df <- read_csv('data/csgoGameData.csv')
fin <- read_csv('data/results.csv')
cv <- read_csv('data/crossValidation.csv')

######

# Bar plot of map win percentages with confidence intervals

tf <- df %>%
  group_by(mapName) %>%
  summarize(tWinPct=sum(tScore)/sum(tScore+ctScore),
            tWins=sum(tScore),
            wins=sum(tScore+ctScore)) %>%
  arrange(desc(tWinPct)) %>%
  mutate(ymin=tWinPct-(1.96*sqrt((tWinPct*(1-tWinPct))/(wins))),
         ymax=tWinPct+(1.96*sqrt((tWinPct*(1-tWinPct))/(wins))))

ggplot(tf,aes(reorder(mapName,-tWinPct),tWinPct,fill=tWinPct)) +
  geom_bar(stat='identity') +
  geom_errorbar(aes(ymin=ymin,ymax=ymax),width=0.4, colour="orange", alpha=0.9, size=1.3) +
  geom_hline(yintercept=0.5,size=1) +
  theme_bw() +
  labs(x='Map Name',y='Proportion of Rounds Won by T Team',
       title='Average T Team Win Percentage vs. Map',
       subtitle='(02/19/2016-12/01/2021)') +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5),
        legend.position = 'none') +
  scale_y_continuous(expand=c(0,0), limits = c(0, 0.6))

######

# Scatter plot of player rating versus coefficient

ggplot(filter(fin,mapsPlayed > 300),
       aes(x=impact,y=rating,size=mapsPlayed)) +
  geom_point(color='black',fill='#FA8D8D',pch=21) +
  theme_bw() +
  labs(x='Coefficient Estimate',y='HLTV Rating',
       title='Player HLTV Rating vs. Coefficient Estimate',
       subtitle='Minimum 300 Maps Played (02/19/2016-12/01/2021)',
       size='Games Played') +
  theme(plot.title = element_text(hjust=0.5),
        plot.subtitle = element_text(hjust=0.5))

######

# Get win percentages for players

player <- names(df)[9:(ncol(df)-11)]
win.pct <- numeric(length(player))
for (i in 1:length(player)) {
  tef <- filter(df,get(player[i])>0)
  tRounds <- sum(tef$tScore)+sum(tef$ctScore)
  tWins <- sum(tef$tScore)
  
  ctf <- filter(df,get(player[i])<0)
  ctRounds <- sum(ctf$tScore)+sum(ctf$ctScore)
  ctWins <- sum(ctf$ctScore)
  
  win.pct[i] <- (ctWins+tWins)/(ctRounds+tRounds)
}

db <- tibble(player,win.pct)
db <- db %>%
  inner_join(fin,by='player') %>%
  drop_na()

# Weighted correlation matrix

cov.wt(select(db,win.pct,impact,rating),
       wt=db$mapsPlayed,cor=T)$cor

######

# Creates cross validation plot

cv <- tibble(log(ridge_cv$lambda),ridge_cv$cvm,log(ridge_cv$lambda.min),log(ridge_cv$lambda.1se))
names(cv) <- c('logLambda','mse','lambdaMin','lambdaOSE')

ggplot(cv,aes(logLambda,mse)) +
  geom_point() +
  geom_vline(aes(xintercept=cv$lambdaOSE[1],color='One Standard\nError Above\nMinimum'),show.legend=T) +
  geom_vline(aes(xintercept=cv$lambdaMin[1],color='Minimum'),show.legend=T) +
  theme_bw() +
  labs(x=paste0('log(lambda)'),y='Mean Squared Error',
       title='Cross validation for ridge regression parameter',
       color='  Selection\n  Method') +
  theme(plot.title=element_text(hjust=0.5))