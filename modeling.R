library(tidyverse)
library(glmnet)
library(broom)

# Read in data
df <- read_csv('data/csgoGameData.csv')
rtg <- read_csv('data/playerRatings.csv')

# Create x and y matrices for model fitting
x <- data.matrix(df[names(df[9:ncol(df)])])
rounds <- df$tScore+df$ctScore
y <- df$tScore/rounds

# Get list of map names
maps <- names(df)[(ncol(df)-10):ncol(df)]

# Cross validation to find optimal lambda 
ridge_cv <- cv.glmnet(x, y, weights=rounds, alpha=0, standardize=F)

# Run model with optimal lambda
ridge_reg <- glmnet(x, y, weights=rounds, alpha=0, lambda=ridge_cv$lambda.1se, standardize=F)

# Count number of occurrences for each term in model
freq_list <- numeric(ncol(x)-11)
for (i in 1:(ncol(x)-11)) {
  freq_list[i] <- sum(x[,i] > 0) 
}

# Clean results
fin <- tidy(ridge_reg) %>% 
  filter(term != '(Intercept)',
         !term %in% maps) %>%
  mutate(mapsPlayed = freq_list) %>%
  arrange(desc(estimate)) %>%
  select(term,mapsPlayed,estimate) %>%
  left_join(rtg,by=c('term'='player')) %>%
  rename(player=term,impact=estimate)
