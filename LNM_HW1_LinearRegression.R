
# Linear & Non-Linear Models Assignment 1 ---------------------------------

setwd()
# 1 -----------------------------------------------------------------------
# Scooter Dataset

# Read Data
div <- read.table('divvy.txt', header=TRUE)

# Linear Model
mod <- lm(Distance~Speed, div)

# Scatter Plot
plot(div$Speed, div$Distance)

# Line of Best Fit
abline(mod)

# Summary of Model
summary(mod) # R-squared = 95.3%


# 2 -----------------------------------------------------------------------
# Soccer Dataset

# Read Data
soc <- read.table('soccer.txt', header=TRUE)

# Linear Model
soc_mod <- lm(Score~Sprint, soc)

# Scatter Plot
plot(soc$Sprint, soc$Score)

# Line of best Fit
abline(soc_mod)

# Summary of Model
summary(soc_mod) # R-squared = 37.6%

# Filter out where Kante's sprint = 210
soc_210 <- soc %>% filter(Sprint != 210)

# Plot 2 on filtered dataset
plot(soc_210$Sprint, soc_210$Score)
soc_mod_210 <- lm(Score~Sprint, soc_210)
abline(soc_mod_210)

# Summary of Model - 210 Filtered Out
summary(soc_mod_210) # R-squared = 19.1%


# 3 -----------------------------------------------------------------------

death <- read.table('death.txt', header=TRUE)

d1 <- lm(deaths~budget, death)
plot(death$budget, death$deaths)
abline(d)


d2 <- lm(budget~deaths, death)
plot(death$deaths, death$budget)
abline(d2)


d3 <- lm(budget~year, death)
plot(death$year, death$budget)
abline(d3)


d4 <- lm(deaths~year, death)
plot(death$year, death$deaths)
abline(d4)
