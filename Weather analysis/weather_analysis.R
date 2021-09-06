
Chicago <- read.csv(here::here('Weather analysis','Weather Data', 'Chicago-F.csv'), row.names = 1)
Houston <- read.csv(here::here('Weather analysis','Weather Data', 'Houston-F.csv'), row.names = 1)
NewYork <- read.csv(here::here('Weather analysis','Weather Data', 'NewYork-F.csv'), row.names = 1)
SanFrancisco <- read.csv(here::here('Weather analysis','Weather Data', 'SanFrancisco-F.csv'), row.names = 1)
is.data.frame(Chicago)

Chicago <- as.matrix(Chicago)
Houston <- as.matrix(Houston)
NewYork <- as.matrix(NewYork)
SanFrancisco <- as.matrix(SanFrancisco)

is.matrix(Chicago)

weather <- list(Chicago = Chicago, Houston = Houston, NewYork = NewYork, SanFrancisco = SanFrancisco)
weather

weather[3]
weather$SanFrancisco
summary(weather)


# Using apply() ----
apply(Chicago, 1, mean)
mean(Chicago["DaysWithPrecip", ])


apply(Chicago, 1, max)
apply(Chicago, 1, min)

# Compare ----- 
apply(Chicago, 1, mean)
apply(Houston, 1, mean)
apply(NewYork, 1, mean)
apply(SanFrancisco, 1, mean)

# Using lapply() ----
# transpose the weather matrix
weather_t <- lapply(weather, t) 

#example 2
rbind(Chicago, NewRow = 1:12)

lapply(weather, rbind, NewRow = 1:12)

#example 3
rowMeans(Chicago) #identical to: apply(Chicago, 1, mean)

weather_means <- lapply(weather, rowMeans)

# Combining lapply with the [] operator ----
weather[[1]][1,1]
weather$Chicago[1,1]

lapply(weather, "[",1,1)

lapply(weather, "[",1,)

lapply(weather, "[", ,3)

# adding functions ----

lapply(weather, rowMeans)
lapply(weather, function(x) x[1,])
lapply(weather, function(x) x[5,])
lapply(weather, function(x) x[,12])
lapply(weather, function(x) round((x[1,] - x[2,]) / x[2,],2))


# sapply() ----

#AvgHigh_F for July:

lapply(weather, "[", 1, 7)
sapply(weather, "[", 1, 7)

#AvgHigh_F for 4th Qtr:
lapply(weather, "[", 1, 10:12)
sapply(weather, "[", 1, 10:12)

#Another exxample
lapply(weather, rowMeans)
round(sapply(weather, rowMeans),2) #<< deliverable 1
sapply(weather, function(x) round((x[1,] - x[2,]) / x[2,],2)) # << deliv 2

# Nesting Apply functions ----
apply(Chicago, 1, max)
# apply across whole list:
sapply(weather, apply, 1, max) # preferred approach
sapply(weather, function(x) apply(x, 1, max)) # << deliv 3
sapply(weather, function(x) apply(x, 1, min)) # << deliv 4

# which.max() ----

which.max(Chicago[1,])
names(which.max(Chicago[1,]))
apply(Chicago,1, function(x) names(which.max(x)))
sapply(weather, function(y) apply(y,1, function(x) names(which.max(x))))
sapply(weather, function(y) apply(y,1, function(x) names(which.min(x))))
