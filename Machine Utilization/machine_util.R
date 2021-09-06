util<- read_csv(here::here("Machine Utilization","P3-Machine-Utilization.csv"))

head(util, 10)
tail(util)
str(util)
summary(util)
util$Machine <- factor(util$Machine)


util$Utilization <- 1 - util$`Percent Idle`

# Handling Date-Times in R----

util$PosixTime <- as.POSIXct(util$Timestamp, format = "%d/%m/%Y %H:%M")
util$Timestamp <-  NULL
util <- util[,c(4,1,2,3)]
head(util,12)

summary(util)


rl1 <- util[util$Machine == "RL1",]
summary(rl1)
rl1$Machine = factor(rl1$Machine)
summary(rl1)

# Construct list: ----
# Character: Machine name
# Vector: (min, mean, max) Utilization for the month (excluding unknown hours)
# Logical: Has utilization ever fallen below 90% ? 

util_stats_rl1 <- c(min(rl1$Utilization, na.rm = TRUE), mean(rl1$Utilization, na.rm = TRUE),max(rl1$Utilization, na.rm = TRUE))
util_under_90_flag <- length(which(rl1$Utilization < .90)) > 0

list_rl1 <- list("RL1", util_stats_rl1, util_under_90_flag)

# Naming components of a list ----

names(list_rl1) <- c("Machine", "Stats", "LowThreshold")

list_rl1

# Another way:

rm(list_rl1)
list_rl1 <- list(Machine = "RL1", Stats = util_stats_rl1, LowThreshold = util_under_90_flag)

list_rl1

# Extracting components of a list ----
# [] returns a list
#[[]] returns the object
# $ same as [[]] but tidier

list_rl1[1]
list_rl1[[1]]
list_rl1$Machine

typeof(list_rl1[2])
typeof(list_rl1[[2]])
typeof(list_rl1$Stats)

# Access elements of a vector in a list
list_rl1[[2]][3]
list_rl1$Stats[3]

# Adding and deleting list components
list_rl1[4] <- "New Information"
list_rl1

#Another way = via $
# Vector: All hours where untilization is unknown

list_rl1$UnknownHours <- rl1[is.na(rl1$Utilization), "PosixTime"]
list_rl1

# Remove component ----
list_rl1[4] <- NULL
list_rl1

# Add another component:
# Dataframe: for this machine
list_rl1$Data <- rl1
list_rl1

summary(list_rl1)


# Subsetting a list ----
list_rl1[[4]][1,1]
list_rl1$UnknownHours[1,1]

list_rl1[1:2]
list_rl1[c("Machine", "UnknownHours")]

# Creating a plot ----
library(ggplot2)

plot <- util %>% 
  ggplot() +
  geom_line(aes(PosixTime, Utilization, colour = Machine), size = 1.2) + 
  facet_grid(Machine~.) +
   geom_hline(yintercept = 0.9, color = "Grey", size =1.2, linetype = 3)

list_rl1$Plot <- plot

list_rl1
summary(list_rl1)
