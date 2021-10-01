fin <- read.csv(here::here("Data prep","P3-Future-500-The-Dataset.csv"), stringsAsFactors = TRUE, na.strings = c(""))


# Quick EDA ----
head(fin, 10)
tail(fin)
str(fin)
summary(fin)


# Data Wrangling

# Change from non-factor to factor ----

fin$ID <- factor(fin$ID)
fin$Inception <- factor(fin$Inception)

# Factor Variable Trap (FVT) ----
#Converting into numerics for characters
a <- c("12","13", "14", "12", "12")
typeof(a)

b <- as.numeric(a)
b
typeof(b)

# Converting into numerics for Factors ----
z <- factor(a)
z
typeof(z)
y <- as.numeric(z)
y
typeof(y)

#--------- Correct method:

x <- as.numeric(as.character(z))
x
typeof(x)

# FVT Example -------------------------

#fin$Profit <- factor(fin$Profit)

#head(fin)
#str(fin)

#summary(fin)

# Don't do this:
#fin$Profit <- as.numeric(fin$Profit)


# gsub() function --------

fin$Expenses <- gsub(" Dollars", "", fin$Expenses, perl = TRUE)
fin$Expenses <- gsub(",", "", fin$Expenses, perl = TRUE)
fin$Revenue <- gsub("[\\$,]", "", fin$Revenue, perl = TRUE)
fin$Growth <- gsub("%", "", fin$Growth, perl = TRUE)

library(tidyverse)

fin <- fin %>% 
  mutate_if(is.character, as.numeric)

fin$Name <- as.character(fin$Name)

glimpse(fin)

# Deal with missing data ------
# 1. Predict with 100% accuracy
# 2. Leave record as is
# 3. Remove record entirely
# 4. Replace with mean or median
# 5. Fill in by exploring correlations and similarities
# 6. Introduce dummy variable for "Missingness"


# Locate missing data ----
# Updated import to: read.csv(here::here("Data prep","P3-Future-500-The-Dataset.csv"), stringsAsFactors = TRUE, na.strings = c(""))
fin[!complete.cases(fin),]

# Filtering: using which() for non-missing data -----

head(fin, 24)
fin[fin$Revenue == 9746272,]
fin[which(fin$Revenue == 9746272),]

fin[fin$Employees == 45,]
fin[which(fin$Employees == 45),]

# Filtering: using is.na() for missing data ----

fin[is.na(fin$Expenses),]

# Removing records with missing data ----
# create backup
fin_backup <-  fin

# remove rows 
fin <- fin[!is.na(fin$Industry),]

# Reset the index of the dataframe ----

rownames(fin) <- NULL

# Replacing missing data with factual analysis method ----

fin[is.na(fin$State) & fin$City == 'New York', "State"] <- "NY"
fin[is.na(fin$State) & fin$City == 'San Francisco', "State"] <- "CA"
# Check:
fin[c(11,377,82,265),]


# Replacing missing data with median imputation method ----

median(fin$Employees, na.rm = TRUE)

fin[is.na(fin$Employees),]

med_empl_retail <- median(fin[fin$Industry == "Retail", "Employees"], na.rm = TRUE)
fin[is.na(fin$Employees) & fin$Industry == 'Retail', "Employees"] <- med_empl_retail

med_empl_fin_serv <- median(fin[fin$Industry == "Financial Services", "Employees"], na.rm = TRUE)
fin[is.na(fin$Employees) & fin$Industry == 'Financial Services', "Employees"] <- med_empl_fin_serv
# check:
fin[c(3,330),]

fin[!complete.cases(fin),]

med_growth_constr <-  median(fin[fin$Industry == 'Construction', "Growth"], na.rm = TRUE)
fin[is.na(fin$Growth) & fin$Industry == 'Construction', "Growth"] <- med_growth_constr
# check:
fin[c(8),]

med_rev_constr <-  median(fin[fin$Industry == 'Construction', "Revenue"], na.rm = TRUE)
fin[is.na(fin$Revenue) & fin$Industry == 'Construction', "Revenue"] <- med_rev_constr


med_exp_constr <-  median(fin[fin$Industry == 'Construction', "Expenses"], na.rm = TRUE)
fin[is.na(fin$Expenses) & fin$Industry == 'Construction' & is.na(fin$Profit), "Expenses"] <- med_exp_constr

# Replacing missing data: deriving values ----

fin[is.na(fin$Profit), "Profit"] <- fin[is.na(fin$Profit), "Revenue"] - fin[is.na(fin$Profit), "Expenses"]
#check:
fin[c(8,42),]

fin[is.na(fin$Expenses), "Expenses"] <- fin[is.na(fin$Expenses), "Revenue"] - fin[is.na(fin$Expenses), "Profit"]

# plot the data ----
# Scatterplot classified by industry showing revenue, expenses, profit
fin %>% 
  ggplot() + 
  geom_point(aes(Revenue, Expenses, colour = Industry, size = Profit)) +
  theme_light()

# Scatterplot that includes industry trends for the expenses
fin %>% 
  ggplot(aes(Revenue, Expenses, colour = Industry)) + 
  geom_point() +
  geom_smooth(fill = NA,  size = 1.2) + 
  theme_light()

# Boxplot of industry growth
fin %>% 
  ggplot(aes(Industry, Growth, colour = Industry)) + 
  geom_jitter() + 
  geom_boxplot(size = 1, alpha=0.5, outlier.colour = NA) +
  theme_light()  
  