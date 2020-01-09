# FIFA 2019 Data Analysis

library(tidyverse)
library(dplyr)
library(data.table)

fifa <- fread(paste0("FIFA_2019.csv"), header = T, stringsAsFactors = F, data.table = T)

colnames(fifa)
clubteam_variables <- c(2,3,4,6,8,9,10,12,13,14,16,22)
club_team <- fifa %>% select(clubteam_variables)
colnames(club_team)
# [1] "ID"                       "Name"                     "Age"                     
# [4] "Nationality"              "Overall"                  "Potential"               
# [7] "Club"                     "Value"                    "Wage"                    
# [10] "Special"                  "International Reputation" "Position" 
nrow(club_team) #18,207 observations - players

# Converting Wages from Characters into Numeric, in Thousands
replace_string <- list('€','K')
find.string <- paste(unlist(replace_string), collapse = "|")
club_team$Wage <- gsub(find.string, replacement = "", club_team$Wage)
club_team$Wage <- as.numeric(as.character(club_team$Wage))

# Converting Value from Characters into Numeric, in Millions
replace_string2 <- list('€','M')
find.string2 <- paste(unlist(replace_string2), collapse = "|")
club_team$Value <- gsub(find.string2, replacement = "", club_team$Value)
club_team$Value <- as.numeric(as.character(club_team$Value))

# Question 1 - Which club team possesses the best overall score and lowest wages?
q1 <- club_team %>% 
  group_by('Age','Nationality','Overall','Club','Wage') %>% 
  filter(Overall > 75 & Wage < 100 & Age < 27)
# Spain - 204, Brazil - 161, France - 130, Argentina - 118, Germany - 109
table(q1$Nationality) # Spain has most amount of players > 75 Overall
art <- club_team[Overall > 80, mean(Wage) < 10, Club] #Shakhtar Donetsk and Lokomotiv Moscow 

# Question 2 - Does there exist a correlation between a player's wages and nationality?
library(polycor)
wage_nation_variables <- c(6,10,13,22)
wage_nation <- fifa %>% select(wage_nation_variables)
colnames(wage_nation)
wage_nation$Nationality <- as.factor(as.character(wage_nation$Nationality))
wage_nation$Wage <- gsub(find.string, replacement = "", wage_nation$Wage)
wage_nation$Wage <- as.numeric(as.character(wage_nation$Wage))

# Statistical Tests for Correlation
hetcor(wage_nation)
chisq.test(table(wage_nation$Nationality, wage_nation$Wage))

# Visualization for Question 2
ggplot(wage_nation, aes(wage_score$Wage, Nationality, color = Position)) +
  geom_jitter(show.legend = TRUE, size = 1, alpha = 0.75) + 
  scale_x_continuous(name = 'Wages', seq(0, 500, 25)) +
  scale_y_discrete() +
  theme(text = element_text(size = 7.5), axis.text.x = element_text(angle =90, hjust = 0.5)) 

# Question 3 - Does there exist a correlation between a player's overall score and wages?
library(corrplot)
library(corrgram)
library(corrr)
wage_score_variables <- c(4,8,6,12,13,22)
wage_score <- fifa %>% select(wage_score_variables)
colnames(wage_score)
wage_score$Wage <- gsub(find.string, replacement = "", wage_score$Wage)
wage_score$Wage <- as.numeric(as.character(wage_score$Wage))
wage_score$Value <- gsub(find.string2, replacement = "", wage_score$Value)
wage_score$Value <- as.numeric(as.character(wage_score$Value))

correlate(wage_score)
cor(wage_score)
corrplot(corrgram(wage_score))
pairs(wage_score)

# Visualization for Question 3
ggplot(wage_score, aes(wage_score$Overall, wage_score$Wage, color = Age)) +
  geom_jitter(show.legend = TRUE, size = 1, alpha = 0.75) + 
  scale_x_continuous(name = 'Overall Score', seq(0, 100, 5)) +
  scale_y_discrete(name = 'Wages in Thousands', seq(0, 500, 50)) +
  theme(text = element_text(size = 7.5), axis.text.x = element_text(angle =90, hjust = 0.5)) 


# Question 4 - Does there exist a correlation between player's height, weight, and score?
# Question 5 - Does these exist a correlation between player's height, weight, and wages?

library(rpart)
library(rpart.plot)
overall_height_weight <- na.omit(overall_height_weight)
which(is.na(overall_height_weight))
overall_height_weight <- select(fifa, 'Overall', 'Height', 'Weight')
overall_height_weight$Height <- gsub("[']", ".", overall_height_weight$Height)
overall_height_weight$Height <- as.numeric(as.character(overall_height_weight$Height))
overall_height_weight$Weight <- as.numeric(gsub("lbs", "", as.character(overall_height_weight$Weight)))
str(overall_height_weight)

overall_tree <- rpart(overall_height_weight$Overall ~ 
                overall_height_weight$Height + 
                overall_height_weight$Weight, method="class", data = overall_height_weight) 
printcp(overall_tree)

# Train Test split
library(caTools)
set.seed(123)
q4 <- sample.split(overall_height_weight$Overall, SplitRatio = 0.7) 
train <- subset(overall_height_weight, q4 == TRUE) 
test <- subset(overall_height_weight, q4 == FALSE) 

mean(overall_height_weight$Overall) # 66.2387 Overall Score

regressor = lm(formula = Overall ~ ., data = train)
summary(regressor)
predict(regressor)
  
# Correlation Plot
library(corrplot)
cor(overall_height_weight)
D1 <- scale(overall_height_weight, center = TRUE)
pca <- prcomp(D1, scale = TRUE)
summary(pca)
corrplot(corrgram(overall_height_weight))

# Visualizing Player's Overall Score based on Height and Weight, Wages
library(ggplot2)
ggplot(overall_height_weight, aes(Weight, Height, color = wage_score$Wage)) +
  geom_jitter(size = 0.75, alpha = 0.70) +
  scale_y_continuous(name = 'Height', seq(0, 7, .5)) +
  scale_x_continuous(name = 'Weight', seq(0, 250, 25)) +
  geom_smooth(color = 'black') +
  scale_color_continuous(low = "orange", high = "blue")

correlate(overall_height_weight)  
corrplot(corrgram(overall_height_weight))


