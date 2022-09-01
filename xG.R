# 0 Load data and dependencies --------------------------------------------

library(dplyr) # for data cleaning
library(magrittr) # for pipe operator %<>%
library(ggplot2) # graphing

# first csv
data <- read.csv("data/1.csv")

# the remainder
i <- 2
while (i <= length(list.files("data/"))) { # the length arg returns the number of files in data subdirectory
  data <- rbind(data, read.csv(glue::glue("data/{i}.csv")))
  i <- i + 1
}

# Take a peek
head(data)
str(data)
summary(data)

# all-caps fixed variables

RINK_WIDTH <- 25
RINK_LENGTH <- 60.96

# 1 Data cleaning ---------------------------------------------------------

data %<>%
  select(-c(duration, half)) %>% # remove unneeded columns
  mutate(totalID = 1:nrow(data)) # add grand id column for data cleaning purposes

# create temp df
temp_data <- data

# remove double-counted shots (the row below a goal is also counted as a shot on goal)
# if the start and end match that of the row above, then remove the row (per totalID)

for (i in 2:nrow(data)) {
  if (data[i,]$start == data[i-1,]$start && data[i,]$end == data[i-1,]$end) {
    temp_data <- temp_data %>% 
      filter(totalID != i)
  }
}

data <- temp_data %>% 
  select(-totalID) %>% 
  mutate(action = ifelse(action == "Goals", 1, 0), # make goals variable numeric
         pos_y = -1*(pos_y - (RINK_WIDTH/2)),
         pos_x = RINK_LENGTH - pos_x, # 60.96 is length of arena, 3.353 is from end boards to goal line
         hypotenuse = sqrt(pos_x^2 + pos_y^2),
         angle = atan(pos_y/pos_x)) 
rm(temp_data) # remove temp_data object

# 2 xG Model --------------------------------------------------------------

model <- glm(action ~ angle + hypotenuse,
             data = data,
             family = "binomial")
summary(model)

data %<>%
  mutate(xG = predict(model, type = "response"))
write.csv(data, "xG.csv")

# impact of angle on xG
data %>% 
  ggplot(mapping = aes(x = angle, y = xG)) +
  geom_point()

# impact of distance on xG
data %>% 
  ggplot(mapping = aes(x = pos_y, y = xG)) +
  geom_point()

data %>% 
  ggplot(mapping = aes(x = xG, y = action)) +
  geom_point() +
  geom_smooth()
