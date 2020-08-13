# Introduction_To_R_Hockey
My attempt at following the basic R for hockey guide on Hockey Graphs

library(tidyverse)
library(here)
library(readxl)
library(stringr)
library(dplyr)
library(ggplot2)
library(knitr)

#Load these libraries.Also load in the data we will be using, in this case it will be 4 games for Philly.

phi_tut_data <- read_csv("https://github.com/hockey-graphs/HG_intro_tutorial/blob/master/PHI_tutorial_data.csv?raw=true")

view(phi_tut_data)

summary(phi_tut_data)

names(phi_tut_data)

#Review the data you loaded in. 
#Lets create a new dataset called "goals". To do this we need to filter the event_type to only include goals.

goals <- phi_tut_data %>%
    filter(event_type == "GOAL")

view(goals)

#What if we only wanted to include goals scored at 5v5?

goals_5v5 <-phi_tut_data %>%
    filter(event_type == "GOAL", game_strength_state == "5v5")
view(goals_5v5)

#Or what if we only wanted to see goals scored at 4v5?

special_team_goals <- phi_tut_data %>%
  filter(event_type == "GOAL", game_strength_state != "5v5" )
view(special_team_goals)

#This filter doesn't work because there are other states that are not special teams involved. 

goals_special_teams <- phi_tut_data %>%
   filter(event_type == "GOAL" & 
    (game_strength_state == "5v4" | 
     game_strength_state == "4v5" ))
view(goals_special_teams)

#And if we wanted to look at goals scored at 5v5, 5v4, and 4v5 we could use something like this:

goals_5v5_ST <- phi_tut_data %>%
  filter(event_type == "GOAL", game_strength_state %in% c("5v5", "5v4", "4v5"))
view(goals_5v5_ST)

#Returning to the goals dataset we can use select() to keep or drop certain variables. 

goals_small <- goals %>%
  select(game_id, game_date, event_type, event_detail, event_team, event_player_1)
view(goals_small)

#NOT IN TUTORIAL. I wanted to see if I could figure out a way to create a table which showed only the Flyer's goals and who scored the most in the dataset. 

player_scored <-  goals_small %>%
    filter(event_team == "PHI") %>%
    count(event_team == "PHI",event_player_1) %>% 
    rename(goals = n) %>% 
    arrange(desc(goals))
View(player_scored)

##Mutate
#Lets create a new data frame with the goal variable in mind. This column will either give a 1, if the event type is a GOAL, or a 0, if it is anything else.
  
goal_var <- phi_tut_data %>%
  mutate(goal = ifelse(event_type == "GOAL", 1, 0))
View(goal_var)

#Check the variable creation worked.

sum(goal_var$goal)

#OR 

count(goal_var, event_type)

##Group & Summarize
#what if we’re curious as to which teams scored those goals in which games? To do that, we can use the powerful combination of the group_by() and the summarize() functions. Group_by() allows you to section your data based on one or more variables, and summarize() can apply certain functions to each of those groups separately.
#Use Group_by() and summarize() to find total goals/game

goals_per_game <- goal_var %>%
    group_by(game_id) %>%
    summarize(toal_goals = sum(goal))
View(goals_per_game)

#If we wanted to see which teams scored the goals in the games we can add event_team to our group_by() function:

goals_per_game <- goal_var %>%
    group_by(game_id, event_team) %>%
    summarize(toal_goals = sum(goal))
View(goals_per_game)

#What do we do about those NAs? By adding a filter() and nesting the !is.na inside of it we can keep just the observations that do not have NA.

goals_by_game_team <- goal_var %>%
   filter(!is.na(event_team)) %>%
   group_by(game_id, event_team) %>%
   summarize(goals = sum(goal))
View(goals_by_game_team)
    
#We can add the arrange() function for sorting

goals_by_game_team <- goals_by_game_team %>%
   arrange(desc(goals))
View(goals_by_game_team)

#Now, let's make a chart!

ggplot(data = phi_tut_data) + 
geom_bar(aes(x = event_zone))

#Lets add some color to the chart!

ggplot(data = phi_tut_data) + 
       geom_bar(aes(x = event_zone, fill = event_zone))

#Let's label the Y axis now:

ggplot(data = phi_tut_data) + 
       geom_bar(aes(x = event_zone, fill = event_zone)) + 
       labs(y = "Number of events")

