# Business Coaching Program in R 

# Set up package 
library(ggplot2)
library(dplyr)

# Read in data 
coaching_df <- read.csv("coaching_data.csv")

# Calculate weekly averages for each participant
weekly_averages_df <- coaching_df %>%
  group_by(participant_id) %>%
  summarise(
    avg_hours = round(mean(hours_spent_working),2)
  )
  
# Create chart for weekly averages 
ggplot(data = weekly_averages_df, aes(x = participant_id, y = avg_hours)) +
  geom_bar(stat="identity", fill = "blue") +
  labs(title="Weekly Average Hours Spent Working by Participant", x="Participant ID", y="Average Hours")

# Calculate total hours spent for each participant
total_hours_df <- coaching_df %>%
  group_by(participant_id) %>%
  summarise(
    total_hours = sum(hours_spent_working)
  )

# Create chart for total hours 
ggplot(data = total_hours_df, aes(x = participant_id, y = total_hours)) +
  geom_bar(stat="identity", fill = "blue") +
  labs(title="Total Hours Spent Working by Participant", x="Participant ID", y="Total Hours")
 
# Calculate average performance improvement for each participant
improvement_df <- coaching_df %>%
  group_by(participant_id) %>%
  summarise(
    avg_improvement = round(mean(performance_improvement),2)
  )
  
# Create chart for average performance improvement 
ggplot(data = improvement_df, aes(x = participant_id, y = avg_improvement)) +
  geom_bar(stat="identity", fill = "blue") +
  labs(title="Average Performance Improvement by Participant", x="Participant ID", y="Average Improvement")
 
# Calculate total number of tasks completed by each participant
tasks_completed_df <- coaching_df %>%
  group_by(participant_id) %>%
  summarise(
    tasks_completed = sum(tasks_completed)
  )
  
# Create chart for total number of tasks completed
ggplot(data = tasks_completed_df, aes(x = participant_id, y = tasks_completed)) + 
  geom_bar(stat="identity", fill = "blue") +
  labs(title="Total Number of Tasks Completed by Participant", x="Participant ID", y="Total Tasks Completed")

# Calculate average hours of sleep per night for each participant
sleep_df <- coaching_df %>%
  group_by(participant_id) %>%
  summarise(
    avg_sleep = round(mean(hours_of_sleep),2)
  )

# Create chart for average hours of sleep
ggplot(data = sleep_df, aes(x = participant_id, y = avg_sleep)) +
  geom_bar(stat="identity", fill = "blue") +
  labs(title="Average Hours of Sleep per Night by Participant", x="Participant ID", y="Average Hours of Sleep")

# Calculate average weekly goal rating for each participant
goal_rating_df <- coaching_df %>%
  group_by(participant_id) %>%
  summarise(
    avg_rating = round(mean(weekly_goal_rating),2)
  )
  
# Create chart for average goal rating 
ggplot(data = goal_rating_df, aes(x = participant_id, y = avg_rating)) +
  geom_bar(stat="identity", fill = "blue") +
  labs(title="Average Weekly Goal Rating by Participant", x="Participant ID", y="Average Rating")
 
# Calculate average amount of time spent on leisure activities by each participant
leisure_df <- coaching_df %>%
  group_by(participant_id) %>%
  summarise(
    avg_leisure = round(mean(time_on_leisure_activities),2)
  )

# Create chart for average amount of time spent on leisure activities
ggplot(data = leisure_df, aes(x = participant_id, y = avg_leisure)) + 
  geom_bar(stat="identity", fill = "blue") +
  labs(title="Average Time Spent on Leisure Activities by Participant", x="Participant ID", y="Average Leisure Time")

# Calculate total amount of time spent on leisure activities by each participant
total_leisure_df <- coaching_df %>%
  group_by(participant_id) %>%
  summarise(
    total_leisure = sum(time_on_leisure_activites)
  )

# Create chart for total amount of time spent on leisure activities
ggplot(data = total_leisure_df, aes(x = participant_id, y = total_leisure)) + 
  geom_bar(stat="identity", fill = "blue") +
  labs(title="Total Time Spent on Leisure Activities by Participant", x="Participant ID", y="Total Leisure Time")
 
# Calculate average stress level for each participant
stress_df <- coaching_df %>%
  group_by(participant_id) %>%
  summarise(
    avg_stress = round(mean(self_reported_stress_level),2)
  )

# Create chart for average stress level
ggplot(data = stress_df, aes(x = participant_id, y = avg_stress)) +
  geom_bar(stat="identity", fill = "blue") +
  labs(title="Average Self-Reported Stress Level by Participant", x="Participant ID", y="Average Stress Level")
 
# Calculate total amount of time spent on leisure activities by each participant
total_leisure_df <- coaching_df %>%
  group_by(participant_id) %>%
  summarise(
    total_leisure = sum(time_on_leisure_activites)
  )

# Create chart for total amount of time spent on leisure activities
ggplot(data = total_leisure_df, aes(x = participant_id, y = total_leisure)) + 
  geom_bar(stat="identity", fill = "blue") +
  labs(title="Total Time Spent on Leisure Activities by Participant", x="Participant ID", y="Total Leisure Time")

# Generate a weekly email report for each participant 
# with summarized data on the eight metrics 
# Add in code to generate weekly email report

 # Generate the weekly summary data 
summary_df <- coaching_df %>% 
  group_by(participant_id) %>% 
  summarise(
    avg_hours = round(mean(hours_spent_working),2), 
    total_hours = sum(hours_spent_working), 
    avg_improvement = round(mean(performance_improvement),2), 
    tasks_completed = sum(tasks_completed), 
    avg_sleep = round(mean(hours_of_sleep),2), 
    avg_rating = round(mean(weekly_goal_rating),2), 
    avg_leisure = round(mean(time_on_leisure_activities),2), 
    total_leisure = sum(time_on_leisure_activites), 
    avg_stress = round(mean(self_reported_stress_level),2) 
  )
  
# Compile data into a string for easy visualization 
summary_string <- summary_df %>% 
  summarise(
    summary_data = paste(
      "Average Hours Spent Working: ", avg_hours, 
      "\n Total Hours Spent Working: ", total_hours, 
      "\n Average Performance Improvement: ", avg_improvement, 
      "\n Total Number of Tasks Completed: ", tasks_completed, 
      "\n Average Hours of Sleep per Night: ", avg_sleep, 
      "\n Average Weekly Goal Rating: ", avg_rating, 
      "\n Average Time Spent on Leisure Activities: ", avg_leisure, 
      "\n Total Time Spent on Leisure Activities: ", total_leisure, 
      "\n Average Self-Reported Stress Level: ", avg_stress)
  )

# Loop through summary data and send email 
for (i in 1:nrow(summary_string)) {
  participant <- summary_string[i,1]
  summary_data <- summary_string[i,2]
  
  # Put code here to send email with subject line 
  # "Weekly Coaching Summary for x" 
  # and body of email containing the summary_data
}