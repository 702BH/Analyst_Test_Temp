# Data Analyst Test Script

# Load Libraries
library(tidyverse)
library(jsonlite)


# Read in data
data <- read.csv("DataAnalyst_TestTask_dataset.csv", header = TRUE)


# Parse the JSON column
df_parsed <- data %>%
  mutate(customDimensions = map(customDimensions, ~parse_json(.) %>% as.data.frame() %>% select(isPortrait,spinMethod, stopMethod))) %>%
  unnest(customDimensions)


# Calculate KPIs

# which country has the largest number of users
number_of_users <- df_parsed %>%
  select(user_Id, client_CountryOrRegion) %>%
  group_by(client_CountryOrRegion) %>%
  summarise(user_count = n_distinct(user_Id))


# Users in which country are more engaged (larger number of spins per user)
engagement <- df_parsed %>%
  select(user_Id, client_CountryOrRegion) %>%
  group_by(client_CountryOrRegion) %>%
  summarise(user_count = n_distinct(user_Id),
            spin_count = n(),
            spin_per_user = spin_count / user_count) %>%
  select(client_CountryOrRegion, spin_per_user)
test_engage2$spin_per_user <- round(test_engage2$spin_per_user, 1)


# Join the two tables
count_engage_join <- inner_join(number_of_users, engagement, by = "client_CountryOrRegion") %>%
  arrange(desc(spin_per_user))

# Write this table to be used in RShiny
write.csv(count_engage_join, "user_count_engagement.csv", row.names = FALSE)



# For the KPI calculations below, I have structured the code in a way that it can be easily turned into a function to automate
# these data preparation steps

# Most popular device models in each of the countries
# Identifying which users used more than one model
models_user_count <- df_parsed %>%
  select(user_Id, client_Model) %>%
  group_by(user_Id) %>%
  summarise(model_count = n_distinct(client_Model)) 

models_user_count

which(models_user_count$model_count > 1)

# some users did use more than one model, this can complicate calculating this KPI
# In this instance, if a user used more than one device, both devices will be added

model_counts <- df_parsed %>%
  select(user_Id, client_Model, client_CountryOrRegion) %>%
  group_by(client_CountryOrRegion, user_Id, client_Model) %>%
  distinct(client_Model) %>%
  ungroup() %>%
  group_by(client_CountryOrRegion, client_Model) %>%
  summarise(count = n()) %>%
  arrange(client_CountryOrRegion, desc(count)) %>%
  slice(1:5)


# Most popular operating systems in each of the countries
os_count <- df_parsed %>%
  select(user_Id, client_OS, client_CountryOrRegion) %>%
  group_by(client_CountryOrRegion, user_Id, client_OS) %>%
  distinct(client_OS) %>%
  ungroup() %>%
  group_by(client_CountryOrRegion, client_OS) %>%
  summarise(count = n()) %>%
  arrange(client_CountryOrRegion, desc(count)) %>%
  slice(1:5)


# most popular browsers in each of the countries?
browser_count <- df_parsed %>%
  select(user_Id, client_Browser, client_CountryOrRegion) %>%
  group_by(client_CountryOrRegion, user_Id, client_Browser) %>%
  distinct(client_Browser) %>%
  ungroup() %>%
  group_by(client_CountryOrRegion, client_Browser) %>%
  summarise(count = n()) %>%
  arrange(client_CountryOrRegion, desc(count)) %>%
  slice(1:5)


# Most popular mode (portrait and landscape mode) in each of the countries
# Create new variables with more useful descriptions
mode_count <- df_parsed %>%
  select(user_Id, isPortrait, client_CountryOrRegion) %>%
  group_by(client_CountryOrRegion, user_Id, isPortrait) %>%
  distinct(isPortrait) %>%
  ungroup() %>%
  group_by(client_CountryOrRegion, isPortrait) %>%
  summarise(count = n()) %>%
  arrange(client_CountryOrRegion, desc(count)) %>%
  mutate(spin_play_mode = case_when(
    isPortrait == "true" ~ "Portrait",
    TRUE ~ "Landscape"
  )) %>%
  select(-isPortrait)

# most popular spin Method in each of the countries
spin_count <- df_parsed %>%
  select(user_Id, spinMethod, client_CountryOrRegion) %>%
  group_by(client_CountryOrRegion, user_Id, spinMethod) %>%
  distinct(spinMethod) %>%
  ungroup() %>%
  group_by(client_CountryOrRegion, spinMethod) %>%
  summarise(count = n()) %>%
  arrange(client_CountryOrRegion, desc(count))


# Most popular stop method in each of the countries
stop_count <- df_parsed %>%
  select(user_Id, stopMethod, client_CountryOrRegion) %>%
  group_by(client_CountryOrRegion, user_Id, stopMethod) %>%
  distinct(stopMethod) %>%
  ungroup() %>%
  group_by(client_CountryOrRegion, stopMethod) %>%
  summarise(count = n()) %>%
  arrange(client_CountryOrRegion, desc(count))



# Pivoting the data and binding the rows
# Again, this code can easily be transfered into a function
model_pivot <- pivot_longer(final_answer_model, 2, names_to = "variable", values_to = "subvariable")

os_pivot <- pivot_longer(model_counts, 2, names_to = "variable", values_to = "subvariable")

browser_pivot <- pivot_longer(browser_count, 2, names_to = "variable", values_to = "subvariable")

mode_pivot <- pivot_longer(mode_count, 3, names_to = "variable", values_to = "subvariable")

spin_pivot <- pivot_longer(spin_count, 2, names_to = "variable", values_to = "subvariable")

stop_pivot <- pivot_longer(stop_count, 2, names_to = "variable", values_to = "subvariable")

final_bind <- bind_rows(model_pivot, os_pivot, browser_pivot, mode_pivot, spin_pivot, stop_pivot)


# Format the subvariable names for easy plotting
# Inserting BR into long strings!
for(i in 1:length(final_bind$subvariable)){
  
  if(str_count(final_bind$subvariable[i], " ") > 0){
    final_bind$label[i] <- str_replace_all(final_bind$subvariable[i], " ", "<br>")
  }else{
    final_bind$label[i] <- final_bind$subvariable[i]
  }
  
}

write.csv(final_bind, "bind_data_2.csv", row.names = FALSE)


