# Load necessary libraries
library(dplyr)
library(ggplot2)

#load pisa spreadsheet
study_names <- c("reading", "maths", "science")
for(i in 1:3){
  pisa <- read_excel("PISA.xls", sheet = i)
  pisa <- pisa[-c(1:8),]
  pisa_names <- pisa[1,]
  pisa <- pisa[-1,]
  colnames(pisa) <- pisa_names
  pisa <- pisa %>%
    fill("Year/Study", .direction = "down")
  pisa$`Year/Study` <- as.numeric(pisa$`Year/Study`)
  pisa$Average <- as.numeric(pisa$Average)
  pisa$study_name <- study_names[i]
  assign(paste0("pisa", i), pisa)
}
#combine pisa dataframes
pisa_combined <- bind_rows(pisa1, pisa2, pisa3)

# filter for peer countries
countries_list <- c(
  "Austria", "Belgium", "Denmark", "Finland", "France", "Germany", 
  "Greece", "Iceland", "Ireland", "Italy", "Netherlands", 
  "Norway", "Portugal", "Sweden", "Switzerland", "United Kingdom",
  "Hong Kong (China)", "Korea", "Japan", "Singapore", "Australia", 
  "New Zealand", "United States"
)

pisa_combined1 <- pisa_combined[pisa_combined$Jurisdiction %in% countries_list,]
pisa_combined1 <- pisa_combined1[pisa_combined1$`Year/Study` %in% c(2006, 2009, 2012, 2015, 2018, 2022),]

# Rename 'Year/Study' column to avoid issues with the slash character
maths_data <- pisa_combined1 %>%
  filter(study_name == "maths") %>%
  rename(Year_Study = `Year/Study`)

# Get a list of unique jurisdictions (countries)
jurisdictions <- unique(maths_data$Jurisdiction)

# Initialize a dataframe to store predictions
predictions <- data.frame(Jurisdiction = character(),
                          predicted_2022 = numeric(),
                          stringsAsFactors = FALSE)

# Loop through each jurisdiction and create a model
for(jur in jurisdictions){
  # Filter data for the current jurisdiction
  data_jur <- maths_data %>% filter(Jurisdiction == jur, Year_Study < 2022)
  
  # Fit a linear model
  model <- lm(Average ~ Year_Study, data = data_jur)
  
  # Predict the 2022 score
  predicted_score <- predict(model, newdata = data.frame(Year_Study = 2022))
  
  # Add the prediction to the predictions dataframe
  predictions <- rbind(predictions, data.frame(Jurisdiction = jur, predicted_2022 = predicted_score))
}

# Merge predicted scores with actual 2022 scores
actual_2022_scores <- maths_data %>% filter(Year_Study == 2022)
comparison <- left_join(predictions, actual_2022_scores, by = "Jurisdiction")

# Calculate the difference between actual and predicted scores
comparison <- comparison %>% mutate(difference = Average - predicted_2022)

# Visualize the differences
ggplot(comparison, aes(x = Jurisdiction, y = difference)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Difference between Actual and Predicted PISA Maths Scores in 2022", 
       x = "Country", 
       y = "Score Difference")

# Display the table
print(comparison)

# Rename 'Year/Study' column to avoid issues with the slash character
pisa_combined_renamed <- pisa_combined1 %>%
  rename(Year_Study = `Year/Study`)

# Get a list of unique jurisdictions (countries)
jurisdictions <- unique(pisa_combined_renamed$Jurisdiction)

# Define a function to process and predict scores for a given subject
process_subject <- function(subject_data) {
  predictions_subject <- data.frame(Jurisdiction = character(),
                                    predicted_2022 = numeric(),
                                    stringsAsFactors = FALSE)
  
  for(jur in jurisdictions){
    data_jur <- subject_data %>% filter(Jurisdiction == jur, Year_Study < 2022)
    model <- lm(Average ~ Year_Study, data = data_jur)
    predicted_score <- predict(model, newdata = data.frame(Year_Study = 2022))
    predictions_subject <- rbind(predictions_subject, data.frame(Jurisdiction = jur, predicted_2022 = predicted_score))
  }
  
  # Merge and calculate difference for the subject
  actual_2022_scores_subject <- subject_data %>% filter(Year_Study == 2022)
  comparison_subject <- left_join(predictions_subject, actual_2022_scores_subject, by = "Jurisdiction")
  comparison_subject <- comparison_subject %>% mutate(difference = Average - predicted_2022)
  
  return(comparison_subject)
}

# Process each subject
maths_data <- pisa_combined_renamed %>% filter(study_name == "maths")
comparison_maths <- process_subject(maths_data)

science_data <- pisa_combined_renamed %>% filter(study_name == "science")
comparison_science <- process_subject(science_data)

reading_data <- pisa_combined_renamed %>% filter(study_name == "reading")
comparison_reading <- process_subject(reading_data)

# Combine all subjects for visualization
all_comparison <- bind_rows(
  comparison_maths %>% mutate(subject = "Maths"),
  comparison_science %>% mutate(subject = "Science"),
  comparison_reading %>% mutate(subject = "Reading")
)

# Visualize the differences
ggplot(all_comparison, aes(x = Jurisdiction, y = difference, fill = subject)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Difference between Actual and Predicted PISA Scores in 2022 by Subject", 
       x = "Country", 
       y = "Score Difference",
       fill = "Subject")

# Display the table for maths, science, and reading
print(comparison_maths)
print(comparison_science)
print(comparison_reading)

# Load necessary libraries

# Assuming pisa_combined1 is your data frame and already loaded
# Rename 'Year/Study' column to avoid issues with the slash character
pisa_data <- pisa_combined1 %>%
  rename(Year_Study = `Year/Study`)

# Define a function to process and plot scores for a given subject
plot_subject_scores <- function(subject_name) {
  # Filter data for the specific subject
  subject_data <- pisa_data %>% 
    filter(study_name == subject_name)
  
  # Get list of jurisdictions
  jurisdictions <- unique(subject_data$Jurisdiction)
  
  # Initialize data frame for predicted scores
  predictions <- data.frame(Jurisdiction = character(),
                            Year_Study = numeric(),
                            Average = numeric(),
                            stringsAsFactors = FALSE)
  
  # Loop through each jurisdiction and predict the 2022 score
  for(jur in jurisdictions){
    data_jur <- subject_data %>% filter(Jurisdiction == jur, Year_Study < 2022)
    if(nrow(data_jur) > 1) {
      model <- lm(Average ~ Year_Study, data = data_jur)
      predicted_score <- predict(model, newdata = data.frame(Year_Study = 2022))
      prediction <- data.frame(Jurisdiction = jur, Year_Study = 2022, Average = predicted_score)
      predictions <- bind_rows(predictions, prediction)
    }
  }
  
  # Combine actual and predicted scores
  combined_data <- bind_rows(subject_data, predictions)
  
  # Plot the scores
  ggplot(combined_data, aes(x = Year_Study, y = Average, group = Jurisdiction)) +
    geom_line() +
    geom_point(data = combined_data %>% filter(Year_Study != 2022), shape = 4) +  # Crosses for actual scores
    geom_point(data = predictions, shape = 1) +  # Circle for predicted 2022
    geom_line(data = combined_data %>% filter(Year_Study >= 2018), linetype = "dotted") +
    facet_wrap(~Jurisdiction) +
    theme_minimal() +
    labs(title = paste("PISA Scores Over Time with Predicted 2022 Score -", subject_name),
         x = "Year",
         y = "Average Score")
}

# Plot for each subject
plot_subject_scores("maths")
plot_subject_scores("science")
plot_subject_scores("reading")

