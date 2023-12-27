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
pisa_combined1 <- pisa_combined1[pisa_combined1$`Year/Study` %in% c(2009, 2012, 2015, 2018, 2022),]

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
