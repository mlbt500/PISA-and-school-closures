library(dplyr)

study_names <- c("maths", "reading", "science")

# Initialize an empty data frame to store the combined differences
combined_differences <- data.frame()

for(i in 1:3){
  # Read the i-th sheet from the PISA.xls file
  pisa <- read_excel("PISA.xls", sheet = i)
  # Perform the necessary preprocessing
  pisa <- pisa[-c(1:8),]
  pisa_names <- pisa[1,]
  pisa <- pisa[-1,]
  colnames(pisa) <- pisa_names
  pisa <- pisa %>%
    fill("Year/Study", .direction = "down") %>%
    mutate(`Year/Study` = as.numeric(`Year/Study`), 
           Average = as.numeric(Average))
  
  # Filtering the data for the years 2022 and 2018
  pisa_2022 <- filter(pisa, `Year/Study` == 2022)
  pisa_2018 <- filter(pisa, `Year/Study` == 2015)
  
  # Merging the 2022 and 2018 data on 'Jurisdiction'
  merged_data <- inner_join(pisa_2022, pisa_2018, by = "Jurisdiction", suffix = c("_2022", "_2018"))
  
  # Calculating the difference and renaming the column
  merged_data <- merged_data %>%
    mutate(difference = Average_2022 - Average_2018) %>%
    select(Jurisdiction, difference)
  
  colnames(merged_data)[2] <- paste0(study_names[i], "_difference")
  
  # Combining the differences into a single data frame
  if(i == 1){
    combined_differences <- merged_data
  } else {
    combined_differences <- full_join(combined_differences, merged_data, by = "Jurisdiction")
  }
}

# Adding a column for the average difference and sorting by it
combined_differences <- combined_differences %>%
  mutate(average = rowMeans(select(., ends_with("_difference")), na.rm = TRUE)) %>%
  arrange(desc(average))

# The final combined and sorted data frame
combined_differences

#filter for Western Euroepan and other peer countries

countries_list <- c(
  "Austria", "Belgium", "Denmark", "Finland", "France", "Germany", 
  "Greece", "Iceland", "Ireland", "Italy", "Netherlands", 
  "Norway", "Portugal", "Sweden", "Switzerland", "United Kingdom",
  "Hong Kong (China)", "Korea", "Japan", "Singapore", "Australia", 
  "New Zealand", "United States"
)

combined_differences1 <- combined_differences[combined_differences$Jurisdiction %in% countries_list,]

# Assuming combined_differences1 is your data frame
# Calculate the mean for each of the difference columns
avg_math = mean(combined_differences1$maths_difference, na.rm = TRUE)
avg_reading = mean(combined_differences1$reading_difference, na.rm = TRUE)
avg_science = mean(combined_differences1$science_difference, na.rm = TRUE)

# Create a new row with these averages
average_row = data.frame(Jurisdiction = "Average", 
                         maths_difference = avg_math, 
                         reading_difference = avg_reading, 
                         science_difference = avg_science, 
                         average = NA) # 'average' column can be NA or calculated as well

# Append this new row to the combined_differences1 data frame
combined_differences1 <- rbind(combined_differences1, average_row)

# View the updated data frame
print(combined_differences1, n = 24)

# Assuming combined_differences is your data frame
# Select only the difference columns
diff_columns <- combined_differences[, c("maths_difference", "reading_difference", "science_difference")]

# Calculate the correlation matrix, using use = "complete.obs" to handle NA values
correlation_matrix <- cor(diff_columns, use = "complete.obs")

# Print the correlation matrix
print(correlation_matrix)
