library(readxl)
library(httr)
library(tidyr)
library(dplyr)
# URL of the file
UNESCO_link <- "https://covid19.uis.unesco.org/wp-content/uploads/sites/11/2021/05/UNESCO_school_closures_database.xlsx"

# Create a temporary file
temp_file <- tempfile(fileext = ".xlsx")

# Download the file
GET(url = UNESCO_link, write_disk(temp_file, overwrite = TRUE))

# Read the file
data <- read_excel(temp_file)

# Optional: Remove the temporary file if not needed anymore
unlink(temp_file)

#read pisa maths files
pisa <- read_excel("PISA2.xls")
pisa <- pisa[-c(1:8),]
pisa_names <- pisa[1,]
pisa <- pisa[-1,]
colnames(pisa) <- pisa_names
pisa <- pisa %>%
  fill("Year/Study", .direction = "down")
pisa$`Year/Study` <- as.numeric(pisa$`Year/Study`)
pisa$Average <- as.numeric(pisa$Average)


# Assuming 'pisa' is the data frame provided by you
# Filtering the data for the years 2022 and 2018
pisa_2022 <- filter(pisa, `Year/Study` == 2022)
pisa_2018 <- filter(pisa, `Year/Study` == 2018)

# Merging the 2022 and 2018 data on 'Jurisdiction'
merged_data <- inner_join(pisa_2022, pisa_2018, by = "Jurisdiction", suffix = c("_2022", "_2018"))

# Calculating the difference
merged_data$difference <- merged_data$Average_2022 - merged_data$Average_2018

# Creating the new data frame with required columns
pisa_difference <- select(merged_data, country = Jurisdiction, difference)

#filter for Western European countries, North America, developed east Asian countries

countries_list <- c(
  "Austria", "Belgium", "Denmark", "Finland", "France", "Germany", 
  "Greece", "Iceland", "Ireland", "Italy", "Luxembourg", "Netherlands", 
  "Norway", "Portugal", "Spain", "Sweden", "Switzerland", "United Kingdom",
  "Hong Kong (China)", "Korea", "Japan", "Singapore", "Australia", 
  "New Zealand", "United States"
)

pisa_difference_developed <- pisa_difference[pisa_difference$country %in% countries_list,]
pisa_difference_developed

#pisa
# Filtering the data for the specified countries
data$Country[data$Country == "United Kingdom of Great Britain and Northern Ireland"]<- "United Kingdom"
data$Country[data$Country == "United States of America"] <- "United States"
data$Country[data$Country == "Hong Kong"] <- "Hong Kong (China)"
filtered_data <- filter(data, Country %in% countries_list)

# Summarizing the number of each status for each country
status_summary <- filtered_data %>%
  group_by(Country, Status) %>%
  summarise(Value = n(), .groups = 'drop')

status_summary
pisa_difference_developed

# Assuming status_summary and pisa_difference_developed are your data frames

# Filter status_summary for 'Fully open' status
fully_open_df <- status_summary[status_summary$Status == "Fully open", ]

# Merge the data frames on the country column
# Ensure that country names are consistently formatted in both data frames
merged_df <- merge(fully_open_df, pisa_difference_developed, by.x = "Country", by.y = "country", all = TRUE)

# Display the first few rows of the merged dataframe
head(merged_df)

plot(merged_df$Value ~ merged_df$difference, main = "Days fully open versus difference in pisa scores", 
     xlab = "Pisa difference", ylab = "Days fully open", pch = 19)
abline(lm(merged_df$Value ~ merged_df$difference), col = "red")


clean_data <- na.omit(merged_df[, c("Value", "difference")])
cor_coefficient <- cor(clean_data$Value, clean_data$difference)
print(cor_coefficient)

# Fit a linear model
model <- lm(Value ~ difference, data = clean_data)

# Summary of the model
summary_model <- summary(model)

# Print the R-squared value
print(summary_model$r.squared)

# Plotting the scatter plot
plot(merged_df$Value ~ merged_df$difference, 
     main = "Days Fully Open versus Difference in PISA Scores", 
     xlab = "PISA Difference", 
     ylab = "Days Fully Open", 
     pch = 19)

# Adding a regression line
abline(lm(merged_df$Value ~ merged_df$difference), col = "red")

# Adding labels
text(merged_df$difference, merged_df$Value, labels = merged_df$Country, pos = 4, cex = 0.7)

# Exporting the data frame to a CSV file
write.csv(merged_df, "pisa_school_closure.csv", row.names = FALSE)
