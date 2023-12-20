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
pisa <- read_excel("PISA.xls")
pisa <- pisa[-c(1:8),]
pisa_names <- pisa[1,]
pisa <- pisa[-1,]
colnames(pisa) <- pisa_names
pisa <- pisa %>%
  fill("Year/Study", .direction = "down")
pisa <- pisa[,-c(306:309)]
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
pisa_difference