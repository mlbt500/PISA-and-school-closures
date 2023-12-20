library(readxl)
library(httr)
library(tidyr)

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
