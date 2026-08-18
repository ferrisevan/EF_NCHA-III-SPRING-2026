##read in the NCHA-III excel file
library(readxl)
library(writexl)
library(tidyverse)

df <- read_excel("ef_NCHA-III WEB SPRING 2026 UNIVERSITY OF TENNESSEE KNOXVILLE1.xlsx", 
                 sheet = "NCHA-III WEB SPRING 2026 UNIVER")

##next, remove the school, start date, and end date columns (metadata that does not provide us with any useful insight)
df <- df[ , !(names(df) %in% c("School", "StartDate", "EndDate"))]

##convert the numbers from characters back into numerics and remove the NAs from the file
df_clean <- df %>%
  mutate(
    across(everything(), ~ {
      cleaned <- gsub("[$, ]", "", .)
      num_val <- as.numeric(cleaned)
      as.character(if_else(is.na(num_val), "", as.character(num_val)))
    })
  )

write_xlsx(df_clean, "NCHA-III_tidy.xlsx")