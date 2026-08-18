##read in the NCHA-III excel file
library(readxl)
ef_NCHA_III_WEB_SPRING_2026_UNIVERSITY_OF_TENNESSEE_KNOXVILLE1 <- read_excel("ef_NCHA-III WEB SPRING 2026 UNIVERSITY OF TENNESSEE KNOXVILLE1.xlsx")
View(ef_NCHA_III_WEB_SPRING_2026_UNIVERSITY_OF_TENNESSEE_KNOXVILLE1)

##remove the NAs from the file
library(openxlsx)
df <- read_excel("ef_NCHA-III WEB SPRING 2026 UNIVERSITY OF TENNESSEE KNOXVILLE1.xlsx",
                 sheet = "NCHA-III WEB SPRING 2026 UNIVER")
df[] <- lapply(df, as.character)
df[is.na(df)] <- ""
write.xlsx(df, "NCHA-III_tidy.xlsx")

##next, remove the school, start date, and end date columns (useless metadata that does not provide us with any insight)
df <- read_excel("ef_NCHA-III WEB SPRING 2026 UNIVERSITY OF TENNESSEE KNOXVILLE1.xlsx",
                 sheet = "NCHA-III WEB SPRING 2026 UNIVER")
df <- df[ , !(names(df) %in% c("School", "StartDate", "EndDate"))]
df[] <- lapply(df, as.character)
df[is.na(df)] <- ""
write.xlsx(df, "NCHA-III_tidy.xlsx")
View(df)
