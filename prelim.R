# NCHA-IIIb (Spring 2026, UTK)
#
# Only what's needed to make the data analyzable:
#   find columns -> blank out non-answers -> fix Yes/No coding
#   -> build scores -> check
#
# Saves data/ncha_clean.rds. Load that from now on.
#
# NOT IN THIS DATASET: sex assigned at birth (N3Q67), gender
# identity, sexual orientation (N3Q68), and race/ethnicity (N3Q75).
# Those questions were not collected. The column names jump straight
# from N3Q66R to N3Q69, and from N3Q74A to N3Q76. So I can't run any
# analysis by gender, orientation, or race, and that has to be
# stated as a limitation.

library(haven)
library(dplyr)
library(psych)

dir.create("out",  showWarnings = FALSE)
dir.create("data", showWarnings = FALSE)

raw <- read_sav("NCHA-III WEB SPRING 2026 UNIVERSITY OF TENNESSEE KNOXVILLE.sav")
cat("Rows:", nrow(raw), "Columns:", ncol(raw), "\n")


