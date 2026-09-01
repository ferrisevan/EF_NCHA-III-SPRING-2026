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

# ---- 1. Find the columns ------------------------------------
# NCHA added an "R" to revised questions one at a time, so a block
# can be mixed (N3Q2A + RN3Q2B/C/D). Searching by pattern finds them
# either way. Stops if the count is wrong, because a score built
# from the wrong questions is wrong.

find_items <- function(pattern, expect) {
  hits <- grep(pattern, names(raw), value = TRUE)
  stopifnot(length(hits) == expect)
  hits
}

flourish <- find_items("^R?N3Q41[A-H]$", 8)   # wellbeing:  8-56
k6i      <- find_items("^R?N3Q44[A-F]$", 6)   # distress:   0-24
ucla     <- find_items("^R?N3Q45[A-C]$", 3)   # loneliness: 3-9
belong   <- find_items("^R?N3Q2[A-D]$",  4)   # belonging:  4-24
safety_i <- find_items("^R?N3Q21[A-D]$", 4)   # safety:     4-16
cdrisc   <- find_items("^R?N3Q42[AB]$",  2)   # resilience: 0-8

# ---- 2. Blank out the non-answers ---------------------------
# Answers that aren't really answers. Left in, they get treated as
# real numbers and mess up analyses.
#
# 99 = "Don't know" on the nine prescription misuse questions. It's
# a label, not a quantity. Only these nine questions have it,
# a blanket version could delete real write-in numbers like
# a weight of 99 lbs.
#
# 5 = "Does not apply" on the safety questions, which run 1-4 from
# unsafe to very safe. Leave the 5 in and those people look like the
# safest students on campus.
#
# 1 = "I didn't do this activity" on the protective behaviors, and
# 6 = "I don't use social media" on N3Q96.

dk    <- grep("^R?N3Q22(E|F1|F2|G|H1|H2|I|J1|J2)$", names(raw), value = TRUE)
prot  <- grep("^R?N3Q17[A-C]$", names(raw), value = TRUE)
socm  <- grep("^R?N3Q96$",      names(raw), value = TRUE)

dat <- raw %>%
  mutate(across(all_of(dk),       ~ replace(., . == 99, NA)),
         across(all_of(safety_i), ~ replace(., . == 5,  NA)),
         across(all_of(prot),     ~ replace(., . == 1,  NA)),
         across(all_of(socm),     ~ replace(., . == 6,  NA)))

# ---- 3. Fix the Yes/No coding -------------------------------
# The survey stores No = 1 and
# Yes = 2. glm() (fits generalized linear models) flat out refuses that. 
# It wants 0 and 1. And even where it runs, 
# everything gets measured against the wrong baseline,
# so the odds ratio I'd report is wrong.
#
# I'm not recoding blind. Two conditions, both have to be true:
#   1. the column is in one of the known Yes/No blocks, and
#   2. every answer in it is actually a 1 or a 2
#
# The second check is what keeps me out of trouble. N3Q63A is Yes/No
# but N3Q63B right next to it is a 1-3 academic impact question, and
# a pattern that's even slightly loose would grab it and silently
# turn a 3-level answer into garbage. Checking the values first
# means that can't happen.

yn_blocks <- paste0("^R?N3Q(19[A-E]|20[A-G]|29[A-L]|23[A-K]|25B[12]|",
                    "33[A-C]|63A[0-9]+|65A[0-9]+|30A|31A|32|39|40|",
                    "54[AB]|55A|64B|22P|77[AB]|81[A-C]|82[A-G])$")

cand <- grep(yn_blocks, names(dat), value = TRUE)

is_12 <- function(x) {
  v <- unique(x[!is.na(x)])
  length(v) > 0 && all(v %in% c(1, 2))
}

yn <- cand[sapply(dat[cand], is_12)]

cat("Yes/No columns recoded to 0/1:", length(yn), "of",
    length(cand), "candidates\n")

dat <- dat %>%
  mutate(across(all_of(yn), ~ case_when(. == 1 ~ 0, . == 2 ~ 1)))

# The substance use grid is the exception. It uses No = 0 and Yes = 3
# because of how that scale is scored. I leave the originals alone
# and add a parallel set of 0/1 copies ending in "_bin", so I have
# whichever one a given analysis needs.

assist <- grep("^R?N3Q22A[0-9]+$", names(dat), value = TRUE)

if (length(assist)) {
  dat <- dat %>%
    mutate(across(all_of(assist),
                  ~ case_when(. == 0 ~ 0, . == 3 ~ 1),
                  .names = "{.col}_bin"))
  cat("ASSIST columns given 0/1 copies:", length(assist), "\n")